// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use std::{
    cell::OnceCell,
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::Arc,
};

use crate::{
    ast::{
        AssignmentKind, BitArrayOption, ClauseGuard, Constant, Pattern, Statement, TypedArg,
        TypedAssignment, TypedClauseGuard, TypedConstant, TypedCustomType, TypedExpr,
        TypedFunction, TypedPattern, TypedRecordConstructor, TypedStatement,
    },
    type_::{Type, TypeVar},
    webassembly::{
        LocalFunction,
        captures::{Reference, Variables},
        never_type,
    },
};
use src_span::SrcSpan;

#[derive(Clone)]
pub(super) struct Monomorphizer {
    map: HashMap<u64, Arc<Type>>,
}

impl Monomorphizer {
    pub(super) fn new() -> Monomorphizer {
        Monomorphizer {
            map: HashMap::new(),
        }
    }

    pub(super) fn with_bound(from: &Arc<Type>, to: &Arc<Type>) -> Monomorphizer {
        let mut mono = Monomorphizer::new();
        let _ = mono.bound(from, to);
        mono
    }

    /// A monomorphizer that binds each type variable of `type_` to `to`.
    pub(super) fn with_variables_bound(type_: &Arc<Type>, to: &Arc<Type>) -> Monomorphizer {
        let mut ids = HashSet::new();
        Monomorphizer::new().free_variables(type_, &mut ids);
        Monomorphizer {
            map: ids.into_iter().map(|id| (id, to.clone())).collect(),
        }
    }

    pub(super) fn variant_constructor(
        custom_type: &TypedCustomType,
        constructor: &TypedRecordConstructor,
        args: &[Arc<Type>],
    ) -> Vec<Arc<Type>> {
        let mut mono = Monomorphizer::new();

        for (from, to) in custom_type.typed_parameters.iter().zip(args) {
            let _ = mono.bound(from, to);
        }

        constructor
            .arguments
            .iter()
            .map(|arg| mono.type_(&arg.type_))
            .collect()
    }

    fn bound(&mut self, from: &Arc<Type>, to: &Arc<Type>) -> &mut Self {
        match (from.as_ref(), to.as_ref()) {
            (Type::Var { type_ }, _) => match &*type_.borrow() {
                TypeVar::Unbound { id } | TypeVar::Generic { id } => {
                    let _ = self.map.insert(*id, to.clone());
                }
                TypeVar::Link { type_ } => {
                    let _ = self.bound(type_, to);
                }
            },
            (
                Type::Named {
                    arguments: from, ..
                },
                Type::Named { arguments: to, .. },
            ) => {
                for (from, to) in from.iter().zip(to) {
                    let _ = self.bound(from, to);
                }
            }
            (Type::Tuple { elements: from }, Type::Tuple { elements: to }) => {
                for (from, to) in from.iter().zip(to) {
                    let _ = self.bound(from, to);
                }
            }
            (
                Type::Fn {
                    arguments: from_args,
                    return_: from_return,
                },
                Type::Fn {
                    arguments: to_args,
                    return_: to_return,
                },
            ) => {
                let _ = self.bound(from_return, to_return);
                for (from, to) in from_args.iter().zip(to_args) {
                    let _ = self.bound(from, to);
                }
            }
            (from, to) => panic!("unexpected type pair in monomorphizer bound: {from:?}, {to:?}"),
        }
        self
    }

    /// Binds the type variables of each variable bound by `let` in
    /// `variables` used with a single type, like a local function, or a block
    /// that results in one. A variable not typed where it is bound can be
    /// unified with them, and then it gets that type. The type of a use can
    /// depend on the type variables of another variable, like a call in the
    /// body of a local function, so the variables are bound until none is.
    /// Then a use can still have type variables that no other variable can
    /// bind, like the result of a call to a function that never returns, and
    /// the variable is bound with them as `type_` resolves them.
    pub(super) fn bind_lets(&mut self, variables: &Variables<'_>) {
        let mut uses: HashMap<SrcSpan, Vec<&Arc<Type>>> = HashMap::new();
        for reference in &variables.references {
            uses.entry(reference.definition)
                .or_default()
                .push(&reference.type_);
        }
        let mut lets: Vec<_> = variables
            .lets
            .iter()
            .filter_map(|assignment| {
                let Pattern::Variable { location, .. } = &assignment.pattern else {
                    return None;
                };
                Some((*location, assignment.value.type_()))
            })
            .collect();
        let mut unbindable = false;
        loop {
            let count = lets.len();
            // The type variables of each variable not bound yet.
            let pending: Vec<_> = lets
                .iter()
                .map(|(location, type_)| {
                    let mut ids = HashSet::new();
                    self.free_variables(type_, &mut ids);
                    (*location, ids)
                })
                .collect();
            lets.retain(|(location, type_)| {
                if !self.has_free_variables(type_) {
                    return false;
                }
                let uses = uses.get(location).map_or(&[][..], Vec::as_slice);
                let Some(first) = uses.first() else {
                    return true;
                };
                let mut free = HashSet::new();
                for use_ in uses {
                    self.free_variables(use_, &mut free);
                }
                let bindable = free.is_empty()
                    || unbindable
                        && pending
                            .iter()
                            .filter(|(other, _)| other != location)
                            .all(|(_, ids)| ids.is_disjoint(&free));
                if bindable {
                    let first = self.type_(first);
                    if uses.iter().all(|other| self.type_(other).same_as(&first)) {
                        let _ = self.bound(type_, &first);
                        return false;
                    }
                }
                true
            });
            if lets.len() < count {
                unbindable = false;
            } else if unbindable {
                break;
            } else {
                unbindable = true;
            }
        }
    }

    /// A reference of `variables` to a variable that has a different type than
    /// another reference to it. Gleam does not generalise `let`, but a
    /// variable whose type has a type variable of an annotation, like a local
    /// function `fn(x: a) -> a { x }`, is instantiated at each reference. The
    /// code of a value is generated once, so it can be used with only one
    /// type. A reference whose type depends on the type variables of a
    /// variable not bound by `bind_lets`, like a call in the body of a local
    /// function, is not considered.
    pub(super) fn conflicting_reference<'v>(
        &self,
        variables: &'v Variables<'_>,
    ) -> Option<&'v Reference> {
        let mut unbound = HashSet::new();
        for assignment in &variables.lets {
            self.free_variables(&assignment.value.type_(), &mut unbound);
        }
        let mut types: HashMap<SrcSpan, Arc<Type>> = HashMap::new();
        variables
            .references
            .iter()
            .filter(|reference| {
                let mut free = HashSet::new();
                self.free_variables(&reference.type_, &mut free);
                free.is_disjoint(&unbound)
            })
            .find(|reference| {
                let type_ = self.type_(&reference.type_);
                !types
                    .entry(reference.definition)
                    .or_insert_with(|| type_.clone())
                    .same_as(&type_)
            })
    }

    /// Adds to `ids` the type variables of `type_` not bound by this
    /// monomorphizer.
    fn free_variables(&self, type_: &Arc<Type>, ids: &mut HashSet<u64>) {
        match type_.as_ref() {
            Type::Var { type_ } => match &*type_.borrow() {
                TypeVar::Unbound { id } | TypeVar::Generic { id } => {
                    if !self.map.contains_key(id) {
                        let _ = ids.insert(*id);
                    }
                }
                TypeVar::Link { type_ } => self.free_variables(type_, ids),
            },
            Type::Named { arguments, .. } => {
                for argument in arguments {
                    self.free_variables(argument, ids);
                }
            }
            Type::Fn { arguments, return_ } => {
                for argument in arguments {
                    self.free_variables(argument, ids);
                }
                self.free_variables(return_, ids);
            }
            Type::Tuple { elements } => {
                for element in elements {
                    self.free_variables(element, ids);
                }
            }
        }
    }

    /// Whether `type_` has type variables not bound by this monomorphizer.
    pub(super) fn has_free_variables(&self, type_: &Arc<Type>) -> bool {
        let mut ids = HashSet::new();
        self.free_variables(type_, &mut ids);
        !ids.is_empty()
    }

    pub(super) fn function_local(
        &self,
        from: &LocalFunction,
    ) -> (Vec<TypedArg>, Vec<TypedStatement>) {
        let mut args = from.arguments.clone();
        for arg in &mut args {
            arg.type_ = self.type_(&arg.type_);
        }
        let mut body = from.body.clone();
        self.statements(&mut body);
        (args, body)
    }

    pub(super) fn function(&self, function: &TypedFunction) -> TypedFunction {
        let mut function = function.clone();

        function.return_type = self.type_(&function.return_type);

        for arg in &mut function.arguments {
            arg.type_ = self.type_(&arg.type_);
        }

        self.statements(&mut function.body);

        function
    }

    pub(super) fn constant(&self, constant: &TypedConstant) -> TypedConstant {
        let mut constant = constant.clone();
        self.constant_types(&mut constant);
        constant
    }

    fn constant_types(&self, constant: &mut TypedConstant) {
        match constant {
            Constant::Int { .. }
            | Constant::Float { .. }
            | Constant::String { .. }
            | Constant::BitArray { .. }
            | Constant::RecordUpdate { .. }
            | Constant::Invalid { .. }
            | Constant::Todo { .. } => {}
            Constant::Tuple {
                elements, type_, ..
            } => {
                *type_ = self.type_(type_);
                for element in elements {
                    self.constant_types(element);
                }
            }
            Constant::List {
                elements,
                type_,
                tail,
                ..
            } => {
                *type_ = self.type_(type_);
                for element in elements {
                    self.constant_types(element);
                }
                if let Some(tail) = tail {
                    self.constant_types(tail);
                }
            }
            Constant::Record {
                arguments, type_, ..
            } => {
                *type_ = self.type_(type_);
                for argument in arguments.iter_mut().flatten() {
                    self.constant_types(&mut argument.value);
                }
            }
            Constant::Var { type_, .. } => *type_ = self.type_(type_),
            Constant::BinaryOperator {
                left, right, type_, ..
            } => {
                *type_ = self.type_(type_);
                self.constant_types(left);
                self.constant_types(right);
            }
        }
    }

    pub(super) fn type_(&self, old: &Arc<Type>) -> Arc<Type> {
        if let Some(id) = get_unbound_or_generic_id(old) {
            if let Some(to) = self.map.get(&id) {
                to.clone()
            } else {
                Monomorphizer::with_bound(old, &never_type()).type_(old)
            }
        } else if let Some(type_) = old.list_type() {
            Type::list(self.type_(&type_)).into()
        } else if let Some(elements) = old.tuple_types() {
            Type::Tuple {
                elements: elements.iter().map(|element| self.type_(element)).collect(),
            }
            .into()
        } else if let Some((arguments, return_)) = old.fn_types() {
            Type::Fn {
                arguments: arguments
                    .iter()
                    .map(|argument| self.type_(argument))
                    .collect(),
                return_: self.type_(&return_),
            }
            .into()
        } else if let Type::Named {
            publicity,
            package,
            module,
            name,
            arguments,
            inferred_variant,
        } = (**old).clone()
        {
            Type::Named {
                publicity,
                package,
                module,
                name,
                arguments: arguments
                    .iter()
                    .map(|argument| self.type_(argument))
                    .collect(),
                inferred_variant,
            }
            .into()
        } else if let Type::Var { type_ } = old.as_ref()
            && let TypeVar::Link { type_ } = &*type_.borrow()
        {
            self.type_(type_)
        } else {
            old.clone()
        }
    }

    fn statements(&self, statements: &mut [TypedStatement]) {
        for statement in statements {
            self.statement(statement);
        }
    }

    fn statement(&self, statement: &mut TypedStatement) {
        match statement {
            Statement::Expression(expression) => self.expression(expression),
            Statement::Assignment(assignment) => {
                self.assignment(assignment);
            }
            Statement::Assert(assert) => {
                self.expression(&mut assert.value);
                if let Some(message) = &mut assert.message {
                    self.expression(message);
                }
            }
            Statement::Use(use_) => {
                self.expression(&mut use_.call);
            }
        }
    }

    fn assignment(&self, assignment: &mut TypedAssignment) {
        self.expression(&mut assignment.value);
        match &mut assignment.kind {
            AssignmentKind::Let => {}
            AssignmentKind::Generated => {}
            AssignmentKind::Assert { message, .. } => {
                if let Some(message) = message {
                    self.expression(message);
                }
            }
        }
        self.pattern(&mut assignment.pattern);
    }

    fn expressions(&self, expressions: &mut [TypedExpr]) {
        for expression in expressions {
            self.expression(expression);
        }
    }

    fn expression(&self, expression: &mut TypedExpr) {
        match expression {
            TypedExpr::Todo { message, type_, .. } | TypedExpr::Panic { message, type_, .. } => {
                *type_ = self.type_(type_);
                if let Some(message) = message {
                    self.expression(&mut *message);
                }
            }
            TypedExpr::Block { statements, .. } => {
                self.statements(statements);
            }
            TypedExpr::Var { constructor, .. } => {
                constructor.type_ = self.type_(&constructor.type_);
            }
            TypedExpr::Fn {
                type_,
                arguments,
                body,
                ..
            } => {
                *type_ = self.type_(type_);
                for arg in arguments {
                    arg.type_ = self.type_(&arg.type_);
                }
                self.statements(body);
            }
            TypedExpr::Call {
                type_,
                fun,
                arguments,
                ..
            } => {
                *type_ = self.type_(type_);
                self.expression(fun);
                for argument in arguments {
                    self.expression(&mut argument.value);
                }
            }
            TypedExpr::BinOp { right, left, .. } => {
                self.expression(right);
                self.expression(left);
            }
            TypedExpr::List {
                type_,
                elements,
                tail,
                ..
            } => {
                *type_ = self.type_(type_);
                self.expressions(elements);
                if let Some(tail) = tail {
                    self.expression(&mut *tail);
                }
            }
            TypedExpr::Tuple {
                type_, elements, ..
            } => {
                *type_ = self.type_(type_);
                self.expressions(elements);
            }
            TypedExpr::TupleIndex { type_, tuple, .. }
            | TypedExpr::PositionalAccess {
                type_,
                record: tuple,
                ..
            } => {
                *type_ = self.type_(type_);
                self.expression(tuple);
            }
            TypedExpr::Case {
                type_,
                subjects,
                clauses,
                ..
            } => {
                *type_ = self.type_(type_);
                self.expressions(subjects);
                for clause in clauses {
                    self.expression(&mut clause.then);
                    self.patterns(&mut clause.pattern);
                    for patterns in &mut clause.alternative_patterns {
                        self.patterns(patterns);
                    }
                    if let Some(guard) = &mut clause.guard {
                        self.guard(guard);
                    }
                }
            }
            TypedExpr::RecordAccess { type_, record, .. } => {
                *type_ = self.type_(type_);
                self.expression(record);
            }
            TypedExpr::RecordUpdate {
                type_,
                updated_record,
                constructor,
                arguments,
                ..
            } => {
                *type_ = self.type_(type_);
                self.expression(updated_record);
                self.expression(constructor);
                for argument in arguments {
                    self.expression(&mut argument.value);
                }
            }
            TypedExpr::Echo {
                type_,
                expression,
                message,
                ..
            } => {
                *type_ = self.type_(type_);
                if let Some(expression) = expression {
                    self.expression(expression);
                }
                if let Some(message) = message {
                    self.expression(message);
                }
            }
            TypedExpr::Pipeline {
                first_value,
                assignments,
                finally,
                ..
            } => {
                self.expression(&mut first_value.value);
                for (assignment, _) in assignments {
                    self.expression(&mut assignment.value)
                }
                self.expression(finally);
            }
            TypedExpr::NegateBool { value, .. } | TypedExpr::NegateInt { value, .. } => {
                self.expression(value);
            }
            TypedExpr::Int { .. } | TypedExpr::Float { .. } | TypedExpr::String { .. } => {}
            TypedExpr::BitArray {
                type_, segments, ..
            } => {
                *type_ = self.type_(type_);
                for segment in segments {
                    self.expression(&mut segment.value);
                    for option in &mut segment.options {
                        if let BitArrayOption::Size { value, .. } = option {
                            self.expression(value);
                        }
                    }
                }
            }
            TypedExpr::ModuleSelect { type_, .. } => {
                *type_ = self.type_(type_);
            }
            TypedExpr::Invalid { .. } => {
                panic!("invalid expressions should not reach code generation")
            }
        }
    }

    fn patterns(&self, patterns: &mut [TypedPattern]) {
        for pattern in patterns {
            self.pattern(pattern);
        }
    }

    fn pattern(&self, pattern: &mut TypedPattern) {
        match pattern {
            Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::BitArray { .. }
            | Pattern::StringPrefix { .. }
            | Pattern::BitArraySize(_) => {}
            Pattern::Variable { type_, .. }
            | Pattern::Invalid { type_, .. }
            | Pattern::Discard { type_, .. } => {
                *type_ = self.type_(type_);
            }
            Pattern::Constructor {
                type_, arguments, ..
            } => {
                *type_ = self.type_(type_);
                for arg in arguments {
                    self.pattern(&mut arg.value);
                }
            }
            Pattern::Assign { pattern, .. } => {
                self.pattern(pattern);
            }
            Pattern::List {
                type_,
                elements,
                tail,
                ..
            } => {
                *type_ = self.type_(type_);
                self.patterns(elements);
                if let Some(tail) = tail {
                    self.pattern(&mut tail.pattern);
                }
            }
            Pattern::Tuple { elements, .. } => {
                self.patterns(elements);
            }
        }
    }

    fn guard(&self, guard: &mut TypedClauseGuard) {
        match guard {
            ClauseGuard::Constant(_) => {}
            ClauseGuard::Block { value, .. } => self.guard(value),
            ClauseGuard::BinaryOperator { left, right, .. } => {
                self.guard(left);
                self.guard(right);
            }
            ClauseGuard::Not { expression, .. } => self.guard(expression),
            ClauseGuard::Var { type_, .. } => *type_ = self.type_(type_),
            ClauseGuard::TupleIndex { type_, tuple, .. } => {
                *type_ = self.type_(type_);
                self.guard(tuple);
            }
            ClauseGuard::FieldAccess {
                type_, container, ..
            } => {
                *type_ = self.type_(type_);
                self.guard(container);
            }
            ClauseGuard::ModuleSelect { type_, .. } => {
                *type_ = self.type_(type_);
            }
            ClauseGuard::Invalid { .. } => {
                panic!("invalid guards should not reach code generation")
            }
        }
    }
}

/// The functions bound to a variable by `let` in `variables`, keyed by the
/// location of the variable definition. Generic type vars are preserved for
/// later monomorphization. The functions defined in their bodies are not
/// included, they are collected when they are specialized.
pub(super) fn collect_local_functions(
    variables: &Variables<'_>,
    specialization: u32,
    enclosing: &Monomorphizer,
) -> HashMap<SrcSpan, Rc<LocalFunction>> {
    let values: HashSet<SrcSpan> = variables
        .references
        .iter()
        .filter(|reference| !reference.called)
        .map(|reference| reference.definition)
        .collect();
    variables
        .let_functions
        .iter()
        .filter(|function| !function.nested)
        .filter_map(|function| {
            let TypedExpr::Fn {
                location,
                type_,
                arguments,
                body,
                ..
            } = &function.assignment.value
            else {
                return None;
            };
            let Pattern::Variable {
                location: definition,
                name,
                ..
            } = &function.assignment.pattern
            else {
                return None;
            };
            let info = LocalFunction {
                name: name.clone(),
                definition: *definition,
                location: *location,
                specialization,
                enclosing: enclosing.clone(),
                type_: type_.clone(),
                arguments: arguments.clone(),
                body: body.to_vec(),
                captures: OnceCell::new(),
                is_value: values.contains(definition),
            };
            Some((*definition, Rc::new(info)))
        })
        .collect()
}

fn get_unbound_or_generic_id(type_: &Arc<Type>) -> Option<u64> {
    match type_.as_ref() {
        Type::Var { type_: var } => match &*var.borrow() {
            TypeVar::Unbound { id } | TypeVar::Generic { id } => Some(*id),
            TypeVar::Link { type_ } => get_unbound_or_generic_id(type_),
        },
        Type::Named { .. } | Type::Fn { .. } | Type::Tuple { .. } => None,
    }
}

pub(super) fn set_ubound_or_generic(old: &Arc<Type>, new: &Arc<Type>) {
    match old.as_ref() {
        Type::Var { type_ } => {
            if let TypeVar::Link { type_ } = &*type_.borrow() {
                set_ubound_or_generic(type_, new);
            } else {
                *type_.borrow_mut() = TypeVar::Link { type_: new.clone() };
            };
        }
        Type::Named { arguments, .. } => {
            for argument in arguments {
                set_ubound_or_generic(argument, new);
            }
        }
        Type::Fn { arguments, return_ } => {
            for argument in arguments {
                set_ubound_or_generic(argument, new);
            }
            set_ubound_or_generic(return_, new);
        }
        Type::Tuple { elements } => {
            for element in elements {
                set_ubound_or_generic(element, new);
            }
        }
    }
}
