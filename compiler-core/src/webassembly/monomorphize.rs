// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use std::{collections::HashMap, rc::Rc, sync::Arc};

use crate::{
    ast::{
        AssignmentKind, ClauseGuard, Pattern, Statement, TypedArg, TypedAssignment,
        TypedClauseGuard, TypedCustomType, TypedExpr, TypedFunction, TypedPattern,
        TypedRecordConstructor, TypedStatement,
        visit::{Visit, visit_typed_assignment, visit_typed_expr_call},
    },
    type_::{
        self, Type, TypeVar, TypedCallArg, ValueConstructor, ValueConstructorVariant,
        error::VariableOrigin,
    },
    webassembly::LocalFunction,
};
use ecow::EcoString;
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

    /// Binds the type variables of each local function of `body` used with a
    /// single type. A variable not typed where it is bound can be unified
    /// with them, and then it gets that type.
    pub(super) fn bind_local_functions(&mut self, body: &[TypedStatement]) {
        struct UsesCollector {
            definitions: HashMap<SrcSpan, Arc<Type>>,
            uses: HashMap<SrcSpan, Vec<Arc<Type>>>,
        }

        impl<'ast> Visit<'ast> for UsesCollector {
            fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
                if let TypedExpr::Fn { type_, .. } = &assignment.value
                    && let Pattern::Variable { location, .. } = &assignment.pattern
                {
                    let _ = self.definitions.insert(*location, type_.clone());
                }
                visit_typed_assignment(self, assignment);
            }

            fn visit_typed_expr_var(
                &mut self,
                _location: &'ast SrcSpan,
                constructor: &'ast ValueConstructor,
                _name: &'ast EcoString,
            ) {
                if let ValueConstructorVariant::LocalVariable { location, .. } =
                    &constructor.variant
                {
                    self.uses
                        .entry(*location)
                        .or_default()
                        .push(constructor.type_.clone());
                }
            }

            fn visit_typed_clause_guard_var(
                &mut self,
                _location: &'ast SrcSpan,
                _name: &'ast EcoString,
                type_: &'ast Arc<Type>,
                definition_location: &'ast SrcSpan,
                _origin: &'ast VariableOrigin,
            ) {
                self.uses
                    .entry(*definition_location)
                    .or_default()
                    .push(type_.clone());
            }
        }

        let mut collector = UsesCollector {
            definitions: HashMap::new(),
            uses: HashMap::new(),
        };
        for statement in body {
            collector.visit_typed_statement(statement);
        }
        for (definition, type_) in collector.definitions {
            if !self.has_free_variables(&type_) {
                continue;
            }
            let uses = collector.uses.remove(&definition).unwrap_or_default();
            if let [first, rest @ ..] = uses.as_slice()
                && !self.has_free_variables(first)
            {
                let first = self.type_(first);
                if rest.iter().all(|other| self.type_(other).same_as(&first)) {
                    let _ = self.bound(&type_, &first);
                }
            }
        }
    }

    /// Whether `type_` has type variables not bound by this monomorphizer.
    pub(super) fn has_free_variables(&self, type_: &Arc<Type>) -> bool {
        match type_.as_ref() {
            Type::Var { type_ } => match &*type_.borrow() {
                TypeVar::Unbound { id } | TypeVar::Generic { id } => !self.map.contains_key(id),
                TypeVar::Link { type_ } => self.has_free_variables(type_),
            },
            Type::Named { arguments, .. } => arguments
                .iter()
                .any(|argument| self.has_free_variables(argument)),
            Type::Fn { arguments, return_ } => {
                self.has_free_variables(return_)
                    || arguments
                        .iter()
                        .any(|argument| self.has_free_variables(argument))
            }
            Type::Tuple { elements } => elements
                .iter()
                .any(|element| self.has_free_variables(element)),
        }
    }

    pub(super) fn for_local_function(from: &LocalFunction, to: &Arc<Type>) -> Monomorphizer {
        let mut mono = from.enclosing.clone();
        let _ = mono.bound(&from.type_, to);
        mono
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

    pub(super) fn type_(&self, old: &Arc<Type>) -> Arc<Type> {
        if let Some(id) = get_unbound_or_generic_id(old) {
            if let Some(to) = self.map.get(&id) {
                to.clone()
            } else {
                Monomorphizer::with_bound(old, &type_::nil()).type_(old)
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
            TypedExpr::BitArray { .. } => todo!("BitArray expressions are not yet supported"),
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

/// Collects the functions bound to a variable by `let`, keyed by the location
/// of the variable definition. Generic type vars are preserved for later
/// monomorphization. The bodies of the collected functions are not visited,
/// the functions defined there are collected when they are specialized.
pub(super) fn collect_local_functions(
    body: &[TypedStatement],
    specialization: u32,
    enclosing: &Monomorphizer,
) -> HashMap<SrcSpan, LocalFunction> {
    struct LocalFunctionCollector<'a> {
        map: HashMap<SrcSpan, LocalFunction>,
        specialization: u32,
        enclosing: &'a Monomorphizer,
    }

    impl<'ast> Visit<'ast> for LocalFunctionCollector<'_> {
        fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
            if let TypedExpr::Fn {
                location,
                type_,
                arguments,
                body,
                ..
            } = &assignment.value
                && let Pattern::Variable {
                    location: definition,
                    name,
                    ..
                } = &assignment.pattern
            {
                let _ = self.map.insert(
                    *definition,
                    LocalFunction {
                        name: name.clone(),
                        definition: *definition,
                        location: *location,
                        specialization: self.specialization,
                        enclosing: self.enclosing.clone(),
                        type_: type_.clone(),
                        arguments: arguments.clone(),
                        body: body.to_vec(),
                        captures: Rc::default(),
                        values: vec![],
                    },
                );
            } else {
                visit_typed_assignment(self, assignment);
            }
        }
    }

    let mut collector = LocalFunctionCollector {
        map: HashMap::new(),
        specialization,
        enclosing,
    };
    for statement in body {
        collector.visit_typed_statement(statement);
    }
    let mut map = collector.map;

    let mut values = ValuesCollector {
        uses: HashMap::new(),
    };
    for statement in body {
        values.visit_typed_statement(statement);
    }
    for (definition, uses) in values.uses {
        if let Some(info) = map.get_mut(&definition) {
            for type_ in uses {
                if enclosing.has_free_variables(&type_) {
                    continue;
                }
                let type_ = enclosing.type_(&type_);
                if !info.values.iter().any(|other| other.same_as(&type_)) {
                    info.values.push(type_);
                }
            }
        }
    }
    map
}

/// Collects the types with which each local variable is used as a value,
/// that is, not as the function of a call.
struct ValuesCollector {
    uses: HashMap<SrcSpan, Vec<Arc<Type>>>,
}

impl<'ast> Visit<'ast> for ValuesCollector {
    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        arguments: &'ast [TypedCallArg],
        open_parenthesis: &'ast Option<u32>,
    ) {
        if let TypedExpr::Var { .. } = fun {
            for argument in arguments {
                self.visit_typed_call_arg(argument);
            }
        } else {
            visit_typed_expr_call(self, location, type_, fun, arguments, open_parenthesis);
        }
    }

    fn visit_typed_expr_var(
        &mut self,
        _location: &'ast SrcSpan,
        constructor: &'ast ValueConstructor,
        _name: &'ast EcoString,
    ) {
        if let ValueConstructorVariant::LocalVariable { location, .. } = &constructor.variant {
            self.uses
                .entry(*location)
                .or_default()
                .push(constructor.type_.clone());
        }
    }

    fn visit_typed_clause_guard_var(
        &mut self,
        _location: &'ast SrcSpan,
        _name: &'ast EcoString,
        type_: &'ast Arc<Type>,
        definition_location: &'ast SrcSpan,
        _origin: &'ast VariableOrigin,
    ) {
        self.uses
            .entry(*definition_location)
            .or_default()
            .push(type_.clone());
    }
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
