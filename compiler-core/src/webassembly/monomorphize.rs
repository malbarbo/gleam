use ecow::EcoString;
use std::{collections::HashMap, sync::Arc};

use crate::{
    ast::{
        AssignmentKind, ClauseGuard, Pattern, SrcSpan, Statement, TypedArg, TypedAssignment,
        TypedClauseGuard, TypedCustomType, TypedExpr, TypedFunction, TypedPattern,
        TypedRecordConstructor, TypedStatement,
        visit::{Visit, visit_typed_assignment, visit_typed_expr_call},
    },
    type_::{self, Type, TypeVar, TypedCallArg},
    webassembly::LocalFunction,
};

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

    fn bound_by_var_cell(&mut self, from: &Arc<Type>, to: &Arc<Type>) {
        match (from.as_ref(), to.as_ref()) {
            (Type::Var { type_ }, _) => {
                let _ = self.map.insert(Arc::as_ptr(type_) as u64, to.clone());
            }
            (
                Type::Named {
                    arguments: from, ..
                },
                Type::Named { arguments: to, .. },
            ) => {
                for (f, t) in from.iter().zip(to) {
                    self.bound_by_var_cell(f, t);
                }
            }
            (Type::Tuple { elements: from }, Type::Tuple { elements: to }) => {
                for (f, t) in from.iter().zip(to) {
                    self.bound_by_var_cell(f, t);
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
                self.bound_by_var_cell(from_return, to_return);
                for (f, t) in from_args.iter().zip(to_args) {
                    self.bound_by_var_cell(f, t);
                }
            }
            (_, _) => {}
        }
    }

    pub(super) fn function_local(
        from: &LocalFunction,
        to: &Arc<Type>,
    ) -> (Vec<TypedArg>, Vec<TypedStatement>) {
        fn propagate_call_mappings(
            map: &mut HashMap<u64, Arc<Type>>,
            statements: &[TypedStatement],
        ) {
            propagate_call_type_vars(map, statements);
        }
        let mut mono = Monomorphizer::new();
        mono.bound_by_var_cell(&from.type_, to);
        propagate_call_mappings(&mut mono.map, &from.body);
        let mut args = from.arguments.clone();
        for arg in &mut args {
            arg.type_ = mono.type_(&arg.type_);
        }
        let mut body = from.body.clone();
        mono.statements(&mut body);
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
        if let Type::Var { type_: cell } = old.as_ref() {
            let ptr_id = Arc::as_ptr(cell) as u64;
            if let Some(mapped) = self.map.get(&ptr_id) {
                return mapped.clone();
            }
        }
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
                record_assignment,
                constructor,
                arguments,
                ..
            } => {
                *type_ = self.type_(type_);
                if let Some(record_assignment) = record_assignment {
                    self.assignment(record_assignment);
                }
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
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. } => {}
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
        }
    }
}

/// Collects original local functions.
/// Preserves Generic type vars for later monomorphization.
pub(super) fn collect_local_functions(
    body: &[TypedStatement],
    parent_id: u32,
) -> HashMap<EcoString, LocalFunction> {
    struct LocalFunctionCollector<'a> {
        map: &'a mut HashMap<EcoString, LocalFunction>,
        parent_id: u32,
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
                && let Pattern::Variable { name, .. } = &assignment.pattern
            {
                let _ = self.map.insert(
                    name.clone(),
                    LocalFunction {
                        location: *location,
                        parent_id: self.parent_id,
                        type_: type_.clone(),
                        arguments: arguments.clone(),
                        body: body.to_vec(),
                    },
                );
            }
            visit_typed_assignment(self, assignment);
        }
    }

    let mut map = HashMap::new();
    let mut collector = LocalFunctionCollector {
        map: &mut map,
        parent_id,
    };
    for statement in body {
        collector.visit_typed_statement(statement);
    }
    map
}

/// Walks Call expressions and propagates Rc-pointer type var mappings
/// from arguments to function parameters. Runs until fixpoint.
fn propagate_call_type_vars(map: &mut HashMap<u64, Arc<Type>>, statements: &[TypedStatement]) {
    struct Propagator<'a> {
        map: &'a mut HashMap<u64, Arc<Type>>,
        changed: &'a mut bool,
    }

    impl<'ast> Visit<'ast> for Propagator<'_> {
        fn visit_typed_expr_call(
            &mut self,
            _location: &'ast SrcSpan,
            _type_: &'ast Arc<Type>,
            fun: &'ast TypedExpr,
            arguments: &'ast [TypedCallArg],
        ) {
            if let Some((params, _)) = fun.type_().fn_types() {
                for (param, arg) in params.iter().zip(arguments) {
                    propagate_between_types(self.map, param, &arg.value.type_(), self.changed);
                }
            }
            visit_typed_expr_call(self, _location, _type_, fun, arguments);
        }
    }

    fn propagate_between_types(
        map: &mut HashMap<u64, Arc<Type>>,
        param: &Arc<Type>,
        arg: &Arc<Type>,
        changed: &mut bool,
    ) {
        match (param.as_ref(), arg.as_ref()) {
            (Type::Var { type_: p }, Type::Var { type_: a }) => {
                let p_ptr = Arc::as_ptr(p) as u64;
                let a_ptr = Arc::as_ptr(a) as u64;
                if !map.contains_key(&p_ptr)
                    && let Some(target) = map.get(&a_ptr).cloned()
                {
                    let _ = map.insert(p_ptr, target);
                    *changed = true;
                }
            }
            (
                Type::Fn {
                    arguments: pa,
                    return_: pr,
                },
                Type::Fn {
                    arguments: aa,
                    return_: ar,
                },
            ) => {
                for (p, a) in pa.iter().zip(aa) {
                    propagate_between_types(map, p, a, changed);
                }
                propagate_between_types(map, pr, ar, changed);
            }
            (Type::Named { arguments: pa, .. }, Type::Named { arguments: aa, .. }) => {
                for (p, a) in pa.iter().zip(aa) {
                    propagate_between_types(map, p, a, changed);
                }
            }
            (Type::Tuple { elements: pe }, Type::Tuple { elements: ae }) => {
                for (p, a) in pe.iter().zip(ae) {
                    propagate_between_types(map, p, a, changed);
                }
            }
            _ => {}
        }
    }

    let mut changed = true;
    while changed {
        changed = false;
        let mut visitor = Propagator {
            map,
            changed: &mut changed,
        };
        for statement in statements {
            visitor.visit_typed_statement(statement);
        }
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
