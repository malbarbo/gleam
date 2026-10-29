// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use ecow::EcoString;
use std::{cell::RefCell, collections::HashMap, ptr, rc::Rc, sync::Arc};
use walrus::{FunctionId, GlobalId, LocalId, ValType};

use crate::{
    ast::{
        AssignName, AssignmentKind, BinOp, ClauseGuard, FunctionLiteralKind,
        StringPrefixLeftSideAssignment, TypeAst, TypedArg, TypedAssert, TypedAssignment,
        TypedClause, TypedClauseGuard, TypedExpr, TypedPattern, TypedPipelineAssignment,
        TypedStatement,
        visit::{
            Visit, visit_typed_assert, visit_typed_assignment, visit_typed_clause_guard,
            visit_typed_expr, visit_typed_expr_bin_op, visit_typed_expr_call,
            visit_typed_expr_case, visit_typed_pattern, visit_typed_pipeline_assignment,
        },
    },
    type_::{ModuleValueConstructor, Type, TypedCallArg},
};
use src_span::SrcSpan;
use vec1::Vec1;

use super::{Generator, divisor_is_safe};

#[derive(Clone, PartialEq)]
pub(super) enum IdKind {
    Global,
    Func,
    Local,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum IdIndex {
    Global(GlobalId),
    Func(FunctionId),
    Local(LocalId),
}

#[derive(Clone)]
pub(super) struct Id {
    pub kind: IdKind,
    pub name: EcoString,
    pub index: IdIndex,
}

impl Id {
    pub(super) fn global(name: EcoString, index: GlobalId) -> Id {
        Id {
            kind: IdKind::Global,
            name,
            index: IdIndex::Global(index),
        }
    }

    pub(super) fn func(name: EcoString, index: FunctionId) -> Id {
        Id {
            kind: IdKind::Func,
            name,
            index: IdIndex::Func(index),
        }
    }

    fn local(name: EcoString, index: LocalId) -> Id {
        Id {
            kind: IdKind::Local,
            name,
            index: IdIndex::Local(index),
        }
    }

    pub(super) fn global_id(&self) -> GlobalId {
        match self.index {
            IdIndex::Global(id) => id,
            IdIndex::Func(_) | IdIndex::Local(_) => panic!("expected global id"),
        }
    }

    pub(super) fn func_id(&self) -> FunctionId {
        match self.index {
            IdIndex::Func(id) => id,
            IdIndex::Global(_) | IdIndex::Local(_) => panic!("expected function id"),
        }
    }

    pub(super) fn local_id(&self) -> LocalId {
        match self.index {
            IdIndex::Local(id) => id,
            IdIndex::Global(_) | IdIndex::Func(_) => panic!("expected local id"),
        }
    }
}

pub(super) fn find_global(name: &str, globals: &RefCell<Vec<Id>>) -> Option<Id> {
    globals.borrow().iter().find(|id| id.name == name).cloned()
}

#[derive(Clone)]
pub(super) enum Scope {
    Global(Rc<RefCell<Vec<Id>>>),
    Entry(Id, Rc<Scope>),
}

impl Scope {
    /// The scope `base` extended with the params.
    pub(super) fn with_params(base: Scope, args: &[TypedArg], param_ids: &[LocalId]) -> Scope {
        let mut scope = base;
        for (arg, &index) in args.iter().zip(param_ids) {
            if let Some(name) = arg.get_variable_name() {
                scope = scope.insert_local(name.clone(), index);
            }
        }
        scope
    }

    pub(super) fn insert_local(&self, name: EcoString, index: LocalId) -> Scope {
        Scope::Entry(Id::local(name, index), self.clone().into())
    }

    pub(super) fn find(&self, name: &EcoString) -> Option<Id> {
        match self {
            Scope::Global(globals) => find_global(name, globals),
            Scope::Entry(id, _scope) if &id.name == name => Some(id.clone()),
            Scope::Entry(_id, scope) => scope.find(name),
        }
    }

    /// Finds a local variable, without looking at the globals.
    pub(super) fn find_local(&self, name: &EcoString) -> Option<LocalId> {
        match self {
            Scope::Global(_) => None,
            Scope::Entry(id, _scope) if &id.name == name => Some(id.local_id()),
            Scope::Entry(_id, scope) => scope.find_local(name),
        }
    }

    fn find_expect(&self, name: &EcoString) -> Id {
        self.find(name)
            .unwrap_or_else(|| panic!("variable \"{name}\" to be in scope"))
    }
}

#[derive(Debug)]
pub(super) struct Locals {
    params: Vec<(LocalId, Option<EcoString>)>,
    locals: HashMap<LocalKey, (LocalId, Option<EcoString>)>,
    names: HashMap<EcoString, usize>,
}

impl Locals {
    pub(super) fn new(
        generator: &mut Generator<'_>,
        arguments: &[TypedArg],
        statements: &[TypedStatement],
    ) -> Self {
        let params: Vec<(LocalId, Option<EcoString>)> = arguments
            .iter()
            .map(|arg| {
                let val_type = generator.val_type(&arg.type_);
                let id = generator.wasm_module.locals.add(val_type);
                (id, arg.names.get_variable_name().cloned())
            })
            .collect();

        let mut locals = Locals {
            params,
            locals: HashMap::new(),
            names: HashMap::from_iter(
                arguments
                    .iter()
                    .flat_map(|arg| arg.names.get_variable_name().map(|name| (name.clone(), 1))),
            ),
        };

        let mut visit = LocalsVisit {
            locals: &mut locals,
            generator,
        };

        for statement in statements {
            visit.visit_typed_statement(statement);
        }

        locals
    }

    pub(super) fn param_ids(&self) -> Vec<LocalId> {
        self.params.iter().map(|(id, _)| *id).collect()
    }

    pub(super) fn names(&self) -> Vec<(LocalId, EcoString)> {
        self.params
            .iter()
            .chain(self.locals.values())
            .filter_map(|(index, name)| name.as_ref().map(|n| (*index, n.clone())))
            .collect()
    }

    fn insert_assignment(&mut self, generator: &mut Generator<'_>, assignment: &TypedAssignment) {
        if matches!(assignment.kind, AssignmentKind::Let) && assignment.pattern.is_variable() {
            // use var name
        } else {
            self._insert(
                generator,
                LocalKey::new(LocalKind::Assignment, assignment),
                &assignment.type_(),
                "assignment",
                assignment.location,
            );
        }
    }

    pub(super) fn for_assigment(&self, assignment: &TypedAssignment) -> LocalId {
        if matches!(assignment.kind, AssignmentKind::Let) && assignment.pattern.is_variable() {
            self.for_pattern(&assignment.pattern)
        } else {
            self._get(LocalKey::new(LocalKind::Assignment, assignment))
        }
    }

    /// Operands that are local variables are read from their locals, the
    /// others (like module constants) are stored in new locals.
    fn insert_div(&mut self, generator: &mut Generator<'_>, left: &TypedExpr, right: &TypedExpr) {
        if !left.is_local_var() {
            let key = LocalKey::new(LocalKind::DivOperand, left);
            self._insert(generator, key, &left.type_(), "div_left", left.location());
        } else {
            // use var name
        }
        if !right.is_local_var() {
            self._insert(
                generator,
                LocalKey::new(LocalKind::DivOperand, right),
                &right.type_(),
                "div_right",
                right.location(),
            );
        } else {
            // use var name
        }
    }

    pub(super) fn for_div(
        &self,
        scope: &Scope,
        left: &TypedExpr,
        right: &TypedExpr,
    ) -> (LocalId, LocalId) {
        let left = if let Some(name) = left.var_name()
            && left.is_local_var()
        {
            scope.find_expect(name).local_id()
        } else {
            self._get(LocalKey::new(LocalKind::DivOperand, left))
        };
        let right = if let Some(name) = right.var_name()
            && right.is_local_var()
        {
            scope.find_expect(name).local_id()
        } else {
            self._get(LocalKey::new(LocalKind::DivOperand, right))
        };
        (left, right)
    }

    fn insert_guard_div(
        &mut self,
        generator: &mut Generator<'_>,
        left: &TypedClauseGuard,
        right: &TypedClauseGuard,
    ) {
        if !matches!(left, ClauseGuard::Var { .. }) {
            let key = LocalKey::new(LocalKind::DivOperand, left);
            self._insert(generator, key, &left.type_(), "div_left", left.location());
        }
        if !matches!(right, ClauseGuard::Var { .. }) {
            self._insert(
                generator,
                LocalKey::new(LocalKind::DivOperand, right),
                &right.type_(),
                "div_right",
                right.location(),
            );
        }
    }

    pub(super) fn for_guard_div(
        &self,
        scope: &Scope,
        left: &TypedClauseGuard,
        right: &TypedClauseGuard,
    ) -> (LocalId, LocalId) {
        let left = if let ClauseGuard::Var { name, .. } = left {
            scope.find_expect(name).local_id()
        } else {
            self._get(LocalKey::new(LocalKind::DivOperand, left))
        };
        let right = if let ClauseGuard::Var { name, .. } = right {
            scope.find_expect(name).local_id()
        } else {
            self._get(LocalKey::new(LocalKind::DivOperand, right))
        };
        (left, right)
    }

    fn insert_call(&mut self, generator: &mut Generator<'_>, fun: &TypedExpr) {
        // Calls to a module select of a function or constructor are direct
        // calls, see `Generator::static_callee`.
        let is_static_module_select = matches!(
            fun,
            TypedExpr::ModuleSelect {
                constructor: ModuleValueConstructor::Fn { .. }
                    | ModuleValueConstructor::Record { .. },
                ..
            }
        );
        if matches!(fun, TypedExpr::Fn { .. }) {
            // Called directly, see `Generator::function_literal_call`
        } else if !fun.is_var() && !is_static_module_select {
            let key = LocalKey::new(LocalKind::Callee, fun);
            self._insert(generator, key, &fun.type_(), "fun", fun.location());
        }
    }

    pub(super) fn for_call(&self, fun: &TypedExpr) -> LocalId {
        assert!(!fun.is_var());
        self._get(LocalKey::new(LocalKind::Callee, fun))
    }

    fn insert_subjects(&mut self, generator: &mut Generator<'_>, subjects: &[TypedExpr]) {
        for subject in subjects {
            if subject.is_local_var() && !generator.is_local_function(subject) {
                continue;
            }
            self._insert(
                generator,
                LocalKey::new(LocalKind::Subject, subject),
                &subject.type_(),
                "subject",
                subject.location(),
            );
        }
    }

    pub(super) fn for_subject(&self, subject: &TypedExpr) -> LocalId {
        self._get(LocalKey::new(LocalKind::Subject, subject))
    }

    fn insert_pattern(&mut self, generator: &mut Generator<'_>, pattern: &TypedPattern) {
        let key = LocalKey::new(LocalKind::Pattern, pattern);
        match pattern {
            TypedPattern::Variable { name, .. } => {
                self._insert(generator, key, &pattern.type_(), name, pattern.location());
            }
            TypedPattern::Discard { .. } => {
                self._insert_with_val_type(generator, key, ValType::I32);
            }
            TypedPattern::Int { .. }
            | TypedPattern::Float { .. }
            | TypedPattern::String { .. }
            | TypedPattern::BitArraySize { .. }
            | TypedPattern::Assign { .. }
            | TypedPattern::List { .. }
            | TypedPattern::Constructor { .. }
            | TypedPattern::Tuple { .. }
            | TypedPattern::BitArray { .. }
            | TypedPattern::StringPrefix { .. }
            | TypedPattern::Invalid { .. } => {
                self._insert(
                    generator,
                    key,
                    &pattern.type_(),
                    "pattern",
                    pattern.location(),
                );
            }
        }
    }

    pub(super) fn for_pattern(&self, pattern: &TypedPattern) -> LocalId {
        self._get(LocalKey::new(LocalKind::Pattern, pattern))
    }

    fn insert_pipeline_assignment(
        &mut self,
        generator: &mut Generator<'_>,
        assignment: &TypedPipelineAssignment,
    ) {
        self._insert(
            generator,
            LocalKey::new(LocalKind::PipelineAssignment, assignment),
            &assignment.type_(),
            "pipeline_assignment",
            assignment.location,
        );
    }

    fn insert_record_update(&mut self, generator: &mut Generator<'_>, record: &TypedExpr) {
        self._insert(
            generator,
            LocalKey::new(LocalKind::UpdatedRecord, record),
            &record.type_(),
            "record",
            record.location(),
        );
    }

    pub(super) fn for_record_update(&self, record: &TypedExpr) -> LocalId {
        self._get(LocalKey::new(LocalKind::UpdatedRecord, record))
    }

    pub(super) fn for_pipeline_assignment(&self, assignment: &TypedPipelineAssignment) -> LocalId {
        self._get(LocalKey::new(LocalKind::PipelineAssignment, assignment))
    }

    /// The operands of an asserted expression, a call or a binary operator,
    /// are stored in locals, as the error has their values, see
    /// `Generator::assert`.
    fn insert_assert(&mut self, generator: &mut Generator<'_>, assert: &TypedAssert) {
        for operand in asserted_operands(&assert.value) {
            if !operand.is_var() {
                let prefix = "asserted";
                self._insert(
                    generator,
                    LocalKey::new(LocalKind::Asserted, operand),
                    &operand.type_(),
                    prefix,
                    operand.location(),
                );
            }
        }
    }

    pub(super) fn for_asserted(&self, operand: &TypedExpr) -> LocalId {
        self._get(LocalKey::new(LocalKind::Asserted, operand))
    }

    fn insert_echo(&mut self, generator: &mut Generator<'_>, echo: &TypedExpr) {
        // the number of written bytes
        let key = LocalKey::new(LocalKind::EchoWritten, echo);
        self._insert_with_val_type(generator, key, ValType::I32);
        if !echo.is_var() {
            let key = LocalKey::new(LocalKind::Echo, echo);
            self._insert(generator, key, &echo.type_(), "echo", echo.location());
        }
    }

    pub(super) fn for_echo<'echo>(
        &self,
        echo: &'echo TypedExpr,
    ) -> (LocalId, Result<LocalId, &'echo EcoString>) {
        (
            self._get(LocalKey::new(LocalKind::EchoWritten, echo)),
            if let Some(name) = echo.var_name() {
                Err(name)
            } else {
                Ok(self._get(LocalKey::new(LocalKind::Echo, echo)))
            },
        )
    }

    fn _insert(
        &mut self,
        generator: &mut Generator<'_>,
        key: LocalKey,
        type_: &Arc<Type>,
        prefix: &str,
        location: SrcSpan,
    ) {
        let val_type = generator.val_type(type_);
        let index = generator.wasm_module.locals.add(val_type);
        let lc = generator
            .line_numbers
            .line_and_utf8_column_number(location.start);
        let name: EcoString = format!("{prefix}@{}:{}", lc.line, lc.column).into();
        let name = {
            let count = self.names.entry(name.clone()).or_insert(0);
            *count += 1;
            if *count == 1 {
                name
            } else {
                name + "'".repeat(*count - 1).as_str()
            }
        };
        if self.locals.insert(key, (index, Some(name))).is_some() {
            panic!("locals collision should not happen during code generation");
        }
    }

    fn _insert_with_val_type(
        &mut self,
        generator: &mut Generator<'_>,
        key: LocalKey,
        val_type: ValType,
    ) {
        let index = generator.wasm_module.locals.add(val_type);
        if self.locals.insert(key, (index, None)).is_some() {
            panic!("locals collision should not happen during code generation");
        }
    }

    fn _get(&self, key: LocalKey) -> LocalId {
        self.locals
            .get(&key)
            .unwrap_or_else(|| panic!("local not found during code generation: {key:?}"))
            .0
    }

    /// The local of the string of the left side of a string prefix pattern,
    /// at `left_location`, or of the rest of the string, at `right_location`.
    pub(super) fn for_string_prefix(&self, location: &SrcSpan) -> LocalId {
        self._get(LocalKey::new(LocalKind::StringPrefix, location))
    }
}

struct LocalsVisit<'a, 'b, 'c> {
    locals: &'a mut Locals,
    generator: &'b mut Generator<'c>,
}

impl<'ast, 'a, 'b, 'c> Visit<'ast> for LocalsVisit<'a, 'b, 'c> {
    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        if self
            .generator
            .local_function_definition(assignment)
            .is_none()
        {
            self.locals.insert_assignment(self.generator, assignment);
            visit_typed_assignment(self, assignment);
        }
    }

    fn visit_typed_assert(&mut self, assert: &'ast TypedAssert) {
        self.locals.insert_assert(self.generator, assert);
        visit_typed_assert(self, assert);
    }

    fn visit_typed_expr_fn(
        &mut self,
        _location: &'ast SrcSpan,
        _type_: &'ast Arc<Type>,
        _kind: &'ast FunctionLiteralKind,
        _arguments: &'ast [TypedArg],
        _body: &'ast Vec1<TypedStatement>,
        _return_annotation: &'ast Option<TypeAst>,
    ) {
        // Anonymous functions are generated as separate functions, with
        // their own locals.
    }

    fn visit_typed_expr_bin_op(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        operator: &'ast BinOp,
        operator_start: &'ast u32,
        left: &'ast TypedExpr,
        right: &'ast TypedExpr,
    ) {
        if matches!(
            operator,
            BinOp::DivInt | BinOp::RemainderInt | BinOp::DivFloat
        ) && !divisor_is_safe(operator, right)
        {
            self.locals.insert_div(self.generator, left, right);
        }
        visit_typed_expr_bin_op(self, location, type_, operator, operator_start, left, right);
    }

    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        arguments: &'ast [TypedCallArg],
        open_parenthesis: &'ast Option<u32>,
    ) {
        self.locals.insert_call(self.generator, fun);
        visit_typed_expr_call(self, location, type_, fun, arguments, open_parenthesis);
    }

    fn visit_typed_expr_case(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        subjects: &'ast [TypedExpr],
        clauses: &'ast [TypedClause],
        compiled_case: &'ast crate::exhaustiveness::CompiledCase,
    ) {
        self.locals.insert_subjects(self.generator, subjects);
        visit_typed_expr_case(self, location, type_, subjects, clauses, compiled_case);
    }

    fn visit_typed_clause_guard(&mut self, guard: &'ast TypedClauseGuard) {
        if let ClauseGuard::BinaryOperator {
            operator: BinOp::DivInt | BinOp::RemainderInt | BinOp::DivFloat,
            left,
            right,
            ..
        } = guard
        {
            self.locals.insert_guard_div(self.generator, left, right);
        }
        visit_typed_clause_guard(self, guard);
    }

    fn visit_typed_pattern(&mut self, pattern: &'ast TypedPattern) {
        self.locals.insert_pattern(self.generator, pattern);
        visit_typed_pattern(self, pattern);
    }

    fn visit_typed_pattern_string_prefix(
        &mut self,
        _location: &'ast SrcSpan,
        left_location: &'ast SrcSpan,
        left_side_assignment: &'ast Option<StringPrefixLeftSideAssignment>,
        right_location: &'ast SrcSpan,
        _left_side_string: &'ast EcoString,
        right_side_assignment: &'ast AssignName,
    ) {
        let string_val_type = self.generator.string.val_type();
        let key = |location| LocalKey::new(LocalKind::StringPrefix, location);
        if let AssignName::Variable(_) = right_side_assignment {
            self.locals
                ._insert_with_val_type(self.generator, key(right_location), string_val_type);
        }
        if left_side_assignment.is_some() {
            self.locals
                ._insert_with_val_type(self.generator, key(left_location), string_val_type);
        }
    }

    fn visit_typed_pipeline_assignment(&mut self, assignment: &'ast TypedPipelineAssignment) {
        self.locals
            .insert_pipeline_assignment(self.generator, assignment);
        visit_typed_pipeline_assignment(self, assignment);
    }

    fn visit_typed_expr(&mut self, expr: &'ast TypedExpr) {
        if let TypedExpr::Echo { .. } = expr {
            self.locals.insert_echo(self.generator, expr);
        } else if let TypedExpr::RecordUpdate {
            updated_record,
            updated_record_assigned_name: Some(_),
            ..
        } = expr
        {
            self.locals
                .insert_record_update(self.generator, updated_record);
        }
        visit_typed_expr(self, expr);
    }
}

/// What a local of `Locals` keeps, see `LocalKey`.
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
enum LocalKind {
    Assignment,
    DivOperand,
    Callee,
    Subject,
    Pattern,
    PipelineAssignment,
    UpdatedRecord,
    Asserted,
    Echo,
    /// The number of bytes written by an `echo`.
    EchoWritten,
    StringPrefix,
}

/// The key of a local of `Locals`: what it keeps and the node of the AST it
/// is for, by its address. A node can have locals of different kinds, like
/// an `echo` that is also the subject of a `case`.
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
struct LocalKey {
    kind: LocalKind,
    node: usize,
}

impl LocalKey {
    fn new<T>(kind: LocalKind, node: &T) -> Self {
        Self {
            kind,
            node: ptr::from_ref(node) as usize,
        }
    }
}

/// The operands of the asserted expression `value` whose values the error
/// has: the arguments of a call and the operands of a binary operator other
/// than `&&` and `||`.
pub(super) fn asserted_operands(value: &TypedExpr) -> Vec<&TypedExpr> {
    match value {
        TypedExpr::Call { arguments, .. } => {
            arguments.iter().map(|argument| &argument.value).collect()
        }
        TypedExpr::BinOp {
            operator: BinOp::And | BinOp::Or,
            ..
        } => vec![],
        TypedExpr::BinOp { left, right, .. } => vec![left, right],
        TypedExpr::Int { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::String { .. }
        | TypedExpr::Block { .. }
        | TypedExpr::Pipeline { .. }
        | TypedExpr::Var { .. }
        | TypedExpr::Fn { .. }
        | TypedExpr::List { .. }
        | TypedExpr::Case { .. }
        | TypedExpr::RecordAccess { .. }
        | TypedExpr::PositionalAccess { .. }
        | TypedExpr::ModuleSelect { .. }
        | TypedExpr::Tuple { .. }
        | TypedExpr::TupleIndex { .. }
        | TypedExpr::Todo { .. }
        | TypedExpr::Panic { .. }
        | TypedExpr::Echo { .. }
        | TypedExpr::BitArray { .. }
        | TypedExpr::RecordUpdate { .. }
        | TypedExpr::NegateBool { .. }
        | TypedExpr::NegateInt { .. }
        | TypedExpr::Invalid { .. } => vec![],
    }
}
