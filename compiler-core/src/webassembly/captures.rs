// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use ecow::EcoString;
use itertools::Itertools;
use std::{collections::HashSet, iter, sync::Arc};

use crate::{
    ast::{
        ArgNames, AssignName, AssignmentKind, FunctionLiteralKind, Pattern,
        StringPrefixLeftSideAssignment, TypeAst, TypedArg, TypedAssignment, TypedExpr,
        TypedPattern, TypedStatement,
        visit::{
            Visit, visit_typed_assignment, visit_typed_expr_call, visit_typed_expr_fn,
            visit_typed_pattern_assign,
        },
    },
    type_::{Type, TypedCallArg, ValueConstructor, ValueConstructorVariant, error::VariableOrigin},
};
use src_span::SrcSpan;
use vec1::Vec1;

/// A local variable that a function uses but does not bind.
#[derive(Debug, Clone)]
pub(super) struct Capture {
    pub name: EcoString,
    pub type_: Arc<Type>,
    /// Where the variable is bound, outside the function.
    pub definition: SrcSpan,
}

/// The local variables captured by the function `location` with these
/// arguments and body, in visit order.
///
/// A variable is identified by its name and the location where it is bound,
/// so a reference is captured when it is not bound by the function or by a
/// function nested in it. Generated variables (`_pipe`, `_capture`, `_use0`,
/// `_record`) are never captured, as they are bound and used in the same
/// function, and the user can not refer to a name that starts with `_`.
pub(super) fn captures(
    location: SrcSpan,
    arguments: &[TypedArg],
    body: &[TypedStatement],
) -> Vec<Capture> {
    Variables::of(arguments, body).captures(location)
}

/// A reference to a local variable.
pub(super) struct Reference {
    pub name: EcoString,
    pub type_: Arc<Type>,
    /// Where the variable is bound.
    pub definition: SrcSpan,
    /// Whether the reference is the function of a call, see
    /// `local_variable_definition`.
    pub called: bool,
}

/// A function bound to a variable by `let`.
pub(super) struct LetFunction<'a> {
    pub assignment: &'a TypedAssignment,
    /// Whether it is in the body of another function bound by `let`.
    pub nested: bool,
}

/// The local variables that a function body binds and references, and the
/// functions it binds by `let`, collected in a single visit.
#[derive(Default)]
pub(super) struct Variables<'a> {
    /// The variables bound in the body, by definition location and name.
    bindings: HashSet<(SrcSpan, EcoString)>,
    /// The references to local variables, in visit order.
    pub references: Vec<Reference>,
    /// The functions bound by `let`, in source order.
    pub let_functions: Vec<LetFunction<'a>>,
    /// How many functions bound by `let` contain the visit.
    depth: usize,
}

impl<'a> Variables<'a> {
    pub(super) fn of(arguments: &'a [TypedArg], body: &'a [TypedStatement]) -> Self {
        let mut variables = Variables::default();
        for argument in arguments {
            variables.bind_argument(argument);
        }
        for statement in body {
            variables.visit_typed_statement(statement);
        }
        variables
    }

    /// The variables referenced but not bound, see `captures`.
    fn captures(&self, location: SrcSpan) -> Vec<Capture> {
        let mut captured = HashSet::new();
        let mut captures = vec![];
        for reference in &self.references {
            let key = (reference.definition, reference.name.clone());
            if is_generated(&reference.name)
                || self.bindings.contains(&key)
                || captured.contains(&key)
            {
                continue;
            }
            let (definition, name) = key;
            if location.start <= definition.start && definition.end <= location.end {
                panic!("binding of variable {name} at {definition:?} not found in the function");
            }
            let _ = captured.insert((definition, name.clone()));
            captures.push(Capture {
                name,
                type_: reference.type_.clone(),
                definition,
            });
        }
        debug_assert!(
            captures.iter().map(|capture| &capture.name).all_unique(),
            "each captured name denotes a single variable"
        );
        captures
    }

    fn bind(&mut self, name: &EcoString, location: SrcSpan) {
        if !is_generated(name) {
            let _ = self.bindings.insert((location, name.clone()));
        }
    }

    fn bind_argument(&mut self, argument: &TypedArg) {
        match &argument.names {
            ArgNames::Named { name, location }
            | ArgNames::NamedLabelled {
                name,
                name_location: location,
                ..
            } => self.bind(name, *location),
            ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => {}
        }
    }

    fn reference(
        &mut self,
        name: &EcoString,
        type_: &Arc<Type>,
        definition: SrcSpan,
        called: bool,
    ) {
        self.references.push(Reference {
            name: name.clone(),
            type_: type_.clone(),
            definition,
            called,
        });
    }
}

fn is_generated(name: &str) -> bool {
    name.starts_with('_')
}

/// Where the local variable that `expr` names is bound. A call whose function
/// is a local function named this way is direct, and every other reference to
/// it is a value, see `Generator::local_function_of`.
pub(super) fn local_variable_definition(expr: &TypedExpr) -> Option<SrcSpan> {
    if let TypedExpr::Var {
        constructor:
            ValueConstructor {
                variant: ValueConstructorVariant::LocalVariable { location, .. },
                ..
            },
        ..
    } = expr
    {
        Some(*location)
    } else {
        None
    }
}

/// The definitions of the local variables that `body` calls, passes to a
/// call or binds to another variable.
pub(super) fn called_or_passed(body: &[TypedStatement]) -> HashSet<SrcSpan> {
    let mut visitor = CallUses::default();
    for statement in body {
        visitor.visit_typed_statement(statement);
    }
    visitor.definitions
}

#[derive(Default)]
struct CallUses {
    definitions: HashSet<SrcSpan>,
}

impl<'ast> Visit<'ast> for CallUses {
    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        arguments: &'ast [TypedCallArg],
        open_parenthesis: &'ast Option<u32>,
    ) {
        let values = iter::once(fun).chain(arguments.iter().map(|argument| &argument.value));
        self.definitions
            .extend(values.filter_map(local_variable_definition));
        visit_typed_expr_call(self, location, type_, fun, arguments, open_parenthesis);
    }

    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        if let Pattern::Variable { .. } = assignment.pattern {
            self.definitions
                .extend(local_variable_definition(&assignment.value));
        }
        visit_typed_assignment(self, assignment);
    }
}

impl<'ast> Visit<'ast> for Variables<'ast> {
    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        arguments: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<TypeAst>,
    ) {
        for argument in arguments {
            self.bind_argument(argument);
        }
        visit_typed_expr_fn(
            self,
            location,
            type_,
            kind,
            arguments,
            body,
            return_annotation,
        );
    }

    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        let is_function = matches!(assignment.value, TypedExpr::Fn { .. })
            && matches!(assignment.pattern, Pattern::Variable { .. });
        if is_function {
            self.let_functions.push(LetFunction {
                assignment,
                nested: self.depth > 0,
            });
            self.depth += 1;
        }
        visit_typed_assignment(self, assignment);
        if is_function {
            self.depth -= 1;
        }
        // The default visit skips the message
        if let AssignmentKind::Assert {
            message: Some(message),
            ..
        } = &assignment.kind
        {
            self.visit_typed_expr(message);
        }
    }

    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        arguments: &'ast [TypedCallArg],
        open_parenthesis: &'ast Option<u32>,
    ) {
        if let (Some(definition), TypedExpr::Var { name, .. }) =
            (local_variable_definition(fun), fun)
        {
            self.reference(name, &fun.type_(), definition, true);
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
        name: &'ast EcoString,
    ) {
        if let ValueConstructorVariant::LocalVariable { location, .. } = &constructor.variant {
            self.reference(name, &constructor.type_, *location, false);
        }
    }

    fn visit_typed_clause_guard_var(
        &mut self,
        _location: &'ast SrcSpan,
        name: &'ast EcoString,
        type_: &'ast Arc<Type>,
        definition_location: &'ast SrcSpan,
        _origin: &'ast VariableOrigin,
    ) {
        self.reference(name, type_, *definition_location, false);
    }

    fn visit_typed_bit_array_size_variable(
        &mut self,
        _location: &'ast SrcSpan,
        name: &'ast EcoString,
        constructor: &'ast Option<Box<ValueConstructor>>,
        type_: &'ast Arc<Type>,
    ) {
        if let Some(constructor) = constructor
            && let ValueConstructorVariant::LocalVariable { location, .. } = &constructor.variant
        {
            self.reference(name, type_, *location, false);
        }
    }

    fn visit_typed_pattern_variable(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        _type_: &'ast Arc<Type>,
        _origin: &'ast VariableOrigin,
    ) {
        self.bind(name, *location);
    }

    fn visit_typed_pattern_assign(
        &mut self,
        location: &'ast SrcSpan,
        name: &'ast EcoString,
        pattern: &'ast TypedPattern,
    ) {
        self.bind(name, *location);
        visit_typed_pattern_assign(self, location, name, pattern);
    }

    fn visit_typed_pattern_string_prefix(
        &mut self,
        _location: &'ast SrcSpan,
        _left_location: &'ast SrcSpan,
        left_side_assignment: &'ast Option<StringPrefixLeftSideAssignment>,
        right_location: &'ast SrcSpan,
        _left_side_string: &'ast EcoString,
        right_side_assignment: &'ast AssignName,
    ) {
        // The left variable is bound at its name up to the end of the
        // assignment, see the pattern typer
        if let Some(left) = left_side_assignment {
            let location = SrcSpan::new(left.name_start_position, left.location.end);
            self.bind(&left.name, location);
        }
        if let AssignName::Variable(name) = right_side_assignment {
            self.bind(name, *right_location);
        }
    }
}
