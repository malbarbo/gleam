use ecow::EcoString;
use itertools::Itertools;
use std::{collections::HashSet, sync::Arc};

use crate::{
    ast::{
        ArgNames, AssignName, AssignmentKind, FunctionLiteralKind, StringPrefixLeftSideAssignment,
        TypeAst, TypedArg, TypedAssignment, TypedPattern, TypedStatement,
        visit::{Visit, visit_typed_assignment, visit_typed_expr_fn, visit_typed_pattern_assign},
    },
    type_::{Type, ValueConstructor, ValueConstructorVariant, error::VariableOrigin},
};
use src_span::SrcSpan;
use vec1::Vec1;

/// A local variable that a function uses but does not bind.
#[derive(Debug)]
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
    let mut visit = CapturesVisit::default();
    for argument in arguments {
        visit.bind_argument(argument);
    }
    for statement in body {
        visit.visit_typed_statement(statement);
    }

    let mut captured = HashSet::new();
    let mut captures = vec![];
    for (name, type_, definition) in visit.references {
        let key = (definition, name);
        if visit.bindings.contains(&key) || captured.contains(&key) {
            continue;
        }
        let (definition, name) = key;
        if location.start <= definition.start && definition.end <= location.end {
            panic!("binding of variable {name} at {definition:?} not found in the function");
        }
        let _ = captured.insert((definition, name.clone()));
        captures.push(Capture {
            name,
            type_,
            definition,
        });
    }
    debug_assert!(
        captures.iter().map(|capture| &capture.name).all_unique(),
        "each captured name denotes a single variable"
    );
    captures
}

#[derive(Default)]
struct CapturesVisit {
    /// The variables bound in the function, by definition location and name.
    bindings: HashSet<(SrcSpan, EcoString)>,
    /// The local variables referenced in the function, with their types and
    /// definition locations.
    references: Vec<(EcoString, Arc<Type>, SrcSpan)>,
}

impl CapturesVisit {
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

    fn reference(&mut self, name: &EcoString, type_: &Arc<Type>, definition: SrcSpan) {
        if !is_generated(name) {
            self.references
                .push((name.clone(), type_.clone(), definition));
        }
    }
}

fn is_generated(name: &str) -> bool {
    name.starts_with('_')
}

impl<'ast> Visit<'ast> for CapturesVisit {
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
        visit_typed_assignment(self, assignment);
        // The default visit skips the message
        if let AssignmentKind::Assert {
            message: Some(message),
            ..
        } = &assignment.kind
        {
            self.visit_typed_expr(message);
        }
    }

    fn visit_typed_expr_var(
        &mut self,
        _location: &'ast SrcSpan,
        constructor: &'ast ValueConstructor,
        name: &'ast EcoString,
    ) {
        if let ValueConstructorVariant::LocalVariable { location, .. } = &constructor.variant {
            self.reference(name, &constructor.type_, *location);
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
        self.reference(name, type_, *definition_location);
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
            self.reference(name, type_, *location);
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
