// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `assert = { module, function }` in `[webassembly]`: each `assert` of the
//! package of the program becomes a call of the function with the `assert`
//! in an anonymous function, so a test library can catch its failure, count
//! it and go on to the next `assert`.

use std::sync::Arc;

use ecow::EcoString;
use src_span::SrcSpan;
use vec1::vec1;

use crate::{
    ast::{
        CallArg, FunctionLiteralKind, Import, Module, Statement, TypedAssert, TypedDefinitions,
        TypedExpr, TypedModule, TypedStatement,
    },
    config::WasmFunction,
    type_::{self, ModuleValueConstructor, Type, expression::Purity},
};

/// The function of `assert = { module, function }`, found in its module.
pub(super) struct AssertFunction {
    module: EcoString,
    name: EcoString,
    location: SrcSpan,
    type_: Arc<Type>,
}

impl AssertFunction {
    pub(super) fn new(assert: &WasmFunction, location: SrcSpan) -> Self {
        AssertFunction {
            module: assert.module.clone(),
            name: assert.function.clone(),
            location,
            type_: assert_function_type(),
        }
    }

    pub(super) fn module(&self) -> &EcoString {
        &self.module
    }
}

/// `fn(fn() -> Nil) -> Nil`.
pub(super) fn assert_function_type() -> Arc<Type> {
    type_::fn_(vec![type_::fn_(vec![], type_::nil())], type_::nil())
}

/// A copy of `module`, whose `assert`s `wrap_asserts` can change.
pub(super) fn clone_module(module: &TypedModule) -> TypedModule {
    let definitions = &module.definitions;
    Module {
        name: module.name.clone(),
        documentation: module.documentation.clone(),
        type_info: module.type_info.clone(),
        definitions: TypedDefinitions {
            imports: definitions.imports.clone(),
            constants: definitions.constants.clone(),
            custom_types: definitions.custom_types.clone(),
            type_aliases: definitions.type_aliases.clone(),
            functions: definitions.functions.clone(),
        },
        names: module.names.clone(),
        unused_definition_positions: module.unused_definition_positions.clone(),
    }
}

/// Wraps each `assert` of `module` in a call of `function`. Returns `true`
/// if `module` has an `assert`, `false` otherwise.
pub(super) fn wrap_asserts(module: &mut TypedModule, function: &AssertFunction) -> bool {
    let mut wrapped = false;
    for definition in &mut module.definitions.functions {
        statements(definition.body.iter_mut(), function, &mut wrapped);
    }
    if wrapped {
        // A module calls only the functions of the modules it imports.
        module.definitions.imports.push(Import {
            documentation: None,
            location: SrcSpan::default(),
            module_location: SrcSpan::default(),
            module: function.module.clone(),
            as_name: None,
            unqualified_values: vec![],
            unqualified_types: vec![],
            package: EcoString::new(),
        });
    }
    wrapped
}

fn statements<'a>(
    statements: impl Iterator<Item = &'a mut TypedStatement>,
    function: &AssertFunction,
    wrapped: &mut bool,
) {
    for statement in statements {
        match statement {
            Statement::Expression(expression) => expr(expression, function, wrapped),
            Statement::Assignment(assignment) => {
                expr(&mut assignment.value, function, wrapped);
                if let crate::ast::AssignmentKind::Assert {
                    message: Some(message),
                    ..
                } = &mut assignment.kind
                {
                    expr(message, function, wrapped);
                }
            }
            Statement::Use(use_) => expr(&mut use_.call, function, wrapped),
            Statement::Assert(assert) => {
                expr(&mut assert.value, function, wrapped);
                if let Some(message) = &mut assert.message {
                    expr(message, function, wrapped);
                }
                let call = call(assert.clone(), function);
                *statement = Statement::Expression(call);
                *wrapped = true;
            }
        }
    }
}

fn expr(expression: &mut TypedExpr, function: &AssertFunction, wrapped: &mut bool) {
    let mut go = |expression: &mut TypedExpr| expr(expression, function, wrapped);
    match expression {
        TypedExpr::Block {
            statements: body, ..
        }
        | TypedExpr::Fn { body, .. } => statements(body.iter_mut(), function, wrapped),
        TypedExpr::Pipeline {
            first_value,
            assignments,
            finally,
            ..
        } => {
            go(&mut first_value.value);
            for (assignment, _) in assignments {
                go(&mut assignment.value);
            }
            go(finally);
        }
        TypedExpr::List { elements, tail, .. } => {
            elements.iter_mut().for_each(&mut go);
            if let Some(tail) = tail {
                go(tail);
            }
        }
        TypedExpr::Call { fun, arguments, .. } => {
            go(fun);
            arguments
                .iter_mut()
                .for_each(|argument| go(&mut argument.value));
        }
        TypedExpr::BinOp { left, right, .. } => {
            go(left);
            go(right);
        }
        TypedExpr::Case {
            subjects, clauses, ..
        } => {
            subjects.iter_mut().for_each(&mut go);
            clauses.iter_mut().for_each(|clause| go(&mut clause.then));
        }
        TypedExpr::RecordAccess { record, .. } | TypedExpr::PositionalAccess { record, .. } => {
            go(record)
        }
        TypedExpr::Tuple { elements, .. } => elements.iter_mut().for_each(&mut go),
        TypedExpr::TupleIndex { tuple, .. } => go(tuple),
        TypedExpr::Todo { message, .. } | TypedExpr::Panic { message, .. } => {
            if let Some(message) = message {
                go(message);
            }
        }
        TypedExpr::Echo {
            expression,
            message,
            ..
        } => {
            if let Some(expression) = expression {
                go(expression);
            }
            if let Some(message) = message {
                go(message);
            }
        }
        TypedExpr::BitArray { segments, .. } => {
            segments
                .iter_mut()
                .for_each(|segment| go(&mut segment.value));
        }
        TypedExpr::RecordUpdate {
            updated_record,
            constructor,
            arguments,
            ..
        } => {
            go(updated_record);
            go(constructor);
            arguments
                .iter_mut()
                .for_each(|argument| go(&mut argument.value));
        }
        TypedExpr::NegateBool { value, .. } | TypedExpr::NegateInt { value, .. } => go(value),
        TypedExpr::Int { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::String { .. }
        | TypedExpr::Var { .. }
        | TypedExpr::ModuleSelect { .. }
        | TypedExpr::Invalid { .. } => {}
    }
}

/// `function(fn() { assert })`. The anonymous function has the location of
/// the `assert`, where no other function starts, as the name of a function
/// comes from its location. The `assert` keeps its location, so its failure
/// has the line and the function of the source.
fn call(assert: TypedAssert, function: &AssertFunction) -> TypedExpr {
    let location = assert.location;
    let nil = type_::nil();
    let run = TypedExpr::ModuleSelect {
        location,
        field_start: location.start,
        type_: function.type_.clone(),
        label: function.name.clone(),
        module_name: function.module.clone(),
        module_alias: function
            .module
            .split('/')
            .next_back()
            .unwrap_or_default()
            .into(),
        constructor: ModuleValueConstructor::Fn {
            location: function.location,
            module: function.module.clone(),
            name: function.name.clone(),
            external_erlang: None,
            external_javascript: None,
            field_map: None,
            documentation: None,
            purity: Purity::Impure,
        },
    };
    let assertion = TypedExpr::Fn {
        location,
        type_: type_::fn_(vec![], nil.clone()),
        kind: FunctionLiteralKind::Anonymous { head: location },
        arguments: vec![],
        body: vec1![Statement::Assert(assert)],
        return_annotation: None,
        purity: Purity::Impure,
    };
    TypedExpr::Call {
        location,
        type_: nil,
        fun: Box::new(run),
        arguments: vec![CallArg {
            label: None,
            location,
            value: assertion,
            implicit: None,
        }],
        open_parenthesis: None,
    }
}
