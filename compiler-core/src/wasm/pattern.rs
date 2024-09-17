use std::sync::Arc;

use ecow::EcoString;
use itertools::Itertools;
use wasm_encoder::Instruction;

use crate::ast::{Pattern, TypedExpr, TypedPattern};

use super::{
    parse_float, parse_integer,
    scope::{Binding, Scope},
    table::{Local, LocalId, LocalStore, SymbolTable},
};

pub struct CompiledPattern {
    pub checks: Vec<Check>,
    pub assignments: Vec<Assignment>,
}

pub enum Check {
    IntegerEquality { local: LocalId, value: i32 },
    FloatEquality { local: LocalId, value: f64 },
}

pub struct Assignment {
    pub name: EcoString,
    pub local: LocalId,
}

pub fn compile_pattern(
    subject: LocalId,
    pat: &TypedPattern,
    table: &SymbolTable,
    env: Arc<Scope>,
    locals: &mut LocalStore,
) -> CompiledPattern {
    match pat {
        Pattern::Int { value, .. } => {
            // Assumes the value is on the stack
            let value = parse_integer(value);
            CompiledPattern {
                checks: vec![Check::IntegerEquality {
                    local: subject,
                    value,
                }],
                assignments: vec![],
            }
        }
        Pattern::Float { value, .. } => {
            let value = parse_float(value);
            CompiledPattern {
                checks: vec![Check::FloatEquality {
                    local: subject,
                    value,
                }],
                assignments: vec![],
            }
        }
        Pattern::Variable { type_, name, .. } => {
            let local_id = locals.new_id();
            locals.insert(
                local_id,
                Local {
                    id: local_id,
                    name: name.clone(),
                    gleam_type: Arc::clone(type_),
                },
            );

            CompiledPattern {
                checks: vec![],
                assignments: vec![Assignment {
                    name: name.clone(),
                    local: local_id,
                }],
            }
        }
        Pattern::Assign { .. } => todo!("Not supported yet"),
        Pattern::Discard { .. } => CompiledPattern {
            checks: vec![],
            assignments: vec![],
        },
        Pattern::Constructor {
            name,
            arguments,
            constructor,
            ..
        } => {
            let type_ = match env.get(&name) {
                Some(Binding::Product(type_id)) => table.products.get(type_id).unwrap(),
                _ => unreachable!("Constructor must be a product"),
            };

            // brb, gonna implement tags
            todo!()
        }
        Pattern::String { .. } => todo!("Strings not implemented yet"),
        Pattern::StringPrefix { .. } => todo!("Strings not implemented yet"),
        Pattern::BitArray { .. } => todo!("BitArrays not implemented yet"),
        Pattern::VarUsage { .. } => todo!("BitArrays not implemented yet"),
        Pattern::List { .. } => todo!("Lists not implemented yet"),
        Pattern::Tuple { .. } => todo!("Tuples not implemented yet"),
        Pattern::Invalid { .. } => unreachable!("Invalid pattern"),
    }
}

type Instructions = Vec<Instruction<'static>>;

pub struct TranslatedPattern {
    pub condition: Instructions,
    pub assignments: Instructions,
    pub bindings: Vec<(EcoString, LocalId)>,
}

pub fn translate_pattern(compiled: CompiledPattern, locals: &LocalStore) -> TranslatedPattern {
    // checks
    let mut cond_expr = vec![];

    if compiled.checks.is_empty() {
        // trivial accept
        cond_expr.push(Instruction::I32Const(1));
    } else {
        // first, concatenate all checks with ands
        let mut first = true;
        for check in compiled.checks {
            match check {
                Check::IntegerEquality { local, value } => {
                    cond_expr.push(Instruction::I32Const(value));
                    cond_expr.push(Instruction::LocalGet(locals.get(local).unwrap().id.id()));
                    cond_expr.push(Instruction::I32Eq);
                }
                Check::FloatEquality { local, value } => {
                    cond_expr.push(Instruction::F64Const(value));
                    cond_expr.push(Instruction::LocalGet(locals.get(local).unwrap().id.id()));
                    cond_expr.push(Instruction::F64Eq);
                }
            }
            if first {
                first = false;
            } else {
                cond_expr.push(Instruction::I32And);
            }
        }
    }

    // assignments
    let mut assign_expr = vec![];
    let mut bindings = vec![];
    for assignment in compiled.assignments.iter() {
        assign_expr.push(Instruction::LocalGet(
            locals.get(assignment.local).unwrap().id.id(),
        ));
        assign_expr.push(Instruction::LocalSet(
            locals.get(assignment.local).unwrap().id.id(),
        ));
        bindings.push((assignment.name.clone(), assignment.local));
    }

    TranslatedPattern {
        condition: cond_expr,
        assignments: assign_expr,
        bindings,
    }
}
