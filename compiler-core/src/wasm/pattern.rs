use std::sync::Arc;

use ecow::EcoString;
use wasm_encoder::Instruction;

use crate::{
    analyse::Inferred,
    ast::{Pattern, TypedPattern},
    type_::PatternConstructor,
};

use super::{
    encoder::WasmTypeImpl,
    environment::{Binding, Environment},
    parse_float, parse_integer,
    table::{Local, LocalId, LocalStore, SumId, SymbolTable, TypeId},
};

pub struct CompiledPattern {
    pub checks: Vec<Check>,
    pub assignments: Vec<Assignment>,
    pub next: Option<Box<CompiledPattern>>,
}

pub enum Check {
    IntegerEquality {
        local: LocalId,
        value: i32,
    },
    FloatEquality {
        local: LocalId,
        value: f64,
    },
    ProductEquality {
        subject: LocalId,
        test_sum: SumId,
        discriminant: u32,
    },
}

pub enum Assignment {
    // Reassigns `subject_local` to `target_local` and creates a named binding `name`
    Named {
        name: EcoString,
        subject_local: LocalId,
        target_local: LocalId,
    },

    // Reassigns `subject_local` to `target_local` without creating a binding
    Unnamed {
        subject_local: LocalId,
        target_local: LocalId,
    },

    Cast {
        subject_local: LocalId,
        target_local: LocalId,
        target_type: TypeId,
    },

    /// Assigns a field of a struct to a local variable
    StructField {
        subject_local: LocalId,
        target_local: LocalId,
        field_index: u32,
        struct_type: TypeId,
    },
}

pub fn compile_pattern(
    subject: LocalId,
    pat: &TypedPattern,
    table: &SymbolTable,
    env: &Environment<'_>,
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
                next: None,
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
                next: None,
            }
        }
        Pattern::Variable { type_, name, .. } => {
            let local_id = locals.new_id();
            locals.insert(
                local_id,
                Local {
                    id: local_id,
                    name: name.clone(),
                    wasm_type: WasmTypeImpl::from_gleam_type(Arc::clone(&type_), env, table),
                },
            );

            CompiledPattern {
                checks: vec![],
                assignments: vec![Assignment::Named {
                    name: name.clone(),
                    subject_local: subject,
                    target_local: local_id,
                }],
                next: None,
            }
        }
        Pattern::Assign { .. } => todo!("Not supported yet"),
        Pattern::Discard { .. } => CompiledPattern {
            checks: vec![],
            assignments: vec![],
            next: None,
        },
        // TODO: module name
        Pattern::Constructor {
            constructor:
                Inferred::Known(PatternConstructor {
                    field_map,
                    name: record_name,
                    ..
                }),
            arguments,
            name,
            type_,
            module,
            ..
        } => {
            // get product type
            let product = if let Some(Binding::Product(id)) = env.get(&record_name) {
                table.products.get(id).unwrap()
            } else {
                unreachable!("Invalid product type")
            };

            let sum = table.sums.get(product.parent).unwrap();

            let product_type = table.types.get(product.type_).unwrap();

            // emit discriminant check
            let checks = vec![Check::ProductEquality {
                subject,
                test_sum: sum.id,
                discriminant: product.tag,
            }];

            // if success, create a new local for the cast subject
            let cast_local_id = locals.new_id();
            locals.insert(
                cast_local_id,
                Local {
                    id: cast_local_id,
                    name: name.clone(),
                    wasm_type: WasmTypeImpl::StructRef(product_type.definition.id),
                },
            );

            let mut bindings = vec![Assignment::Cast {
                subject_local: subject,
                target_local: cast_local_id,
                target_type: product_type.id,
            }];

            // chain remaining patterns
            let mut head = None;
            for arg in arguments {
                // TODO: handle other fields in arg struct
                // TODO: what are labels???
                if arg.label.is_none() {
                    // TODO: what do we do here?
                }

                // let label = arg.label.as_ref().unwrap();

                // get the field index
                let field_index = product
                    .fields
                    .iter()
                    .position(|field| &field.name == label)
                    .unwrap();
                let field = &product.fields[field_index];

                // create a new named variable for each argument
                let arg_variable = locals.new_id();
                locals.insert(
                    arg_variable,
                    Local {
                        id: arg_variable,
                        name: label.clone(),
                        wasm_type: field.type_.clone(),
                    },
                );

                // assign the argument to the new variable
                bindings.push(Assignment::StructField {
                    subject_local: subject,
                    target_local: arg_variable,
                    field_index: field_index as u32 + 1,
                    struct_type: product.type_,
                });

                let mut cp = compile_pattern(arg_variable, &arg.value, table, env, locals);
                cp.next = head;
                head = Some(Box::new(cp));
            }

            CompiledPattern {
                checks,
                assignments: bindings,
                next: head,
            }
        }
        Pattern::Constructor {
            constructor: Inferred::Unknown,
            ..
        } => unreachable!("Generating code for uninferred type"),
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
    pub next: Option<Box<TranslatedPattern>>,
}

pub fn translate_pattern(
    compiled: CompiledPattern,
    locals: &LocalStore,
    table: &SymbolTable,
) -> TranslatedPattern {
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
                Check::ProductEquality {
                    subject,
                    test_sum,
                    discriminant,
                } => {
                    // get type info
                    let sum_type_index = if let Some(sum) = table.sums.get(test_sum) {
                        let type_ = table.types.get(sum.type_).unwrap();
                        type_.definition.id
                    } else {
                        unreachable!("Expected product type to exist")
                    };

                    // push subject on stack
                    cond_expr.push(Instruction::LocalGet(locals.get(subject).unwrap().id.id()));
                    // push discriminant on stack
                    cond_expr.push(Instruction::StructGet {
                        struct_type_index: sum_type_index,
                        field_index: 0,
                    });
                    // push comparison
                    cond_expr.push(Instruction::I32Const(discriminant as i32));
                    cond_expr.push(Instruction::I32Eq);
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
    for assignment in compiled.assignments.into_iter() {
        match assignment {
            Assignment::Named {
                subject_local,
                target_local,
                name,
            } => {
                assign_expr.push(Instruction::LocalGet(
                    locals.get(subject_local).unwrap().id.id(),
                ));
                assign_expr.push(Instruction::LocalSet(
                    locals.get(target_local).unwrap().id.id(),
                ));
                bindings.push((name.clone(), target_local));
            }
            Assignment::Unnamed {
                subject_local,
                target_local,
            } => {
                assign_expr.push(Instruction::LocalGet(
                    locals.get(subject_local).unwrap().id.id(),
                ));
                assign_expr.push(Instruction::LocalSet(
                    locals.get(target_local).unwrap().id.id(),
                ));
            }
            Assignment::Cast {
                subject_local,
                target_local,
                target_type,
            } => {
                assign_expr.push(Instruction::LocalGet(
                    locals.get(subject_local).unwrap().id.id(),
                ));
                // CAST!
                assign_expr.push(Instruction::RefCastNonNull(
                    wasm_encoder::HeapType::Concrete(
                        table.types.get(target_type).unwrap().definition.id,
                    ),
                ));
                assign_expr.push(Instruction::LocalSet(
                    locals.get(target_local).unwrap().id.id(),
                ));
            }
            Assignment::StructField {
                subject_local,
                target_local,
                field_index,
                struct_type,
            } => {
                let struct_type_index = if let Some(type_) = table.types.get(struct_type) {
                    type_.definition.id
                } else {
                    unreachable!("Expected struct type to exist")
                };

                assign_expr.push(Instruction::LocalGet(
                    locals.get(subject_local).unwrap().id.id(),
                ));
                assign_expr.push(Instruction::StructGet {
                    struct_type_index,
                    field_index,
                });
                assign_expr.push(Instruction::LocalSet(
                    locals.get(target_local).unwrap().id.id(),
                ));
            }
        }
    }

    TranslatedPattern {
        condition: cond_expr,
        assignments: assign_expr,
        bindings,
        next: compiled
            .next
            .map(|cp| Box::new(translate_pattern(*cp, locals, table))),
    }
}
