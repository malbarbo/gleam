use std::sync::Arc;

use ecow::EcoString;
use wasm_encoder::Instruction;

use crate::{
    analyse::Inferred,
    ast::{Pattern, TypedPattern},
    type_::{FieldMap, PatternConstructor},
};

use super::{
    encoder::WasmTypeImpl,
    environment::{Binding, Environment},
    integer, parse_float,
    table::{Local, LocalId, LocalStore, SumId, SymbolTable, TypeId},
};

#[derive(Debug, Clone)]
pub struct CompiledPattern {
    pub checks: Vec<Check>,
    pub assignments: Vec<Assignment>,
    pub nested: Vec<CompiledPattern>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone)]
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
            let value = integer::parse(value);
            CompiledPattern {
                checks: vec![Check::IntegerEquality {
                    local: subject,
                    value,
                }],
                assignments: vec![],
                nested: vec![],
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
                nested: vec![],
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
                nested: vec![],
            }
        }
        Pattern::Assign { name, pattern, .. } => {
            let local_id = locals.new_id();
            locals.insert(
                local_id,
                Local {
                    id: local_id,
                    name: name.clone(),
                    wasm_type: WasmTypeImpl::from_gleam_type(pattern.type_(), env, table),
                },
            );

            let cp = compile_pattern(subject, pattern, table, env, locals);
            CompiledPattern {
                checks: vec![],
                assignments: vec![Assignment::Named {
                    name: name.clone(),
                    subject_local: subject,
                    target_local: local_id,
                }],
                nested: vec![cp],
            }
        }
        Pattern::Discard { .. } => CompiledPattern {
            checks: vec![],
            assignments: vec![],
            nested: vec![],
        },
        Pattern::Constructor {
            constructor:
                Inferred::Known(PatternConstructor {
                    field_map,
                    name: record_name,
                    ..
                }),
            arguments,
            name,
            module,
            ..
        } => {
            if module.is_some() {
                todo!("Constructor access with module name not implemented yet")
            }

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

            let mut assignments = vec![Assignment::Cast {
                subject_local: subject,
                target_local: cast_local_id,
                target_type: product_type.id,
            }];

            // chain remaining patterns
            let mut nested_patterns = vec![];
            for (i, arg) in arguments.iter().enumerate() {
                let field_index = match field_map {
                    None => i,
                    Some(FieldMap { fields, .. }) => {
                        let find = |(key, &val)| {
                            if val as usize == i {
                                Some(key)
                            } else {
                                None
                            }
                        };
                        let label = fields.iter().find_map(find);
                        match label {
                            Some(label) => {
                                // the field is the one with this name
                                product
                                    .fields
                                    .iter()
                                    .position(|f| &f.name == label)
                                    .unwrap()
                            }
                            None => i,
                        }
                    }
                };
                let field = &product.fields[field_index];

                // create a local for the argument
                let arg_variable = locals.new_id();
                locals.insert(
                    arg_variable,
                    Local {
                        id: arg_variable,
                        name: field.name.clone(),
                        wasm_type: field.type_.clone(),
                    },
                );

                // assign the argument to the new variable
                assignments.push(Assignment::StructField {
                    subject_local: cast_local_id,
                    target_local: arg_variable,
                    field_index: i as u32 + 1,
                    struct_type: product.type_,
                });

                // recursively compile
                let inner_compiled_pattern =
                    compile_pattern(arg_variable, &arg.value, table, env, locals);

                nested_patterns.push(inner_compiled_pattern);
            }

            dbg!(CompiledPattern {
                checks,
                assignments,
                nested: nested_patterns,
            })
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
    pub nested: Vec<TranslatedPattern>,
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
        nested: compiled
            .nested
            .into_iter()
            .map(|cp| translate_pattern(cp, locals, table))
            .collect(),
    }
}
