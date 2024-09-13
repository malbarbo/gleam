#![allow(clippy::todo)]

mod encoder;
mod pattern;
mod scope;
mod table;

use std::sync::Arc;

use ecow::EcoString;
use encoder::{
    WasmFunction, WasmGlobal, WasmInstructions, WasmModule, WasmPrimitive, WasmType,
    WasmTypeDefinition,
};
use itertools::Itertools;
use scope::{Binding, Scope};
use table::{Local, LocalStore, SymbolTable};

use crate::{
    ast::{
        BinOp, Pattern, Statement, TypedAssignment, TypedClause, TypedExpr, TypedFunction,
        TypedModule, TypedRecordConstructor, TypedStatement,
    },
    io::FileSystemWriter,
    type_::{Type, ValueConstructor, ValueConstructorVariant},
};

pub fn module(writer: &impl FileSystemWriter, ast: &TypedModule) {
    dbg!(&ast);
    let module = construct_module(ast);
    let bytes = encoder::emit(module);
    writer.write_bytes("out.wasm".into(), &bytes[..]).unwrap();
}

fn construct_module(ast: &TypedModule) -> WasmModule {
    use crate::ast::TypedDefinition;

    // FIRST PASS: generate indices for all functions and types in the top-level module
    let mut table = SymbolTable::new();

    let mut root_environment = Scope::new();

    let mut functions = vec![];
    let mut constants = vec![];

    // generate prelude types
    generate_prelude_types(&mut table);

    // TODO: handle local function/type definitions
    for definition in &ast.definitions {
        match definition {
            TypedDefinition::Function(f) => {
                let function_type_id = table.types.new_id();
                let function_type_name: EcoString = format!("fun@{}", f.name).into();
                let function_type = table::Type {
                    id: function_type_id,
                    name: function_type_name.clone(),
                    definition: WasmType::from_function(
                        f,
                        &function_type_name,
                        function_type_id.id(),
                    ),
                };
                table.types.insert(function_type_id, function_type);

                let function_id = table.functions.new_id();
                let function = table::Function {
                    id: function_id,
                    signature: function_type_id,
                    name: f.name.clone(),
                    arity: f.arguments.len() as u32,
                };
                table.functions.insert(function_id, function);

                root_environment = root_environment.set(&f.name, Binding::Function(function_id));
            }
            TypedDefinition::CustomType(t) => {
                if !t.parameters.is_empty() {
                    todo!("Only concrete types");
                }

                let sum_id = table.sums.new_id();

                let sum_type_id = table.types.new_id();
                let sum_type_name: EcoString = format!("sum@{}", t.name).into();
                let sum_type = table::Type {
                    id: sum_type_id,
                    name: sum_type_name.clone(),
                    definition: WasmType {
                        id: sum_type_id.id(),
                        name: sum_type_name,
                        definition: WasmTypeDefinition::Sum,
                    },
                };
                table.types.insert(sum_type_id, sum_type);

                let mut product_ids = vec![];
                for variant in &t.constructors {
                    // type
                    let product_type_id = table.types.new_id();
                    let product_type_name: EcoString =
                        format!("typ@{}.{}", t.name, variant.name).into();
                    let product_type = table::Type {
                        id: product_type_id,
                        name: product_type_name.clone(),
                        definition: WasmType::from_product_type(
                            variant,
                            &product_type_name,
                            product_type_id.id(),
                            sum_type_id.id(),
                        ),
                    };
                    table.types.insert(product_type_id, product_type);

                    // constructor signature
                    let constructor_sig_id = table.types.new_id();
                    let constructor_sig_name: EcoString =
                        format!("new@{}.{}", t.name, variant.name).into();
                    let constructor_sig = table::Type {
                        id: constructor_sig_id,
                        name: constructor_sig_name.clone(),
                        definition: WasmType::from_product_type_constructor(
                            variant,
                            &constructor_sig_name,
                            product_type_id.id(),
                            constructor_sig_id.id(),
                        ),
                    };
                    table.types.insert(constructor_sig_id, constructor_sig);

                    // constructor
                    let constructor_id = table.functions.new_id();
                    let constructor = table::Function {
                        id: constructor_id,
                        signature: constructor_sig_id,
                        name: variant.name.clone(),
                        arity: variant.arguments.len() as u32,
                    };
                    table.functions.insert(constructor_id, constructor);

                    // product
                    let product_id = table.products.new_id();

                    let product = if variant.arguments.is_empty() {
                        let global_name = format!("global@{}.{}", t.name, variant.name);

                        // add a global to the symbol table
                        let global_id = table.constants.new_id();
                        let global = table::Constant {
                            id: global_id,
                            name: global_name.clone().into(),
                            type_: product_type_id,
                        };
                        table.constants.insert(global_id, global);

                        // add global
                        constants.push(WasmGlobal {
                            name: global_name.into(),
                            type_index: product_type_id.id(),
                            global_index: global_id.id(),
                            initializer: WasmInstructions {
                                lst: vec![wasm_encoder::Instruction::Call(constructor_id.id())],
                            },
                        });

                        table::Product {
                            id: product_id,
                            name: format!("product@{}.{}", t.name, variant.name).into(),
                            type_: product_type_id,
                            parent: sum_id,
                            constructor: constructor_id,
                            kind: table::ProductKind::Simple {
                                instance: global_id,
                            },
                        }
                    } else {
                        // add constructor to the environment
                        table::Product {
                            id: product_id,
                            name: format!("product@{}.{}", t.name, variant.name).into(),
                            type_: product_type_id,
                            parent: sum_id,
                            constructor: constructor_id,
                            kind: table::ProductKind::Composite,
                        }
                    };
                    table.products.insert(product_id, product);
                    product_ids.push(product_id);

                    root_environment =
                        root_environment.set(&variant.name, Binding::Product(product_id));
                }

                let sum = table::Sum {
                    id: sum_id,
                    name: t.name.clone(),
                    type_: sum_type_id,
                    variants: product_ids,
                };
                table.sums.insert(sum_id, sum);
            }
            _ => {}
        }
    }

    // SECOND PASS: generate the actual function bodies and types
    for definition in &ast.definitions {
        match definition {
            TypedDefinition::Function(f) => {
                let function_id = root_environment.get(&f.name).unwrap();
                match function_id {
                    Binding::Function(id) => {
                        let function_data = table.functions.get(id).unwrap();
                        let function = emit_function(
                            f,
                            function_data.id,
                            &table,
                            Arc::clone(&root_environment),
                        );
                        functions.push(function);
                    }
                    _ => unreachable!("Expected function binding in environment"),
                }
            }
            TypedDefinition::CustomType(c) => {
                // generate the type constructors
                for variant in &c.constructors {
                    let product_id = root_environment.get(&variant.name).unwrap();
                    match product_id {
                        Binding::Product(id) => {
                            let table_product = table.products.get(id).unwrap();
                            let function = emit_variant_constructor(variant, table_product, &table);
                            functions.push(function);
                        }
                        _ => unreachable!("Expected product binding in environment"),
                    }
                }
            }
            _ => todo!("unimplemented"),
        }
    }

    WasmModule {
        functions,
        constants,
        types: table
            .types
            .as_list()
            .into_iter()
            .map(|x| x.definition)
            .collect(),
    }
}

fn generate_prelude_types(_table: &mut SymbolTable) {
    // Implementing these is not necessary:
    // - PreludeType::Float
    // - PreludeType::Int

    // Implemented:
    // - PreludeType::Nil
    // - PreludeType::Bool
    // - PreludeType::String

    // To be implemented:
    // - PreludeType::BitArray
    // - PreludeType::List
    // - PreludeType::Result
    // - PreludeType::UtfCodepoint
}

fn emit_variant_constructor(
    constructor: &TypedRecordConstructor,
    variant_data: &table::Product,
    table: &SymbolTable,
) -> WasmFunction {
    let mut instructions = (0..constructor.arguments.len())
        .map(|i| wasm_encoder::Instruction::LocalGet(i as u32))
        .collect_vec();
    instructions.push(wasm_encoder::Instruction::StructNew(
        variant_data.type_.id(),
    ));
    instructions.push(wasm_encoder::Instruction::End);

    let function_index = variant_data.constructor;
    let function = table.functions.get(function_index).unwrap();

    WasmFunction {
        name: format!("new@{}", &function.name).into(),
        function_index: function_index.id(),
        type_index: function.signature.id(),
        instructions: WasmInstructions { lst: instructions },
        argument_names: constructor.arguments.iter().map(|_| None).collect(),
        locals: vec![],
    }
}

fn emit_function(
    function: &TypedFunction,
    function_id: table::FunctionId,
    table: &SymbolTable,
    top_level_env: Arc<Scope>,
) -> WasmFunction {
    let mut env = Scope::new_enclosing(top_level_env);
    let mut locals = LocalStore::new();

    let function_data = table
        .functions
        .get(function_id)
        .expect("The function exists");

    for arg in &function.arguments {
        // get a variable number
        let idx = locals.new_id();

        let name = arg
            .names
            .get_variable_name()
            .cloned()
            .unwrap_or_else(|| "#{idx}".into());

        locals.insert(
            idx,
            Local {
                id: idx,
                name: name.clone(),
                gleam_type: Arc::clone(&arg.type_),
            },
        );

        // add arguments to the environment
        env = env.set(&name, Binding::Local(idx));
    }

    let (_, mut instructions) = emit_statement_list(&function.body, env, &mut locals, table);
    instructions.lst.push(wasm_encoder::Instruction::End);

    WasmFunction {
        name: function_data.name.clone(),
        function_index: function_data.id.id(),
        type_index: function_data.signature.id(),
        instructions,
        argument_names: locals
            .as_list()
            .into_iter()
            .take(function_data.arity as _)
            .map(|local| Some(local.name))
            .collect_vec(),
        locals: locals
            .as_list()
            .into_iter()
            .skip(function_data.arity as _)
            .map(|local| {
                if local.gleam_type.is_int() {
                    (local.name, WasmPrimitive::Int)
                } else {
                    todo!("Only int return types")
                }
            })
            .collect_vec(),
    }
}

fn emit_statement_list(
    statements: &[TypedStatement],
    env: Arc<Scope>,
    locals: &mut LocalStore,
    table: &SymbolTable,
) -> (Arc<Scope>, WasmInstructions) {
    let mut instructions = WasmInstructions { lst: vec![] };
    let mut env = env;

    for statement in statements.iter().dropping_back(1) {
        let (new_env, new_insts) = emit_statement(statement, env, locals, table);
        env = new_env;
        instructions.lst.extend(new_insts.lst);
        instructions.lst.push(wasm_encoder::Instruction::Drop);
    }
    if let Some(statement) = statements.last() {
        let (new_env, new_insts) = emit_statement(statement, env, locals, table);
        env = new_env;
        instructions.lst.extend(new_insts.lst);
    }

    (env, instructions)
}

fn emit_statement(
    statement: &TypedStatement,
    env: Arc<Scope>,
    locals: &mut LocalStore,
    table: &SymbolTable,
) -> (Arc<Scope>, WasmInstructions) {
    match statement {
        Statement::Expression(expression) => emit_expression(expression, env, locals, table),
        Statement::Assignment(assignment) => emit_assignment(assignment, env, locals, table),
        Statement::Use(_) => todo!("Only expressions and assignments"),
    }
}

fn emit_assignment(
    assignment: &TypedAssignment,
    env: Arc<Scope>,
    locals: &mut LocalStore,
    table: &SymbolTable,
) -> (Arc<Scope>, WasmInstructions) {
    // only non-assertions
    if assignment.kind.is_assert() {
        todo!("Only non-assertions");
    }

    // only support simple assignments for now
    match assignment.pattern {
        Pattern::Variable {
            ref name,
            ref type_,
            ..
        } => {
            // emit value
            let (env, mut insts) = emit_expression(&assignment.value, env, locals, table);
            // add variable to the environment
            let id = locals.new_id();
            locals.insert(
                id,
                Local {
                    id,
                    name: name.clone(),
                    gleam_type: Arc::clone(type_),
                },
            );
            let env = env.set(name, Binding::Local(id));
            // create local
            insts.lst.push(wasm_encoder::Instruction::LocalTee(id.id()));

            (env, insts)
        }
        _ => todo!("Only simple assignments"),
    }
}

fn emit_expression(
    expression: &TypedExpr,
    env: Arc<Scope>,
    locals: &mut LocalStore,
    table: &SymbolTable,
) -> (Arc<Scope>, WasmInstructions) {
    match expression {
        TypedExpr::Int { value, .. } => {
            let val = parse_integer(value);
            (
                env,
                WasmInstructions {
                    lst: vec![wasm_encoder::Instruction::I32Const(val)],
                },
            )
        }
        TypedExpr::NegateInt { value, .. } => {
            let (env, mut insts) = emit_expression(value, env, locals, table);
            insts.lst.push(wasm_encoder::Instruction::I32Const(-1));
            insts.lst.push(wasm_encoder::Instruction::I32Mul);
            (env, insts)
        }
        TypedExpr::Block { statements, .. } => {
            // create new scope
            // TODO: fix environment pop
            // maybe use a block instruction?
            let (_, statements) = emit_statement_list(
                statements,
                Scope::new_enclosing(Arc::clone(&env)),
                locals,
                table,
            );
            (env, statements)
        }
        TypedExpr::BinOp {
            typ,
            name,
            left,
            right,
            ..
        } => emit_binary_operation(env, locals, table, typ, *name, left, right),
        TypedExpr::Var {
            constructor, name, ..
        } => match &constructor.variant {
            ValueConstructorVariant::LocalVariable { .. } => match env.get(name).unwrap() {
                Binding::Local(id) => (
                    env,
                    WasmInstructions {
                        lst: vec![wasm_encoder::Instruction::LocalGet(id.id())],
                    },
                ),
                _ => todo!("Expected local variable binding"),
            },
            ValueConstructorVariant::ModuleFn { name, .. } => match env.get(name).unwrap() {
                Binding::Function(id) => (
                    env,
                    WasmInstructions {
                        lst: vec![wasm_encoder::Instruction::Call(id.id())],
                    },
                ),
                _ => todo!("Expected function binding"),
            },
            ValueConstructorVariant::Record { name, arity: 0, .. } => {
                match env.get(name).unwrap() {
                    Binding::Product(id) => {
                        let product = table.products.get(id).unwrap();
                        match product {
                            table::Product {
                                kind: table::ProductKind::Simple { instance },
                                ..
                            } => (
                                env,
                                WasmInstructions {
                                    lst: vec![wasm_encoder::Instruction::GlobalGet(instance.id())],
                                },
                            ),
                            _ => todo!("Expected simple product"),
                        }
                    }
                    _ => todo!("Expected product binding"),
                }
            }
            _ => todo!("Only local variables and records"),
        },
        TypedExpr::Call { fun, args, .. } => {
            let (mut env, mut insts) = (env, WasmInstructions { lst: vec![] });
            for arg in args {
                let (new_env, new_insts) = emit_expression(&arg.value, env, locals, table);
                env = new_env;
                insts.lst.extend(new_insts.lst);
            }
            // TODO: check this field map thing to see if it's constructing it in the right order.
            match fun.as_ref() {
                TypedExpr::Var {
                    constructor:
                        ValueConstructor {
                            variant: ValueConstructorVariant::ModuleFn { name, .. },
                            ..
                        },
                    ..
                } => match env.get(name).unwrap() {
                    Binding::Function(id) => {
                        insts.lst.push(wasm_encoder::Instruction::Call(id.id()));
                        (env, insts)
                    }
                    _ => todo!("Expected function binding"),
                },
                TypedExpr::Var {
                    constructor:
                        ValueConstructor {
                            variant: ValueConstructorVariant::Record { name, .. },
                            ..
                        },
                    ..
                } => match env.get(name).unwrap() {
                    Binding::Product(id) => {
                        let product = table.products.get(id).unwrap();
                        insts
                            .lst
                            .push(wasm_encoder::Instruction::Call(product.constructor.id()));
                        (env, insts)
                    }
                    _ => todo!("Expected product binding"),
                },
                _ => todo!("Only simple function calls and type constructors"),
            }
        }
        TypedExpr::Case {
            subjects,
            clauses,
            typ,
            ..
        } => emit_case_expression(subjects, clauses, Arc::clone(typ), env, locals, table),
        _ => todo!("Not supported yet"),
    }
}

fn emit_case_expression(
    subjects: &[TypedExpr],
    clauses: &[TypedClause],
    type_: Arc<Type>,
    env: Arc<Scope>,
    locals: &mut LocalStore,
    table: &SymbolTable,
) -> (Arc<Scope>, WasmInstructions) {
    // first, declare a new local for every subject
    let ids: Vec<_> = subjects
        .iter()
        .map(|subject| {
            let id = locals.new_id();
            let name = format!("#case#{}", id.id());
            locals.insert(
                id,
                Local {
                    id,
                    name: name.into(),
                    gleam_type: Arc::clone(&subject.type_()),
                },
            );
            id
        })
        .collect();

    // then, emit the subject expressions and store them in the locals
    let mut env = env;
    let mut insts = WasmInstructions { lst: vec![] };
    for (id, subject) in ids.iter().zip(subjects) {
        let (new_env, new_insts) = emit_expression(subject, env, locals, table);
        env = new_env;
        insts.lst.extend(new_insts.lst);
        insts.lst.push(wasm_encoder::Instruction::LocalSet(id.id()));
    }

    let result_type = if type_.is_int() {
        WasmPrimitive::Int
    } else {
        todo!("Only int types")
    };

    for clause in clauses {
        let mut conditions = vec![];
        let mut assignments = vec![];
        let mut inner_env = Scope::new_enclosing(Arc::clone(&env));

        let mut first = true;
        for (pattern, subject_id) in clause.pattern.iter().zip(&ids) {
            let compiled = pattern::compile_pattern(*subject_id, pattern, table, locals);
            let translated = pattern::translate_pattern(compiled, locals);
            conditions.extend(translated.condition);
            assignments.extend(translated.assignments);
            for (name, local_id) in translated.bindings.iter() {
                inner_env = inner_env.set(name, Binding::Local(*local_id));
            }
            if first {
                first = false;
            } else {
                conditions.push(wasm_encoder::Instruction::I32And);
            }
        }

        insts.lst.extend(conditions);
        insts.lst.push(wasm_encoder::Instruction::If(
            wasm_encoder::BlockType::Result(result_type.to_val_type()),
        ));
        insts.lst.extend(assignments);

        let (_, new_insts) = emit_expression(&clause.then, inner_env, locals, table);
        insts.lst.extend(new_insts.lst);

        insts.lst.push(wasm_encoder::Instruction::Else);

        // if we're the last clause, this is unreachable
        // place a trap instruction
        if clause == clauses.last().unwrap() {
            // this is safe because pattern matching is exhaustive
            insts.lst.push(wasm_encoder::Instruction::Unreachable);
        }
    }

    // place n ends
    for _ in 0..clauses.len() {
        insts.lst.push(wasm_encoder::Instruction::End);
    }

    (env, insts.into())
}

fn emit_binary_operation(
    env: Arc<Scope>,
    locals: &mut LocalStore,
    table: &SymbolTable,
    // only used to disambiguate equals
    _typ: &Type,
    name: BinOp,
    left: &TypedExpr,
    right: &TypedExpr,
) -> (Arc<Scope>, WasmInstructions) {
    match name {
        BinOp::AddInt => {
            let (env, mut insts) = emit_expression(left, env, locals, table);
            let (env, right_insts) = emit_expression(right, env, locals, table);
            insts.lst.extend(right_insts.lst);
            insts.lst.push(wasm_encoder::Instruction::I32Add);
            (env, insts)
        }
        BinOp::SubInt => {
            let (env, mut insts) = emit_expression(left, env, locals, table);
            let (env, right_insts) = emit_expression(right, env, locals, table);
            insts.lst.extend(right_insts.lst);
            insts.lst.push(wasm_encoder::Instruction::I32Sub);
            (env, insts)
        }
        BinOp::MultInt => {
            let (env, mut insts) = emit_expression(left, env, locals, table);
            let (env, right_insts) = emit_expression(right, env, locals, table);
            insts.lst.extend(right_insts.lst);
            insts.lst.push(wasm_encoder::Instruction::I32Mul);
            (env, insts)
        }
        BinOp::DivInt => {
            let (env, mut insts) = emit_expression(left, env, locals, table);
            let (env, right_insts) = emit_expression(right, env, locals, table);
            insts.lst.extend(right_insts.lst);
            insts.lst.push(wasm_encoder::Instruction::I32DivS);
            (env, insts)
        }
        BinOp::RemainderInt => {
            let (env, mut insts) = emit_expression(left, env, locals, table);
            let (env, right_insts) = emit_expression(right, env, locals, table);
            insts.lst.extend(right_insts.lst);
            insts.lst.push(wasm_encoder::Instruction::I32RemS);
            (env, insts)
        }
        _ => todo!("Only integer arithmetic"),
    }
}

fn parse_integer(value: &str) -> i32 {
    let val = value.replace("_", "");

    // TODO: support integers other than i32
    // why do i have do this at codegen?
    if let Some(val) = val.strip_prefix("0b") {
        // base 2 literal
        i32::from_str_radix(val, 2).expect("expected int to be a valid binary integer")
    } else if let Some(val) = val.strip_prefix("0o") {
        // base 8 literal
        i32::from_str_radix(val, 8).expect("expected int to be a valid octal integer")
    } else if let Some(val) = val.strip_prefix("0x") {
        // base 16 literal
        i32::from_str_radix(val, 16).expect("expected int to be a valid hexadecimal integer")
    } else {
        // base 10 literal
        val.parse()
            .expect("expected int to be a valid decimal integer")
    }
}

fn parse_float(value: &str) -> f64 {
    let val = value.replace("_", "");
    val.parse()
        .expect("expected float to be a valid decimal float")
}
