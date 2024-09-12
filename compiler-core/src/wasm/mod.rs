#![allow(clippy::todo)]

mod encoder;
mod scope;
mod table;

use std::sync::Arc;

use ecow::EcoString;
use itertools::Itertools;
use scope::Scope;
use table::{Id, Local, LocalStore, SymbolTable};

use crate::{
    ast::{
        BinOp, Pattern, Statement, TypedAssignment, TypedExpr, TypedFunction, TypedModule,
        TypedRecordConstructor, TypedStatement,
    },
    io::FileSystemWriter,
    type_::{Type, ValueConstructor, ValueConstructorVariant},
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct WasmType {
    name: EcoString,
    id: u32,
    definition: WasmTypeDefinition,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum WasmTypeDefinition {
    Function {
        parameters: Vec<WasmPrimitive>,
        returns: WasmPrimitive,
    },
    Sum,
    Product {
        supertype_index: u32,
        fields: Vec<WasmPrimitive>,
    },
}

impl WasmType {
    fn from_function(f: &TypedFunction, name: &str, id: u32) -> Self {
        let mut parameters = vec![];

        for arg in &f.arguments {
            if arg.type_.is_int() {
                parameters.push(WasmPrimitive::Int);
            } else {
                todo!("Only int parameters")
            }
        }

        let returns = if f.return_type.is_int() {
            WasmPrimitive::Int
        } else {
            todo!("Only int return types")
        };

        WasmType {
            name: name.into(),
            id,
            definition: WasmTypeDefinition::Function {
                parameters,
                returns,
            },
        }
    }

    fn from_product_type(
        variant: &TypedRecordConstructor,
        name: &str,
        type_id: u32,
        supertype_index: u32,
    ) -> Self {
        let mut fields = vec![];
        for arg in &variant.arguments {
            if arg.type_.is_int() {
                fields.push(WasmPrimitive::Int);
            } else {
                todo!("Only int fields")
            }
        }

        WasmType {
            name: name.into(),
            id: type_id,
            definition: WasmTypeDefinition::Product {
                supertype_index,
                fields,
            },
        }
    }

    fn from_product_type_constructor(
        variant: &TypedRecordConstructor,
        name: &str,
        product_type_index: u32,
        constructor_type_index: u32,
    ) -> Self {
        let mut fields = vec![];
        for arg in &variant.arguments {
            if arg.type_.is_int() {
                fields.push(WasmPrimitive::Int);
            } else {
                todo!("Only int fields")
            }
        }

        WasmType {
            name: name.into(),
            id: constructor_type_index,
            definition: WasmTypeDefinition::Function {
                parameters: fields,
                returns: WasmPrimitive::StructRef(product_type_index),
            },
        }
    }
}

struct WasmModule {
    functions: Vec<WasmFunction>,
    constants: Vec<WasmGlobal>,
    types: Vec<WasmType>,
}

struct WasmGlobal {
    name: EcoString,
    global_index: u32,
    type_index: u32,
    initializer: WasmInstructions,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum WasmPrimitive {
    Int,
    StructRef(u32),
}

impl WasmPrimitive {
    fn to_val_type(self) -> wasm_encoder::ValType {
        match self {
            WasmPrimitive::Int => wasm_encoder::ValType::I32,
            WasmPrimitive::StructRef(typ) => wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                nullable: false,
                heap_type: wasm_encoder::HeapType::Concrete(typ),
            }),
        }
    }
}

#[derive(Debug)]
struct WasmInstructions {
    lst: Vec<wasm_encoder::Instruction<'static>>,
}

#[derive(Debug)]
struct WasmFunction {
    name: EcoString,
    type_index: u32,
    instructions: WasmInstructions,
    locals: Vec<(EcoString, WasmPrimitive)>,
    argument_names: Vec<Option<EcoString>>,
    function_index: u32,
}

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

                root_environment = root_environment.set(&f.name, function_id.id());
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

                        // add global variable to the environment
                        root_environment = root_environment.set(&variant.name, global_id.id());

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
                        root_environment = root_environment.set(&variant.name, constructor_id.id());

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
                let function_index = root_environment.get(&f.name).unwrap();
                let function_data = table.functions.get_from_id(function_index).unwrap();
                let function =
                    emit_function(f, function_data.id, &table, Arc::clone(&root_environment));
                functions.push(function);
            }
            TypedDefinition::CustomType(c) => {
                // generate the type constructors
                for variant in &c.constructors {
                    // we need to iterate over the products because the type constructor is shadowed by the global
                    // in the environment
                    for product in table.products.as_list().into_iter() {
                        if product.name == format!("product@{}.{}", c.name, variant.name) {
                            let function = emit_variant_constructor(variant, &product, &table);
                            functions.push(function);
                            break;
                        }
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
        env = env.set(&name, idx.id());
    }

    let (_, mut instructions) = emit_statement_list(env, &mut locals, &function.body);
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
    env: Arc<Scope>,
    locals: &mut LocalStore,
    statements: &[TypedStatement],
) -> (Arc<Scope>, WasmInstructions) {
    let mut instructions = WasmInstructions { lst: vec![] };
    let mut env = env;

    for statement in statements.iter().dropping_back(1) {
        let (new_env, new_insts) = emit_statement(statement, env, locals);
        env = new_env;
        instructions.lst.extend(new_insts.lst);
        instructions.lst.push(wasm_encoder::Instruction::Drop);
    }
    if let Some(statement) = statements.last() {
        let (new_env, new_insts) = emit_statement(statement, env, locals);
        env = new_env;
        instructions.lst.extend(new_insts.lst);
    }

    (env, instructions)
}

fn emit_statement(
    statement: &TypedStatement,
    env: Arc<Scope>,
    locals: &mut LocalStore,
) -> (Arc<Scope>, WasmInstructions) {
    match statement {
        Statement::Expression(expression) => emit_expression(expression, env, locals),
        Statement::Assignment(assignment) => emit_assignment(assignment, env, locals),
        Statement::Use(_) => todo!("Only expressions and assignments"),
    }
}

fn emit_assignment(
    assignment: &TypedAssignment,
    env: Arc<Scope>,
    locals: &mut LocalStore,
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
            let (env, mut insts) = emit_expression(&assignment.value, env, locals);
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
            let env = env.set(name, id.id());
            // create local
            insts
                .lst
                .push(wasm_encoder::Instruction::LocalTee(env.get(name).unwrap()));

            (env, insts)
        }
        _ => todo!("Only simple assignments"),
    }
}

fn emit_expression(
    expression: &TypedExpr,
    env: Arc<Scope>,
    locals: &mut LocalStore,
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
            let (env, mut insts) = emit_expression(value, env, locals);
            insts.lst.push(wasm_encoder::Instruction::I32Const(-1));
            insts.lst.push(wasm_encoder::Instruction::I32Mul);
            (env, insts)
        }
        TypedExpr::Block { statements, .. } => {
            // create new scope
            // TODO: fix environment pop
            // maybe use a block instruction?
            let (_, statements) = emit_statement_list(Scope::new_enclosing(Arc::clone(&env)), locals, statements);
            (env, statements)
        },
        TypedExpr::BinOp {
            typ,
            name,
            left,
            right,
            ..
        } => emit_binary_operation(env, locals, typ, *name, left, right),
        TypedExpr::Var {
            constructor, name, ..
        } => {
            match &constructor.variant {
                ValueConstructorVariant::LocalVariable { .. } => {
                    let index = env.get(name).unwrap();
                    (
                        env,
                        WasmInstructions {
                            lst: vec![wasm_encoder::Instruction::LocalGet(index)],
                        },
                    )
                },
                ValueConstructorVariant::ModuleFn { name, .. } => {
                    let function_index = env.get(name).unwrap();
                    (
                        env,
                        WasmInstructions {
                            lst: vec![wasm_encoder::Instruction::RefFunc(function_index)],
                        },
                    )
                }
                _ => todo!("Only local variables and records"),
            }
        },
        TypedExpr::Call { fun, args, .. } => {
            let (mut env, mut insts) = (env, WasmInstructions { lst: vec![] });
            for arg in args {
                let (new_env, new_insts) = emit_expression(&arg.value, env, locals);
                env = new_env;
                insts.lst.extend(new_insts.lst);
            }
            // TODO: check this field map thing to see if it's constructing it in the right order.
            match fun.as_ref() {
                TypedExpr::Var { constructor: ValueConstructor { variant: ValueConstructorVariant::ModuleFn { name, .. }, .. }, ..} => {
                    let function_index = env.get(name).unwrap();
                    insts.lst.push(wasm_encoder::Instruction::Call(function_index));
                    (env, insts)
                }
                TypedExpr::Var { constructor: ValueConstructor { variant: ValueConstructorVariant::Record { name, .. }, ..}, ..} => {
                    let constructor_index = env.get(name).unwrap();
                    insts.lst.push(wasm_encoder::Instruction::Call(constructor_index));
                    (env, insts)
                }
                _ => todo!("Only simple function calls and type constructors"),
            }
        }
        _ => todo!("Only integer constants, integer negation, blocks, binary operations, case and variable accesses"),
    }
}

fn emit_binary_operation(
    env: Arc<Scope>,
    locals: &mut LocalStore,
    // only used to disambiguate equals
    _typ: &Type,
    name: BinOp,
    left: &TypedExpr,
    right: &TypedExpr,
) -> (Arc<Scope>, WasmInstructions) {
    match name {
        BinOp::AddInt => {
            let (env, mut insts) = emit_expression(left, env, locals);
            let (env, right_insts) = emit_expression(right, env, locals);
            insts.lst.extend(right_insts.lst);
            insts.lst.push(wasm_encoder::Instruction::I32Add);
            (env, insts)
        }
        BinOp::SubInt => {
            let (env, mut insts) = emit_expression(left, env, locals);
            let (env, right_insts) = emit_expression(right, env, locals);
            insts.lst.extend(right_insts.lst);
            insts.lst.push(wasm_encoder::Instruction::I32Sub);
            (env, insts)
        }
        BinOp::MultInt => {
            let (env, mut insts) = emit_expression(left, env, locals);
            let (env, right_insts) = emit_expression(right, env, locals);
            insts.lst.extend(right_insts.lst);
            insts.lst.push(wasm_encoder::Instruction::I32Mul);
            (env, insts)
        }
        BinOp::DivInt => {
            let (env, mut insts) = emit_expression(left, env, locals);
            let (env, right_insts) = emit_expression(right, env, locals);
            insts.lst.extend(right_insts.lst);
            insts.lst.push(wasm_encoder::Instruction::I32DivS);
            (env, insts)
        }
        BinOp::RemainderInt => {
            let (env, mut insts) = emit_expression(left, env, locals);
            let (env, right_insts) = emit_expression(right, env, locals);
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
