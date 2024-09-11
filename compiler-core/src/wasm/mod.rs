mod encoder;
mod scope;
mod table;

use std::sync::Arc;

use itertools::Itertools;
use scope::Scope;
use table::SymbolTable;

use crate::{
    ast::{
        BinOp, Pattern, Statement, TypedAssignment, TypedExpr, TypedFunction, TypedModule,
        TypedRecordConstructor, TypedStatement,
    },
    io::FileSystemWriter,
    type_::{Type, ValueConstructor, ValueConstructorVariant},
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum WasmType {
    FunctionType {
        parameters: Vec<WasmPrimitive>,
        returns: WasmPrimitive,
    },
    SumType,
    ProductType {
        supertype_index: u32,
        fields: Vec<WasmPrimitive>,
    },
}

impl WasmType {
    fn from_function(f: &TypedFunction) -> Self {
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

        WasmType::FunctionType {
            parameters,
            returns,
        }
    }

    fn from_product_type(variant: &TypedRecordConstructor, supertype_index: u32) -> Self {
        let mut fields = vec![];
        for arg in &variant.arguments {
            if arg.type_.is_int() {
                fields.push(WasmPrimitive::Int);
            } else {
                todo!("Only int fields")
            }
        }

        WasmType::ProductType {
            supertype_index,
            fields,
        }
    }

    fn from_product_type_constructor(
        variant: &TypedRecordConstructor,
        product_type_index: u32,
    ) -> Self {
        let mut fields = vec![];
        for arg in &variant.arguments {
            if arg.type_.is_int() {
                fields.push(WasmPrimitive::Int);
            } else {
                todo!("Only int fields")
            }
        }

        WasmType::FunctionType {
            parameters: fields,
            returns: WasmPrimitive::StructRef(product_type_index),
        }
    }
}

pub fn module(writer: &impl FileSystemWriter, ast: &TypedModule) {
    dbg!(&ast);
    let module = construct_module(ast);
    let bytes = encoder::emit(module);
    writer.write_bytes("out.wasm".into(), &bytes[..]).unwrap();
}

struct WasmModule {
    functions: Vec<WasmFunction>,
    types: Vec<WasmType>,
}

fn construct_module(ast: &TypedModule) -> WasmModule {
    use crate::ast::TypedDefinition;

    // FIRST PASS: generate indices for all functions and types in the top-level module

    // Function indices generator.
    //let mut function_index_generator = FunctionIndexGenerator::new();

    // Type indices generator.
    //let mut type_index_generator = TypeIndexGenerator::new();

    // Maps function types to their respective type indices.
    //let mut function_type_to_index = HashMap::new();

    // Maps constructor function indices to their respective product type indices.
    //let mut constructor_to_variant = HashMap::new();

    // Maps function indices to their type indices.
    //let mut function_to_function_type = HashMap::new();

    // Maps constructor function indices to their sum type indices.
    //let mut constructor_to_sum = HashMap::new();

    let mut table = SymbolTable::new();

    let mut root_environment = Scope::new();

    // generate prelude types
    generate_prelude_types(&mut table);

    // TODO: handle local function/type definitions
    for definition in &ast.definitions {
        match definition {
            TypedDefinition::Function(f) => {
                let function_type_id = table.types.new_id();
                let function_type = table::Type {
                    id: function_type_id,
                    name: format!("type@function@{}", f.name).into(),
                    definition: WasmType::from_function(f),
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

                let sum_type_index = table.types.new_id();
                let sum_type = table::Type {
                    id: sum_type_index,
                    name: format!("type@sum@{}", t.name).into(),
                    definition: WasmType::SumType,
                };
                table.types.insert(sum_type_index, sum_type);

                let mut product_ids = vec![];
                for variant in &t.constructors {
                    if variant.arguments.is_empty() {
                        todo!("Only types with arguments");
                    } else {
                        // type
                        let product_type_id = table.types.new_id();
                        let product_type = table::Type {
                            id: product_type_id,
                            name: format!("type@product@{}@{}", t.name, variant.name).into(),
                            definition: WasmType::from_product_type(variant, sum_type_index.id()),
                        };
                        table.types.insert(product_type_id, product_type);

                        // constructor signature
                        let constructor_sig_id = table.types.new_id();
                        let constructor_sig = table::Type {
                            id: constructor_sig_id,
                            name: format!("type@constructor@{}@{}", t.name, variant.name).into(),
                            definition: WasmType::from_product_type_constructor(
                                variant,
                                product_type_id.id(),
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
                        let product = table::Product {
                            id: product_id,
                            name: format!("product@{}@{}", t.name, variant.name).into(),
                            type_: product_type_id,
                            parent: sum_id,
                            kind: table::ProductKind::Composite {
                                constructor: constructor_id,
                            },
                        };
                        table.products.insert(product_id, product);
                        product_ids.push(product_id);

                        root_environment = root_environment.set(&variant.name, constructor_id.id());
                    }
                }

                let sum = table::Sum {
                    id: sum_id,
                    name: t.name.clone(),
                    type_: sum_type_index,
                    variants: product_ids,
                };
                table.sums.insert(sum_id, sum);
            }
            _ => {}
        }
    }

    // SECOND PASS: generate the actual function bodies and types
    let mut functions = vec![];
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
                    if variant.arguments.len() == 0 {
                        todo!("Only types with arguments");
                    } else {
                        let product_id = root_environment.get(&variant.name).unwrap();
                        let product = table.products.get_from_id(product_id).unwrap();
                        let function = emit_variant_constructor(variant, &product, &table);
                        functions.push(function);
                    }
                }
            }
            _ => todo!("unimplemented"),
        }
    }

    WasmModule {
        functions,
        types: table
            .types
            .as_list()
            .into_iter()
            .map(|x| x.definition)
            .collect(),
    }
}

fn generate_prelude_types(table: &mut SymbolTable) -> () {
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

struct WasmInstructions {
    lst: Vec<wasm_encoder::Instruction<'static>>,
}

struct WasmFunction {
    type_index: u32,
    instructions: WasmInstructions,
    locals: Vec<WasmPrimitive>,
    function_index: u32,
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

    let function_index = match variant_data.kind {
        table::ProductKind::Composite { constructor } => constructor,
        _ => unreachable!(),
    };
    let function = table.functions.get(function_index).unwrap();

    WasmFunction {
        function_index: function_index.id(),
        type_index: function.signature.id(),
        instructions: WasmInstructions { lst: instructions },
        locals: vec![],
    }
}

type LocalStore = table::Store<Arc<Type>>;

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
        locals.insert(idx, Arc::clone(&arg.type_));

        let name = arg
            .names
            .get_variable_name()
            .cloned()
            .unwrap_or_else(|| "#{idx}".into());

        // add arguments to the environment
        env = env.set(&name, idx.id());
    }

    let (_, mut instructions) = emit_statement_list(env, &mut locals, &function.body);
    instructions.lst.push(wasm_encoder::Instruction::End);

    WasmFunction {
        function_index: function_data.id.id(),
        type_index: function_data.signature.id(),
        instructions,
        locals: locals
            .as_list()
            .into_iter()
            .skip(function_data.arity as _)
            .map(|x| {
                if x.is_int() {
                    WasmPrimitive::Int
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

    for statement in statements.into_iter().dropping_back(1) {
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
        _ => todo!("Only expressions and assignments"),
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
            locals.insert(id, Arc::clone(type_));
            let env = env.set(&name, id.id());
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
            match fun.as_ref() {
                TypedExpr::Var { constructor: ValueConstructor { variant: ValueConstructorVariant::ModuleFn { name, .. }, .. }, ..} => {
                    let function_index = env.get(name).unwrap();
                    insts.lst.push(wasm_encoder::Instruction::Call(function_index));
                    (env, insts)
                }
                _ => todo!("Only simple function calls"),
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
    let int = if val.starts_with("0b") {
        // base 2 literal
        i32::from_str_radix(&val[2..], 2).expect("expected int to be a valid binary integer")
    } else if val.starts_with("0o") {
        // base 8 literal
        i32::from_str_radix(&val[2..], 8).expect("expected int to be a valid octal integer")
    } else if val.starts_with("0x") {
        // base 16 literal
        i32::from_str_radix(&val[2..], 16).expect("expected int to be a valid hexadecimal integer")
    } else {
        // base 10 literal
        i32::from_str_radix(&val, 10).expect("expected int to be a valid decimal integer")
    };

    int
}
