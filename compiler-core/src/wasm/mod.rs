use std::sync::Arc;

use itertools::Itertools;

use crate::{
    ast::{
        BinOp, CustomType, Function, Pattern, Statement, TypedArg, TypedAssignment, TypedExpr,
        TypedModule, TypedStatement,
    },
    io::FileSystemWriter,
    type_::Type,
};

mod encoder;
mod environment;

#[derive(Clone, Default, Debug)]
struct LocalGenerator {
    locals: Vec<Arc<Type>>,
}

impl LocalGenerator {
    fn new() -> Self {
        Self { locals: vec![] }
    }

    fn new_local(&mut self, type_: Arc<Type>) -> u32 {
        let l = self.locals.len() as u32;
        self.locals.push(type_);
        l
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
}

fn construct_module(ast: &TypedModule) -> WasmModule {
    use crate::ast::TypedDefinition;

    let mut functions = vec![];

    // populate functions
    for def in &ast.definitions {
        match def {
            TypedDefinition::Function(Function {
                name,
                arguments,
                body,
                return_type,
                ..
            }) => {
                functions.push(emit_function(name, arguments, body, return_type));
            }
            TypedDefinition::CustomType(CustomType { .. }) => {} // we handle this elsewhere
            _ => todo!("Only functions"),
        }
    }

    WasmModule { functions }
}

#[derive(Copy, Clone, Debug)]
enum WasmPrimitive {
    Int,
}

impl WasmPrimitive {
    fn to_val_type(self) -> wasm_encoder::ValType {
        match self {
            WasmPrimitive::Int => wasm_encoder::ValType::I32,
        }
    }
}

struct WasmInstructions {
    lst: Vec<wasm_encoder::Instruction<'static>>,
}

struct WasmFunction {
    parameters: Vec<WasmPrimitive>,
    instructions: WasmInstructions,
    returns: WasmPrimitive,
    locals: Vec<WasmPrimitive>,
}

fn emit_function(
    _name: &str,
    arguments: &[TypedArg],
    body: &[TypedStatement],
    return_type: &Arc<Type>,
) -> WasmFunction {
    let mut env = environment::Environment::new();
    let mut locals = LocalGenerator::new();

    let mut parameters = vec![];
    for arg in arguments {
        // get a variable number
        let idx = locals.new_local(Arc::clone(&arg.type_));
        // add arguments to the environment
        env = env.set(
            &arg.names
                .get_variable_name()
                .cloned()
                .unwrap_or_else(|| format!("#{idx}").into()),
            idx,
            Arc::clone(&arg.type_),
        );
        // add argument types to the parameters
        parameters.push(if arg.type_.is_int() {
            WasmPrimitive::Int
        } else {
            todo!("Only int parameters")
        });
    }

    let n_params = parameters.len();
    let (_, locals, mut instructions) = emit_statement_list(env, locals, body);
    instructions.lst.push(wasm_encoder::Instruction::End);

    WasmFunction {
        parameters,
        instructions,
        locals: locals
            .locals
            .into_iter()
            .skip(n_params)
            .map(|x| {
                if x.is_int() {
                    WasmPrimitive::Int
                } else {
                    todo!("Only int return types")
                }
            })
            .collect_vec(),
        returns: if return_type.is_int() {
            WasmPrimitive::Int
        } else {
            todo!("Only int return types")
        },
    }
}

fn emit_statement_list(
    env: Arc<environment::Environment>,
    locals: LocalGenerator,
    statements: &[TypedStatement],
) -> (
    Arc<environment::Environment>,
    LocalGenerator,
    WasmInstructions,
) {
    let mut instructions = WasmInstructions { lst: vec![] };
    let mut env = env;
    let mut locals = locals;

    for statement in statements.into_iter().dropping_back(1) {
        let (new_env, new_locals, new_insts) = emit_statement(statement, env, locals);
        env = new_env;
        locals = new_locals;
        instructions.lst.extend(new_insts.lst);
        instructions.lst.push(wasm_encoder::Instruction::Drop);
    }
    if let Some(statement) = statements.last() {
        let (new_env, new_locals, new_insts) = emit_statement(statement, env, locals);
        env = new_env;
        locals = new_locals;
        instructions.lst.extend(new_insts.lst);
    }

    (env, locals, instructions)
}

fn emit_statement(
    statement: &TypedStatement,
    env: Arc<environment::Environment>,
    locals: LocalGenerator,
) -> (
    Arc<environment::Environment>,
    LocalGenerator,
    WasmInstructions,
) {
    match statement {
        Statement::Expression(expression) => emit_expression(expression, env, locals),
        Statement::Assignment(assignment) => emit_assignment(assignment, env, locals),
        _ => todo!("Only expressions and assignments"),
    }
}

fn emit_assignment(
    assignment: &TypedAssignment,
    env: Arc<environment::Environment>,
    locals: LocalGenerator,
) -> (
    Arc<environment::Environment>,
    LocalGenerator,
    WasmInstructions,
) {
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
            let (env, mut locals, mut insts) = emit_expression(&assignment.value, env, locals);
            // add variable to the environment
            let id = locals.new_local(Arc::clone(type_));
            let env = env.set(&name, id, Arc::clone(type_));
            // create local
            insts
                .lst
                .push(wasm_encoder::Instruction::LocalTee(env.get(name).unwrap()));

            (env, locals, insts)
        }
        _ => todo!("Only simple assignments"),
    }
}

fn emit_expression(
    expression: &TypedExpr,
    env: Arc<environment::Environment>,
    locals: LocalGenerator,
) -> (
    Arc<environment::Environment>,
    LocalGenerator,
    WasmInstructions,
) {
    match expression {
        TypedExpr::Int { value, .. } => {
            let val = parse_integer(value);
            (
                env,
                locals,
                WasmInstructions {
                    lst: vec![wasm_encoder::Instruction::I64Const(val)],
                },
            )
        }
        TypedExpr::NegateInt { value, .. } => {
            let (env, locals, mut insts) = emit_expression(value, env, locals);
            insts.lst.push(wasm_encoder::Instruction::I64Const(-1));
            insts.lst.push(wasm_encoder::Instruction::I64Mul);
            (env, locals, insts)
        }
        TypedExpr::Block { statements, .. } => {
            // create new scope
            // TODO: fix environment pop
            // maybe use a block instruction?
            let (_, locals, statements) = emit_statement_list(environment::Environment::new_enclosing(Arc::clone(&env)), locals, statements);
            (env, locals, statements)
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
            if !constructor.is_local_variable() {
                todo!("Only local variables");
            }

            dbg!(&env);
            let index = env.get(name).unwrap();
            (
                env,
                locals,
                WasmInstructions {
                    lst: vec![wasm_encoder::Instruction::LocalGet(index)],
                },
            )
        },
        _ => todo!("Only integer constants, integer negation, blocks, binary operations, case and variable accesses"),
    }
}

fn emit_binary_operation(
    env: Arc<environment::Environment>,
    locals: LocalGenerator,
    // only used to disambiguate equals
    _typ: &Type,
    name: BinOp,
    left: &TypedExpr,
    right: &TypedExpr,
) -> (
    Arc<environment::Environment>,
    LocalGenerator,
    WasmInstructions,
) {
    match name {
        BinOp::AddInt => {
            let (env, locals, mut insts) = emit_expression(left, env, locals);
            let (env, locals, right_insts) = emit_expression(right, env, locals);
            insts.lst.extend(right_insts.lst);
            insts.lst.push(wasm_encoder::Instruction::I64Add);
            (env, locals, insts)
        }
        BinOp::SubInt => {
            let (env, locals, mut insts) = emit_expression(left, env, locals);
            let (env, locals, right_insts) = emit_expression(right, env, locals);
            insts.lst.extend(right_insts.lst);
            insts.lst.push(wasm_encoder::Instruction::I64Sub);
            (env, locals, insts)
        }
        BinOp::MultInt => {
            let (env, locals, mut insts) = emit_expression(left, env, locals);
            let (env, locals, right_insts) = emit_expression(right, env, locals);
            insts.lst.extend(right_insts.lst);
            insts.lst.push(wasm_encoder::Instruction::I64Mul);
            (env, locals, insts)
        }
        BinOp::DivInt => {
            let (env, locals, mut insts) = emit_expression(left, env, locals);
            let (env, locals, right_insts) = emit_expression(right, env, locals);
            insts.lst.extend(right_insts.lst);
            insts.lst.push(wasm_encoder::Instruction::I64DivS);
            (env, locals, insts)
        }
        BinOp::RemainderInt => {
            let (env, locals, mut insts) = emit_expression(left, env, locals);
            let (env, locals, right_insts) = emit_expression(right, env, locals);
            insts.lst.extend(right_insts.lst);
            insts.lst.push(wasm_encoder::Instruction::I64RemS);
            (env, locals, insts)
        }
        _ => todo!("Only integer arithmetic"),
    }
}

fn parse_integer(value: &str) -> i64 {
    let val = value.replace("_", "");

    // TODO: support integers other than i64
    // why do i have do this at codegen?
    let int = if val.starts_with("0b") {
        // base 2 literal
        i64::from_str_radix(&val[2..], 2).expect("expected int to be a valid binary integer")
    } else if val.starts_with("0o") {
        // base 8 literal
        i64::from_str_radix(&val[2..], 8).expect("expected int to be a valid octal integer")
    } else if val.starts_with("0x") {
        // base 16 literal
        i64::from_str_radix(&val[2..], 16).expect("expected int to be a valid hexadecimal integer")
    } else {
        // base 10 literal
        i64::from_str_radix(&val, 10).expect("expected int to be a valid decimal integer")
    };

    int
}
