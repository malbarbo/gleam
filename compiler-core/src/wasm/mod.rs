use std::{collections::HashMap, sync::Arc};

use ecow::EcoString;
use itertools::Itertools;
use wasm_encoder::{CodeSection, FunctionSection, TypeSection};

use crate::{
    ast::{Arg, Assignment, BinOp, Function, Pattern, Statement, TypedExpr, TypedModule},
    io::FileSystemWriter,
    type_::Type,
};

#[derive(Clone, Default, Debug)]
pub struct Environment {
    bindings: HashMap<EcoString, (u32, Arc<Type>)>,
    enclosing: Option<Arc<Environment>>,
}

impl Environment {
    fn new() -> Arc<Self> {
        Arc::new(Self {
            bindings: HashMap::new(),
            enclosing: None,
        })
    }

    fn new_enclosing(enclosing: Arc<Self>) -> Arc<Self> {
        Arc::new(Self {
            bindings: HashMap::new(),
            enclosing: Some(enclosing),
        })
    }

    fn set(&self, name: &str, binding: u32, type_: Arc<Type>) -> Arc<Self> {
        let mut new_env = self.clone();
        let _ = new_env.bindings.insert(name.into(), (binding, type_));
        Arc::new(new_env)
    }

    fn get(&self, name: &str) -> Option<u32> {
        if let Some(val) = self.bindings.get(name) {
            Some(val.0)
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.get(name)
        } else {
            None
        }
    }

    fn len(&self) -> u32 {
        self.bindings.len() as u32 + self.enclosing.as_ref().map_or(0, |e| e.len())
    }
}

#[derive(Clone, Default, Debug)]
struct LocalGenerator {
    locals: Vec<Arc<Type>>,
    offset: u32,
}

impl LocalGenerator {
    fn new() -> Self {
        Self {
            locals: vec![],
            offset: 0,
        }
    }

    fn with_offset(offset: u32) -> Self {
        Self {
            locals: vec![],
            offset,
        }
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
    let bytes = emit(module);
    writer.write_bytes("out.wasm".into(), &bytes[..]).unwrap();
}

fn emit(wasm_module: WasmModule) -> Vec<u8> {
    let mut module = wasm_encoder::Module::new();

    // types
    let mut types = TypeSection::new();
    for func in wasm_module.functions.iter() {
        let _ = types.function(
            func.parameters.iter().copied().map(WasmType::to_val_type),
            std::iter::once(func.returns.to_val_type()),
        );
    }
    let _ = module.section(&types);

    // functions
    let mut functions = FunctionSection::new();
    for i in 0..wasm_module.functions.len() {
        let _ = functions.function(i as _);
    }
    let _ = module.section(&functions);

    // code
    let mut codes = CodeSection::new();
    for func in wasm_module.functions.iter() {
        let locals = func
            .locals
            .iter()
            .copied()
            .map(|typ| (1, typ.to_val_type()));
        let mut f = wasm_encoder::Function::new(locals);
        for inst in &func.instructions.lst {
            let _ = f.instruction(inst);
        }
        let _ = codes.function(&f);
    }
    let _ = module.section(&codes);

    module.finish()
}

struct WasmModule {
    functions: Vec<WasmFunction>,
}

fn construct_module(ast: &TypedModule) -> WasmModule {
    use crate::ast::TypedDefinition;

    let mut functions = vec![];

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
            _ => todo!("Only functions"),
        }
    }

    WasmModule { functions }
}

#[derive(Copy, Clone, Debug)]
enum WasmType {
    Int,
}

impl WasmType {
    fn to_val_type(self) -> wasm_encoder::ValType {
        match self {
            WasmType::Int => wasm_encoder::ValType::I64,
        }
    }
}

struct WasmInstructions {
    lst: Vec<wasm_encoder::Instruction<'static>>,
}

struct WasmFunction {
    parameters: Vec<WasmType>,
    instructions: WasmInstructions,
    returns: WasmType,
    locals: Vec<WasmType>,
}

fn emit_function(
    name: &str,
    arguments: &[Arg<Arc<Type>>],
    body: &[Statement<Arc<Type>, TypedExpr>],
    return_type: &Arc<Type>,
) -> WasmFunction {
    let mut env = Environment::new();
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
            WasmType::Int
        } else {
            todo!("Only int parameters")
        });
    }

    let n_params = parameters.len();
    let (env, locals, mut instructions) = emit_statement_list(env, locals, body);
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
                    WasmType::Int
                } else {
                    todo!("Only int return types")
                }
            })
            .collect_vec(),
        returns: if return_type.is_int() {
            WasmType::Int
        } else {
            todo!("Only int return types")
        },
    }
}

fn emit_statement_list(
    env: Arc<Environment>,
    locals: LocalGenerator,
    statements: &[Statement<Arc<Type>, TypedExpr>],
) -> (Arc<Environment>, LocalGenerator, WasmInstructions) {
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
    statement: &Statement<Arc<Type>, TypedExpr>,
    env: Arc<Environment>,
    locals: LocalGenerator,
) -> (Arc<Environment>, LocalGenerator, WasmInstructions) {
    match statement {
        Statement::Expression(expression) => emit_expression(expression, env, locals),
        Statement::Assignment(assignment) => emit_assignment(assignment, env, locals),
        _ => todo!("Only expressions and assignments"),
    }
}

fn emit_assignment(
    assignment: &Assignment<Arc<Type>, TypedExpr>,
    env: Arc<Environment>,
    locals: LocalGenerator,
) -> (Arc<Environment>, LocalGenerator, WasmInstructions) {
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
    env: Arc<Environment>,
    locals: LocalGenerator,
) -> (Arc<Environment>, LocalGenerator, WasmInstructions) {
    match expression {
        TypedExpr::Int { typ, value, .. } => {
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
            let (_, locals, statements) = emit_statement_list(Environment::new_enclosing(Arc::clone(&env)), locals, statements);
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
        TypedExpr::Case {
            typ,
            subjects,
            clauses,
            ..
        } => todo!("case todo"),
        _ => todo!("Only integer constants, integer negation, blocks, binary operations, case and variable accesses"),
    }
}

fn emit_binary_operation(
    env: Arc<Environment>,
    locals: LocalGenerator,
    // only used to disambiguate equals
    typ: &Type,
    name: BinOp,
    left: &TypedExpr,
    right: &TypedExpr,
) -> (Arc<Environment>, LocalGenerator, WasmInstructions) {
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
