use std::sync::Arc;

use ecow::EcoString;
use wasm_encoder::{CodeSection, FunctionSection, Instruction, TypeSection, ValType};

use crate::ast::{BinOp, Function, Statement, TypedDefinition, TypedExpr, TypedModule};
use crate::io::FileSystemWriter;
use crate::type_::Type;

pub fn module(writer: &impl FileSystemWriter, ast: &TypedModule) {
    let mut compiler = ModuleEmitter::new();
    for def in &ast.definitions {
        compiler.definition(def);
    }
    let bytes = compiler.to_wasm_bytecode();
    writer.write_bytes("out.wasm".into(), &bytes[..]).unwrap();
}

#[derive(Debug, Default, Clone)]
struct WasmFunction<'a> {
    parameters: Vec<ValType>,
    returns: Option<ValType>,
    instructions: Vec<Instruction<'a>>,
}

#[derive(Debug, Default, Clone)]
struct ModuleEmitter<'a> {
    functions: Vec<WasmFunction<'a>>,
}

impl<'a> ModuleEmitter<'a> {
    // Util
    fn new() -> Self {
        Self { functions: vec![] }
    }

    fn to_wasm_bytecode(self) -> Vec<u8> {
        let mut module = wasm_encoder::Module::new();

        // types
        let mut types = TypeSection::new();
        for func in self.functions.iter() {
            let _ = types.function(
                func.parameters.clone(),
                func.returns.map_or_else(|| vec![], |x| vec![x]),
            );
        }
        let _ = module.section(&types);

        // functions
        let mut functions = FunctionSection::new();
        for i in 0..self.functions.iter().len() {
            let _ = functions.function(i as _);
        }
        let _ = module.section(&functions);

        // code
        let mut codes = CodeSection::new();
        for func in self.functions.iter() {
            let locals = vec![];
            let mut f = wasm_encoder::Function::new(locals);
            for inst in &func.instructions {
                let _ = f.instruction(inst);
            }
            let _ = codes.function(&f);
        }
        let _ = module.section(&codes);

        module.finish()
    }

    // Recursive codegen
    fn definition(&mut self, def: &TypedDefinition) {
        match def {
            TypedDefinition::Function(f) => {
                let func = self.function(f);
                self.functions.push(func);
            }
            _ => todo!(),
        }
    }

    fn function(&mut self, func: &Function<Arc<Type>, TypedExpr>) -> WasmFunction<'a> {
        // arguments must be all integers for now
        let mut params = vec![];
        for argument in &func.arguments {
            if argument.type_.is_int() {
                params.push(ValType::I32);
            } else if argument.type_.is_float() {
                params.push(ValType::F64);
            } else {
                todo!("Only integer and floating-point arguments");
            }
        }

        // return type must be a single integer or nothing
        let return_type = if func.return_type.is_int() {
            Some(ValType::I32)
        } else if func.return_type.is_float() {
            Some(ValType::F64)
        } else if func.return_type.is_nil() {
            None
        } else {
            todo!("Only integer/floating-point return types or nil");
        };

        // get statements
        let mut instrs = vec![];
        for stmt in &func.body[..func.body.len() - 1] {
            instrs.extend(self.statement(stmt));
            instrs.push(Instruction::Drop);
        }
        instrs.extend(self.statement(func.body.last()));

        instrs.push(Instruction::End);

        WasmFunction {
            returns: return_type,
            instructions: instrs,
            parameters: params,
        }
    }

    fn statement(&mut self, stmt: &Statement<Arc<Type>, TypedExpr>) -> Vec<Instruction<'a>> {
        match stmt {
            Statement::Expression(e) => self.expression(&e),
            _ => todo!(),
        }
    }

    fn expression(&mut self, expr: &TypedExpr) -> Vec<Instruction<'a>> {
        match expr {
            TypedExpr::Int {
                value,
                ..
            } => Self::emit_integer(value),
            TypedExpr::Float {
                value,
                ..
            } => Self::emit_float(value),
            TypedExpr::BinOp {
                name,
                left,
                right,
                ..
            } => self.bin_op(left, *name, right),
            TypedExpr::NegateInt { value, .. } => self.negate_int(value),
            TypedExpr::Block { statements, .. } => {
                let mut insts = vec![];
                for stmt in &statements[..(statements.len() - 1)] {
                    insts.extend(self.statement(stmt));
                    insts.push(Instruction::Drop);
                }
                insts.extend(self.statement(statements.last()));
                insts
            }
            _ => todo!(),
        }
    }

    fn negate_int(&mut self, intexpr: &TypedExpr) -> Vec<Instruction<'a>> {
        let mut insts = vec![];
        insts.push(Instruction::I32Const(0));
        insts.extend(self.expression(intexpr));
        insts.push(Instruction::I32Sub);
        insts
    }

    fn bin_op(&mut self, lhs: &TypedExpr, op: BinOp, rhs: &TypedExpr) -> Vec<Instruction<'a>> {
        match op {
            BinOp::AddInt => {
                // emit lhs (integer, supposedly)
                let mut insts = self.expression(lhs);
                // emit rhs
                insts.extend(self.expression(rhs));
                // emit summation
                insts.push(Instruction::I32Add);
                insts
            }
            BinOp::SubInt => {
                // emit lhs (integer, supposedly)
                let mut insts = self.expression(lhs);
                // emit rhs
                insts.extend(self.expression(rhs));
                // emit subtraction
                insts.push(Instruction::I32Sub);
                insts
            }
            BinOp::MultInt => {
                // emit lhs (integer, supposedly)
                let mut insts = self.expression(lhs);
                // emit rhs
                insts.extend(self.expression(rhs));
                // emit multiplication
                insts.push(Instruction::I32Mul);
                insts
            }
            BinOp::DivInt => {
                // emit lhs (integer, supposedly)
                let mut insts = self.expression(lhs);
                // emit rhs
                insts.extend(self.expression(rhs));
                // emit division
                // TODO: add special case for division by 0
                insts.push(Instruction::I32DivS);
                insts
            }
            BinOp::RemainderInt => {
                // emit lhs (integer, supposedly)
                let mut insts = self.expression(lhs);
                // emit rhs
                insts.extend(self.expression(rhs));
                // emit remainder
                // TODO: add special case for division by 0
                insts.push(Instruction::I32RemS);
                insts
            }
            BinOp::AddFloat => {
                // emit lhs (integer, supposedly)
                let mut insts = self.expression(lhs);
                // emit rhs
                insts.extend(self.expression(rhs));
                // emit summation
                insts.push(Instruction::F64Add);
                insts
            }
            BinOp::SubFloat => {
                // emit lhs (integer, supposedly)
                let mut insts = self.expression(lhs);
                // emit rhs
                insts.extend(self.expression(rhs));
                // emit subtraction
                insts.push(Instruction::F64Sub);
                insts
            }
            BinOp::MultFloat => {
                // emit lhs (integer, supposedly)
                let mut insts = self.expression(lhs);
                // emit rhs
                insts.extend(self.expression(rhs));
                // emit multiplication
                insts.push(Instruction::F64Mul);
                insts
            }
            BinOp::DivFloat => {
                // emit lhs (integer, supposedly)
                let mut insts = self.expression(lhs);
                // emit rhs
                insts.extend(self.expression(rhs));
                // emit division
                // TODO: add special case for division by 0
                insts.push(Instruction::F64Div);
                insts
            }
            _ => todo!(),
        }
    }

    fn emit_integer(value: &EcoString) -> Vec<Instruction<'a>> {
        let val = value.replace("_", "");

        // TODO: support integers other than i32
        // why do i have do this at codegen?
        let int = if val.starts_with("0b") {
            // base 2 literal
            i32::from_str_radix(&val[2..], 2).expect("it to be a valid binary integer")
        } else if val.starts_with("0o") {
            // base 8 literal
            i32::from_str_radix(&val[2..], 8).expect("it to be a valid octal integer")
        } else if val.starts_with("0x") {
            // base 16 literal
            i32::from_str_radix(&val[2..], 16).expect("it to be a valid hexadecimal integer")
        } else {
            // base 10 literal
            i32::from_str_radix(&val, 10).expect("it to be a valid decimal integer")
        };

        vec![Instruction::I32Const(int)]
    }

    fn emit_float(value: &str) -> Vec<Instruction<'a>> {
        let sign = if value.starts_with('-') {
            -1.0
        } else {
            1.0
        };

        let value = value.trim_start_matches(['+', '-'].as_ref());
        let value: f64 = value.parse().expect("it to be a valid floating-point constant");

        vec![Instruction::F64Const(value * sign)]
    }
}
