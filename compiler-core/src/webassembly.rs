use std::collections::HashMap;

use ecow::EcoString;
use num_bigint::BigInt;
use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function, FunctionSection, InstructionSink, Module,
    TypeSection, ValType,
};

use crate::{
    ast::{BinOp, Definition, Statement, TypedExpr, TypedFunction, TypedModule, TypedStatement},
    line_numbers::LineNumbers,
    type_::Type,
};

#[derive(Default)]
struct Generator {
    types: HashMap<(Vec<ValType>, Vec<ValType>), u32>,
    functions: HashMap<EcoString, u32>,
    types_section: TypeSection,
    functions_section: FunctionSection,
    exports_section: ExportSection,
    codes_section: CodeSection,
}

impl Generator {
    fn function(&mut self, function: &TypedFunction) -> u32 {
        let name = function.name.clone().unwrap().1;
        if let Some(index) = self.functions.get(&name) {
            return *index;
        }
        let index = self.functions.len() as u32;
        let type_index = self.funtion_type(function);
        let _ = self.functions.insert(name.clone(), index);
        self.gen_function(name, type_index, index, &function.body);
        index
    }

    fn funtion_type(&mut self, function: &TypedFunction) -> u32 {
        let params: Vec<ValType> = function
            .arguments
            .iter()
            .map(|t| self.get_val_type(&t.type_))
            .collect();
        let results = vec![self.get_val_type(&function.return_type)];
        if let Some(index) = self.types.get(&(params.clone(), results.clone())) {
            return *index;
        }
        let index = self.types.len() as u32;
        let _ = self.types.insert((params.clone(), results.clone()), index);
        self.types_section.ty().function(params, results);
        index
    }

    fn get_val_type(&self, type_: &Type) -> ValType {
        if type_.is_int() {
            return INT.val_type();
        }
        panic!();
    }

    fn gen_function(
        &mut self,
        name: EcoString,
        type_index: u32,
        index: u32,
        body: &[TypedStatement],
    ) {
        let _ = self.functions_section.function(type_index);
        let _ = self.exports_section.export(&name, ExportKind::Func, index);
        let mut code = Function::new(vec![]);
        for stat in body {
            match stat {
                Statement::Expression(expression) => {
                    self.gen_expression(&mut code.instructions(), expression)
                }
                Statement::Assignment(_assignment) => todo!(),
                Statement::Use(_) => todo!(),
                Statement::Assert(_assert) => todo!(),
            }
        }
        let _ = self.codes_section.function(&code);
    }

    fn gen_expression(&self, instructions: &mut InstructionSink<'_>, expression: &TypedExpr) {
        let _ = match expression {
            TypedExpr::Int { int_value, .. } => {
                instructions.int_const(int_value.try_into().unwrap())
            }
            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                self.gen_expression(instructions, left);
                self.gen_expression(instructions, right);
                match name {
                    BinOp::AddInt => instructions.int_add(),
                    BinOp::SubInt => instructions.int_sub(),
                    BinOp::MultInt => instructions.int_mul(),
                    BinOp::DivInt => instructions.int_div(),
                    BinOp::RemainderInt => instructions.int_rem(),
                    _ => todo!(),
                }
            }
            TypedExpr::NegateInt { value, .. } => {
                self.gen_expression(instructions, value);
                instructions.int_neg()
            }
            _ => panic!(),
        };
    }
}

pub fn module(ast: &TypedModule, _line_numbers: &LineNumbers) -> Vec<u8> {
    let mut generator = Generator::default();
    for definition in &ast.definitions {
        match definition {
            Definition::Function(function) => {
                let _ = generator.function(function);
            }
            Definition::TypeAlias(_type_alias) => todo!(),
            Definition::CustomType(_custom_type) => todo!(),
            Definition::Import(_import) => todo!(),
            Definition::ModuleConstant(_module_constant) => todo!(),
        }
    }
    let mut module = Module::default();
    let _ = module
        .section(&generator.types_section)
        .section(&generator.functions_section)
        .section(&generator.exports_section)
        .section(&generator.codes_section);

    module.finish()
}

#[allow(dead_code)]
enum IntType {
    Int32,
    Int64,
}

impl IntType {
    fn val_type(&self) -> ValType {
        match self {
            IntType::Int32 => ValType::I32,
            IntType::Int64 => ValType::I64,
        }
    }
}

const INT: IntType = IntType::Int32;

trait IntInstructions {
    fn int_const(&mut self, value: &BigInt) -> &mut Self;
    fn int_add(&mut self) -> &mut Self;
    fn int_sub(&mut self) -> &mut Self;
    fn int_mul(&mut self) -> &mut Self;
    fn int_rem(&mut self) -> &mut Self;
    fn int_div(&mut self) -> &mut Self;
    fn int_neg(&mut self) -> &mut Self;
}

macro_rules! int_op {
    ($name:ident, $i32:ident, $i64:ident) => {
        fn $name(&mut self) -> &mut Self {
            match INT {
                IntType::Int32 => self.$i32(),
                IntType::Int64 => self.$i64(),
            }
        }
    };
}

impl<'a> IntInstructions for InstructionSink<'a> {
    fn int_const(&mut self, value: &BigInt) -> &mut Self {
        match INT {
            IntType::Int32 => self.i32_const(value.try_into().unwrap()),
            IntType::Int64 => self.i64_const(value.try_into().unwrap()),
        }
    }

    fn int_div(&mut self) -> &mut Self {
        match INT {
            IntType::Int32 => todo!(),
            IntType::Int64 => todo!(),
        }
    }

    fn int_neg(&mut self) -> &mut Self {
        match INT {
            IntType::Int32 => self.i32_const(0).i32_sub(),
            IntType::Int64 => self.i64_const(0).i64_sub(),
        }
    }

    int_op!(int_add, i32_add, i64_add);
    int_op!(int_sub, i32_sub, i64_sub);
    int_op!(int_mul, i32_mul, i64_mul);
    int_op!(int_rem, i32_rem_s, i64_rem_s);
}
