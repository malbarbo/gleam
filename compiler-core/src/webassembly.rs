#![allow(clippy::todo, clippy::unwrap_used)]
use std::{cell::RefCell, collections::HashMap, fmt::Write, ops::Deref, rc::Rc, sync::Arc};

use ecow::EcoString;
use num_bigint::BigInt;
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, ExportKind, ExportSection, Function, FunctionSection,
    GlobalSection, GlobalType, HeapType, InstructionSink, Module, RefType, StartSection,
    TypeSection, ValType,
};

use crate::{
    ast::{
        AssignmentKind, BinOp, Constant, Definition, OperatorKind, Pattern, SrcSpan, Statement,
        TypedArg, TypedAssignment, TypedExpr, TypedFunction, TypedModule, TypedModuleConstant,
        TypedPattern, TypedStatement,
    },
    line_numbers::LineNumbers,
    type_::{Type, TypeVar},
};

const MAIN: &str = "main";
const START: &str = "$start";
const TRUE: &str = "True";
const FALSE: &str = "False";

pub fn module(module: &TypedModule, _line_numbers: &LineNumbers) -> Vec<u8> {
    let mut generator = Generator::new(module);
    generator.all_pub();
    let mut module = Module::default();
    let mut codes_section = CodeSection::new();
    generator.functions.sort_by_key(|t| t.0);
    for function in generator.functions {
        let _ = codes_section.function(&function.1);
    }
    let _ = module
        .section(&generator.types_section)
        .section(&generator.functions_section)
        .section(&generator.globals_sections)
        .section(&generator.exports_section);
    if let Some(function_index) = generator.start {
        let _ = module.section(&StartSection { function_index });
    }
    let _ = module.section(&codes_section);
    module.finish()
}

struct Generator<'a> {
    start: Option<u32>,
    functions_types: HashMap<(Vec<ValType>, Vec<ValType>), u32>,
    functions: Vec<(u32, Function)>,
    next_function_id: u32,
    globals_sections: GlobalSection,
    types_section: TypeSection,
    functions_section: FunctionSection,
    exports_section: ExportSection,
    globals: Rc<RefCell<Vec<Id>>>,
    module: &'a TypedModule,
}

fn find_global(name: &EcoString, globals: &RefCell<Vec<Id>>) -> Option<Id> {
    globals.borrow().iter().find(|id| &id.name == name).cloned()
}

impl<'a> Generator<'a> {
    fn new(module: &'a TypedModule) -> Self {
        Generator {
            start: None,
            functions_types: HashMap::new(),
            functions: vec![],
            next_function_id: 0,
            globals_sections: GlobalSection::new(),
            types_section: TypeSection::new(),
            functions_section: FunctionSection::new(),
            exports_section: ExportSection::new(),
            globals: Rc::default(),
            module,
        }
    }

    fn all_pub(&mut self) {
        for definition in &self.module.definitions {
            match definition {
                Definition::ModuleConstant(module_constant) => {
                    let _ = self.constant(module_constant);
                }
                Definition::Function(function) => {
                    if !function.publicity.is_public() || is_generic_type(&function_type(function))
                    {
                        continue;
                    }
                    let id = self.function(function);
                    if is_main_funtion(function) {
                        self.start(id);
                    }
                }
                Definition::TypeAlias(_type_alias) => todo!(),
                Definition::CustomType(_custom_type) => todo!(),
                Definition::Import(_import) => todo!(),
            }
        }
    }

    fn start(&mut self, id: Id) {
        let type_index = self.function_type_index(vec![], None);
        let index = self.next_function_id();
        let _ = self.functions_section.function(type_index);
        let mut code = Function::new([]);
        let _ = code.instructions().call(id.index).drop().end();
        self.functions.push((index, code));
        let _ = self.exports_section.export(START, ExportKind::Func, index);
        self.start = Some(index);
    }

    fn on_demand(&mut self, name: &EcoString, required_type: &Type) -> Id {
        for definition in &self.module.definitions {
            match definition {
                Definition::ModuleConstant(module_constant) if &module_constant.name == name => {
                    assert!(required_type.same_as(&module_constant.type_));
                    return self.constant(module_constant);
                }
                Definition::Function(function) if &function.name.as_ref().unwrap().1 == name => {
                    let (params, return_) = required_type.fn_types().unwrap();
                    let declared_type = function_type(function);
                    return if is_generic_type(&declared_type) {
                        match find_global(&mangle(name, &params, &return_), &self.globals) {
                            Some(id) => id, // the function has already been monomorphized
                            None => self.function(&monomorphize(function, &params, &return_)),
                        }
                    } else {
                        self.function(function)
                    };
                }
                _ => {}
            }
        }
        panic!(
            "Name not found: {:?}. Are you using closures? They are not supporte yet.",
            name
        );
    }

    fn next_function_id(&mut self) -> u32 {
        let id = self.next_function_id;
        self.next_function_id += 1;
        id
    }

    fn function(&mut self, function: &TypedFunction) -> Id {
        let name = function.name.clone().unwrap().1;
        if let Some(id) = find_global(&name, &self.globals) {
            return id;
        }

        let index = self.next_function_id();
        let _ = self.exports_section.export(&name, ExportKind::Func, index);
        self.globals
            .borrow_mut()
            .push(Id::func(name.clone(), index));
        let type_index = self.function_type(&function.arguments, &function.return_type);
        let _ = self.functions_section.function(type_index);

        let locals = Locals::new(self, function.arguments.len() as u32, &function.body);
        let mut code = Function::new(locals.val_types());
        self.statements(
            &mut code.instructions(),
            Scope::with_params(self.globals.clone(), &function.arguments),
            &locals,
            &function.body,
        );
        let _ = code.instructions().end();
        self.functions.push((index, code));
        Id::func(name, index)
    }

    fn local_function(
        &mut self,
        name: EcoString,
        type_: &Arc<Type>,
        arguments: &[TypedArg],
        body: &[TypedStatement],
    ) -> Id {
        if let Some(id) = find_global(&name, &self.globals) {
            return id;
        }

        let index = self.next_function_id();
        let _ = self.exports_section.export(&name, ExportKind::Func, index);
        self.globals
            .borrow_mut()
            .push(Id::func(name.clone(), index));

        let (_, return_) = type_.fn_types().unwrap();
        let type_index = self.function_type(arguments, &return_);
        let _ = self.functions_section.function(type_index);

        let locals = Locals::new(self, arguments.len() as u32, body);
        let mut code = Function::new(locals.val_types());
        self.statements(
            &mut code.instructions(),
            Scope::with_params(self.globals.clone(), arguments),
            &locals,
            body,
        );
        let _ = code.instructions().end();
        self.functions.push((index, code));
        Id::func(name, index)
    }

    fn function_type(&mut self, arguments: &[TypedArg], return_: &Arc<Type>) -> u32 {
        let params: Vec<ValType> = arguments.iter().map(|t| self.val_type(&t.type_)).collect();
        let result = self.val_type(return_);
        self.function_type_index(params, Some(result))
    }

    fn function_type_index(&mut self, params: Vec<ValType>, result: Option<ValType>) -> u32 {
        let results: Vec<_> = result.into_iter().collect();
        if let Some(index) = self.functions_types.get(&(params.clone(), results.clone())) {
            return *index;
        }
        let index = self.functions_types.len() as u32;
        let _ = self
            .functions_types
            .insert((params.clone(), results.clone()), index);
        self.types_section.ty().function(params, results);
        index
    }

    fn val_type(&mut self, type_: &Type) -> ValType {
        if type_.is_int() {
            return INT.val_type();
        }
        if type_.is_bool() {
            return BOOL.val_type();
        }
        if type_.is_float() {
            return FLOAT.val_type();
        }
        if let Some((params, return_)) = type_.fn_types() {
            let params: Vec<_> = params.iter().map(|type_| self.val_type(type_)).collect();
            let return_ = self.val_type(&return_);
            let type_index = self.function_type_index(params, Some(return_));
            return ValType::Ref(RefType {
                heap_type: HeapType::Concrete(type_index),
                nullable: false,
            });
        }
        panic!("Type not supported: {:?}", type_);
    }

    fn statements(
        &mut self,
        instructions: &mut InstructionSink<'_>,
        mut scope: Scope,
        locals: &Locals,
        statements: &[TypedStatement],
    ) {
        for statement in statements {
            scope = self.statement(instructions, scope, locals, statement);
            if Some(statement) != statements.last() {
                let _ = instructions.drop();
            }
        }
    }

    fn statement(
        &mut self,
        instructions: &mut InstructionSink<'_>,
        mut scope: Scope,
        locals: &Locals,
        statement: &TypedStatement,
    ) -> Scope {
        match statement {
            Statement::Expression(expression) => {
                self.expression(locals, scope.clone(), instructions, expression)
            }
            Statement::Assignment(assignment) => {
                scope = self.assigment(locals, scope, instructions, assignment);
            }
            Statement::Use(_) => todo!(),
            Statement::Assert(_assert) => todo!(),
        }
        scope
    }

    fn expression(
        &mut self,
        locals: &Locals,
        scope: Scope,
        instructions: &mut InstructionSink<'_>,
        expression: &TypedExpr,
    ) {
        match expression {
            TypedExpr::Todo { .. } | TypedExpr::Panic { .. } => {
                let _ = instructions.unreachable();
            }
            TypedExpr::Int { int_value, .. } => {
                let _ = instructions.int_const(int_value);
            }
            TypedExpr::Float { value, .. } => {
                let _ = instructions.float_const(value);
            }
            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                if name.operator_kind() != OperatorKind::BooleanLogic {
                    self.expression(locals, scope.clone(), instructions, left);
                    self.expression(locals, scope.clone(), instructions, right);
                }

                let _ = match name {
                    // Bool
                    BinOp::And => {
                        self.expression(locals, scope.clone(), instructions, left);
                        let _ = instructions.if_(BlockType::Result(BOOL.val_type()));
                        self.expression(locals, scope, instructions, right);
                        instructions.else_().bool_const(false).end()
                    }
                    BinOp::Or => {
                        self.expression(locals, scope.clone(), instructions, left);
                        let _ = instructions.if_(BlockType::Result(BOOL.val_type()));
                        let _ = instructions.bool_const(true);
                        let _ = instructions.else_();
                        self.expression(locals, scope, instructions, right);
                        instructions.end()
                    }
                    BinOp::Eq if left.type_().is_bool() => instructions.bool_eq(),
                    BinOp::NotEq if left.type_().is_bool() => instructions.bool_ne(),
                    // Int
                    BinOp::AddInt => instructions.int_add(),
                    BinOp::SubInt => instructions.int_sub(),
                    BinOp::MultInt => instructions.int_mul(),
                    BinOp::DivInt => instructions.int_div(locals.get_int_div()),
                    BinOp::RemainderInt => instructions.int_rem(),
                    BinOp::LtInt => instructions.int_lt(),
                    BinOp::LtEqInt => instructions.int_le(),
                    BinOp::GtInt => instructions.int_gt(),
                    BinOp::GtEqInt => instructions.int_ge(),
                    BinOp::Eq if left.type_().is_int() => instructions.int_eq(),
                    BinOp::NotEq if left.type_().is_int() => instructions.int_ne(),
                    // Float
                    BinOp::AddFloat => instructions.float_add(),
                    BinOp::SubFloat => instructions.float_sub(),
                    BinOp::MultFloat => instructions.float_mul(),
                    BinOp::DivFloat => instructions.float_div(locals.get_float_div()),
                    BinOp::LtFloat => instructions.float_lt(),
                    BinOp::LtEqFloat => instructions.float_le(),
                    BinOp::GtFloat => instructions.float_gt(),
                    BinOp::GtEqFloat => instructions.float_ge(),
                    BinOp::Eq if left.type_().is_float() => instructions.float_eq(),
                    BinOp::NotEq if left.type_().is_float() => instructions.float_ne(),
                    _ => todo!(),
                };
            }
            TypedExpr::NegateInt { value, .. } => {
                let _ = instructions.int_const(&0.into());
                self.expression(locals, scope, instructions, value);
                let _ = instructions.int_sub();
            }
            TypedExpr::NegateBool { value, .. } => {
                self.expression(locals, scope, instructions, value);
                let _ = instructions.bool_neg();
            }
            TypedExpr::Block { statements, .. } => {
                self.statements(instructions, scope, locals, statements);
            }
            TypedExpr::Var { name, .. } if is_bool_const(name, &expression.type_()) => {
                let _ = instructions.bool_const(name == TRUE);
            }
            TypedExpr::Var { name, .. } => {
                let id = scope
                    .find(name)
                    .unwrap_or_else(|| self.on_demand(name, &expression.type_()));

                let _ = match id.kind {
                    IdKind::Func => instructions.ref_func(id.index),
                    IdKind::Global => instructions.global_get(id.index),
                    IdKind::Local => instructions.local_get(id.index),
                };
            }
            TypedExpr::Call {
                type_,
                fun,
                arguments,
                ..
            } => {
                // FIXME: function must be evaluated first
                for arg in arguments {
                    self.expression(locals, scope.clone(), instructions, &arg.value);
                }
                self.expression(locals, scope, instructions, fun);
                let params: Vec<ValType> = arguments
                    .iter()
                    .map(|arg| self.val_type(&arg.value.type_()))
                    .collect();
                let result = self.val_type(type_);
                let index = self.function_type_index(params, Some(result));
                let _ = instructions.call_ref(index);
            }
            TypedExpr::Fn {
                location,
                type_,
                arguments,
                body,
                ..
            } => {
                let name: EcoString =
                    format!("anonymous@{}-{}", location.start, location.end).into();
                let id = self.local_function(name, type_, arguments, body);
                let _ = instructions.ref_func(id.index);
            }
            _ => todo!(),
        };
    }

    fn assigment(
        &mut self,
        locals: &Locals,
        mut scope: Scope,
        instructions: &mut InstructionSink<'_>,
        assignment: &TypedAssignment,
    ) -> Scope {
        self.expression(locals, scope.clone(), instructions, &assignment.value);
        match assignment.kind {
            AssignmentKind::Assert { .. } => match &assignment.pattern {
                Pattern::Int { int_value, .. } => {
                    let _ = instructions
                        .int_const(int_value)
                        .int_eq()
                        .if_(BlockType::Result(INT.val_type()))
                        .int_const(int_value)
                        .else_()
                        .unreachable()
                        .end();
                }
                Pattern::Float { value, .. } => {
                    let _ = instructions
                        .float_const(value)
                        .float_eq()
                        .if_(BlockType::Result(FLOAT.val_type()))
                        .float_const(value)
                        .else_()
                        .unreachable()
                        .end();
                }
                Pattern::Constructor { name, type_, .. }
                    if (name == TRUE || name == FALSE) && type_.is_bool() =>
                {
                    let value = name == TRUE;
                    let _ = instructions
                        .bool_const(value)
                        .bool_eq()
                        .if_(BlockType::Result(BOOL.val_type()))
                        .bool_const(value)
                        .else_()
                        .unreachable()
                        .end();
                }
                _ => todo!(),
            },
            AssignmentKind::Let => match &assignment.pattern {
                Pattern::Variable { name, location, .. } => {
                    let index = locals.get(location);
                    scope = scope.insert_local(name.clone(), index);
                    let _ = instructions.local_set(index).local_get(index);
                }
                _ => todo!(),
            },
            AssignmentKind::Generated => todo!(),
        }
        scope
    }

    fn constant(&mut self, module_constant: &TypedModuleConstant) -> Id {
        let (val_type, expr) = match &*module_constant.value {
            Constant::Int { int_value, .. } => (INT.val_type(), INT.int_const(int_value)),
            Constant::Float { value, .. } => (FLOAT.val_type(), FLOAT.float_const(value)),
            Constant::Record { name, type_, .. } if is_bool_const(name, type_) => {
                (BOOL.val_type(), BOOL.bool_const(name == TRUE))
            }
            _ => todo!(),
        };
        let global_type = GlobalType {
            val_type,
            mutable: false,
            shared: false,
        };
        let index = self.globals_sections.len();
        let _ = self.globals_sections.global(global_type, &expr);
        if module_constant.publicity.is_public() {
            let _ = self
                .exports_section
                .export(&module_constant.name, ExportKind::Global, index);
        }
        let id = Id::global(module_constant.name.clone(), index);
        self.globals.borrow_mut().push(id.clone());
        id
    }
}

fn is_main_funtion(function: &TypedFunction) -> bool {
    function
        .name
        .as_ref()
        .map(|name| name.1 == MAIN)
        .unwrap_or(false)
        && function.arguments.is_empty()
}

struct BoolType {}

const BOOL: BoolType = BoolType {};

impl BoolType {
    fn val_type(&self) -> ValType {
        ValType::I32
    }

    fn bool_const(&self, bool_: bool) -> ConstExpr {
        ConstExpr::i32_const(bool_.into())
    }
}

trait BoolInstructions {
    fn bool_const(&mut self, value: bool) -> &mut Self;
    fn bool_neg(&mut self) -> &mut Self;
    fn bool_eq(&mut self) -> &mut Self;
    fn bool_ne(&mut self) -> &mut Self;
}

impl<'a> BoolInstructions for InstructionSink<'a> {
    fn bool_const(&mut self, value: bool) -> &mut Self {
        self.i32_const(value as _)
    }
    fn bool_neg(&mut self) -> &mut Self {
        self.i32_eqz()
    }

    fn bool_eq(&mut self) -> &mut Self {
        self.i32_eq()
    }

    fn bool_ne(&mut self) -> &mut Self {
        self.i32_ne()
    }
}

#[allow(dead_code)]
enum IntType {
    Int32,
    Int64,
}

const INT: IntType = IntType::Int32;

impl IntType {
    fn val_type(&self) -> ValType {
        match self {
            IntType::Int32 => ValType::I32,
            IntType::Int64 => ValType::I64,
        }
    }

    fn int_const(&self, value: &BigInt) -> ConstExpr {
        match self {
            IntType::Int32 => ConstExpr::i32_const(value.try_into().unwrap()),
            IntType::Int64 => ConstExpr::i64_const(value.try_into().unwrap()),
        }
    }
}

trait IntInstructions {
    fn int_const(&mut self, value: &BigInt) -> &mut Self;
    fn int_add(&mut self) -> &mut Self;
    fn int_sub(&mut self) -> &mut Self;
    fn int_mul(&mut self) -> &mut Self;
    fn int_rem(&mut self) -> &mut Self;
    fn int_div(&mut self, local: u32) -> &mut Self;
    fn int_eq(&mut self) -> &mut Self;
    fn int_ne(&mut self) -> &mut Self;
    fn int_lt(&mut self) -> &mut Self;
    fn int_le(&mut self) -> &mut Self;
    fn int_gt(&mut self) -> &mut Self;
    fn int_ge(&mut self) -> &mut Self;
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

    fn int_div(&mut self, local: u32) -> &mut Self {
        let divisor = local;
        let dividend = local + 1;
        match INT {
            IntType::Int32 => self
                .local_set(divisor)
                .local_set(dividend)
                .local_get(divisor)
                .if_(BlockType::Result(ValType::I32))
                .local_get(dividend)
                .local_get(divisor)
                .i32_div_s()
                .else_()
                .i32_const(0)
                .end(),
            IntType::Int64 => self
                .local_set(divisor)
                .local_set(dividend)
                .local_get(divisor)
                .if_(BlockType::Result(ValType::I64))
                .local_get(dividend)
                .local_get(divisor)
                .i64_div_s()
                .else_()
                .i64_const(0)
                .end(),
        }
    }

    int_op!(int_add, i32_add, i64_add);
    int_op!(int_sub, i32_sub, i64_sub);
    int_op!(int_mul, i32_mul, i64_mul);
    int_op!(int_rem, i32_rem_s, i64_rem_s);
    int_op!(int_eq, i32_eq, i64_eq);
    int_op!(int_ne, i32_ne, i64_ne);
    int_op!(int_lt, i32_lt_s, i64_lt_s);
    int_op!(int_le, i32_le_s, i64_le_s);
    int_op!(int_gt, i32_gt_s, i64_gt_s);
    int_op!(int_ge, i32_ge_s, i64_ge_s);
}

struct FloatType {}

const FLOAT: FloatType = FloatType {};

impl FloatType {
    fn val_type(&self) -> ValType {
        ValType::F64
    }

    fn float_const(&self, value: &EcoString) -> ConstExpr {
        ConstExpr::f64_const(value.parse::<f64>().unwrap().into())
    }
}

trait FloatInstructions {
    fn float_const(&mut self, value: &str) -> &mut Self;
    fn float_add(&mut self) -> &mut Self;
    fn float_sub(&mut self) -> &mut Self;
    fn float_mul(&mut self) -> &mut Self;
    fn float_div(&mut self, local: u32) -> &mut Self;
    fn float_eq(&mut self) -> &mut Self;
    fn float_ne(&mut self) -> &mut Self;
    fn float_lt(&mut self) -> &mut Self;
    fn float_le(&mut self) -> &mut Self;
    fn float_gt(&mut self) -> &mut Self;
    fn float_ge(&mut self) -> &mut Self;
}

macro_rules! float_op {
    ($name:ident, $f64:ident) => {
        fn $name(&mut self) -> &mut Self {
            self.$f64()
        }
    };
}

impl<'a> FloatInstructions for InstructionSink<'a> {
    fn float_const(&mut self, value: &str) -> &mut Self {
        self.f64_const(value.parse::<f64>().unwrap().into())
    }

    fn float_div(&mut self, local: u32) -> &mut Self {
        let divisor = local;
        let dividend = local + 1;
        self.local_set(divisor)
            .local_set(dividend)
            .local_get(divisor)
            .f64_const(0.0f64.into())
            .f64_ne()
            .if_(BlockType::Result(ValType::F64))
            .local_get(dividend)
            .local_get(divisor)
            .f64_div()
            .else_()
            .f64_const(0.0f64.into())
            .end()
    }

    float_op!(float_add, f64_add);
    float_op!(float_sub, f64_sub);
    float_op!(float_mul, f64_mul);
    float_op!(float_eq, f64_eq);
    float_op!(float_ne, f64_ne);
    float_op!(float_lt, f64_lt);
    float_op!(float_le, f64_le);
    float_op!(float_gt, f64_gt);
    float_op!(float_ge, f64_ge);
}

#[derive(Clone)]
enum IdKind {
    Global,
    Func,
    Local,
}

#[derive(Clone)]
struct Id {
    kind: IdKind,
    name: EcoString,
    index: u32,
}

impl Id {
    fn global(name: EcoString, index: u32) -> Id {
        Id {
            kind: IdKind::Global,
            name,
            index,
        }
    }

    fn func(name: EcoString, index: u32) -> Id {
        Id {
            kind: IdKind::Func,
            name,
            index,
        }
    }

    fn local(name: EcoString, index: u32) -> Id {
        Id {
            kind: IdKind::Local,
            name,
            index,
        }
    }
}

#[derive(Clone)]
enum Scope {
    Global(Rc<RefCell<Vec<Id>>>),
    Entry(Id, Rc<Scope>),
}

impl Scope {
    fn with_params(globals: Rc<RefCell<Vec<Id>>>, args: &[TypedArg]) -> Scope {
        let mut scope = Scope::Global(globals);
        for (index, arg) in args.iter().enumerate() {
            if let Some(name) = arg.get_variable_name() {
                scope = scope.insert_local(name.clone(), index as u32);
            }
        }
        scope
    }

    fn insert_local(&self, name: EcoString, index: u32) -> Scope {
        Scope::Entry(Id::local(name, index), self.clone().into())
    }

    fn find(&self, name: &EcoString) -> Option<Id> {
        match self {
            Scope::Global(globals) => find_global(name, globals),
            Scope::Entry(id, _scope) if &id.name == name => Some(id.clone()),
            Scope::Entry(_id, scope) => scope.find(name),
        }
    }
}

#[derive(Default, Debug)]
struct Locals {
    skip: u32,
    locals: HashMap<SrcSpan, u32>,
    val_types: Vec<ValType>,
    int_div: bool,
    float_div: bool,
}

impl Locals {
    fn new(generator: &mut Generator<'_>, num_params: u32, statements: &[TypedStatement]) -> Self {
        let mut locals = Locals {
            skip: num_params,
            locals: HashMap::new(),
            int_div: false,
            float_div: false,
            val_types: vec![],
        };
        locals.statements(generator, statements);
        locals
    }

    fn statements(&mut self, generator: &mut Generator<'_>, statements: &[TypedStatement]) {
        for statement in statements {
            self.statement(generator, statement);
        }
    }

    fn statement(&mut self, generator: &mut Generator<'_>, statement: &TypedStatement) {
        match statement {
            Statement::Expression(expression) => self.expression(generator, expression),
            Statement::Assignment(assignment) => self.assignment(generator, assignment),
            _ => todo!(),
        }
    }

    fn expression(&mut self, generator: &mut Generator<'_>, expression: &TypedExpr) {
        match expression {
            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                self.expression(generator, left);
                self.expression(generator, right);
                if matches!(name, BinOp::DivInt) {
                    self.int_div = true;
                }
                if matches!(name, BinOp::DivFloat) {
                    self.float_div = true;
                }
            }
            TypedExpr::Block { statements, .. } => {
                self.statements(generator, statements);
            }
            TypedExpr::Call { fun, arguments, .. } => {
                self.expression(generator, fun);
                for arg in arguments {
                    self.expression(generator, &arg.value);
                }
            }
            TypedExpr::NegateInt { value, .. } | TypedExpr::NegateBool { value, .. } => {
                self.expression(generator, value);
            }
            TypedExpr::Todo { message, .. } | TypedExpr::Panic { message, .. } => {
                if let Some(value) = message {
                    self.expression(generator, value);
                }
            }
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::Fn { .. } => {}
            _ => todo!("{:?}", expression),
        }
    }

    fn assignment(&mut self, generator: &mut Generator<'_>, assignment: &TypedAssignment) {
        self.expression(generator, &assignment.value);
        match &assignment.kind {
            AssignmentKind::Let => match &assignment.pattern {
                Pattern::Variable {
                    name,
                    location,
                    type_,
                    ..
                } => {
                    if is_generic_type(type_) {
                        panic!("Local function \"{name}\" cannot be generic.");
                    }
                    self.insert(generator, location, type_)
                }
                _ => todo!(),
            },
            AssignmentKind::Assert { .. } => match &assignment.pattern {
                Pattern::Int { .. } | Pattern::Float { .. } => {}
                Pattern::Constructor { name, type_, .. } if is_bool_const(name, type_) => {}
                _ => todo!(),
            },
            AssignmentKind::Generated => todo!(),
        }
    }

    fn insert(&mut self, generator: &mut Generator<'_>, location: &SrcSpan, type_: &Arc<Type>) {
        let index = self.locals.len() as u32 + self.skip;
        let _ = self.locals.insert(*location, index);
        self.val_types.push(generator.val_type(type_));
    }

    fn get(&self, location: &SrcSpan) -> u32 {
        *self.locals.get(location).unwrap()
    }

    fn get_int_div(&self) -> u32 {
        assert!(self.int_div);
        self.locals.len() as u32 + self.skip
    }

    fn get_float_div(&self) -> u32 {
        assert!(self.float_div);
        let div = if self.int_div { 2 } else { 0 };
        self.locals.len() as u32 + div + self.skip
    }

    fn val_types(&self) -> Vec<(u32, ValType)> {
        // FIXME: group locals by type
        let mut val_types: Vec<_> = self.val_types.iter().map(|e| (1, *e)).collect();
        if self.int_div {
            val_types.push((2, INT.val_type()));
        }
        if self.float_div {
            val_types.push((2, FLOAT.val_type()));
        }
        val_types
    }
}

fn is_bool_const(name: &str, type_: &Arc<Type>) -> bool {
    (name == TRUE || name == FALSE) && type_.is_bool()
}

fn is_generic_type(type_: &Arc<Type>) -> bool {
    match &**type_ {
        Type::Named { arguments, .. } => arguments.iter().any(is_generic_type),
        Type::Fn { arguments, return_ } => {
            arguments.iter().any(is_generic_type) || is_generic_type(return_)
        }
        Type::Var { type_ } => match type_.borrow().deref() {
            TypeVar::Unbound { .. } | TypeVar::Generic { .. } => true,
            TypeVar::Link { type_ } => is_generic_type(type_),
        },
        Type::Tuple { elements } => elements.iter().any(is_generic_type),
    }
}

fn monomorphize(
    function: &TypedFunction,
    params: &[Arc<Type>],
    return_: &Arc<Type>,
) -> TypedFunction {
    let new_name = mangle(&function.name.as_ref().unwrap().1, params, return_);
    let mut new_function = function.clone();
    let mut map = vec![(&function.return_type, return_)];
    for (old, new) in function.arguments.iter().zip(params) {
        map.push((&old.type_, new));
    }
    new_function.name.as_mut().unwrap().1 = new_name;
    new_function.return_type = return_.clone();
    for arg in &mut new_function.arguments {
        arg.type_ = monomorphize_type(&arg.type_, &map);
    }
    monomorphize_statements(&mut new_function.body, &map);
    new_function
}

fn monomorphize_type(old: &Arc<Type>, map: &[(&Arc<Type>, &Arc<Type>)]) -> Arc<Type> {
    for (key, value) in map {
        if old.same_as(key) {
            return (*value).clone();
        }
    }
    old.clone()
}

fn monomorphize_statements(statements: &mut [TypedStatement], map: &[(&Arc<Type>, &Arc<Type>)]) {
    for statement in statements {
        monomorphize_statement(statement, map);
    }
}

fn monomorphize_statement(statement: &mut TypedStatement, map: &[(&Arc<Type>, &Arc<Type>)]) {
    match statement {
        Statement::Expression(expression) => monomorphize_expression(expression, map),
        Statement::Assignment(assignment) => {
            monomorphize_expression(&mut assignment.value, map);
            match &mut assignment.kind {
                AssignmentKind::Let => {}
                AssignmentKind::Generated => {}
                AssignmentKind::Assert { message, .. } => {
                    if let Some(message) = message {
                        monomorphize_expression(message, map);
                    }
                }
            }
            monomorphize_pattern(&mut assignment.pattern, map);
        }
        Statement::Use(_) => todo!(),
        Statement::Assert(_assert) => todo!(),
    }
}

fn monomorphize_expression(expression: &mut TypedExpr, map: &[(&Arc<Type>, &Arc<Type>)]) {
    match expression {
        TypedExpr::Block { statements, .. } => {
            monomorphize_statements(statements, map);
        }
        TypedExpr::Var { constructor, .. } => {
            constructor.type_ = monomorphize_type(&constructor.type_, map);
        }
        TypedExpr::Fn {
            type_,
            arguments,
            body,
            ..
        } => {
            *type_ = monomorphize_type(type_, map);
            for arg in arguments {
                arg.type_ = monomorphize_type(&arg.type_, map);
            }
            monomorphize_statements(body, map);
        }
        TypedExpr::Call {
            type_,
            fun,
            arguments,
            ..
        } => {
            *type_ = monomorphize_type(type_, map);
            monomorphize_expression(&mut *fun, map);
            for arg in arguments {
                monomorphize_expression(&mut arg.value, map);
            }
        }
        TypedExpr::Int { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::String { .. }
        | TypedExpr::BinOp { .. }
        | TypedExpr::NegateBool { .. }
        | TypedExpr::NegateInt { .. } => {}

        TypedExpr::Pipeline { .. } => todo!(),
        TypedExpr::List { .. } => todo!(),
        TypedExpr::Case { .. } => todo!(),
        TypedExpr::RecordAccess { .. } => todo!(),
        TypedExpr::ModuleSelect { .. } => todo!(),
        TypedExpr::Tuple { .. } => todo!(),
        TypedExpr::TupleIndex { .. } => todo!(),
        TypedExpr::Todo { .. } => todo!(),
        TypedExpr::Panic { .. } => todo!(),
        TypedExpr::Echo { .. } => todo!(),
        TypedExpr::BitArray { .. } => todo!(),
        TypedExpr::RecordUpdate { .. } => todo!(),
        TypedExpr::Invalid { .. } => todo!(),
    }
}

fn monomorphize_pattern(pattern: &mut TypedPattern, map: &[(&Arc<Type>, &Arc<Type>)]) {
    match pattern {
        Pattern::Int { .. }
        | Pattern::Float { .. }
        | Pattern::String { .. }
        | Pattern::BitArray { .. }
        | Pattern::StringPrefix { .. }
        | Pattern::BitArraySize(_) => {}
        Pattern::Variable { type_, .. } => {
            *type_ = monomorphize_type(&*type_, map);
        }
        Pattern::List {
            type_, elements, ..
        } => {
            *type_ = monomorphize_type(&*type_, map);
            for pattern in elements {
                monomorphize_pattern(pattern, map);
            }
        }
        Pattern::Constructor {
            type_, arguments, ..
        } => {
            *type_ = monomorphize_type(&*type_, map);
            for arg in arguments {
                monomorphize_pattern(&mut arg.value, map);
            }
        }
        Pattern::Invalid { type_, .. } => {
            *type_ = monomorphize_type(&*type_, map);
        }
        Pattern::Assign { pattern, .. } => {
            monomorphize_pattern(pattern, map);
        }
        Pattern::Discard { type_, .. } => {
            *type_ = monomorphize_type(&*type_, map);
        }
        Pattern::Tuple { elements, .. } => {
            for pattern in elements {
                monomorphize_pattern(pattern, map);
            }
        }
    }
}

fn type_str(type_: &Arc<Type>, to: &mut String) {
    match &**type_ {
        Type::Named {
            name, arguments, ..
        } => {
            let _ = write!(to, "{}", name);
            if arguments.is_empty() {
                types_str(arguments, to);
            }
        }
        Type::Fn { arguments, return_ } => {
            let _ = write!(to, "fn");
            types_str(arguments, to);
            let _ = write!(to, "->");
            type_str(return_, to);
        }
        Type::Var { type_ } => {
            if let TypeVar::Link { type_ } = type_.borrow().deref() {
                type_str(type_, to);
            } else {
                panic!("{:#?}", type_);
            }
        }
        Type::Tuple { .. } => todo!(),
    }
}

fn types_str(types: &[Arc<Type>], to: &mut String) {
    let _ = write!(to, "(");
    if let Some((first, rest)) = types.split_first() {
        type_str(first, to);
        for type_ in rest {
            let _ = write!(to, ",");
            type_str(type_, to);
        }
    }
    let _ = write!(to, ")");
}

fn function_type(function: &TypedFunction) -> Arc<Type> {
    Type::Fn {
        arguments: function
            .arguments
            .iter()
            .map(|arg| arg.type_.clone())
            .collect(),
        return_: function.return_type.clone(),
    }
    .into()
}

fn mangle(name: &EcoString, params: &[Arc<Type>], return_: &Arc<Type>) -> EcoString {
    let mut name = String::from(name);
    types_str(params, &mut name);
    let _ = write!(&mut name, "->");
    type_str(return_, &mut name);
    name.into()
}
