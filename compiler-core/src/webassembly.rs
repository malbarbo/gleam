#![allow(clippy::todo, clippy::unwrap_used)]
use std::{cell::RefCell, collections::HashMap, fmt::Write, ops::Deref, rc::Rc, sync::Arc};

use ecow::EcoString;
use num_bigint::BigInt;
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, ExportKind, ExportSection, Function, FunctionSection,
    GlobalSection, GlobalType, InstructionSink, Module, StartSection, TypeSection, ValType,
};

use crate::{
    ast::{
        AssignmentKind, BinOp, Constant, Definition, Pattern, SrcSpan, Statement, TypedArg,
        TypedAssignment, TypedExpr, TypedFunction, TypedModule, TypedModuleConstant, TypedPattern,
        TypedStatement,
    },
    line_numbers::LineNumbers,
    type_::{Type, TypeVar},
};

const MAIN: &str = "main";
const START: &str = "$start";

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
                    if !function.publicity.is_public()
                        || is_generic_function(&function_type(function))
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
        let index = self.functions_section.len();
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
                Definition::ModuleConstant(module_constant) => {
                    assert!(required_type.same_as(&module_constant.type_));
                    return self.constant(module_constant);
                }
                Definition::Function(function) if &function.name.as_ref().unwrap().1 == name => {
                    let (params, return_) = required_type.fn_types().unwrap();
                    let declared_type = function_type(function);
                    return if is_generic_function(&declared_type) {
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
        panic!("name not found: {:?}", name);
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

        let type_index = self.funtion_type(function);
        let _ = self.functions_section.function(type_index);

        let locals = Locals::new(function.arguments.len() as u32, &function.body);
        let mut code = Function::new(vec![(locals.len(), INT.val_type())]);
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

    fn funtion_type(&mut self, function: &TypedFunction) -> u32 {
        let params: Vec<ValType> = function
            .arguments
            .iter()
            .map(|t| self.val_type(&t.type_))
            .collect();
        let result = self.val_type(&function.return_type);
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

    fn val_type(&self, type_: &Type) -> ValType {
        if type_.is_int() {
            return INT.val_type();
        }
        panic!("{:?}", type_);
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
            TypedExpr::Int { int_value, .. } => {
                let _ = instructions.int_const(int_value);
            }
            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                self.expression(locals, scope.clone(), instructions, left);
                self.expression(locals, scope, instructions, right);

                let _ = match name {
                    BinOp::AddInt => instructions.int_add(),
                    BinOp::SubInt => instructions.int_sub(),
                    BinOp::MultInt => instructions.int_mul(),
                    BinOp::DivInt => instructions.int_div(locals.get_int_div()),
                    BinOp::RemainderInt => instructions.int_rem(),
                    _ => todo!(),
                };
            }
            TypedExpr::NegateInt { value, .. } => {
                self.expression(locals, scope, instructions, value);
                let _ = instructions.int_neg();
            }

            TypedExpr::Block { statements, .. } => {
                self.statements(instructions, scope, locals, statements);
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
        match &*module_constant.value {
            Constant::Int { int_value, .. } => {
                let index = self.globals_sections.len();
                let _ = self.globals_sections.global(
                    GlobalType {
                        val_type: INT.val_type(),
                        mutable: false,
                        shared: false,
                    },
                    &INT.int_const(int_value),
                );
                if module_constant.publicity.is_public() {
                    let _ = self.exports_section.export(
                        &module_constant.name,
                        ExportKind::Global,
                        index,
                    );
                }
                let id = Id::global(module_constant.name.clone(), index);
                self.globals.borrow_mut().push(id.clone());
                id
            }

            _ => todo!(),
        }
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

    fn int_const(&self, value: &BigInt) -> ConstExpr {
        match self {
            IntType::Int32 => ConstExpr::i32_const(value.try_into().unwrap()),
            IntType::Int64 => ConstExpr::i64_const(value.try_into().unwrap()),
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
    fn int_div(&mut self, local: u32) -> &mut Self;
    fn int_neg(&mut self) -> &mut Self;
    fn int_eq(&mut self) -> &mut Self;
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
    int_op!(int_eq, i32_eq, i64_eq);
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
    int_div: bool,
}

impl Locals {
    fn new(num_params: u32, statements: &[TypedStatement]) -> Self {
        let mut locals = Locals {
            skip: num_params,
            locals: HashMap::new(),
            int_div: false,
        };
        locals.statements(statements);
        locals
    }

    fn statements(&mut self, statements: &[TypedStatement]) {
        for statement in statements {
            self.statement(statement);
        }
    }

    fn statement(&mut self, statement: &TypedStatement) {
        match statement {
            Statement::Expression(expression) => self.expression(expression),
            Statement::Assignment(assignment) => self.assignment(assignment),
            _ => todo!(),
        }
    }

    fn expression(&mut self, expression: &TypedExpr) {
        match expression {
            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                self.expression(left);
                self.expression(right);
                if matches!(name, BinOp::DivInt) {
                    self.int_div = true;
                }
            }
            TypedExpr::Block { statements, .. } => {
                self.statements(statements);
            }
            TypedExpr::Int { .. } | TypedExpr::Var { .. } => {}
            TypedExpr::Call { fun, arguments, .. } => {
                self.expression(fun);
                for arg in arguments {
                    self.expression(&arg.value);
                }
            }
            _ => todo!("{:?}", expression),
        }
    }

    fn assignment(&mut self, assignment: &TypedAssignment) {
        self.expression(&assignment.value);
        match &assignment.kind {
            AssignmentKind::Let => match &assignment.pattern {
                Pattern::Variable {
                    location, type_, ..
                } => self.insert(location, type_),
                _ => todo!(),
            },
            AssignmentKind::Assert { .. } => todo!(),
            AssignmentKind::Generated => todo!(),
        }
    }

    fn insert(&mut self, location: &SrcSpan, type_: &Type) {
        assert!(type_.is_int());
        let index = self.locals.len() as u32 + self.skip;
        let _ = self.locals.insert(*location, index);
    }

    fn get(&self, location: &SrcSpan) -> u32 {
        *self.locals.get(location).unwrap()
    }

    fn get_int_div(&self) -> u32 {
        assert!(self.int_div);
        self.locals.len() as u32
    }

    fn len(&self) -> u32 {
        self.locals.len() as u32 + if self.int_div { 2 } else { 0 }
    }
}

fn is_generic_function(type_: &Type) -> bool {
    type_.is_unbound()
        || if let Some((params, return_)) = type_.fn_types() {
            params.into_iter().any(|type_| is_generic_function(&type_))
                || is_generic_function(&return_)
        } else {
            false
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

fn function_type(function: &TypedFunction) -> Type {
    Type::Fn {
        arguments: function
            .arguments
            .iter()
            .map(|arg| arg.type_.clone())
            .collect(),
        return_: function.return_type.clone(),
    }
}

fn mangle(name: &EcoString, params: &[Arc<Type>], return_: &Arc<Type>) -> EcoString {
    let mut name = String::from(name);
    types_str(params, &mut name);
    let _ = write!(&mut name, "->");
    type_str(return_, &mut name);
    name.into()
}
