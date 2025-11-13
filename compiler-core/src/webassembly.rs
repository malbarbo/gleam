#![allow(clippy::todo, clippy::unwrap_used)]
use std::{cell::RefCell, collections::HashMap, fmt::Write, ops::Deref, rc::Rc, sync::Arc};

use ecow::EcoString;
use num_bigint::BigInt;
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataCountSection, DataSection, ExportKind, ExportSection,
    FieldType, Function, FunctionSection, GlobalSection, GlobalType, HeapType, InstructionSink,
    Module, RefType, StartSection, StorageType, TypeSection, ValType,
};

use crate::{
    ast::{
        AssignmentKind, BinOp, Constant, Definition, OperatorKind, Pattern, SrcSpan, Statement,
        TypedArg, TypedAssignment, TypedConstant, TypedExpr, TypedFunction, TypedModule,
        TypedModuleConstant, TypedPattern, TypedStatement,
    },
    line_numbers::LineNumbers,
    type_::{Type, TypeVar},
};

const MAIN: &str = "main";
const TRUE: &str = "True";
const FALSE: &str = "False";

pub fn module(module: &TypedModule, _line_numbers: &LineNumbers) -> Vec<u8> {
    let mut generator = Generator::new(module);
    generator.all_pub();
    let mut module = Module::default();

    let start = generator.function_start();

    // type section
    let mut types: Vec<_> = generator.types.iter().collect();
    types.sort_by_key(|t| t.1);
    let mut type_section = TypeSection::new();
    for (type_, type_index) in types {
        match type_ {
            WasmType::Array(storage_type) => type_section.ty().array(storage_type, true),
            WasmType::Function(params, results) => {
                type_section.ty().function(params.clone(), results.clone())
            }
            WasmType::List(val_type) => {
                let list_val_type = generator.list_val_type(*type_index);
                type_section.ty().struct_(vec![
                    FieldType {
                        element_type: StorageType::Val(list_val_type),
                        mutable: false,
                    },
                    FieldType {
                        element_type: StorageType::Val(*val_type),
                        mutable: false,
                    },
                ]);
            }
            WasmType::Tuple(val_types) => {
                type_section
                    .ty()
                    .struct_(val_types.iter().map(|val_type| FieldType {
                        element_type: StorageType::Val(*val_type),
                        mutable: false,
                    }));
            }
        }
    }
    let _ = module.section(&type_section);

    // function section
    let _ = module.section(&generator.function_section);

    // global section
    let _ = module.section(&generator.global_section);

    // export section
    let _ = module.section(&generator.export_section);

    // start section
    let _ = module.section(&StartSection {
        function_index: start,
    });

    // data count section
    let _ = module.section(&DataCountSection {
        count: generator.strings.len() as u32,
    });

    // code section
    generator.functions.sort_by_key(|t| t.1);
    let mut codes_section = CodeSection::new();
    for function in generator.functions {
        let _ = codes_section.function(&function.0);
    }
    let _ = module.section(&codes_section);

    // data section
    let _ = module.section(&generator.data_section);

    // finalize
    module.finish()
}

#[derive(Hash, Eq, PartialEq)]
enum WasmType {
    Array(StorageType),
    Function(Vec<ValType>, Vec<ValType>),
    List(ValType),
    Tuple(Vec<ValType>),
}

#[derive(Hash, PartialEq, Eq, Copy, Clone, Debug)]
enum Builtins {
    Start,
    StringEq,
    StringConcat,
}

impl Builtins {
    fn name(&self) -> &'static str {
        match self {
            Builtins::Start => "$start",
            Builtins::StringEq => "$string_eq",
            Builtins::StringConcat => "$string_concat",
        }
    }

    fn export(&self) -> bool {
        match self {
            Builtins::Start => true,
            Builtins::StringEq | Builtins::StringConcat => false,
        }
    }
}

#[derive(Clone)]
enum Const {
    // The index in the global section for the const string name and
    // the index in the global section for the string literal
    String {
        from: u32,
        to: u32,
    },
    List {
        global_index: u32,
        type_index: u32,
        elements: Vec<TypedConstant>,
    },
    Tuple {
        global_index: u32,
        type_index: u32,
        elements: Vec<TypedConstant>,
    },
}

struct Generator<'a> {
    // Wasm types and its indexes in the type section
    types: HashMap<WasmType, u32>,
    function_section: FunctionSection,
    global_section: GlobalSection,
    export_section: ExportSection,
    // Function code and its index in the code section
    functions: Vec<(Function, u32)>,
    data_section: DataSection,
    // String literals and its index in the global section
    strings: HashMap<EcoString, u32>,
    consts: Vec<Const>,
    module: &'a TypedModule,
    main: Option<u32>,
    next_function_id: u32,
    globals: Rc<RefCell<Vec<Id>>>,
    builtins: HashMap<Builtins, u32>,
    eq: HashMap<u32, u32>,
    bool_: BoolType,
    int: IntType,
    float: FloatType,
    string: StringType,
}

fn find_global(name: &EcoString, globals: &RefCell<Vec<Id>>) -> Option<Id> {
    globals.borrow().iter().find(|id| &id.name == name).cloned()
}

impl<'a> Generator<'a> {
    fn new(module: &'a TypedModule) -> Self {
        let mut generator = Generator {
            types: HashMap::new(),
            function_section: FunctionSection::new(),
            global_section: GlobalSection::new(),
            export_section: ExportSection::new(),
            functions: vec![],
            data_section: DataSection::new(),
            strings: HashMap::new(),
            consts: vec![],
            module,
            main: None,
            next_function_id: 0,
            globals: Rc::default(),
            builtins: HashMap::new(),
            eq: HashMap::new(),
            bool_: BoolType {},
            int: IntType::Int32,
            float: FloatType {},
            string: StringType { type_index: 0 },
        };
        let _ = generator
            .types
            .insert(WasmType::Array(StringType::store_type()), 0);
        generator
    }

    fn all_pub(&mut self) {
        for definition in &self.module.definitions {
            match definition {
                Definition::ModuleConstant(module_constant) => {
                    let _ = self.module_constant(module_constant);
                }
                Definition::Function(function) => {
                    if !function.publicity.is_public() || is_generic_type(&function_type(function))
                    {
                        continue;
                    }
                    let id = self.function(function);
                    if is_main_funtion(function) {
                        self.main = Some(id.index)
                    }
                }
                _ => todo!("Definition not supported: {:#?}", definition),
            }
        }
    }

    fn val_type(&mut self, type_: &Arc<Type>) -> ValType {
        if type_.is_int() {
            return self.int.val_type();
        }
        if type_.is_bool() {
            return self.bool_.val_type();
        }
        if type_.is_float() {
            return self.float.val_type();
        }
        if type_.is_string() {
            return self.string.val_type();
        }
        if let Some(item_type) = type_.list_type() {
            let type_index = self.list_type(&item_type);
            return self.list_val_type(type_index);
        }
        if let Some(types) = type_.tuple_types() {
            let type_index = self.tuple_type(types);
            return self.tuple_val_type(type_index);
        }
        if let Some((params, return_)) = type_.fn_types() {
            let type_index = self.function_type(params, Some(return_));
            return self.function_val_type(type_index);
        }
        todo!("Type not supported: {:#?}", type_);
    }

    fn val_type_ref(&self, type_index: u32) -> ValType {
        RefType {
            heap_type: HeapType::Concrete(type_index),
            nullable: false,
        }
        .into()
    }

    fn val_type_ref_nullable(&self, type_index: u32) -> ValType {
        RefType {
            heap_type: HeapType::Concrete(type_index),
            nullable: true,
        }
        .into()
    }

    fn function_type(
        &mut self,
        arguments: impl IntoIterator<Item = Arc<Type>>,
        return_: Option<Arc<Type>>,
    ) -> u32 {
        let params: Vec<_> = arguments
            .into_iter()
            .map(|type_| self.val_type(&type_))
            .collect();
        let result = return_.map(|return_| self.val_type(&return_));
        self.function_type_with_val_types(params, result.into_iter().collect())
    }

    fn function_type_with_val_types(&mut self, params: Vec<ValType>, result: Vec<ValType>) -> u32 {
        let index = self.types.len() as u32;
        *self
            .types
            .entry(WasmType::Function(params, result))
            .or_insert(index)
    }

    fn function_val_type(&self, type_index: u32) -> ValType {
        self.val_type_ref(type_index)
    }

    fn list_type(&mut self, item_type: &Arc<Type>) -> u32 {
        let item_val_type = self.val_type(item_type);
        let index = self.types.len() as u32;
        *self
            .types
            .entry(WasmType::List(item_val_type))
            .or_insert(index)
    }

    fn list_val_type(&self, type_index: u32) -> ValType {
        self.val_type_ref_nullable(type_index)
    }

    fn tuple_type(&mut self, types: impl IntoIterator<Item = Arc<Type>>) -> u32 {
        let types: Vec<_> = types
            .into_iter()
            .map(|type_| self.val_type(&type_))
            .collect();
        let index = self.types.len() as u32;
        *self.types.entry(WasmType::Tuple(types)).or_insert(index)
    }

    fn tuple_val_type(&self, type_index: u32) -> ValType {
        self.val_type_ref(type_index)
    }

    fn tuple_val_type_nullable(&self, type_index: u32) -> ValType {
        self.val_type_ref_nullable(type_index)
    }

    fn module_constant(&mut self, module_constant: &TypedModuleConstant) -> Id {
        self.register_string_const(&module_constant.value);
        let (val_type, expr) = match &*module_constant.value {
            Constant::Int { int_value, .. } => (self.int.val_type(), self.int.int_const(int_value)),
            Constant::Float { value, .. } => (self.float.val_type(), self.float.float_const(value)),
            Constant::String { .. } => (
                self.string.val_type_nullable(),
                ConstExpr::ref_null(HeapType::Concrete(self.string.type_index)),
            ),
            Constant::List { type_, .. } => {
                let item_type = type_.list_type().unwrap();
                let type_index = self.list_type(&item_type);
                (
                    self.list_val_type(type_index),
                    ConstExpr::ref_null(HeapType::Concrete(type_index)),
                )
            }
            Constant::Tuple { elements, .. } => {
                let type_index = self.tuple_type(elements.iter().map(|element| element.type_()));
                (
                    self.tuple_val_type_nullable(type_index),
                    ConstExpr::ref_null(HeapType::Concrete(type_index)),
                )
            }
            Constant::Record { name, type_, .. } if is_bool_const(name, type_) => {
                (self.bool_.val_type(), self.bool_.bool_const(name == TRUE))
            }
            _ => todo!("Module constant not supported: {:#?}", module_constant),
        };

        let id = self.add_global(
            &module_constant.name,
            val_type,
            expr,
            module_constant.publicity.is_public(),
        );

        match &*module_constant.value {
            Constant::String { value, .. } => {
                let from = self.string_index(value);
                self.consts.push(Const::String { from, to: id.index });
            }
            Constant::List {
                elements, type_, ..
            } => {
                let item_type = type_.list_type().unwrap();
                let type_index = self.list_type(&item_type);
                self.consts.push(Const::List {
                    global_index: id.index,
                    type_index,
                    elements: elements.clone(),
                });
            }
            Constant::Tuple { elements, .. } => {
                let type_index = self.tuple_type(elements.iter().map(|e| e.type_()));
                self.consts.push(Const::Tuple {
                    global_index: id.index,
                    type_index,
                    elements: elements.clone(),
                });
            }
            _ => {}
        };

        id
    }

    fn register_string_const(&mut self, const_: &TypedConstant) {
        match const_ {
            Constant::String { value, .. } => {
                let _ = self.string_index(value);
            }
            Constant::Tuple { elements, .. } | Constant::List { elements, .. } => {
                for element in elements {
                    self.register_string_const(element);
                }
            }
            Constant::Int { .. } | Constant::Float { .. } => {}
            Constant::Record { type_, .. } if type_.is_bool() => {}
            _ => todo!("Constant not supported: {:#?}", const_),
        }
    }

    fn _constant(
        &mut self,
        instructions: &mut ExtendedInstructionSink<'_>,
        const_: &TypedConstant,
    ) {
        match const_ {
            Constant::Int { int_value, .. } => {
                let _ = instructions.int_const(int_value);
            }
            Constant::Float { value, .. } => {
                let _ = instructions.float_const(value);
            }
            Constant::String { value, .. } => {
                let index = self.string_index(value);
                let _ = instructions.global_as_non_null(index);
            }
            Constant::List {
                elements, type_, ..
            } => {
                let item_type = type_.list_type().unwrap();
                let type_index = self.list_type(&item_type);
                let _ = instructions.list_null(type_index);
                for element in elements.iter().rev() {
                    let _ = instructions.constant(self, element).struct_new(type_index);
                }
            }
            Constant::Tuple { elements, .. } => {
                let type_index = self.tuple_type(elements.iter().map(|e| e.type_()));
                let _ = instructions
                    .constants(self, elements)
                    .struct_new(type_index);
            }
            Constant::Record { name, .. } if const_.type_().is_bool() => {
                let _ = instructions.bool_const(name == TRUE);
            }
            _ => todo!("Constant not supported: {:#?}", const_),
        }
    }

    fn var(&mut self, name: &EcoString, required_type: &Type) -> Id {
        for definition in &self.module.definitions {
            match definition {
                Definition::ModuleConstant(module_constant) if &module_constant.name == name => {
                    assert!(required_type.same_as(&module_constant.type_));
                    return self.module_constant(module_constant);
                }
                Definition::Function(function)
                    if function.name.as_ref().map(|s| &s.1) == Some(name) =>
                {
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
        todo!(
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
        let _ = self.export_section.export(&name, ExportKind::Func, index);
        self.globals
            .borrow_mut()
            .push(Id::func(name.clone(), index));
        let type_index = self.function_type(
            function.arguments.iter().map(|arg| arg.type_.clone()),
            Some(function.return_type.clone()),
        );
        let _ = self.function_section.function(type_index);

        let locals = Locals::new(self, function.arguments.len() as u32, &function.body);
        let mut code = Function::new(locals.val_types());
        self.statements(
            &mut code.extend_instructions(self),
            Scope::with_params(self.globals.clone(), &function.arguments),
            &locals,
            &function.body,
        );
        let _ = code.instructions().end();
        self.functions.push((code, index));
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
        let _ = self.export_section.export(&name, ExportKind::Func, index);
        self.globals
            .borrow_mut()
            .push(Id::func(name.clone(), index));

        let (params, return_) = type_.fn_types().unwrap();
        let type_index = self.function_type(params, Some(return_));
        let _ = self.function_section.function(type_index);

        let locals = Locals::new(self, arguments.len() as u32, body);
        let mut code = Function::new(locals.val_types());
        self.statements(
            &mut code.extend_instructions(self),
            Scope::with_params(self.globals.clone(), arguments),
            &locals,
            body,
        );
        let _ = code.instructions().end();
        self.functions.push((code, index));
        Id::func(name, index)
    }

    fn statements(
        &mut self,
        instructions: &mut ExtendedInstructionSink<'_>,
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
        instructions: &mut ExtendedInstructionSink<'_>,
        mut scope: Scope,
        locals: &Locals,
        statement: &TypedStatement,
    ) -> Scope {
        match statement {
            Statement::Expression(expression) => {
                let _ = instructions.expression(self, locals, scope.clone(), expression);
            }
            Statement::Assignment(assignment) => {
                scope = self.assignment(locals, scope, instructions, assignment);
            }
            Statement::Assert(assert) => {
                assert!(assert.value.type_().is_bool());
                // FIXME: show message
                #[rustfmt::skip]
                let _ = instructions
                    .expression(self, locals, scope.clone(), &assert.value)
                    .if_(BlockType::Result(self.bool_.val_type()))
                      .bool_const(true)
                    .else_()
                      .unreachable()
                    .end();
            }
            Statement::Use(_) => todo!("Statement not supported: {:#?}", statement),
        }
        scope
    }

    fn _expression(
        &mut self,
        locals: &Locals,
        scope: Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
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
            TypedExpr::String { value, .. } => {
                let index = self.string_index(value);
                let _ = instructions.global_as_non_null(index);
            }
            TypedExpr::List {
                type_,
                elements,
                tail,
                ..
            } => {
                let item_type = type_.list_type().unwrap();
                let type_index = self.list_type(&item_type);
                if let Some(rest) = tail {
                    let _ = instructions.expression(self, locals, scope.clone(), rest);
                } else {
                    let _ = instructions.list_null(type_index);
                }
                for element in elements.iter().rev() {
                    let _ = instructions
                        .expression(self, locals, scope.clone(), element)
                        .struct_new(type_index);
                }
            }
            TypedExpr::Tuple { elements, .. } => {
                let type_index = self.tuple_type(elements.iter().map(|e| e.type_()));
                let _ = instructions
                    .expressions(self, locals, scope.clone(), elements)
                    .struct_new(type_index);
            }
            TypedExpr::TupleIndex { index, tuple, .. } => {
                let type_index = self.tuple_type(tuple.type_().tuple_types().unwrap());
                let _ = instructions
                    .expression(self, locals, scope.clone(), tuple)
                    .struct_get(type_index, *index as u32);
            }
            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                if name.operator_kind() != OperatorKind::BooleanLogic {
                    let _ = instructions
                        .expression(self, locals, scope.clone(), left)
                        .expression(self, locals, scope.clone(), right);
                }

                let _ = match name {
                    // Bool
                    #[rustfmt::skip]
                    BinOp::And => instructions
                        .expression(self, locals, scope.clone(), left)
                        .if_(BlockType::Result(self.bool_.val_type()))
                          .expression(self, locals, scope, right)
                        .else_()
                          .bool_const(false)
                        .end(),
                    #[rustfmt::skip]
                    BinOp::Or => instructions
                        .expression(self, locals, scope.clone(), left)
                        .if_(BlockType::Result(self.bool_.val_type()))
                          .bool_const(true)
                        .else_()
                          .expression(self, locals, scope, right)
                        .end(),
                    BinOp::Eq if left.type_().is_bool() => instructions.bool_eq(),
                    BinOp::NotEq if left.type_().is_bool() => instructions.bool_ne(),
                    // Int
                    BinOp::AddInt => instructions.int_add(),
                    BinOp::SubInt => instructions.int_sub(),
                    BinOp::MultInt => instructions.int_mul(),
                    BinOp::DivInt => instructions
                        .int_div(locals.get(&left.location()), locals.get(&right.location())),
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
                    BinOp::DivFloat => instructions
                        .float_div(locals.get(&left.location()), locals.get(&right.location())),
                    BinOp::LtFloat => instructions.float_lt(),
                    BinOp::LtEqFloat => instructions.float_le(),
                    BinOp::GtFloat => instructions.float_gt(),
                    BinOp::GtEqFloat => instructions.float_ge(),
                    BinOp::Eq if left.type_().is_float() => instructions.float_eq(),
                    BinOp::NotEq if left.type_().is_float() => instructions.float_ne(),
                    // String
                    BinOp::Concatenate => {
                        let concat = self.function_string_concat();
                        instructions.call(concat)
                    }
                    BinOp::Eq | BinOp::NotEq if left.type_().is_string() => {
                        let eq = self.function_string_eq();
                        let _ = instructions.call(eq);
                        if let BinOp::NotEq = name {
                            let _ = instructions.bool_neg();
                        }
                        instructions
                    }
                    // List
                    BinOp::Eq | BinOp::NotEq if left.type_().is_list() => {
                        // FIXME: remove unwrap when if let guard stabilize
                        // https://github.com/rust-lang/rust/issues/51114
                        let eq = self.function_list_eq(&left.type_().list_type().unwrap());
                        let _ = instructions.call(eq);
                        if let BinOp::NotEq = name {
                            let _ = instructions.bool_neg();
                        }
                        instructions
                    }
                    // Tuple
                    BinOp::Eq | BinOp::NotEq if left.type_().is_tuple() => {
                        // FIXME: remove unwrap when if let guard stabilize
                        // https://github.com/rust-lang/rust/issues/51114
                        let eq = self.function_tuple_eq(left.type_().tuple_types().unwrap());
                        let _ = instructions.call(eq);
                        if let BinOp::NotEq = name {
                            let _ = instructions.bool_neg();
                        }
                        instructions
                    }
                    // FIXME: add function eq
                    _ => todo!("Expression not supported: {:#?}", expression),
                };
            }
            TypedExpr::NegateInt { value, .. } => {
                let _ = instructions
                    .int_const(&0.into())
                    .expression(self, locals, scope, value)
                    .int_sub();
            }
            TypedExpr::NegateBool { value, .. } => {
                let _ = instructions
                    .expression(self, locals, scope, value)
                    .bool_neg();
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
                    .unwrap_or_else(|| self.var(name, &expression.type_()));
                let type_ = expression.type_();

                let _ = match id.kind {
                    IdKind::Func => instructions.ref_func(id.index),
                    IdKind::Global if type_.is_string() || type_.tuple_types().is_some() => {
                        instructions.global_as_non_null(id.index)
                    }
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
                let args_types = arguments.iter().map(|arg| arg.value.type_().clone());
                let index = self.function_type(args_types, Some(type_.clone()));
                let args = arguments.iter().map(|arg| &arg.value);
                let _ = instructions
                    .expressions(self, locals, scope.clone(), args)
                    .expression(self, locals, scope, fun)
                    .call_ref(index);
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
            _ => todo!("Expression not supported: {:#?}", expression),
        };
    }

    fn assignment(
        &mut self,
        locals: &Locals,
        mut scope: Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        assignment: &TypedAssignment,
    ) -> Scope {
        let right = locals.get(&assignment.value.location());
        let _ = instructions
            .expression(self, locals, scope.clone(), &assignment.value)
            .local_tee(right);
        match assignment.kind {
            AssignmentKind::Assert { .. } => {
                #[rustfmt::skip]
                let _ = instructions
                    .pattern(self, locals, &mut scope, &assignment.pattern)
                    .if_(BlockType::Result(self.val_type(&assignment.value.type_())))
                      .local_get(right)
                    .else_()
                      .unreachable()
                    .end();
            }
            AssignmentKind::Let => {
                let _ = instructions
                    .pattern(self, locals, &mut scope, &assignment.pattern)
                    .drop()
                    .local_get(right);
            }
            AssignmentKind::Generated => {
                todo!("Generated Assignment not implemented: {:#?}", assignment)
            }
        }
        scope
    }

    fn _pattern(
        &mut self,
        locals: &Locals,
        mut scope: Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        pattern: &TypedPattern,
    ) -> Scope {
        match pattern {
            Pattern::Discard { .. } => {
                let _ = instructions.drop().bool_const(true);
            }
            Pattern::Int { int_value, .. } => {
                let _ = instructions.int_const(int_value).int_eq();
            }
            Pattern::Float { value, .. } => {
                let _ = instructions.float_const(value).float_eq();
            }
            Pattern::String { value, .. } => {
                let eq = self.function_string_eq();
                let index = self.string_index(value);
                let _ = instructions.global_as_non_null(index).call(eq);
            }
            Pattern::List {
                elements,
                tail,
                location,
                type_,
            } => {
                let item_type = type_.list_type().unwrap();
                let type_index = self.list_type(&item_type);
                let right = locals.get(location);
                let _ = instructions
                    .local_set(right)
                    .block(BlockType::Result(self.bool_.val_type()));
                for element in elements {
                    #[rustfmt::skip]
                    let _ = instructions
                        .local_get(right)
                        .struct_get(type_index, 1)
                        .pattern(self, locals, &mut scope, element)
                        .bool_neg()
                        .if_(BlockType::Empty)
                          .bool_const(false)
                          .br(1)
                        .end()
                        .local_get(right)
                        .struct_get(type_index, 0)
                        .local_set(right);
                }
                if let Some(tail) = tail {
                    #[rustfmt::skip]
                    let _ = instructions
                        .local_get(right)
                        .pattern(self, locals, &mut scope, &tail.pattern)
                        .bool_neg()
                        .if_(BlockType::Empty)
                          .bool_const(false)
                          .br(1)
                        .end();
                } else {
                    #[rustfmt::skip]
                    let _ = instructions
                        .local_get(right)
                        .ref_is_null()
                        .bool_neg()
                        .if_(BlockType::Empty)
                          .bool_const(false)
                          .br(1)
                        .end();
                }
                let _ = instructions.bool_const(true).end();
            }
            Pattern::Tuple { location, elements } => {
                let type_index = self.tuple_type(elements.iter().map(|element| element.type_()));
                let right = locals.get(location);
                let _ = instructions
                    .local_set(right)
                    .block(BlockType::Result(self.bool_.val_type()));
                for (field_index, element) in elements.iter().enumerate() {
                    #[rustfmt::skip]
                    let _ = instructions
                        .local_get(right)
                        .struct_get(type_index, field_index as u32)
                        .pattern(self, locals, &mut scope, element)
                        .bool_neg()
                        .if_(BlockType::Empty)
                          .bool_const(false)
                          .br(1)
                        .end();
                }
                let _ = instructions.bool_const(true).end();
            }
            Pattern::Constructor { name, type_, .. } if is_bool_const(name, type_) => {
                let _ = instructions.bool_const(name == TRUE).bool_eq();
            }
            Pattern::Variable { name, location, .. } => {
                let right = locals.get(location);
                scope = scope.insert_local(name.clone(), right);
                let _ = instructions.local_set(right).bool_const(true);
            }
            _ => todo!("Assigment Assert Pattern not implemented: {:#?}", pattern),
        }
        scope
    }

    fn add_global(
        &mut self,
        name: &EcoString,
        val_type: ValType,
        expr: ConstExpr,
        export: bool,
    ) -> Id {
        let index = self.global_section.len();
        let _ = self.global_section.global(
            GlobalType {
                val_type,
                mutable: val_type.is_reference(),
                shared: false,
            },
            &expr,
        );
        if export {
            let _ = self.export_section.export(name, ExportKind::Global, index);
        }
        let id = Id::global(name.clone(), index);
        self.globals.borrow_mut().push(id.clone());
        id
    }

    fn function_start(&mut self) -> u32 {
        let function = self.code_start();
        self.add_builtins(Builtins::Start, function)
    }

    fn code_start(&mut self) -> Function {
        let mut function = Function::new(vec![]);
        let mut instructions = function.extend_instructions(self);
        // string data
        for (string, index) in &self.strings {
            let data_segment = self.data_section.len();
            let _ = self.data_section.passive(string.as_bytes().iter().cloned());
            let _ = instructions
                .i32_const(0)
                .i32_const(string.len() as i32)
                .array_new_data(self.string.type_index, data_segment)
                .ref_as_non_null()
                .global_set(*index);
        }
        // constants
        for const_ in self.consts.clone() {
            match const_ {
                Const::String { from, to } => {
                    let _ = instructions.global_get(from).global_set(to);
                }
                Const::List {
                    global_index,
                    type_index,
                    elements,
                } => {
                    let _ = instructions.global_get(global_index);
                    for element in elements.iter().rev() {
                        let _ = instructions.constant(self, element).struct_new(type_index);
                    }
                    let _ = instructions.global_set(global_index);
                }
                Const::Tuple {
                    global_index,
                    type_index,
                    elements,
                } => {
                    let _ = instructions
                        .constants(self, &elements)
                        .struct_new(type_index)
                        .global_set(global_index);
                }
            }
        }
        // main
        if let Some(main) = &self.main {
            let _ = function.instructions().call(*main).drop();
        }
        let _ = function.instructions().end();
        function
    }

    fn string_index(&mut self, string: &EcoString) -> u32 {
        let index = self.global_section.len();
        *self.strings.entry(string.into()).or_insert_with(|| {
            let _ = self.global_section.global(
                GlobalType {
                    val_type: self.string.val_type_nullable(),
                    mutable: true,
                    shared: false,
                },
                &ConstExpr::ref_null(self.string.heap_type()),
            );
            index
        })
    }

    fn function_string_eq(&mut self) -> u32 {
        if let Some(index) = self.builtins.get(&Builtins::StringEq) {
            return *index;
        }
        let function = self.code_string_eq();
        self.add_builtins(Builtins::StringEq, function)
    }

    fn code_string_eq(&mut self) -> Function {
        let mut function = Function::new(vec![(2, ValType::I32)]);
        let mut instructions = function.extend_instructions(self);
        let a = 0;
        let b = 1;
        let i = 2;
        let len = 3;
        #[rustfmt::skip]
        let _ = instructions
            .local_get(a)
            .local_get(b)
            .ref_eq()
            // if ref a == ref b
            .if_(BlockType::Empty)
              .bool_const(true)
              .return_()
            .end()
            // len = a.len; push len
            .local_get(a)
            .array_len()
            .local_tee(len)
            // b.len
            .local_get(b)
            .array_len()
            .i32_ne()
            // if a.len != b.len
            .if_(BlockType::Empty)
              .bool_const(false)
              .return_()
            .end()
            // i = 0
            .i32_const(0)
            .local_set(i)
            // loop
            .loop_(BlockType::Empty)
              .local_get(i)
              .local_get(len)
              .i32_ge_u()
              // if i >= len
              .if_(BlockType::Empty)
                .bool_const(true)
                .return_()
              .end()
              // a[i]
              .local_get(a)
              .local_get(i)
              .array_get_u(self.string.type_index)
              // b[i]
              .local_get(b)
              .local_get(i)
              .array_get_u(self.string.type_index)
              .i32_ne()
              // if a[i] != b[i]
              .if_(BlockType::Empty)
                .bool_const(false)
                .return_()
              .end()
              // i = i + 1
              .local_get(i)
              .i32_const(1)
              .i32_add()
              .local_set(i)
              // loop
              .br(0)
            // end loop
            .end()
            .bool_const(true)
            // end function
            .end();
        function
    }

    fn function_string_concat(&mut self) -> u32 {
        if let Some(index) = self.builtins.get(&Builtins::StringConcat) {
            return *index;
        }
        let function = self.code_string_concat();
        self.add_builtins(Builtins::StringConcat, function)
    }

    fn code_string_concat(&mut self) -> Function {
        let mut function = Function::new(vec![(3, ValType::I32), (1, self.string.val_type())]);
        let mut instructions = function.extend_instructions(self);
        let a = 0;
        let b = 1;
        let len_a = 2;
        let len_b = 3;
        let i = 4;
        let r = 5;
        #[rustfmt::skip]
        let _ = instructions
            // len_a = a.len; push len_a
            .local_get(a)
            .array_len()
            .local_tee(len_a)
            // len_b = b.len; push len_b
            .local_get(b)
            .array_len()
            .local_tee(len_b)
            // r = array.new_default(len_a + len_b)
            .i32_add()
            .array_new_default(self.string.type_index)
            .local_set(r)
            // i = 0
            .i32_const(0)
            .local_set(i)
            // loop copy a to r[0..len_a]
            .loop_(BlockType::Empty)
              // if i <= len_a
              .local_get(i)
              .local_get(len_a)
              .i32_lt_u()
              .if_(BlockType::Empty)
                // r[i] = a[i]
                .local_get(r)
                .local_get(i)
                .local_get(a)
                .local_get(i)
                .array_get_u(self.string.type_index)
                .array_set(self.string.type_index)
                // i = i + 1
                .local_get(i)
                .i32_const(1)
                .i32_add()
                .local_set(i)
                // loop
                .br(1)
              // end if i <= len_a
              .end()
            // end loop
            .end()
            // i = 0
            .i32_const(0)
            .local_set(i)
            // loop copy b to r[len_a..len_a+len_b]
            .loop_(BlockType::Empty)
              // if i <= len_b
              .local_get(i)
              .local_get(len_b)
              .i32_lt_u()
              .if_(BlockType::Empty)
                // r[len_a + i] = b[i]
                .local_get(r)
                .local_get(len_a)
                .local_get(i)
                .i32_add()
                .local_get(b)
                .local_get(i)
                .array_get_u(self.string.type_index)
                .array_set(self.string.type_index)
                // i = i + 1
                .local_get(i)
                .i32_const(1)
                .i32_add()
                .local_set(i)
                // loop
                .br(1)
              // end if i <= len_b
              .end()
            // end loop
            .end()
            // return r
            .local_get(r)
            // function
            .end();
        function
    }

    fn function_eq(&mut self, type_: &Arc<Type>) -> Eq {
        if type_.is_int() {
            Eq::Int
        } else if type_.is_float() {
            Eq::Float
        } else if type_.is_bool() {
            Eq::Bool
        } else if type_.is_string() {
            Eq::Call(self.function_string_eq())
        } else if let Some(item_type) = type_.list_type() {
            Eq::Call(self.function_list_eq(&item_type))
        } else if let Some(types) = type_.tuple_types() {
            Eq::Call(self.function_tuple_eq(types))
        } else {
            panic!();
        }
    }

    fn function_list_eq(&mut self, item_type: &Arc<Type>) -> u32 {
        let type_index = self.list_type(item_type);
        if let Some(index) = self.eq.get(&type_index) {
            return *index;
        }
        let eq = self.function_eq(item_type);
        let function = self.code_list_eq(type_index, eq);
        let val_type = self.list_val_type(type_index);
        let index = self.add_function(
            None,
            vec![val_type, val_type],
            Some(self.bool_.val_type()),
            function,
        );
        let _ = self.eq.insert(type_index, index);
        index
    }

    fn code_list_eq(&mut self, type_index: u32, eq: Eq) -> Function {
        let mut function = Function::new(vec![]);
        let mut instructions = function.extend_instructions(self);
        let rest_index = 0;
        let value_index = 1;
        let a = 0;
        let b = 1;
        #[rustfmt::skip]
        let _ = instructions
            .loop_(BlockType::Empty)
              // if ref a == ref b
              .local_get(a)
              .local_get(b)
              .ref_eq()
              .if_(BlockType::Empty)
                .bool_const(true)
                .return_()
              .end()
              // if a == null
              .local_get(a)
              .ref_is_null()
              .if_(BlockType::Empty)
                // a == null
                // if b == null
                .local_get(b)
                .ref_is_null()
                .if_(BlockType::Empty)
                  // a == null and b == bull
                  .bool_const(true)
                  .return_()
                .else_()
                  // a != null and b == null
                  .bool_const(false)
                  .return_()
                // end if b == null
                .end()
              .else_()
                // a == null
                // if b == null
                .local_get(b)
                .ref_is_null()
                .if_(BlockType::Empty)
                  // a != null and b == null
                  .bool_const(false)
                  .return_()
                .else_()
                  // a != null and b != null
                  // a.value
                  .local_get(a)
                  .struct_get(type_index, value_index)
                  // b.value
                  .local_get(b)
                  .struct_get(type_index, value_index)
                  .eq(eq)
                  // if a.value == b.value
                  .if_(BlockType::Empty)
                    // a = a.rest
                    .local_get(a)
                    .struct_get(type_index, rest_index)
                    .local_set(a)
                    // b = b.rest
                    .local_get(b)
                    .struct_get(type_index, rest_index)
                    .local_set(b)
                  .else_()
                    // a.value != b.value
                    .bool_const(false)
                    .return_()
                  // end if a.value == b.value
                  .end()
                // end if b == null
                .end()
               // end if a == null
               .end()
            // loop with a = a.rest and b = b.rest
            .br(0)
          // end loop
          .end()
          .bool_const(false)
          // end function
          .end();
        function
    }

    fn function_tuple_eq(&mut self, types: impl IntoIterator<Item = Arc<Type>> + Clone) -> u32 {
        let type_index = self.tuple_type(types.clone());
        if let Some(index) = self.eq.get(&type_index) {
            return *index;
        }
        let function = self.code_tuple_eq(type_index, types);
        let val_type = self.tuple_val_type(type_index);
        let index = self.add_function(
            None,
            vec![val_type, val_type],
            Some(self.bool_.val_type()),
            function,
        );
        let _ = self.eq.insert(type_index, index);
        index
    }

    fn code_tuple_eq(
        &mut self,
        type_index: u32,
        types: impl IntoIterator<Item = Arc<Type>>,
    ) -> Function {
        let mut function = Function::new(vec![]);
        let mut instructions = function.extend_instructions(self);
        let a = 0;
        let b = 1;
        #[rustfmt::skip]
        let _ = instructions
            .local_get(a)
            .local_get(b)
            .ref_eq()
            .if_(BlockType::Empty)
              .bool_const(true)
              .return_()
            .end();
        for (field_index, type_) in types.into_iter().enumerate() {
            #[rustfmt::skip]
            let _ = instructions
                .local_get(a)
                .struct_get(type_index, field_index as u32)
                .local_get(b)
                .struct_get(type_index, field_index as u32)
                .eq(self.function_eq(&type_))
                .bool_neg()
                .if_(BlockType::Empty)
                  .bool_const(false)
                  .return_()
                .end();
        }
        let _ = instructions.bool_const(true).end();
        function
    }

    fn add_builtins(&mut self, builtin: Builtins, function: Function) -> u32 {
        let (params, result) = match builtin {
            Builtins::Start => (vec![], None),
            Builtins::StringEq => (
                vec![self.string.val_type(), self.string.val_type()],
                Some(self.bool_.val_type()),
            ),
            Builtins::StringConcat => (
                vec![self.string.val_type(), self.string.val_type()],
                Some(self.string.val_type()),
            ),
        };
        let export_name = if builtin.export() {
            Some(builtin.name())
        } else {
            None
        };
        self.add_function(export_name, params, result, function)
    }

    fn add_function(
        &mut self,
        export_name: Option<&str>,
        params: Vec<ValType>,
        result: Option<ValType>,
        function: Function,
    ) -> u32 {
        let type_index = self.function_type_with_val_types(params, result.into_iter().collect());
        let _ = self.function_section.function(type_index);
        let index = self.next_function_id();
        self.functions.push((function, index));
        if let Some(name) = export_name {
            let _ = self.export_section.export(name, ExportKind::Func, index);
        }
        index
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

#[allow(unused)]
struct ExtendedInstructionSink<'a> {
    bool_: BoolType,
    int: IntType,
    float: FloatType,
    instructions: InstructionSink<'a>,
}

#[derive(Clone, Copy)]
enum Eq {
    Bool,
    Int,
    Float,
    Call(u32),
}

trait NewExtendedInstructionSink {
    fn extend_instructions<'a>(
        &'a mut self,
        generator: &Generator<'_>,
    ) -> ExtendedInstructionSink<'a>;
}

impl NewExtendedInstructionSink for Function {
    fn extend_instructions<'a>(
        &'a mut self,
        generator: &Generator<'_>,
    ) -> ExtendedInstructionSink<'a> {
        ExtendedInstructionSink {
            bool_: generator.bool_,
            int: generator.int,
            float: generator.float,
            instructions: self.instructions(),
        }
    }
}

macro_rules! delegate {
    ($($name:ident ( $( $arg:ident : $typ:ty ),* ) ),+ $(,)? ) => {
        $(
            fn $name(&mut self $(, $arg: $typ )* ) -> &mut Self {
                let _ = self.instructions.$name($( $arg, )*);
                self
            }
        )+
    };
}

// We do not implement Deref and DerefMut so we do not call "native" int and float instructions directly.
impl<'a> ExtendedInstructionSink<'a> {
    fn global_as_non_null(&mut self, index: u32) -> &mut Self {
        self.global_get(index).ref_as_non_null()
    }

    fn list_null(&mut self, type_index: u32) -> &mut Self {
        self.ref_null(HeapType::Concrete(type_index))
    }

    fn eq(&mut self, eq: Eq) -> &mut Self {
        match eq {
            Eq::Bool => self.bool_eq(),
            Eq::Int => self.int_eq(),
            Eq::Float => self.float_eq(),
            Eq::Call(index) => self.call(index),
        }
    }

    fn constant(&mut self, generator: &mut Generator<'_>, const_: &TypedConstant) -> &mut Self {
        generator._constant(self, const_);
        self
    }

    fn constants<'b, 'c>(
        &mut self,
        generator: &mut Generator<'b>,
        consts: impl IntoIterator<Item = &'c TypedConstant>,
    ) -> &mut Self {
        for const_ in consts {
            generator._constant(self, const_);
        }
        self
    }

    fn expression(
        &mut self,
        generator: &mut Generator<'_>,
        locals: &Locals,
        scope: Scope,
        expression: &TypedExpr,
    ) -> &mut Self {
        generator._expression(locals, scope, self, expression);
        self
    }

    fn expressions<'b, 'c>(
        &mut self,
        generator: &mut Generator<'b>,
        locals: &Locals,
        scope: Scope,
        expressions: impl IntoIterator<Item = &'c TypedExpr>,
    ) -> &mut Self {
        for expression in expressions {
            generator._expression(locals, scope.clone(), self, expression);
        }
        self
    }

    fn pattern(
        &mut self,
        generator: &mut Generator<'_>,
        locals: &Locals,
        scope: &mut Scope,
        pattern: &TypedPattern,
    ) -> &mut Self {
        *scope = generator._pattern(locals, scope.clone(), self, pattern);
        self
    }

    delegate! {
        if_(bt: BlockType),
        else_(),
        end(),
        loop_(bt: BlockType),
        block(bt: BlockType),
        br(l: u32),
        unreachable(),
        drop(),
        local_set(index: u32),
        local_get(index: u32),
        local_tee(index: u32),
        global_set(index: u32),
        global_get(index: u32),
        struct_new(struct_type_index: u32),
        struct_get(struct_type_index: u32, field_index: u32),
        ref_func(index: u32),
        ref_null(ht: HeapType),
        ref_is_null(),
        ref_as_non_null(),
        ref_eq(),
        call_ref(index: u32),
        call(index: u32),
        array_new_default(type_index: u32),
        array_new_data(type_index: u32, data_segment: u32),
        array_len(),
        array_get_u(type_index: u32),
        array_set(type_index: u32),
        return_(),
        i32_const(x: i32),
        i32_ne(),
        i32_ge_u(),
        i32_lt_u(),
        i32_add(),
    }
}

#[derive(Debug, Copy, Clone)]
struct BoolType {}

impl BoolType {
    fn val_type(&self) -> ValType {
        ValType::I32
    }

    fn bool_const(&self, bool_: bool) -> ConstExpr {
        ConstExpr::i32_const(bool_.into())
    }
}

impl<'a> ExtendedInstructionSink<'a> {
    fn bool_const(&mut self, value: bool) -> &mut Self {
        let _ = self.instructions.i32_const(value as _);
        self
    }

    fn bool_neg(&mut self) -> &mut Self {
        let _ = self.instructions.i32_eqz();
        self
    }

    fn bool_eq(&mut self) -> &mut Self {
        let _ = self.instructions.i32_eq();
        self
    }

    fn bool_ne(&mut self) -> &mut Self {
        let _ = self.instructions.i32_ne();
        self
    }
}

#[allow(unused)]
#[derive(Debug, Copy, Clone)]
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

macro_rules! int_op {
    ($name:ident, $i32:ident, $i64:ident) => {
        fn $name(&mut self) -> &mut Self {
            let _ = match self.int {
                IntType::Int32 => self.instructions.$i32(),
                IntType::Int64 => self.instructions.$i64(),
            };
            self
        }
    };
}

impl<'a> ExtendedInstructionSink<'a> {
    fn int_const(&mut self, value: &BigInt) -> &mut Self {
        let _ = match self.int {
            IntType::Int32 => self.instructions.i32_const(value.try_into().unwrap()),
            IntType::Int64 => self.instructions.i64_const(value.try_into().unwrap()),
        };
        self
    }

    fn int_div(&mut self, dividend: u32, divisor: u32) -> &mut Self {
        #[rustfmt::skip]
        let _ = match self.int {
            IntType::Int32 => self
                .instructions
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
                .instructions
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
        };
        self
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

#[derive(Debug, Copy, Clone)]
struct FloatType {}

macro_rules! float_op {
    ($name:ident, $f64:ident) => {
        fn $name(&mut self) -> &mut Self {
            let _ = self.instructions.$f64();
            self
        }
    };
}

impl FloatType {
    fn val_type(&self) -> ValType {
        ValType::F64
    }

    fn float_const(&self, value: &EcoString) -> ConstExpr {
        ConstExpr::f64_const(value.parse::<f64>().unwrap().into())
    }
}

impl<'a> ExtendedInstructionSink<'a> {
    fn float_const(&mut self, value: &str) -> &mut Self {
        let value = value.replace("_", "");
        let _ = self
            .instructions
            .f64_const(value.parse::<f64>().unwrap().into());
        self
    }

    fn float_div(&mut self, dividend: u32, divisor: u32) -> &mut Self {
        #[rustfmt::skip]
        let _ = self
            .instructions
            .local_set(divisor)
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
            .end();
        self
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

struct StringType {
    type_index: u32,
}

impl StringType {
    fn store_type() -> StorageType {
        StorageType::I8
    }

    fn val_type(&self) -> ValType {
        ValType::Ref(RefType {
            heap_type: self.heap_type(),
            nullable: false,
        })
    }

    fn val_type_nullable(&self) -> ValType {
        ValType::Ref(RefType {
            heap_type: self.heap_type(),
            nullable: true,
        })
    }

    fn heap_type(&self) -> HeapType {
        HeapType::Concrete(self.type_index)
    }
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

#[derive(Debug)]
struct Locals {
    skip: u32,
    locals: HashMap<SrcSpan, u32>,
    val_types: Vec<ValType>,
}

impl Locals {
    fn new(generator: &mut Generator<'_>, num_params: u32, statements: &[TypedStatement]) -> Self {
        let mut locals = Locals {
            skip: num_params,
            locals: HashMap::new(),
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
            Statement::Assert(assert) => {
                self.expression(generator, &assert.value);
                if let Some(message) = &assert.message {
                    self.expression(generator, message);
                }
            }
            Statement::Use(_) => todo!("Statement not supported: {:#?}", statement),
        }
    }

    fn expression(&mut self, generator: &mut Generator<'_>, expression: &TypedExpr) {
        match expression {
            TypedExpr::BinOp {
                name,
                left,
                right,
                type_,
                ..
            } => {
                self.expression(generator, left);
                self.expression(generator, right);
                if matches!(name, BinOp::DivInt | BinOp::DivFloat) {
                    self.insert(generator, &left.location(), type_);
                    self.insert(generator, &right.location(), type_);
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
            TypedExpr::List { elements, tail, .. } => {
                for element in elements {
                    self.expression(generator, element);
                }
                if let Some(tail) = tail {
                    self.expression(generator, tail);
                }
            }
            TypedExpr::Tuple { elements, .. } => {
                for element in elements {
                    self.expression(generator, element);
                }
            }
            TypedExpr::TupleIndex { tuple, .. } => {
                self.expression(generator, tuple);
            }
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::Fn { .. } => {}
            _ => todo!("Expression not supported: {:#?}", expression),
        }
    }

    fn assignment(&mut self, generator: &mut Generator<'_>, assignment: &TypedAssignment) {
        self.insert(
            generator,
            &assignment.value.location(),
            &assignment.value.type_(),
        );
        self.expression(generator, &assignment.value);
        match &assignment.kind {
            AssignmentKind::Let => {
                self.pattern(generator, &assignment.pattern);
            }
            AssignmentKind::Assert { message, .. } => {
                self.pattern(generator, &assignment.pattern);
                if let Some(message) = message {
                    self.expression(generator, message);
                }
            }
            AssignmentKind::Generated => todo!("Assignment not supported: {:#?}", assignment),
        }
    }

    fn pattern(&mut self, generator: &mut Generator<'_>, pattern: &TypedPattern) {
        match pattern {
            Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::Discard { .. } => {}
            Pattern::Constructor { name, type_, .. } if is_bool_const(name, type_) => {}
            Pattern::List {
                elements,
                tail,
                location,
                type_,
            } => {
                self.insert(generator, location, type_);
                for element in elements {
                    self.pattern(generator, element);
                }
                if let Some(tail) = tail {
                    self.pattern(generator, &tail.pattern)
                }
            }
            Pattern::Tuple { location, elements } => {
                let type_ = Type::Tuple {
                    elements: elements.iter().map(|element| element.type_()).collect(),
                };
                self.insert(generator, location, &type_.into());
                for element in elements {
                    self.pattern(generator, element);
                }
            }
            Pattern::Variable {
                name,
                location,
                type_,
                ..
            } => {
                self.insert(generator, location, type_);
                if is_generic_type(type_) {
                    panic!("Local function \"{name}\" cannot be generic.");
                }
            }
            _ => todo!("Pattern not supported: {:#?}", pattern),
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

    fn val_types(&self) -> Vec<(u32, ValType)> {
        // FIXME: group locals by type
        self.val_types.iter().map(|e| (1, *e)).collect()
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
        Statement::Assert(assert) => {
            monomorphize_expression(&mut assert.value, map);
            if let Some(message) = &mut assert.message {
                monomorphize_expression(message, map);
            }
        }
        Statement::Use(_) => todo!("Statement not supported: {:#?}", statement),
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

        _ => todo!("Expression not supported: {:#?}", expression),
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
                panic!("Cannot mangle TypeVar that is not a Link: {:#?}", type_);
            }
        }
        Type::Tuple { elements, .. } => {
            let _ = write!(to, "#(");
            for element in elements {
                type_str(element, to);
            }
            let _ = write!(to, ")");
        }
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
