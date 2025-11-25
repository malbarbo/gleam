#![allow(clippy::todo, clippy::unwrap_used)]
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::Write,
    iter,
    ops::Deref,
    ptr,
    rc::Rc,
    sync::Arc,
};

use ecow::EcoString;
use itertools::Itertools;
use num_bigint::BigInt;
use wasm_encoder::{
    AbstractHeapType, BlockType, CodeSection, ConstExpr, DataCountSection, DataSection,
    ElementSection, Elements, EntityType, ExportKind, ExportSection, FieldType, Function,
    FunctionSection, GlobalSection, GlobalType, HeapType, ImportSection, InstructionSink,
    MemorySection, MemoryType, Module, RefType, StartSection, StorageType, TypeSection, ValType,
};

use crate::{
    ast::{
        AssignmentKind, BinOp, ClauseGuard, Constant, Definition, OperatorKind, Pattern, Statement,
        TypedArg, TypedAssignment, TypedClause, TypedClauseGuard, TypedConstant, TypedExpr,
        TypedFunction, TypedModule, TypedModuleConstant, TypedPattern, TypedPipelineAssignment,
        TypedStatement,
    },
    line_numbers::LineNumbers,
    type_::{self, Type, TypeVar},
};

const BUILTINS_WASM: &[u8] =
    include_bytes!("../../builtins-wasm/target/wasm32-unknown-unknown/release/builtins_wasm.wasm");

const MAIN: &str = "main";
const TRUE: &str = "True";
const FALSE: &str = "False";

pub fn module(module: &TypedModule, _line_numbers: &LineNumbers) -> Vec<u8> {
    let mut generator = Generator::new(module);
    generator.all_pub();
    let start = generator.function_start();

    let mut module = Module::default();

    // type section
    let mut type_section = TypeSection::new();
    for (type_, type_index) in generator.types.iter().sorted_by_key(|t| t.1) {
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

    // import section
    let _ = module.section(&generator.import_section);

    // function section
    let _ = module.section(&generator.function_section);

    // memory section
    // FIXME: create only if it is necessary
    let mut memory_section = MemorySection::new();
    let _ = memory_section.memory(MemoryType {
        minimum: 17,
        maximum: None,
        memory64: false,
        shared: false,
        page_size_log2: None,
    });
    let _ = module.section(&memory_section);

    // global section
    let _ = module.section(&generator.global_section);

    // export section
    let _ = module.section(&generator.export_section);

    // start section
    let _ = module.section(&StartSection {
        function_index: start,
    });

    // element section
    let mut element_section = ElementSection::new();
    let _ = element_section.declared(Elements::Functions(
        generator.functions.iter().map(|f| f.1).sorted().collect(),
    ));
    let _ = module.section(&element_section);

    // data count section
    let _ = module.section(&DataCountSection {
        count: generator.data_section.len(),
    });

    // code section
    generator.functions.sort_by_key(|t| t.1);
    let mut codes_section = CodeSection::new();
    for (body, _) in generator.functions {
        let _ = codes_section.raw(&body);
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
    import_section: ImportSection,
    export_section: ExportSection,
    // Function code and its index in the code section
    functions: Vec<(Vec<u8>, u32)>,
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
            import_section: ImportSection::new(),
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
            float: FloatType::Float64,
            string: StringType { type_index: 0 },
        };

        generator.externals_wasm();

        // String type
        let index = generator.types.len() as u32;
        generator.string.type_index = *generator
            .types
            .entry(WasmType::Array(StringType::store_type()))
            .or_insert(index);

        generator
    }

    fn externals_wasm(&mut self) {
        let mut externals = Externals::new(self.int, self.float);
        externals.functions(
            self,
            self.module.definitions.iter().filter_map(|def| match def {
                Definition::Function(function) => Some(function),
                _ => None,
            }),
        );

        let module = prepare_wasm_module(BUILTINS_WASM, &externals.used);

        let mut types = vec![];
        let mut functions_types = vec![];
        let mut code_index = 0;
        for payload in wasmparser::Parser::new(0).parse_all(&module) {
            match payload.expect("Payload") {
                wasmparser::Payload::TypeSection(section) => {
                    for item in section.into_iter_with_offsets() {
                        let (_, group) = item.expect("Type entry");
                        for type_ in group.into_types() {
                            let func_type = type_.composite_type.unwrap_func();
                            let params = wasmparser_types_to_wasmencoder_types(func_type.params());
                            let results =
                                wasmparser_types_to_wasmencoder_types(func_type.results());
                            let _ =
                                self.function_type_with_val_types(params.clone(), results.clone());
                            types.push((params, results))
                        }
                    }
                }
                wasmparser::Payload::ImportSection(section) => {
                    for item in section.into_iter_with_offsets() {
                        let (_, import) = item.expect("Import entry");
                        let type_index = match import.ty {
                            wasmparser::TypeRef::Func(index) => index,
                            _ => panic!("Import type not expected"),
                        };
                        let _ = self.import_section.import(
                            import.module,
                            import.name,
                            EntityType::Function(type_index),
                        );
                        self.next_function_id += 1;
                    }
                }
                wasmparser::Payload::GlobalSection(section) => {
                    for entry in section.into_iter_with_offsets() {
                        let (_, global) = entry.expect("Global entry");
                        let _ = self.global_section.global(
                            GlobalType {
                                val_type: wasmparser_type_to_wasmencoder_type(
                                    &global.ty.content_type,
                                ),
                                mutable: global.ty.mutable,
                                shared: global.ty.shared,
                            },
                            &const_expr_i32_const(&global.init_expr),
                        );
                    }
                }
                wasmparser::Payload::FunctionSection(section) => {
                    for function in section.into_iter_with_offsets() {
                        let (_, type_index) = function.expect("Function entry");
                        functions_types.push(types[type_index as usize].clone());
                    }
                }
                wasmparser::Payload::ExportSection(section) => {
                    for export in section.into_iter_with_offsets() {
                        let (_, export) = export.expect("Export entry");
                        let kind = match export.kind {
                            wasmparser::ExternalKind::Func => ExportKind::Func,
                            wasmparser::ExternalKind::Table => ExportKind::Table,
                            wasmparser::ExternalKind::Memory => ExportKind::Memory,
                            wasmparser::ExternalKind::Global => ExportKind::Global,
                            wasmparser::ExternalKind::Tag => ExportKind::Tag,
                        };
                        let _ = self.export_section.export(export.name, kind, export.index);
                    }
                }
                wasmparser::Payload::CodeSectionEntry(section) => {
                    let (params, results) = functions_types[code_index].clone();
                    let _ = self.add_function(None, params, results, section.as_bytes().into());
                    code_index += 1;
                }
                wasmparser::Payload::DataSection(section) => {
                    if !externals.use_data_section() {
                        continue;
                    }
                    for entry in section.into_iter_with_offsets() {
                        let (_, entry) = entry.expect("Data entry");
                        match &entry.kind {
                            wasmparser::DataKind::Passive => {
                                let _ = self.data_section.passive(entry.data.into_iter().cloned());
                            }
                            wasmparser::DataKind::Active {
                                memory_index,
                                offset_expr,
                            } => {
                                let _ = self.data_section.active(
                                    *memory_index,
                                    &const_expr_i32_const(offset_expr),
                                    entry.data.into_iter().cloned(),
                                );
                            }
                        }
                    }
                }
                wasmparser::Payload::CustomSection(section) => {
                    if let wasmparser::KnownCustom::Name(section) = section.as_known() {
                        for sub in section {
                            match sub.expect("Name section") {
                                wasmparser::Name::Function(section_limited) => {
                                    for item in section_limited.into_iter_with_offsets() {
                                        let (_, name) = item.expect("Name entry");
                                        if let Some(external) =
                                            externals.get_by_wasm_name(name.name)
                                        {
                                            let _ = self.globals.borrow_mut().push(Id {
                                                kind: IdKind::Func,
                                                name: external.gleam_name.into(),
                                                index: name.index,
                                            });
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn all_pub(&mut self) {
        for definition in &self.module.definitions {
            match definition {
                Definition::ModuleConstant(module_constant) => {
                    let _ = self.module_constant(module_constant);
                }
                Definition::Function(function) => {
                    if !function.publicity.is_public()
                        || is_generic_type(&function_type(function))
                        || function.external_webassembly.is_some()
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

    fn val_types(&mut self, types: impl IntoIterator<Item = Arc<Type>>) -> Vec<ValType> {
        types
            .into_iter()
            .map(|type_| self.val_type(&type_))
            .collect()
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
        let params = self.val_types(arguments);
        let results = self.val_types(return_.into_iter());
        self.function_type_with_val_types(params, results)
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

    fn var(&mut self, name: &EcoString, required_type: &Arc<Type>) -> Id {
        if is_generic_type(required_type) {
            panic!("Required type is generic:\n{:#?}", required_type);
        }
        for definition in &self.module.definitions {
            match definition {
                Definition::ModuleConstant(module_constant) if &module_constant.name == name => {
                    assert!(required_type.same_as(&module_constant.type_));
                    return self.module_constant(module_constant);
                }
                Definition::Function(function)
                    if function.name.as_ref().map(|s| &s.1) == Some(name) =>
                {
                    let declared_type = function_type(function);
                    return if is_generic_type(&declared_type) {
                        match find_global(&mangle(name, required_type), &self.globals) {
                            Some(id) => id, // the function has already been monomorphized
                            None => {
                                let function = Monomorphizer::new(&declared_type, required_type)
                                    .function(function);
                                if is_generic_type(&function_type(&function))
                                    || function
                                        .body
                                        .iter()
                                        .any(|statement| is_generic_type(&statement.type_()))
                                {
                                    panic!("Could not monomorphize:\n{:#?}", function);
                                }
                                self.function(&function)
                            }
                        }
                    } else {
                        if let Some((_, fname, _)) = &function.external_webassembly {
                            return find_global(&fname.into(), &self.globals).unwrap();
                        }
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
            function_params_types(function),
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
        self.functions.push((code.into_raw_body(), index));
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
        self.functions.push((code.into_raw_body(), index));
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
            Statement::Use(use_) => {
                let _ = instructions.expression(self, locals, scope.clone(), &use_.call);
            }
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
                    // Int
                    BinOp::AddInt => instructions.int_add(),
                    BinOp::SubInt => instructions.int_sub(),
                    BinOp::MultInt => instructions.int_mul(),
                    BinOp::DivInt => {
                        let (left, right) = locals.for_div(left, right);
                        instructions.int_div(left, right)
                    }
                    BinOp::RemainderInt => instructions.int_rem(),
                    BinOp::LtInt => instructions.int_lt(),
                    BinOp::LtEqInt => instructions.int_le(),
                    BinOp::GtInt => instructions.int_gt(),
                    BinOp::GtEqInt => instructions.int_ge(),
                    // Float
                    BinOp::AddFloat => instructions.float_add(),
                    BinOp::SubFloat => instructions.float_sub(),
                    BinOp::MultFloat => instructions.float_mul(),
                    BinOp::DivFloat => {
                        let (left, right) = locals.for_div(left, right);
                        instructions.float_div(left, right)
                    }
                    BinOp::LtFloat => instructions.float_lt(),
                    BinOp::LtEqFloat => instructions.float_le(),
                    BinOp::GtFloat => instructions.float_gt(),
                    BinOp::GtEqFloat => instructions.float_ge(),
                    // String
                    BinOp::Concatenate => {
                        let concat = self.function_string_concat();
                        instructions.call(concat)
                    }
                    // Eq
                    BinOp::Eq | BinOp::NotEq => {
                        let eq = self.function_eq(&left.type_());
                        let _ = instructions.eq(eq);
                        if let BinOp::NotEq = name {
                            let _ = instructions.bool_not();
                        }
                        instructions
                    }
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
                    .bool_not();
            }
            TypedExpr::Block { statements, .. } => {
                self.statements(instructions, scope, locals, statements);
            }
            TypedExpr::Pipeline {
                first_value,
                assignments,
                finally,
                ..
            } => {
                let mut scope = scope;
                let first_index = locals.for_pipeline_assignment(first_value);
                let _ = instructions
                    .expression(self, locals, scope.clone(), &first_value.value)
                    .local_set(first_index);
                scope = scope.insert_local(first_value.name.clone(), first_index);
                for (assignment, _) in assignments {
                    let assign_index = locals.for_pipeline_assignment(assignment);
                    let _ = instructions
                        .expression(self, locals, scope.clone(), &assignment.value)
                        .local_set(assign_index);
                    scope = scope.insert_local(assignment.name.clone(), assign_index);
                }
                let _ = instructions.expression(self, locals, scope, finally);
            }
            TypedExpr::Var { name, .. } if is_bool_const(name, &expression.type_()) => {
                let _ = instructions.bool_const(name == TRUE);
            }
            TypedExpr::Var { name, .. } => {
                self.expression_var(&scope, instructions, name, &expression.type_());
            }
            TypedExpr::Call {
                type_,
                fun,
                arguments,
                ..
            } => {
                let args_types = arguments.iter().map(|arg| arg.value.type_().clone());
                let args = arguments.iter().map(|arg| &arg.value);
                let index = locals.for_call(fun);
                let _ = instructions
                    .expression(self, locals, scope.clone(), fun)
                    .local_set(index)
                    .expressions(self, locals, scope, args)
                    .local_get(index)
                    .call_ref(self.function_type(args_types, Some(type_.clone())));
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
            TypedExpr::Case {
                type_,
                subjects,
                clauses,
                ..
            } => {
                self.expression_case(locals, scope, instructions, type_, subjects, clauses);
            }
            _ => todo!("Expression not supported: {:#?}", expression),
        };
    }

    fn expression_case(
        &mut self,
        locals: &Locals,
        scope: Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        type_: &Arc<Type>,
        subjects: &[TypedExpr],
        clauses: &[TypedClause],
    ) {
        let subjects_locals = locals.for_subjects(subjects);
        for (subject, index) in subjects.iter().zip(&subjects_locals) {
            // evaluate and save each subject
            let _ = instructions
                .expression(self, locals, scope.clone(), subject)
                .local_set(*index);
        }
        // block case
        let _ = instructions.block(BlockType::Result(self.val_type(type_)));
        for clause in clauses {
            // block clause
            let _ = instructions.block(BlockType::Result(self.bool_.val_type()));
            let mut scope = scope.clone();
            for patterns in iter::once(&clause.pattern).chain(&clause.alternative_patterns) {
                // block patterns
                let _ = instructions.block(BlockType::Result(self.bool_.val_type()));
                for (pattern, subject_local) in patterns.iter().zip(&subjects_locals) {
                    #[rustfmt::skip]
                    let _ = instructions
                        .local_get(*subject_local)
                        .pattern(self, locals, &mut scope, pattern)
                        .bool_not()
                        .if_(BlockType::Empty)
                          .bool_const(false)
                          // exit block patterns
                          .br(1)
                        .end();
                }
                let _ = instructions
                    // none of the patterns failed to match
                    .bool_const(true)
                    // end block patterns
                    .end()
                    // if matches
                    .if_(BlockType::Empty);
                if let Some(guard) = &clause.guard {
                    #[rustfmt::skip]
                    let _ = instructions
                        .clause_guard(self, locals, &scope, guard)
                        .if_(BlockType::Empty)
                          .bool_const(true)
                          .br(2) // exit block clause
                        .end();
                } else {
                    // exit block clause
                    let _ = instructions.bool_const(true).br(1);
                }
                // end if matches
                let _ = instructions.end();
            }
            #[rustfmt::skip]
            let _ = instructions
                .bool_const(false)
                // end block clause
                .end()
                .if_(BlockType::Empty)
                  .expression(self, locals, scope, &clause.then)
                  .br(1) // exit block case
                .end();
        }
        // end block case
        let _ = instructions.unreachable().end();
    }

    fn expression_var(
        &mut self,
        scope: &Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        name: &EcoString,
        type_: &Arc<Type>,
    ) {
        let id = scope.find(name).unwrap_or_else(|| self.var(name, type_));
        let _ = match id.kind {
            IdKind::Func => instructions.ref_func(id.index),
            IdKind::Global if type_.is_string() || type_.tuple_types().is_some() => {
                instructions.global_as_non_null(id.index)
            }
            IdKind::Global => instructions.global_get(id.index),
            IdKind::Local => instructions.local_get(id.index),
        };
    }

    fn assignment(
        &mut self,
        locals: &Locals,
        mut scope: Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        assignment: &TypedAssignment,
    ) -> Scope {
        let right = locals.for_assigment(assignment);
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
            AssignmentKind::Let | AssignmentKind::Generated => {
                let _ = instructions
                    .pattern(self, locals, &mut scope, &assignment.pattern)
                    .drop()
                    .local_get(right);
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
                type_,
                ..
            } => {
                let item_type = type_.list_type().unwrap();
                let type_index = self.list_type(&item_type);
                let right = locals.for_pattern(pattern);
                let _ = instructions
                    .local_set(right)
                    .block(BlockType::Result(self.bool_.val_type()));
                for element in elements {
                    #[rustfmt::skip]
                    let _ = instructions
                        .local_get(right)
                        .struct_get(type_index, 1)
                        .pattern(self, locals, &mut scope, element)
                        .bool_not()
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
                        .bool_not()
                        .if_(BlockType::Empty)
                          .bool_const(false)
                          .br(1)
                        .end();
                } else {
                    #[rustfmt::skip]
                    let _ = instructions
                        .local_get(right)
                        .ref_is_null()
                        .bool_not()
                        .if_(BlockType::Empty)
                          .bool_const(false)
                          .br(1)
                        .end();
                }
                let _ = instructions.bool_const(true).end();
            }
            Pattern::Tuple { elements, .. } => {
                let type_index = self.tuple_type(elements.iter().map(|element| element.type_()));
                let right = locals.for_pattern(pattern);
                let _ = instructions
                    .local_set(right)
                    .block(BlockType::Result(self.bool_.val_type()));
                for (field_index, element) in elements.iter().enumerate() {
                    #[rustfmt::skip]
                    let _ = instructions
                        .local_get(right)
                        .struct_get(type_index, field_index as u32)
                        .pattern(self, locals, &mut scope, element)
                        .bool_not()
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
            Pattern::Variable { name, .. } => {
                let right = locals.for_pattern(pattern);
                scope = scope.insert_local(name.clone(), right);
                let _ = instructions.local_set(right).bool_const(true);
            }
            _ => todo!("Assigment Assert Pattern not implemented: {:#?}", pattern),
        }
        scope
    }

    fn _clause_guard(
        &mut self,
        locals: &Locals,
        scope: &Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        guard: &TypedClauseGuard,
    ) {
        match guard {
            // Bool
            ClauseGuard::Or { left, right, .. } => {
                #[rustfmt::skip]
                let _ = instructions
                    .clause_guard(self, locals, scope, left)
                    .if_(BlockType::Result(self.bool_.val_type()))
                      .bool_const(true)
                    .else_()
                      .clause_guard(self, locals, scope, right)
                    .end();
            }
            ClauseGuard::And { left, right, .. } => {
                #[rustfmt::skip]
                let _ = instructions
                    .clause_guard(self, locals, scope, left)
                    .if_(BlockType::Result(self.bool_.val_type()))
                      .clause_guard(self, locals, scope, right)
                    .else_()
                      .bool_const(false)
                    .end();
            }
            ClauseGuard::Not { expression, .. } => {
                let _ = instructions
                    .clause_guard(self, locals, scope, expression)
                    .bool_not();
            }
            ClauseGuard::Equals { left, right, .. } if left.type_().is_bool() => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .bool_eq();
            }
            ClauseGuard::NotEquals { left, right, .. } if left.type_().is_bool() => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .bool_ne();
            }
            // Int
            ClauseGuard::AddInt { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .int_add();
            }
            ClauseGuard::SubInt { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .int_sub();
            }
            ClauseGuard::MultInt { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .int_mul();
            }
            ClauseGuard::RemainderInt { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .int_rem();
            }
            ClauseGuard::DivInt { left, right, .. } => {
                let (a, b) = locals.for_guard_div(left, right);
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .int_div(a, b);
            }
            ClauseGuard::GtInt { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .int_gt();
            }
            ClauseGuard::GtEqInt { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .int_ge();
            }
            ClauseGuard::LtInt { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .int_lt();
            }
            ClauseGuard::LtEqInt { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .int_le();
            }
            ClauseGuard::Equals { left, right, .. } if left.type_().is_int() => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .int_eq();
            }
            ClauseGuard::NotEquals { left, right, .. } if left.type_().is_int() => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .int_ne();
            }
            // Float
            ClauseGuard::AddFloat { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .float_add();
            }
            ClauseGuard::SubFloat { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .float_sub();
            }
            ClauseGuard::MultFloat { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .float_mul();
            }
            ClauseGuard::DivFloat { left, right, .. } => {
                let (a, b) = locals.for_guard_div(left, right);
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .float_div(a, b);
            }
            ClauseGuard::GtFloat { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .float_gt();
            }
            ClauseGuard::GtEqFloat { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .float_ge();
            }
            ClauseGuard::LtFloat { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .float_lt();
            }
            ClauseGuard::LtEqFloat { left, right, .. } => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .float_le();
            }
            ClauseGuard::Equals { left, right, .. } if left.type_().is_float() => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .float_eq();
            }
            ClauseGuard::NotEquals { left, right, .. } if left.type_().is_float() => {
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .float_ne();
            }
            // Others
            ClauseGuard::Constant(constant) => {
                let _ = instructions.constant(self, constant);
            }
            ClauseGuard::Block { value, .. } => {
                let _ = instructions.clause_guard(self, locals, scope, value);
            }
            ClauseGuard::Var { name, type_, .. } => {
                self.expression_var(scope, instructions, name, type_);
            }
            ClauseGuard::TupleIndex { tuple, index, .. } => {
                let type_index = self.tuple_type(tuple.type_().tuple_types().unwrap());
                let _ = instructions
                    .clause_guard(self, locals, scope, tuple)
                    .struct_get(type_index, *index as u32);
            }
            _ => todo!("Guard: {:#?}", guard),
        }
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
            todo!("Eq: {:#?}", type_);
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
            vec![self.bool_.val_type()],
            function.into_raw_body(),
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
            vec![self.bool_.val_type()],
            function.into_raw_body(),
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
                .bool_not()
                .if_(BlockType::Empty)
                  .bool_const(false)
                  .return_()
                .end();
        }
        let _ = instructions.bool_const(true).end();
        function
    }

    fn add_builtins(&mut self, builtin: Builtins, function: Function) -> u32 {
        let (params, results) = match builtin {
            Builtins::Start => (vec![], vec![]),
            Builtins::StringEq => (
                vec![self.string.val_type(), self.string.val_type()],
                vec![self.bool_.val_type()],
            ),
            Builtins::StringConcat => (
                vec![self.string.val_type(), self.string.val_type()],
                vec![self.string.val_type()],
            ),
        };
        let export_name = if builtin.export() {
            Some(builtin.name())
        } else {
            None
        };
        self.add_function(export_name, params, results, function.into_raw_body())
    }

    fn add_function(
        &mut self,
        export_name: Option<&str>,
        params: Vec<ValType>,
        results: Vec<ValType>,
        function: Vec<u8>,
    ) -> u32 {
        let type_index = self.function_type_with_val_types(params, results);
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

    fn clause_guard(
        &mut self,
        generator: &mut Generator<'_>,
        locals: &Locals,
        scope: &Scope,
        guard: &TypedClauseGuard,
    ) -> &mut Self {
        generator._clause_guard(locals, scope, self, guard);
        self
    }

    fn clause_guards<'b, 'c>(
        &mut self,
        generator: &mut Generator<'b>,
        locals: &Locals,
        scope: &Scope,
        guards: impl IntoIterator<Item = &'c TypedClauseGuard>,
    ) -> &mut Self {
        for guard in guards {
            generator._clause_guard(locals, scope, self, guard);
        }
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

    fn bool_not(&mut self) -> &mut Self {
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
enum FloatType {
    Float64,
}

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
    locals: HashMap<u64, u32>,
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

    fn val_types(&self) -> Vec<(u32, ValType)> {
        // FIXME: group locals by type
        self.val_types.iter().map(|e| (1, *e)).collect()
    }

    fn insert_assignment(&mut self, generator: &mut Generator<'_>, assignment: &TypedAssignment) {
        self._insert(generator, assignment, &assignment.type_());
    }

    fn for_assigment(&self, assignment: &TypedAssignment) -> u32 {
        self._get(assignment)
    }

    fn insert_div(&mut self, generator: &mut Generator<'_>, left: &TypedExpr, right: &TypedExpr) {
        self._insert(generator, left, &left.type_());
        self._insert(generator, right, &right.type_());
    }

    fn for_div(&self, left: &TypedExpr, right: &TypedExpr) -> (u32, u32) {
        (self._get(left), self._get(right))
    }

    fn insert_guard_div(
        &mut self,
        generator: &mut Generator<'_>,
        left: &TypedClauseGuard,
        right: &TypedClauseGuard,
    ) {
        self._insert(generator, left, &left.type_());
        self._insert(generator, right, &right.type_());
    }

    fn for_guard_div(&self, left: &TypedClauseGuard, right: &TypedClauseGuard) -> (u32, u32) {
        (self._get(left), self._get(right))
    }

    fn insert_call(&mut self, generator: &mut Generator<'_>, fun: &TypedExpr) {
        self._insert(generator, fun, &fun.type_());
    }

    fn for_call(&self, fun: &TypedExpr) -> u32 {
        self._get(fun)
    }

    fn insert_subjects(&mut self, generator: &mut Generator<'_>, subjects: &[TypedExpr]) {
        for subject in subjects {
            self._insert(generator, subject, &subject.type_());
        }
    }

    fn for_subjects(&self, subjects: &[TypedExpr]) -> Vec<u32> {
        subjects.iter().map(|subject| self._get(subject)).collect()
    }

    fn insert_pattern(&mut self, generator: &mut Generator<'_>, pattern: &TypedPattern) {
        self._insert(generator, pattern, &pattern.type_());
    }

    fn for_pattern(&self, pattern: &TypedPattern) -> u32 {
        self._get(pattern)
    }

    fn insert_pipeline_assignment(
        &mut self,
        generator: &mut Generator<'_>,
        assignment: &TypedPipelineAssignment,
    ) {
        self._insert(generator, assignment, &assignment.type_());
    }

    fn for_pipeline_assignment(&self, assignment: &TypedPipelineAssignment) -> u32 {
        self._get(assignment)
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
            Statement::Use(use_) => {
                self.expression(generator, &use_.call);
                // use_.assignments is not necessary because it is desugared in use._call
            }
        }
    }

    fn expressions<'a>(
        &mut self,
        generator: &mut Generator<'_>,
        expressions: impl IntoIterator<Item = &'a TypedExpr>,
    ) {
        for expression in expressions {
            self.expression(generator, expression);
        }
    }

    fn expression(&mut self, generator: &mut Generator<'_>, expression: &TypedExpr) {
        match expression {
            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                self.expression(generator, left);
                self.expression(generator, right);
                if matches!(name, BinOp::DivInt | BinOp::DivFloat) {
                    self.insert_div(generator, left, right);
                }
            }
            TypedExpr::Block { statements, .. } => {
                self.statements(generator, statements);
            }
            TypedExpr::Call { fun, arguments, .. } => {
                self.expressions(generator, arguments.iter().map(|arg| &arg.value));
                self.expression(generator, fun);
                self.insert_call(generator, fun);
            }
            TypedExpr::Pipeline {
                first_value,
                assignments,
                finally,
                ..
            } => {
                self.expression(generator, &first_value.value);
                self.insert_pipeline_assignment(generator, first_value);
                for (assignment, _) in assignments {
                    self.expression(generator, &assignment.value);
                    self.insert_pipeline_assignment(generator, assignment);
                }
                self.expression(generator, finally);
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
                self.expressions(generator, elements);
                if let Some(tail) = tail {
                    self.expression(generator, tail);
                }
            }
            TypedExpr::Tuple { elements, .. } => {
                self.expressions(generator, elements);
            }
            TypedExpr::TupleIndex { tuple, .. } => {
                self.expression(generator, tuple);
            }
            TypedExpr::Case {
                clauses, subjects, ..
            } => {
                self.insert_subjects(generator, subjects);
                self.expressions(generator, subjects);
                for clause in clauses {
                    self.patterns(generator, &clause.pattern);
                    for pattern in &clause.alternative_patterns {
                        self.patterns(generator, pattern);
                    }
                    if let Some(guard) = &clause.guard {
                        self.guard(generator, guard);
                    }
                    self.expression(generator, &clause.then);
                }
            }
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::Var { .. }
            | TypedExpr::Fn { .. } => {}
            _ => todo!("Expression not supported: {:#?}", expression),
        }
    }

    fn guard(&mut self, generator: &mut Generator<'_>, guard: &TypedClauseGuard) {
        match guard {
            ClauseGuard::Block { value, .. } => self.guard(generator, value),
            ClauseGuard::Constant(constant) => self.constant(generator, constant),
            ClauseGuard::Equals { left, right, .. }
            | ClauseGuard::NotEquals { left, right, .. }
            | ClauseGuard::GtInt { left, right, .. }
            | ClauseGuard::GtEqInt { left, right, .. }
            | ClauseGuard::LtInt { left, right, .. }
            | ClauseGuard::LtEqInt { left, right, .. }
            | ClauseGuard::GtFloat { left, right, .. }
            | ClauseGuard::GtEqFloat { left, right, .. }
            | ClauseGuard::LtFloat { left, right, .. }
            | ClauseGuard::LtEqFloat { left, right, .. }
            | ClauseGuard::AddInt { left, right, .. }
            | ClauseGuard::AddFloat { left, right, .. }
            | ClauseGuard::SubInt { left, right, .. }
            | ClauseGuard::SubFloat { left, right, .. }
            | ClauseGuard::MultInt { left, right, .. }
            | ClauseGuard::MultFloat { left, right, .. }
            | ClauseGuard::RemainderInt { left, right, .. }
            | ClauseGuard::Or { left, right, .. }
            | ClauseGuard::And { left, right, .. } => {
                self.guard(generator, left);
                self.guard(generator, right);
            }
            ClauseGuard::DivInt { left, right, .. } | ClauseGuard::DivFloat { left, right, .. } => {
                self.guard(generator, left);
                self.guard(generator, right);
                self.insert_guard_div(generator, left, right);
            }
            ClauseGuard::Not { expression, .. } => self.guard(generator, expression),
            ClauseGuard::Var { .. } => {}
            ClauseGuard::TupleIndex { tuple, .. } => self.guard(generator, tuple),
            _ => todo!("Guard: {:#?}", guard),
        }
    }

    fn constant(&mut self, _generator: &mut Generator<'_>, _constant: &TypedConstant) {
        // We don't need any local for constants, do we?
    }

    fn assignment(&mut self, generator: &mut Generator<'_>, assignment: &TypedAssignment) {
        self.insert_assignment(generator, assignment);
        self.expression(generator, &assignment.value);
        match &assignment.kind {
            AssignmentKind::Let | AssignmentKind::Generated => {
                self.pattern(generator, &assignment.pattern);
            }
            AssignmentKind::Assert { message, .. } => {
                self.pattern(generator, &assignment.pattern);
                if let Some(message) = message {
                    self.expression(generator, message);
                }
            }
        }
    }

    fn patterns<'a>(
        &mut self,
        generator: &mut Generator<'_>,
        patterns: impl IntoIterator<Item = &'a TypedPattern>,
    ) {
        for pattern in patterns {
            self.pattern(generator, pattern);
        }
    }

    fn pattern(&mut self, generator: &mut Generator<'_>, pattern: &TypedPattern) {
        self.insert_pattern(generator, pattern);
        match pattern {
            Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::Discard { .. } => {}
            Pattern::Constructor { name, type_, .. } if is_bool_const(name, type_) => {}
            Pattern::List { elements, tail, .. } => {
                for element in elements {
                    self.pattern(generator, element);
                }
                if let Some(tail) = tail {
                    self.pattern(generator, &tail.pattern)
                }
            }
            Pattern::Tuple { elements, .. } => {
                self.patterns(generator, elements);
            }
            Pattern::Variable { name, type_, .. } => {
                if is_generic_type(type_) {
                    panic!("Local function \"{name}\" cannot be generic.");
                }
            }
            _ => todo!("Pattern not supported: {:#?}", pattern),
        }
    }

    fn _insert(&mut self, generator: &mut Generator<'_>, key: impl LocalHash, type_: &Arc<Type>) {
        let index = self.locals.len() as u32 + self.skip;
        if self.locals.insert(key.hash(), index).is_some() {
            panic!("Locals collision.");
        }
        // Some local variable can still be unbound or generic,
        // like [], None, etc, so we choose arbitrarily to monormorphize
        // the types to int. The locals are determined before code generation,
        // so we choose to do the monomorphization here to avoid doing a
        // another complete pass in the ast before the code generation.
        set_ubound_or_generic(type_, &type_::int());
        self.val_types.push(generator.val_type(type_));
    }

    fn _get(&self, key: impl LocalHash) -> u32 {
        let id = key.hash();
        *self
            .locals
            .get(&id)
            .unwrap_or_else(|| panic!("Expect local with {id}."))
    }
}

trait LocalHash {
    fn hash(&self) -> u64;
}

impl<T> LocalHash for &T {
    fn hash(&self) -> u64 {
        // We started using location as key, but we got collision on generated
        // assigments. Let's hope we do not get collisions with this.
        ptr::from_ref(*self) as u64
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

struct Monomorphizer {
    map: HashMap<u64, Arc<Type>>,
}

impl Monomorphizer {
    fn new(from: &Arc<Type>, to: &Arc<Type>) -> Monomorphizer {
        let mut mono = Monomorphizer {
            map: HashMap::new(),
        };
        let _ = mono.bound(from, to);
        mono
    }

    fn bound(&mut self, from: &Arc<Type>, to: &Arc<Type>) -> &mut Self {
        match (from.as_ref(), to.as_ref()) {
            (Type::Var { type_ }, _) => match &*type_.borrow() {
                TypeVar::Unbound { id } | TypeVar::Generic { id } => {
                    let _ = self.map.insert(*id, to.clone());
                }
                TypeVar::Link { type_ } => {
                    let _ = self.bound(type_, to);
                }
            },
            (
                Type::Named {
                    arguments: from, ..
                },
                Type::Named { arguments: to, .. },
            ) => {
                for (from, to) in from.iter().zip(to) {
                    let _ = self.bound(from, to);
                }
            }
            (Type::Tuple { elements: from }, Type::Tuple { elements: to }) => {
                for (from, to) in from.iter().zip(to) {
                    let _ = self.bound(from, to);
                }
            }
            (
                Type::Fn {
                    arguments: from_args,
                    return_: from_return,
                },
                Type::Fn {
                    arguments: to_args,
                    return_: to_return,
                },
            ) => {
                let _ = self.bound(from_return, to_return);
                for (from, to) in from_args.iter().zip(to_args) {
                    let _ = self.bound(from, to);
                }
            }
            (_, _) => panic!(),
        }
        self
    }

    fn function(&self, function: &TypedFunction) -> TypedFunction {
        let mut function = function.clone();

        function.return_type = self.type_(&function.return_type);
        for arg in &mut function.arguments {
            arg.type_ = self.type_(&arg.type_);
        }

        self.statements(&mut function.body);

        let type_ = function_type(&function);
        assert!(!is_generic_type(&type_));

        let name = mangle(&function_name(&function), &type_);
        set_function_name(&mut function, name);

        function
    }

    fn type_(&self, old: &Arc<Type>) -> Arc<Type> {
        if let Some(id) = get_unbound_or_generic_id(old) {
            self.map.get(&id).unwrap().clone()
        } else if let Some(type_) = old.list_type() {
            Type::list(self.type_(&type_)).into()
        } else if let Some(elements) = old.tuple_types() {
            Type::Tuple {
                elements: elements.iter().map(|element| self.type_(element)).collect(),
            }
            .into()
        } else if let Some((arguments, return_)) = old.fn_types() {
            Type::Fn {
                arguments: arguments
                    .iter()
                    .map(|argument| self.type_(argument))
                    .collect(),
                return_: self.type_(&return_),
            }
            .into()
        } else {
            old.clone()
        }
    }

    fn statements(&self, statements: &mut [TypedStatement]) {
        for statement in statements {
            self.statement(statement);
        }
    }

    fn statement(&self, statement: &mut TypedStatement) {
        match statement {
            Statement::Expression(expression) => self.expression(expression),
            Statement::Assignment(assignment) => {
                self.expression(&mut assignment.value);
                match &mut assignment.kind {
                    AssignmentKind::Let => {}
                    AssignmentKind::Generated => {}
                    AssignmentKind::Assert { message, .. } => {
                        if let Some(message) = message {
                            self.expression(message);
                        }
                    }
                }
                self.pattern(&mut assignment.pattern);
            }
            Statement::Assert(assert) => {
                self.expression(&mut assert.value);
                if let Some(message) = &mut assert.message {
                    self.expression(message);
                }
            }
            Statement::Use(use_) => {
                self.expression(&mut use_.call);
            }
        }
    }

    fn expressions(&self, expressions: &mut [TypedExpr]) {
        for expression in expressions {
            self.expression(expression);
        }
    }

    fn expression(&self, expression: &mut TypedExpr) {
        match expression {
            TypedExpr::Todo { message, type_, .. } | TypedExpr::Panic { message, type_, .. } => {
                *type_ = self.type_(type_);
                if let Some(message) = message {
                    self.expression(&mut *message);
                }
            }
            TypedExpr::Block { statements, .. } => {
                self.statements(statements);
            }
            TypedExpr::Var { constructor, .. } => {
                constructor.type_ = self.type_(&constructor.type_);
            }
            TypedExpr::Fn {
                type_,
                arguments,
                body,
                ..
            } => {
                *type_ = self.type_(type_);
                for arg in arguments {
                    arg.type_ = self.type_(&arg.type_);
                }
                self.statements(body);
            }
            TypedExpr::Call {
                type_,
                fun,
                arguments,
                ..
            } => {
                *type_ = self.type_(type_);
                self.expression(fun);
                for argument in arguments {
                    self.expression(&mut argument.value);
                }
            }
            TypedExpr::BinOp { right, left, .. } => {
                self.expression(right);
                self.expression(left);
            }
            TypedExpr::List {
                type_,
                elements,
                tail,
                ..
            } => {
                *type_ = self.type_(type_);
                self.expressions(elements);
                if let Some(tail) = tail {
                    self.expression(&mut *tail);
                }
            }
            TypedExpr::Tuple {
                type_, elements, ..
            } => {
                *type_ = self.type_(type_);
                self.expressions(elements);
            }
            TypedExpr::TupleIndex { type_, tuple, .. } => {
                *type_ = self.type_(type_);
                self.expression(tuple);
            }
            TypedExpr::Case {
                type_,
                subjects,
                clauses,
                ..
            } => {
                *type_ = self.type_(type_);
                self.expressions(subjects);
                for clause in clauses {
                    self.expression(&mut clause.then);
                    self.patterns(&mut clause.pattern);
                    for patterns in &mut clause.alternative_patterns {
                        self.patterns(patterns);
                    }
                    if let Some(guard) = &mut clause.guard {
                        todo!("Monomorphize Guard: {:#?}", guard);
                    }
                }
            }
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. } => {}
            _ => todo!("Expression not supported: {:#?}", expression),
        }
    }

    fn patterns(&self, patterns: &mut [TypedPattern]) {
        for pattern in patterns {
            self.pattern(pattern);
        }
    }

    fn pattern(&self, pattern: &mut TypedPattern) {
        match pattern {
            Pattern::Int { .. }
            | Pattern::Float { .. }
            | Pattern::String { .. }
            | Pattern::BitArray { .. }
            | Pattern::StringPrefix { .. }
            | Pattern::BitArraySize(_) => {}
            Pattern::Variable { type_, .. }
            | Pattern::Invalid { type_, .. }
            | Pattern::Discard { type_, .. } => {
                *type_ = self.type_(type_);
            }
            Pattern::Constructor {
                type_, arguments, ..
            } => {
                *type_ = self.type_(type_);
                for arg in arguments {
                    self.pattern(&mut arg.value);
                }
            }
            Pattern::Assign { pattern, .. } => {
                self.pattern(pattern);
            }
            Pattern::List {
                type_,
                elements,
                tail,
                ..
            } => {
                *type_ = self.type_(type_);
                self.patterns(elements);
                if let Some(tail) = tail {
                    self.pattern(&mut tail.pattern);
                }
            }
            Pattern::Tuple { elements, .. } => {
                self.patterns(elements);
            }
        }
    }
}

fn get_unbound_or_generic_id(type_: &Arc<Type>) -> Option<u64> {
    match type_.as_ref() {
        Type::Var { type_: var } => match &*var.borrow() {
            TypeVar::Unbound { id } | TypeVar::Generic { id } => Some(*id),
            TypeVar::Link { type_ } => get_unbound_or_generic_id(type_),
        },
        _ => None,
    }
}

fn set_ubound_or_generic(old: &Arc<Type>, new: &Arc<Type>) {
    match old.as_ref() {
        Type::Var { type_ } => {
            if let TypeVar::Link { type_ } = &*type_.borrow() {
                set_ubound_or_generic(type_, new);
            } else {
                *type_.borrow_mut() = TypeVar::Link { type_: new.clone() };
            };
        }
        Type::Named { arguments, .. } => {
            for argument in arguments {
                set_ubound_or_generic(argument, new);
            }
        }
        Type::Fn { arguments, return_ } => {
            for argument in arguments {
                set_ubound_or_generic(argument, new);
            }
            set_ubound_or_generic(return_, new);
        }
        Type::Tuple { elements } => {
            for element in elements {
                set_ubound_or_generic(element, new);
            }
        }
    }
}

#[allow(unused)]
fn type_str(type_: &Arc<Type>) -> String {
    let mut s = String::new();
    type_str_acc(type_, &mut s);
    s
}

fn type_str_acc(type_: &Arc<Type>, to: &mut String) {
    match &**type_ {
        Type::Named {
            name, arguments, ..
        } => {
            let _ = write!(to, "{}", name);
            if !arguments.is_empty() {
                types_str(arguments, to);
            }
        }
        Type::Fn { arguments, return_ } => {
            let _ = write!(to, "fn");
            types_str(arguments, to);
            let _ = write!(to, "->");
            type_str_acc(return_, to);
        }
        Type::Var { type_ } => {
            if let TypeVar::Link { type_ } = type_.borrow().deref() {
                type_str_acc(type_, to);
            } else {
                panic!("Cannot mangle TypeVar that is not a Link: {:#?}", type_);
            }
        }
        Type::Tuple { elements, .. } => {
            let _ = write!(to, "#(");
            for element in elements {
                type_str_acc(element, to);
            }
            let _ = write!(to, ")");
        }
    }
}

fn types_str(types: &[Arc<Type>], to: &mut String) {
    let _ = write!(to, "(");
    if let Some((first, rest)) = types.split_first() {
        type_str_acc(first, to);
        for type_ in rest {
            let _ = write!(to, ",");
            type_str_acc(type_, to);
        }
    }
    let _ = write!(to, ")");
}

fn function_type(function: &TypedFunction) -> Arc<Type> {
    Type::Fn {
        arguments: function_params_types(function),
        return_: function_return_type(function),
    }
    .into()
}

fn function_params_types(function: &TypedFunction) -> Vec<Arc<Type>> {
    function
        .arguments
        .iter()
        .map(|arg| arg.type_.clone())
        .collect()
}

fn function_return_type(function: &TypedFunction) -> Arc<Type> {
    function.return_type.clone()
}

fn function_name(function: &TypedFunction) -> &EcoString {
    &function.name.as_ref().unwrap().1
}

fn set_function_name(function: &mut TypedFunction, name: EcoString) {
    function.name.as_mut().unwrap().1 = name;
}

fn mangle(name: &EcoString, type_: &Arc<Type>) -> EcoString {
    let mut name = String::from(name);
    if let Some((params, return_)) = type_.fn_types() {
        types_str(&params, &mut name);
        let _ = write!(&mut name, "->");
        type_str_acc(&return_, &mut name);
    } else {
        type_str_acc(type_, &mut name);
    }
    name.into()
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
struct ExternalFunction {
    gleam_name: &'static str,
    wasm_name: &'static str,
    params: Vec<ValType>,
    results: Vec<ValType>,
}

const PRINT: &'static str = "print";
const INT_TO_STRING: &'static str = "int_to_string";
const FLOAT_TO_STRING: &'static str = "float_to_string";

struct Externals {
    all: Vec<ExternalFunction>,
    used: HashSet<ExternalFunction>,
    echo: usize,
}

impl Externals {
    fn new(int_type: IntType, float_type: FloatType) -> Externals {
        let int = int_type.val_type();
        let float = float_type.val_type();
        Externals {
            all: vec![
                ExternalFunction {
                    gleam_name: PRINT,
                    wasm_name: "_print",
                    params: vec![int, int, int],
                    results: vec![int],
                },
                ExternalFunction {
                    gleam_name: INT_TO_STRING,
                    wasm_name: match int_type {
                        IntType::Int32 => "_i32_to_str",
                        IntType::Int64 => "_i64_to_str",
                    },
                    params: vec![int, int],
                    results: vec![int],
                },
                ExternalFunction {
                    gleam_name: FLOAT_TO_STRING,
                    wasm_name: match float_type {
                        FloatType::Float64 => "_f64_to_str",
                    },
                    params: vec![float, int],
                    results: vec![int],
                },
            ],
            used: HashSet::new(),
            echo: 0,
        }
    }

    fn int_to_str(&self) -> ExternalFunction {
        self.get_by_gleam_name(INT_TO_STRING).unwrap()
    }

    fn float_to_str(&self) -> ExternalFunction {
        self.get_by_gleam_name(FLOAT_TO_STRING).unwrap()
    }

    fn get_by_gleam_name(&self, name: &str) -> Option<ExternalFunction> {
        self.all.iter().find(|ex| ex.gleam_name == name).cloned()
    }

    fn get_by_wasm_name(&self, name: &str) -> Option<ExternalFunction> {
        self.all.iter().find(|ex| ex.wasm_name == name).cloned()
    }

    fn use_data_section(&self) -> bool {
        self.used.iter().any(|ex| ex.gleam_name == FLOAT_TO_STRING)
    }

    fn functions<'a>(
        &mut self,
        generator: &mut Generator<'_>,
        functions: impl IntoIterator<Item = &'a TypedFunction>,
    ) {
        for function in functions {
            if let Some((module, name, _)) = &function.external_webassembly {
                // FIXME: return an error, add line number
                assert_eq!(module, "builtins");
                let external = match self.get_by_gleam_name(name) {
                    Some(external) => external,
                    None => {
                        panic!("There is no function {name} in {module}");
                    }
                };
                let params =
                    generator.val_types(function.arguments.iter().map(|arg| arg.type_.clone()));
                let results = generator.val_types(iter::once(function.return_type.clone()));
                if (&external.params, &external.results) != (&params, &results) {
                    panic!(
                        "Wrong type for {module}/{name}. Expected {:?}, but got {:?}.",
                        (&external.params, &external.results),
                        (params, results)
                    );
                }
                let _ = self.used.insert(external);
            }
            self.function(function);
        }
    }

    fn function(&mut self, function: &TypedFunction) {
        self.statements(&function.body);
    }

    fn statements(&mut self, statements: &[TypedStatement]) {
        for statement in statements {
            self.statement(statement);
        }
    }

    fn statement(&mut self, statement: &TypedStatement) {
        match statement {
            Statement::Expression(expression) => self.expression(expression),
            Statement::Assignment(assignment) => {
                self.expression(&assignment.value);
            }
            Statement::Use(use_) => self.expression(&use_.call),
            Statement::Assert(assert) => {
                self.expression(&assert.value);
                if let Some(message) = &assert.message {
                    self.expression(message);
                }
            }
        }
    }

    fn expressions(&mut self, expressions: &[TypedExpr]) {
        for expression in expressions {
            self.expression(expression);
        }
    }

    fn expression(&mut self, expression: &TypedExpr) {
        match expression {
            TypedExpr::Int { .. } => {
                if self.echo > 0 {
                    let _ = self.used.insert(self.int_to_str());
                }
            }
            TypedExpr::Float { .. } => {
                if self.echo > 0 {
                    let _ = self.used.insert(self.float_to_str());
                }
            }
            TypedExpr::String { .. } => {}
            TypedExpr::Block { statements, .. } => self.statements(statements),
            TypedExpr::Pipeline {
                first_value,
                assignments,
                finally,
                ..
            } => {
                self.expression(&first_value.value);
                for (assignment, _) in assignments {
                    self.expression(&assignment.value);
                }
                self.expression(finally);
            }
            TypedExpr::Var { .. } => {}
            TypedExpr::Fn { body, .. } => {
                self.statements(&body);
            }
            TypedExpr::List { elements, tail, .. } => {
                self.expressions(elements);
                if let Some(tail) = tail {
                    self.expression(tail)
                }
            }
            TypedExpr::Call { fun, arguments, .. } => {
                self.expression(fun);
                for arg in arguments {
                    self.expression(&arg.value);
                }
            }
            TypedExpr::BinOp { left, right, .. } => {
                self.expression(left);
                self.expression(right);
            }
            TypedExpr::Case {
                subjects, clauses, ..
            } => {
                self.expressions(subjects);
                for clause in clauses {
                    self.expression(&clause.then);
                }
            }
            TypedExpr::Tuple { elements, .. } => {
                self.expressions(elements);
            }
            TypedExpr::TupleIndex { tuple, .. } => {
                self.expression(tuple.as_ref());
            }
            TypedExpr::Todo { message, .. } | TypedExpr::Panic { message, .. } => {
                if let Some(message) = message {
                    self.expression(message);
                }
            }
            TypedExpr::Echo {
                expression,
                message,
                ..
            } => {
                self.echo += 1;
                if let Some(expression) = expression {
                    self.expression(expression);
                }
                if let Some(message) = message {
                    self.expression(message);
                }
                self.echo -= 1;
            }
            TypedExpr::NegateBool { value, .. } | TypedExpr::NegateInt { value, .. } => {
                self.expression(value)
            }
            _ => todo!("Expression: {:#?}", expression),
        }
    }
}

fn prepare_wasm_module<'a>(
    buffer: &[u8],
    externals: impl IntoIterator<Item = &'a ExternalFunction>,
) -> Vec<u8> {
    let mut module = walrus::Module::from_buffer(buffer).unwrap();
    let mut roots = HashSet::new();
    'loop_: for external in externals {
        for func in module.funcs.iter() {
            if func.name.as_deref() == Some(external.wasm_name) {
                let type_ = module.types.get(func.ty());
                let params = walrus_types_to_wasmencoder_types(type_.params());
                let results = walrus_types_to_wasmencoder_types(type_.results());
                assert_eq!((&external.params, &external.results), (&params, &results));
                let _ = roots.insert(func.id());
                continue 'loop_;
            }
        }
        panic!()
    }

    // Find functions and globals used by root functions
    struct State<'a> {
        queue: &'a mut Vec<walrus::FunctionId>,
        used_globals: &'a mut HashSet<walrus::GlobalId>,
    }

    impl<'a> walrus::ir::Visitor<'a> for State<'a> {
        fn visit_function_id(&mut self, function: &walrus::FunctionId) {
            self.queue.push(function.clone());
        }

        fn visit_global_id(&mut self, global: &walrus::GlobalId) {
            let _ = self.used_globals.insert(global.clone());
        }
    }
    let mut used_functions = HashSet::new();
    let mut used_types = HashSet::new();
    let mut used_globals = HashSet::new();
    let mut queue: Vec<_> = roots.into_iter().collect();
    while let Some(id) = queue.pop() {
        let func = module.funcs.get(id);
        let _ = used_types.insert(func.ty());
        if used_functions.insert(func.id()) {
            if let walrus::FunctionKind::Local(local) = &func.kind {
                walrus::ir::dfs_in_order(
                    &mut State {
                        queue: &mut queue,
                        used_globals: &mut used_globals,
                    },
                    local,
                    local.entry_block(),
                );
            }
        }
    }

    // Remove unused functions
    let unused: Vec<_> = module
        .funcs
        .iter()
        .map(|f| f.id())
        .filter(|id| !used_functions.contains(id))
        .collect();
    for id in unused {
        module.funcs.delete(id);
        if let Some(export) = module.exports.get_exported_func(id) {
            module.exports.delete(export.id());
        }
        if let Some(import) = module.imports.get_imported_func(id) {
            module.imports.delete(import.id());
        }
    }

    // Remove unused types
    let unused: Vec<_> = module
        .types
        .iter()
        .map(|t| t.id())
        .filter(|id| !used_types.contains(id))
        .collect();
    for id in unused {
        module.types.delete(id);
    }

    // Remove unused globals
    let unused: Vec<_> = module
        .globals
        .iter()
        .map(|g| g.id())
        .filter(|id| !used_globals.contains(id))
        .collect();
    for id in unused {
        module.globals.delete(id);
        if let Some(export) = module.exports.get_exported_global(id) {
            module.exports.delete(export.id());
        }
    }

    module.emit_wasm()
}

fn walrus_types_to_wasmencoder_types(types: &[walrus::ValType]) -> Vec<ValType> {
    types
        .iter()
        .map(|type_| match type_ {
            walrus::ValType::I32 => ValType::I32,
            walrus::ValType::I64 => ValType::I64,
            walrus::ValType::F32 => ValType::F32,
            walrus::ValType::F64 => ValType::F64,
            walrus::ValType::V128 => ValType::V128,
            walrus::ValType::Ref(ref_type) => match *ref_type {
                walrus::RefType::Externref => ValType::Ref(RefType::EXTERNREF),
                walrus::RefType::Funcref => ValType::Ref(RefType::FUNCREF),
                walrus::RefType::Exnref => ValType::Ref(RefType::EXNREF),
                _ => todo!(),
            },
        })
        .collect()
}

fn wasmparser_types_to_wasmencoder_types(types: &[wasmparser::ValType]) -> Vec<ValType> {
    types
        .iter()
        .map(wasmparser_type_to_wasmencoder_type)
        .collect()
}

fn wasmparser_type_to_wasmencoder_type(type_: &wasmparser::ValType) -> ValType {
    match type_ {
        wasmparser::ValType::I32 => ValType::I32,
        wasmparser::ValType::I64 => ValType::I64,
        wasmparser::ValType::F32 => ValType::F32,
        wasmparser::ValType::F64 => ValType::F64,
        wasmparser::ValType::V128 => ValType::V128,
        wasmparser::ValType::Ref(ref_type) => match ref_type.heap_type() {
            wasmparser::HeapType::Abstract { shared, ty } => ValType::Ref(RefType {
                nullable: ref_type.is_nullable(),
                heap_type: HeapType::Abstract {
                    shared,
                    ty: match ty {
                        wasmparser::AbstractHeapType::Func => AbstractHeapType::Func,
                        wasmparser::AbstractHeapType::Extern => AbstractHeapType::Extern,
                        wasmparser::AbstractHeapType::Any => AbstractHeapType::Any,
                        wasmparser::AbstractHeapType::None => AbstractHeapType::None,
                        wasmparser::AbstractHeapType::NoExtern => AbstractHeapType::NoExtern,
                        wasmparser::AbstractHeapType::NoFunc => AbstractHeapType::NoFunc,
                        wasmparser::AbstractHeapType::Eq => AbstractHeapType::Eq,
                        wasmparser::AbstractHeapType::Struct => AbstractHeapType::Struct,
                        wasmparser::AbstractHeapType::Array => AbstractHeapType::Array,
                        wasmparser::AbstractHeapType::I31 => AbstractHeapType::I31,
                        wasmparser::AbstractHeapType::Exn => AbstractHeapType::Exn,
                        wasmparser::AbstractHeapType::NoExn => AbstractHeapType::NoExn,
                        wasmparser::AbstractHeapType::Cont => AbstractHeapType::Cont,
                        wasmparser::AbstractHeapType::NoCont => AbstractHeapType::NoCont,
                    },
                },
            }),
            wasmparser::HeapType::Concrete(unpacked_index) => ValType::Ref(RefType {
                nullable: ref_type.is_nullable(),
                heap_type: HeapType::Concrete(unpacked_index.as_module_index().unwrap()),
            }),
        },
    }
}

fn const_expr_i32_const(const_: &wasmparser::ConstExpr<'_>) -> ConstExpr {
    let mut i32_value = 0;
    for op in const_.get_operators_reader().into_iter_with_offsets() {
        match op.expect("operator").0 {
            wasmparser::Operator::I32Const { value } => {
                assert_eq!(i32_value, 0, "Too much ops");
                i32_value = value;
            }
            wasmparser::Operator::End => {}
            op @ _ => panic!("Operator not expected: {:?}", op),
        }
    }
    ConstExpr::i32_const(i32_value)
}
