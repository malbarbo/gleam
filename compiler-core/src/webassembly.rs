#![allow(clippy::todo, clippy::unwrap_used)]
use ecow::EcoString;
use itertools::Itertools;
use num_bigint::BigInt;
use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{BTreeSet, HashMap, HashSet},
    iter,
    ops::Deref,
    ptr,
    rc::Rc,
    str::Chars,
    sync::Arc,
};
use wasm_encoder::{
    AbstractHeapType, BlockType, CodeSection, CompositeInnerType, CompositeType, ConstExpr,
    DataCountSection, DataSection, ElementSection, Elements, EntityType, ExportKind, ExportSection,
    FieldType, Function, FunctionSection, GlobalSection, GlobalType, HeapType, ImportSection,
    IndirectNameMap, InstructionSink, MemArg, MemorySection, MemoryType, Module, NameMap,
    NameSection, RefType, StartSection, StorageType, StructType, SubType, TypeSection, ValType,
};

use crate::{
    ast::{
        AssignmentKind, BinOp, ClauseGuard, Constant, OperatorKind, Pattern, Publicity, SrcSpan,
        Statement, TodoKind, TypeAst, TypeAstVar, TypedArg, TypedAssert, TypedAssignment,
        TypedClause, TypedClauseGuard, TypedConstant, TypedCustomType, TypedExpr, TypedFunction,
        TypedModule, TypedModuleConstant, TypedPattern, TypedPipelineAssignment,
        TypedRecordConstructor, TypedRecordConstructorArg, TypedStatement,
        visit::{
            Visit, visit_typed_assert, visit_typed_assignment, visit_typed_clause_guard,
            visit_typed_expr, visit_typed_expr_bin_op, visit_typed_expr_call,
            visit_typed_expr_case, visit_typed_expr_echo, visit_typed_expr_panic,
            visit_typed_expr_todo, visit_typed_pattern, visit_typed_pipeline_assignment,
        },
    },
    line_numbers::LineNumbers,
    type_::{
        self, PRELUDE_MODULE_NAME, Type, TypeVar,
        printer::{Names, Printer},
    },
};

const BUILTINS_WASM: &[u8] =
    include_bytes!("../../builtins-wasm/target/wasm32-unknown-unknown/release/builtins_wasm.wasm");

const MAIN: &str = "main";

const I32_TO_STR: &str = "_i32_to_str";
const I64_TO_STR: &str = "_i64_to_str";
const F64_TO_STR: &str = "_f64_to_str";

const PARSE_I32: &str = "_parse_i32";
const PARSE_I64: &str = "_parse_i64";
const PARSE_F64: &str = "_parse_f64";

const HEAP_BASE: &str = "_heap_base";
const EXIT: &str = "_exit";
const PRINT: &str = "_print";

const STDERR: i32 = 2;

const BOOL_VALTYPE: ValType = ValType::I32;

pub fn module(module: &TypedModule, line_numbers: &LineNumbers) -> Vec<u8> {
    let mut generator = Generator::new(module, line_numbers);
    let start = generator.generate();

    let mut module = Module::default();

    // type section
    let mut type_section = TypeSection::new();
    for (type_, index) in generator.wasm_types.iter().sorted_by(|a, b| a.1.cmp(b.1)) {
        match &type_.kind {
            WasmTypeKind::Array(storage_type) => type_section.ty().array(storage_type, true),
            WasmTypeKind::Function(params, results) => {
                type_section.ty().function(params.clone(), results.clone())
            }
            WasmTypeKind::List(val_type) => {
                let list_val_type = generator.list_val_type(*index);
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
            WasmTypeKind::Struct(val_types) => {
                type_section
                    .ty()
                    .struct_(val_types.iter().map(|val_type| FieldType {
                        element_type: StorageType::Val(val_type.1),
                        mutable: false,
                    }));
            }
            WasmTypeKind::Union(val_types, supertype_idx) => {
                type_section.ty().subtype(&SubType {
                    is_final: false,
                    supertype_idx: *supertype_idx,
                    composite_type: CompositeType {
                        inner: CompositeInnerType::Struct(StructType {
                            fields: iter::once(&("tag".into(), ValType::I32))
                                .chain(val_types)
                                .map(|val_type| FieldType {
                                    element_type: StorageType::Val(val_type.1),
                                    mutable: false,
                                })
                                .collect_vec()
                                .into(),
                        }),
                        shared: false,
                    },
                });
            }
        }
    }
    let _ = module.section(&type_section);

    // import section
    let _ = module.section(&generator.import_section);

    // function section
    let mut function_section = FunctionSection::new();
    for function in &generator.functions {
        let _ = function_section.function(function.type_index);
    }
    let _ = module.section(&function_section);

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
    for function in generator.functions.iter().filter(|f| f.export) {
        let _ = generator
            .export_section
            .export(&function.name, ExportKind::Func, function.index);
    }
    let _ = module.section(&generator.export_section);

    // start section
    let _ = module.section(&StartSection {
        function_index: start,
    });

    // element section
    let mut element_section = ElementSection::new();
    let _ = element_section.declared(Elements::Functions(
        generator.functions.iter().map(|f| f.index).collect(),
    ));
    let _ = module.section(&element_section);

    // data count section
    let _ = module.section(&DataCountSection {
        count: generator.data_section.len(),
    });

    // code section
    let mut codes_section = CodeSection::new();
    for function in &generator.functions {
        let _ = codes_section.raw(&function.code);
    }
    let _ = module.section(&codes_section);

    // data section
    let _ = module.section(&generator.data_section);

    // name section
    let mut names = NameSection::new();
    names.module(&generator.module.name);

    // name section / function names
    let mut function_names = NameMap::new();
    for function in &generator.functions {
        function_names.append(function.index, &function.name);
    }
    names.functions(&function_names);

    // name section / type names
    let mut type_names = NameMap::new();
    for (wasm_type, index) in &generator.wasm_types {
        if let Some(name) = &wasm_type.name {
            type_names.append(*index, name);
        }
    }
    names.types(&type_names);

    // name section / global names
    names.globals(&generator.global_names);

    // name section / local names
    let mut locals = IndirectNameMap::new();
    for function in &generator.functions {
        let mut name_map = NameMap::new();
        for (index, name) in &function.locals {
            name_map.append(*index, name);
        }
        locals.append(function.index, &name_map);
    }
    names.locals(&locals);

    // name section / local names
    let mut fields = IndirectNameMap::new();
    for (type_, index) in &generator.wasm_types {
        let mut name_map = NameMap::new();
        match &type_.kind {
            WasmTypeKind::List(_) => {
                name_map.append(0, "rest");
                name_map.append(1, "first");
            }
            WasmTypeKind::Struct(items) => {
                for (index, (name, _)) in items.iter().enumerate() {
                    name_map.append(index as u32, name);
                }
            }
            WasmTypeKind::Union(items, _) => {
                name_map.append(0, "tag");
                for (index, (name, _)) in items.iter().enumerate() {
                    name_map.append(index as u32 + 1, name);
                }
            }
            _ => {}
        }
        fields.append(*index, &name_map);
    }
    names.fields(&fields);

    let _ = module.section(&names);

    // finalize
    module.finish()
}

#[derive(Hash, PartialEq, Eq)]
struct WasmType {
    name: Option<EcoString>,
    kind: WasmTypeKind,
}

impl WasmType {
    fn array(store_type: StorageType) -> WasmType {
        WasmType {
            name: None,
            kind: WasmTypeKind::Array(store_type),
        }
    }

    fn function(params: Vec<ValType>, results: Vec<ValType>) -> WasmType {
        WasmType {
            name: None,
            kind: WasmTypeKind::Function(params, results),
        }
    }

    // FIXME: use list val type
    fn list(name: EcoString, item_val_type: ValType) -> WasmType {
        WasmType {
            name: Some(name),
            kind: WasmTypeKind::List(item_val_type),
        }
    }

    fn struct_(name: EcoString, fields: Vec<(EcoString, ValType)>) -> WasmType {
        WasmType {
            name: Some(name),
            kind: WasmTypeKind::Struct(fields),
        }
    }

    fn union(
        name: EcoString,
        fields: Vec<(EcoString, ValType)>,
        supertype: Option<u32>,
    ) -> WasmType {
        WasmType {
            name: Some(name),
            kind: WasmTypeKind::Union(fields, supertype),
        }
    }
}

#[derive(Hash, PartialEq, Eq)]
enum WasmTypeKind {
    Array(StorageType),
    Function(Vec<ValType>, Vec<ValType>),
    List(ValType),
    Struct(Vec<(EcoString, ValType)>),
    Union(Vec<(EcoString, ValType)>, Option<u32>),
}

#[derive(Clone)]
enum WasmConst {
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
    Struct {
        global_index: u32,
        type_index: u32,
        elements: Vec<TypedConstant>,
    },
    Var {
        global_index: u32,
        name: EcoString,
    },
}

#[derive(Clone)]
struct WasmFunction {
    name: EcoString,
    export: bool,
    index: u32,
    type_index: u32,
    code: Rc<Vec<u8>>,
    locals: Vec<(u32, EcoString)>,
}

impl std::cmp::Eq for WasmFunction {}

impl PartialEq for WasmFunction {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl PartialOrd for WasmFunction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WasmFunction {
    fn cmp(&self, other: &Self) -> Ordering {
        self.index.cmp(&other.index)
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
enum BuiltinFunction {
    StringConcat,
    Equal(ValType, EcoString),
    ListRepr(ValType, EcoString),
    TupleRepr(ValType, EcoString),
    FunctionRepr(ValType, EcoString),
    CustomTypeRepr(ValType, EcoString, Option<EcoString>),
    VariantConstructor(Vec<ValType>, ValType, EcoString),
}

impl BuiltinFunction {
    fn name(&self) -> EcoString {
        match self {
            BuiltinFunction::StringConcat => "_string_concat".into(),
            BuiltinFunction::Equal(_, name) => format!("_equal({name})").into(),
            BuiltinFunction::ListRepr(_, name) => format!("_repr({name})").into(),
            BuiltinFunction::TupleRepr(_, name) => format!("_repr({name})").into(),
            BuiltinFunction::FunctionRepr(_, name) => format!("_repr({name})").into(),
            BuiltinFunction::CustomTypeRepr(_, name, variant) => {
                if let Some(variant) = variant {
                    format!("_repr({name}.{variant})").into()
                } else {
                    format!("_repr({name})").into()
                }
            }
            BuiltinFunction::VariantConstructor(_, _, name) => format!("_create({name})").into(),
        }
    }
}

#[derive(Hash, PartialEq, Eq, Ord, PartialOrd, Clone, Debug)]
enum BuiltinFunctionExternal {
    // In topological order of dependencies
    I32ToInt,
    IntToI32,
    IntRepr,
    FloatRepr,
    StringRepr,
    StringToMemory,
    MemoryToString,
    ParseInt,
    ParseFloat,
}

impl BuiltinFunctionExternal {
    fn all() -> &'static [BuiltinFunctionExternal] {
        &[
            BuiltinFunctionExternal::I32ToInt,
            BuiltinFunctionExternal::IntToI32,
            BuiltinFunctionExternal::IntRepr,
            BuiltinFunctionExternal::FloatRepr,
            BuiltinFunctionExternal::StringRepr,
            BuiltinFunctionExternal::StringToMemory,
            BuiltinFunctionExternal::MemoryToString,
            BuiltinFunctionExternal::ParseInt,
            BuiltinFunctionExternal::ParseFloat,
        ]
    }

    fn by_name(name: &str) -> Option<BuiltinFunctionExternal> {
        Self::all().iter().find(|f| f.name() == name).cloned()
    }

    fn name(&self) -> &'static str {
        match self {
            BuiltinFunctionExternal::I32ToInt => "_i32_to_int",
            BuiltinFunctionExternal::IntToI32 => "_int_to_i32",
            BuiltinFunctionExternal::IntRepr => "_repr_int",
            BuiltinFunctionExternal::FloatRepr => "_repr_float",
            BuiltinFunctionExternal::StringRepr => "_repr_string",
            BuiltinFunctionExternal::StringToMemory => "_string_to_memory",
            BuiltinFunctionExternal::MemoryToString => "_memory_to_string",
            BuiltinFunctionExternal::ParseInt => "_parse_int",
            BuiltinFunctionExternal::ParseFloat => "_parse_float",
        }
    }

    fn type_(&self, i32: Arc<Type>) -> (Vec<Arc<Type>>, Arc<Type>) {
        let int = type_::int();
        match self {
            BuiltinFunctionExternal::I32ToInt => (vec![i32], int),
            BuiltinFunctionExternal::IntToI32 => (vec![int], i32),
            BuiltinFunctionExternal::IntRepr => (vec![int, i32.clone()], i32),
            BuiltinFunctionExternal::FloatRepr => (vec![type_::float(), i32.clone()], i32),
            BuiltinFunctionExternal::StringRepr => (vec![type_::string(), i32.clone()], int),
            BuiltinFunctionExternal::StringToMemory => (vec![type_::string(), i32.clone()], int),
            BuiltinFunctionExternal::MemoryToString => (vec![i32.clone(), i32], type_::string()),
            BuiltinFunctionExternal::ParseInt => {
                (vec![type_::string()], type_::result(int, type_::nil()))
            }
            BuiltinFunctionExternal::ParseFloat => (
                vec![type_::string()],
                type_::result(type_::float(), type_::nil()),
            ),
        }
    }

    fn dependencies(&self) -> &'static [BuiltinFunctionExternal] {
        match self {
            BuiltinFunctionExternal::ParseInt | BuiltinFunctionExternal::ParseFloat => {
                &[BuiltinFunctionExternal::StringToMemory]
            }
            _ => &[],
        }
    }

    fn externals_dependencies(&self, int: IntType, float: FloatType) -> &'static [&'static str] {
        match self {
            BuiltinFunctionExternal::I32ToInt
            | BuiltinFunctionExternal::IntToI32
            | BuiltinFunctionExternal::StringRepr
            | BuiltinFunctionExternal::StringToMemory
            | BuiltinFunctionExternal::MemoryToString => &[],
            BuiltinFunctionExternal::IntRepr => match int {
                IntType::I32 => &[I32_TO_STR],
                IntType::I64 => &[I64_TO_STR],
            },
            BuiltinFunctionExternal::FloatRepr => match float {
                FloatType::F64 => &[F64_TO_STR],
            },
            BuiltinFunctionExternal::ParseInt => match int {
                IntType::I32 => &[PARSE_I32],
                IntType::I64 => &[PARSE_I64],
            },
            BuiltinFunctionExternal::ParseFloat => match float {
                FloatType::F64 => &[PARSE_F64],
            },
        }
    }
}

#[derive(Debug, Clone)]
enum CustomType {
    ExternalI32,
    Enum {
        values: Vec<EcoString>,
    },
    Struct {
        custom_type: TypedCustomType,
        constructor: TypedRecordConstructor,
    },
    Union {
        custom_type: TypedCustomType,
    },
}

impl CustomType {
    fn is_ref_non_null(&self) -> bool {
        if let CustomType::Struct { .. } | CustomType::Union { .. } = self {
            true
        } else {
            false
        }
    }
}

#[derive(Clone, Debug)]
struct Variant {
    custom_type: CustomType,
    constructor: TypedRecordConstructor,
    tag: Option<i32>,
}

struct Generator<'a> {
    // FIXME: generate all sections and names in function module?
    global_section: GlobalSection,
    import_section: ImportSection,
    export_section: ExportSection,
    data_section: DataSection,
    global_names: NameMap,
    wasm_types: HashMap<WasmType, u32>,
    types: HashMap<(EcoString, EcoString), CustomType>,
    variants: HashMap<EcoString, Variant>,
    functions: BTreeSet<WasmFunction>,
    function_next_id: u32,
    builtins: HashMap<BuiltinFunction, u32>,
    builtins_external: HashMap<BuiltinFunctionExternal, u32>,
    main: Option<u32>,
    // String literals and its index in the global section
    strings: HashMap<EcoString, u32>,
    consts: Vec<WasmConst>,
    globals: Rc<RefCell<Vec<Id>>>,
    int: IntType,
    float: FloatType,
    string: StringType,
    module: &'a TypedModule,
    line_numbers: &'a LineNumbers,
}

fn find_global(name: &str, globals: &RefCell<Vec<Id>>) -> Option<Id> {
    globals.borrow().iter().find(|id| &id.name == name).cloned()
}

impl<'a> Generator<'a> {
    fn new(module: &'a TypedModule, line_numbers: &'a LineNumbers) -> Self {
        Generator {
            global_section: GlobalSection::new(),
            import_section: ImportSection::new(),
            export_section: ExportSection::new(),
            data_section: DataSection::new(),
            global_names: NameMap::new(),
            wasm_types: HashMap::new(),
            types: HashMap::new(),
            variants: HashMap::new(),
            functions: BTreeSet::new(),
            function_next_id: 0,
            builtins: HashMap::new(),
            builtins_external: HashMap::new(),
            main: None,
            strings: HashMap::new(),
            consts: vec![],
            globals: Rc::default(),
            int: IntType::I32,
            float: FloatType::F64,
            string: StringType { type_index: 0 },
            module,
            line_numbers,
        }
    }

    fn generate(&mut self) -> u32 {
        if !self.module.definitions.imports.is_empty() {
            todo!("Imports\n{:#?}", self.module.definitions.imports);
        }

        // External types, like I32, must come first because of the external function type checking.
        // Externals functions come first because of the indexes in the builtin wasm file.
        self.types_external();
        let builtins = self.functions_external();
        self.types_prelude();
        self.functions_builtins(builtins);
        self.types();
        self.constants();
        self.functions();
        self.function_start()
    }

    fn functions_builtins(
        &mut self,
        builtins: Vec<(BuiltinFunctionExternal, Vec<(EcoString, Arc<Type>)>)>,
    ) {
        let i32 = type_::named("wasm", &self.module.name, "I32", Publicity::Private, vec![]);
        let builtins = builtins
            .into_iter()
            .map(|(builtin, used_as)| {
                let mut names = vec![];
                let (arguments, result) = builtin.type_(i32.clone());
                let expected = type_::fn_(arguments, result);
                for (name, type_) in used_as {
                    if !expected.same_as(&type_) {
                        panic!(
                            "{expected:#?}\n{type_:#?}\n{} != {}",
                            self.type_pretty_name(&expected),
                            self.type_pretty_name(&type_)
                        );
                    }
                    names.push(name);
                }
                (builtin, names)
            })
            .collect_vec();

        for (builtin, names) in builtins {
            let name = builtin.name();
            let index = self.get_function_builtin_external(builtin);
            for name in iter::once(name.into()).chain(names) {
                let _ = self.add_function_to_globals(name, index);
            }
        }
    }

    fn add_function_to_globals(&mut self, name: EcoString, index: u32) -> Id {
        let id = Id::func(name, index);
        self.globals.borrow_mut().push(id.clone());
        id
    }

    fn find_global(&self, name: &str) -> Option<Id> {
        find_global(name, &self.globals)
    }

    fn find_global_expect(&self, name: &str) -> Id {
        self.find_global(name).unwrap_or_else(|| {
            panic!(
                "Global \"{name}\".\n{}",
                std::backtrace::Backtrace::capture()
            )
        })
    }

    fn builtin_type(&self, function: &BuiltinFunction) -> (Vec<ValType>, Vec<ValType>) {
        match function {
            BuiltinFunction::StringConcat => (
                vec![self.string.val_type(), self.string.val_type()],
                vec![self.string.val_type()],
            ),
            BuiltinFunction::Equal(val_type, _) => (vec![*val_type, *val_type], vec![BOOL_VALTYPE]),
            BuiltinFunction::ListRepr(val_type, _) => {
                (vec![*val_type, ValType::I32], vec![ValType::I32])
            }
            BuiltinFunction::TupleRepr(val_type, _) => {
                (vec![*val_type, ValType::I32], vec![ValType::I32])
            }
            BuiltinFunction::FunctionRepr(val_type, _) => {
                (vec![*val_type, ValType::I32], vec![ValType::I32])
            }
            BuiltinFunction::CustomTypeRepr(val_type, _, _) => {
                (vec![*val_type, ValType::I32], vec![ValType::I32])
            }
            BuiltinFunction::VariantConstructor(params, return_, _) => {
                (params.clone(), vec![*return_])
            }
        }
    }

    fn is_external_type(&self, type_: &Arc<Type>) -> bool {
        if let Some((module, name, args)) = type_.named_type_information()
            && args.is_empty()
            && self.types.contains_key(&(module, name))
        {
            true
        } else {
            false
        }
    }

    fn type_pretty_name(&self, type_: &Arc<Type>) -> EcoString {
        type_pretty_name(&self.module.names, type_)
    }

    fn mangle(&self, name: &EcoString, type_: &Arc<Type>) -> EcoString {
        if type_.fn_types().is_some() {
            name.clone() + &self.type_pretty_name(type_)[2..]
        } else {
            name.clone()
                + "::"
                + self
                    .type_pretty_name(type_)
                    .as_str()
                    .replace(' ', "")
                    .as_str()
        }
    }

    fn types_external(&mut self) {
        for custom_type in &self.module.definitions.custom_types {
            let type_ = if let Some((module, name, _)) = &custom_type.external_webassembly {
                assert!(custom_type.constructors.is_empty());
                assert_eq!(module, "builtins");
                assert_eq!(name, "I32");
                CustomType::ExternalI32
            } else if custom_type.name == "I32" {
                CustomType::ExternalI32
            } else {
                continue;
            };
            let _ = self
                .types
                .insert((self.module.name.clone(), custom_type.name.clone()), type_);
        }
    }

    fn functions_external(
        &mut self,
    ) -> Vec<(BuiltinFunctionExternal, Vec<(EcoString, Arc<Type>)>)> {
        // FIXME: move this code to Externals
        let externals = Externals::new(self);

        let module = prepare_wasm_module(
            BUILTINS_WASM,
            externals.externals.iter().flat_map(|(name, external)| {
                if !external.used_names.is_empty() {
                    Some(name)
                } else {
                    None
                }
            }),
        );

        let mut types = vec![];
        let mut functions_types = vec![];
        let mut functions = vec![];

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
                            let _ = self.function_type_index_with_val_types(
                                params.clone(),
                                results.clone(),
                            );
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
                        self.function_next_id += 1;
                    }
                }
                wasmparser::Payload::FunctionSection(section) => {
                    for function in section.into_iter_with_offsets() {
                        let (_, type_index) = function.expect("Function entry");
                        functions_types.push(types.get(type_index as usize).expect("Type").clone());
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
                    let (params, results) = functions_types
                        .get(functions.len())
                        .expect("Function type")
                        .clone();
                    functions.push((params, results, section.as_bytes().to_vec()));
                }
                wasmparser::Payload::DataSection(section) => {
                    if !externals.use_data_section() {
                        continue;
                    }
                    for entry in section.into_iter_with_offsets() {
                        let (_, entry) = entry.expect("Data entry");
                        match &entry.kind {
                            wasmparser::DataKind::Passive => {
                                let _ = self.data_section.passive(entry.data.iter().cloned());
                            }
                            wasmparser::DataKind::Active {
                                memory_index,
                                offset_expr,
                            } => {
                                let _ = self.data_section.active(
                                    *memory_index,
                                    &const_expr_i32_const(offset_expr),
                                    entry.data.iter().cloned(),
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
                                        if let Some(external) = externals.externals.get(name.name) {
                                            let index =
                                                (name.index - self.import_section.len()) as usize;
                                            let (params, results, code) =
                                                functions.get(index).unwrap();
                                            let _ = self.add_function(
                                                name.name.into(),
                                                false,
                                                params.clone(),
                                                results.clone(),
                                                code.clone(),
                                            );
                                            for used_name in &external.used_names {
                                                let _ = self.add_function_to_globals(
                                                    used_name.clone(),
                                                    name.index,
                                                );
                                            }
                                        }
                                    }
                                }
                                wasmparser::Name::Global(section_limited) => {
                                    for item in section_limited.into_iter_with_offsets() {
                                        let (_, name) = item.expect("Name entry");
                                        self.global_names.append(name.index, name.name);
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

        let mut builtins: Vec<(BuiltinFunctionExternal, Vec<(EcoString, Arc<Type>)>)> = vec![];
        for (builtin, use_) in externals.builtins {
            if let Some(use_) = use_ {
                if let Some((_, entry)) = builtins.iter_mut().find(|(b, _)| *b == builtin) {
                    entry.push(use_);
                } else {
                    builtins.push((builtin, vec![use_]));
                }
            } else {
                builtins.push((builtin, vec![]));
            }
        }

        builtins.sort_by_key(|(builtin, _)| builtin.clone());
        builtins
    }

    fn types(&mut self) {
        fn is_not_external(custom_type: &TypedCustomType) -> bool {
            custom_type.external_webassembly.is_none() && !custom_type.constructors.is_empty()
        }

        self.add_custom_types(
            self.module
                .definitions
                .custom_types
                .iter()
                .filter(|t| is_not_external(t))
                .map(|c| (c, self.module.name.as_str())),
        )
    }

    fn types_prelude(&mut self) {
        // Add Nil, Bool and Result
        self.add_custom_types(
            Self::prelude_custom_types()
                .iter()
                .map(|c| (c, PRELUDE_MODULE_NAME)),
        );

        // Add String
        let index = self.wasm_types.len() as u32;
        self.string.type_index = *self
            .wasm_types
            .entry(StringType::wasm_type())
            .or_insert(index);

        // TODO: Add List to avois special handling
    }

    fn add_custom_types<'b>(
        &mut self,
        types: impl IntoIterator<Item = (&'b TypedCustomType, &'b str)>,
    ) {
        for (custom_type, module_name) in types {
            let mut is_union = false;
            let type_ = if custom_type
                .constructors
                .iter()
                .all(|constructor| constructor.arguments.is_empty())
            {
                for (value, constructor) in custom_type.constructors.iter().enumerate() {
                    let _ = self.add_const(
                        &constructor.name,
                        ValType::I32,
                        ConstExpr::i32_const(value as i32),
                        custom_type.publicity.is_public(),
                        false,
                    );
                }
                CustomType::Enum {
                    values: custom_type
                        .constructors
                        .iter()
                        .map(|c| c.name.clone())
                        .collect(),
                }
            } else if let Some((constructor, [])) = custom_type.constructors.split_first() {
                CustomType::Struct {
                    custom_type: custom_type.clone(),
                    constructor: constructor.clone(),
                }
            } else {
                is_union = true;
                CustomType::Union {
                    custom_type: custom_type.clone(),
                }
            };

            for (tag, constructor) in custom_type.constructors.iter().enumerate() {
                let _ = self.variants.insert(
                    constructor.name.clone(),
                    Variant {
                        custom_type: type_.clone(),
                        constructor: constructor.clone(),
                        tag: if is_union { Some(tag as i32) } else { None },
                    },
                );
            }

            let _ = self
                .types
                .insert((module_name.into(), custom_type.name.clone()), type_);
        }
    }

    fn prelude_custom_types() -> Vec<TypedCustomType> {
        fn type_var_generic(id: u64) -> Arc<Type> {
            Type::Var {
                type_: RefCell::new(TypeVar::Generic { id }).into(),
            }
            .into()
        }

        fn record_constructor_arg(ast: TypeAst, type_: Arc<Type>) -> TypedRecordConstructorArg {
            TypedRecordConstructorArg {
                label: Default::default(),
                ast,
                location: Default::default(),
                type_,
                doc: Default::default(),
            }
        }

        fn record_constructor(
            name: &str,
            arguments: Vec<TypedRecordConstructorArg>,
        ) -> TypedRecordConstructor {
            TypedRecordConstructor {
                location: Default::default(),
                name_location: Default::default(),
                name: name.into(),
                arguments,
                documentation: Default::default(),
                deprecation: Default::default(),
            }
        }

        fn ast_var(name: &str) -> TypeAst {
            TypeAst::Var(TypeAstVar {
                location: Default::default(),
                name: name.into(),
            })
        }

        fn custom_type(
            name: &str,
            constructors: Vec<TypedRecordConstructor>,
            parameters: Vec<(SrcSpan, EcoString)>,
            typed_parameters: Vec<Arc<Type>>,
        ) -> TypedCustomType {
            TypedCustomType {
                location: Default::default(),
                end_position: Default::default(),
                name: name.into(),
                name_location: Default::default(),
                publicity: Publicity::Public,
                constructors,
                documentation: Default::default(),
                deprecation: Default::default(),
                opaque: Default::default(),
                parameters,
                typed_parameters,
                external_erlang: Default::default(),
                external_javascript: Default::default(),
                external_webassembly: Default::default(),
            }
        }

        let nil = custom_type(
            "Nil",
            vec![record_constructor("Nil", vec![])],
            vec![],
            vec![],
        );

        let bool_ = custom_type(
            "Bool",
            vec![
                record_constructor("False", vec![]),
                record_constructor("True", vec![]),
            ],
            vec![],
            vec![],
        );

        let type_ok = type_var_generic(u64::MAX - 1);
        let type_err = type_var_generic(u64::MAX);
        let result = custom_type(
            "Result",
            vec![
                record_constructor(
                    "Ok",
                    vec![record_constructor_arg(ast_var("ok"), type_ok.clone())],
                ),
                record_constructor(
                    "Error",
                    vec![record_constructor_arg(ast_var("err"), type_err.clone())],
                ),
            ],
            vec![
                (Default::default(), "ok".into()),
                (Default::default(), "err".into()),
            ],
            vec![type_ok, type_err],
        );

        vec![nil, bool_, result]
    }

    fn custom_type(&self, type_: &Arc<Type>) -> Option<(CustomType, Vec<Arc<Type>>)> {
        if let Some((module, name, args)) = type_.named_type_information()
            && let Some(custom_type) = self.types.get(&(module, name.clone()))
        {
            Some((custom_type.clone(), args))
        } else {
            None
        }
    }

    fn constants(&mut self) {
        for module_constant in &self.module.definitions.constants {
            let _ = self.module_constant(module_constant);
        }
    }

    fn functions(&mut self) {
        for function in &self.module.definitions.functions {
            if function.publicity.is_public()
                && !is_generic_type(&function_type(function))
                && function.external_webassembly.is_none()
            {
                let id = self.function(function, true);
                if is_main_funtion(function) {
                    self.main = Some(id.index)
                }
            } else {
                // FIXME: show message explaning why somo function was not compiled?
            }
        }
    }

    fn val_type(&mut self, type_: &Arc<Type>) -> ValType {
        if type_.is_int() {
            self.int.val_type()
        } else if type_.is_float() {
            self.float.val_type()
        } else if type_.is_string() {
            self.string.val_type()
        } else if let Some(item_type) = type_.list_type() {
            let type_index = self.list_type_index(&item_type);
            self.list_val_type(type_index)
        } else if let Some(types) = type_.tuple_types() {
            let type_index = self.tuple_type_index(types);
            self.composite_val_type(type_index)
        } else if let Some((params, return_)) = type_.fn_types() {
            let type_index = self.function_type_index(params, Some(return_));
            self.function_val_type(type_index)
        } else if let Some((custom_type, args)) = self.custom_type(type_) {
            self.custom_type_val_type(type_, &custom_type, args)
        } else if let Some((_, name)) = type_.named_type_name()
            && name == "I32"
        {
            ValType::I32
        } else {
            todo!(
                "Type not supported: {:#?}\n{}",
                type_,
                std::backtrace::Backtrace::capture()
            );
        }
    }

    fn val_types(&mut self, types: impl IntoIterator<Item = Arc<Type>>) -> Vec<ValType> {
        types
            .into_iter()
            .map(|type_| self.val_type(&type_))
            .collect()
    }

    fn val_type_ref(&self, type_index: u32) -> ValType {
        // FIXME: set nullable to false
        // The way we perform pattern matching can leave local variables of
        // Wasm structure types uninitialized, causing the generated code to
        // fail validation. If the structures can be null, they are initialized
        // by default with null and the code validates.
        RefType {
            heap_type: HeapType::Concrete(type_index),
            nullable: true,
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

    fn function_type_index(
        &mut self,
        arguments: impl IntoIterator<Item = Arc<Type>>,
        return_: Option<Arc<Type>>,
    ) -> u32 {
        let params = self.val_types(arguments);
        let results = self.val_types(return_);
        self.function_type_index_with_val_types(params, results)
    }

    fn function_type_index_with_val_types(
        &mut self,
        params: Vec<ValType>,
        result: Vec<ValType>,
    ) -> u32 {
        let index = self.wasm_types.len() as u32;
        *self
            .wasm_types
            .entry(WasmType::function(params, result))
            .or_insert(index)
    }

    fn function_val_type(&self, type_index: u32) -> ValType {
        self.val_type_ref(type_index)
    }

    fn list_type_index(&mut self, item_type: &Arc<Type>) -> u32 {
        let item_val_type = self.val_type(item_type);
        let index = self.wasm_types.len() as u32;
        *self
            .wasm_types
            .entry(WasmType::list(
                type_pretty_name(&self.module.names, &type_::list(item_type.clone())),
                item_val_type,
            ))
            .or_insert(index)
    }

    fn list_val_type(&self, type_index: u32) -> ValType {
        self.val_type_ref_nullable(type_index)
    }

    fn tuple_type_index(&mut self, types: impl IntoIterator<Item = Arc<Type>>) -> u32 {
        let types = types.into_iter().collect_vec();
        let val_types = self.val_types(types.iter().cloned());
        let index = self.wasm_types.len() as u32;
        let name = self
            .type_pretty_name(&type_::tuple(types))
            .replace("#", "Tuple");
        *self
            .wasm_types
            .entry(WasmType::struct_(
                name,
                val_types
                    .into_iter()
                    .enumerate()
                    .map(|(index, v)| (index.to_string().into(), v))
                    .collect(),
            ))
            .or_insert(index)
    }

    fn struct_type_index(&mut self, name: EcoString, fields: Vec<(EcoString, ValType)>) -> u32 {
        let index = self.wasm_types.len() as u32;
        *self
            .wasm_types
            .entry(WasmType::struct_(name, fields))
            .or_insert(index)
    }

    fn fields(
        &mut self,
        constructor: &TypedRecordConstructor,
        types: &[Arc<Type>],
    ) -> Vec<(EcoString, ValType)> {
        constructor
            .arguments
            .iter()
            .enumerate()
            .map(|(index, c)| {
                if let Some((_, label)) = &c.label {
                    label.clone()
                } else {
                    index.to_string().into()
                }
            })
            .zip(self.val_types(types.iter().cloned()))
            .collect()
    }

    fn mono_struct_type_index(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &TypedCustomType,
        constructor: &TypedRecordConstructor,
        args: &[Arc<Type>],
    ) -> (u32, Vec<Arc<Type>>) {
        let types = Monomorphizer::variant_constructor(custom_type, constructor, args);
        let fields = self.fields(constructor, &types);
        let name = self.type_pretty_name(type_);
        (self.struct_type_index(name, fields), types)
    }

    fn mono_union_supertype_index(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &TypedCustomType,
    ) -> u32 {
        self.mono_union_type_index(type_, custom_type, None, None, &[])
            .0
    }

    fn mono_union_subtype_index(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &TypedCustomType,
        constructor: &TypedRecordConstructor,
        args: &[Arc<Type>],
    ) -> (u32, u32, Vec<Arc<Type>>) {
        let supertype_index = self.mono_union_supertype_index(type_, custom_type);
        let (type_index, types) = self.mono_union_type_index(
            type_,
            custom_type,
            Some(constructor),
            Some(supertype_index),
            args,
        );
        (supertype_index, type_index, types)
    }

    fn mono_union_type_index(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &TypedCustomType,
        constructor: Option<&TypedRecordConstructor>,
        supertype_index: Option<u32>,
        args: &[Arc<Type>],
    ) -> (u32, Vec<Arc<Type>>) {
        let index = self.wasm_types.len() as u32;
        let mut name = self.type_pretty_name(type_);
        let (fields, types) = if let Some(constructor) = constructor {
            name += ".";
            name += constructor.name.clone();
            let types = Monomorphizer::variant_constructor(custom_type, constructor, args);
            (self.fields(constructor, &types), types)
        } else {
            (vec![], vec![])
        };

        (
            *self
                .wasm_types
                .entry(WasmType::union(name, fields, supertype_index))
                .or_insert(index),
            types,
        )
    }

    fn composite_val_type(&self, type_index: u32) -> ValType {
        self.val_type_ref(type_index)
    }

    fn custom_type_val_type(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &CustomType,
        args: Vec<Arc<Type>>,
    ) -> ValType {
        match custom_type {
            CustomType::ExternalI32 => ValType::I32,
            CustomType::Enum { .. } => ValType::I32,
            CustomType::Struct {
                custom_type,
                constructor,
                ..
            } => {
                let (struct_index, _) =
                    self.mono_struct_type_index(type_, custom_type, constructor, &args);
                self.composite_val_type(struct_index)
            }
            CustomType::Union { custom_type } => {
                let struct_index = self.mono_union_supertype_index(type_, custom_type);
                self.composite_val_type(struct_index)
            }
        }
    }

    fn module_constant(&mut self, module_constant: &TypedModuleConstant) -> Id {
        self.register_string_const(&module_constant.value);

        let const_name = &module_constant.name;
        let export = module_constant.publicity.is_public();

        match &*module_constant.value {
            Constant::Int { int_value, .. } => self.add_const(
                const_name,
                self.int.val_type(),
                self.int.int_const(int_value),
                export,
                false,
            ),
            Constant::Float { value, .. } => self.add_const(
                const_name,
                self.float.val_type(),
                self.float.float_const(value),
                export,
                false,
            ),
            Constant::String { value, .. } => {
                let id = self.add_const(
                    const_name,
                    self.string.val_type_nullable(),
                    const_expr_ref_null(self.string.type_index),
                    export,
                    true,
                );
                let from = self.string_index(value);
                self.consts.push(WasmConst::String { from, to: id.index });
                id
            }
            Constant::Tuple { elements, .. } => {
                let type_index = self.tuple_type_index(elements.iter().map(|e| e.type_()));
                let val_type = self.val_type_ref_nullable(type_index);
                let id = self.add_const(
                    const_name,
                    val_type,
                    const_expr_ref_null(type_index),
                    export,
                    true,
                );
                self.consts.push(WasmConst::Struct {
                    global_index: id.index,
                    type_index,
                    elements: elements.clone(),
                });
                id
            }
            Constant::List {
                elements, type_, ..
            } => {
                let item_type = type_.list_type().unwrap();
                let type_index = self.list_type_index(&item_type);
                let val_type = self.list_val_type(type_index);
                let id = self.add_const(
                    const_name,
                    val_type,
                    const_expr_ref_null(type_index),
                    export,
                    true,
                );
                self.consts.push(WasmConst::List {
                    global_index: id.index,
                    type_index,
                    elements: elements.clone(),
                });
                id
            }
            Constant::Record {
                type_,
                arguments,
                name,
                ..
            } => {
                let (custom_type, args) = self.custom_type(type_).unwrap();
                match custom_type {
                    CustomType::Enum { values } => {
                        assert!(arguments.is_empty());
                        let value = values.iter().position(|value| value == name).unwrap();
                        self.add_const(
                            const_name,
                            self.int.val_type(),
                            self.int.int_const(&value.into()),
                            export,
                            false,
                        )
                    }
                    CustomType::Struct {
                        custom_type,
                        constructor,
                    } => {
                        let (type_index, _) =
                            self.mono_struct_type_index(type_, &custom_type, &constructor, &args);
                        let val_type = self.val_type_ref_nullable(type_index);
                        let id = self.add_const(
                            const_name,
                            val_type,
                            const_expr_ref_null(type_index),
                            export,
                            true,
                        );
                        self.consts.push(WasmConst::Struct {
                            global_index: id.index,
                            type_index,
                            elements: arguments.iter().map(|e| &e.value).cloned().collect(),
                        });
                        id
                    }
                    CustomType::Union { custom_type } => {
                        let index = type_.custom_type_inferred_variant().unwrap();
                        let constructor = custom_type.constructors.get(index as usize).unwrap();
                        let (_, type_index, _) =
                            self.mono_union_subtype_index(type_, &custom_type, constructor, &args);
                        let val_type = self.val_type_ref_nullable(type_index);
                        let id = self.add_const(
                            const_name,
                            val_type,
                            const_expr_ref_null(type_index),
                            export,
                            true,
                        );
                        self.consts.push(WasmConst::Struct {
                            global_index: id.index,
                            type_index,
                            elements: iter::once(TypedConstant::Int {
                                location: SrcSpan::new(0, 0),
                                value: "".into(),
                                int_value: index.into(),
                            })
                            .chain(arguments.iter().map(|e| &e.value).cloned())
                            .collect(),
                        });
                        id
                    }
                    CustomType::ExternalI32 => todo!(),
                }
            }
            Constant::Var { name, type_, .. } => {
                let (expr, val_type) = if type_.is_int() {
                    (self.int.int_const(&0.into()), self.int.val_type())
                } else if type_.is_float() {
                    (self.float.float_const(&"0".into()), self.float.val_type())
                } else if let Some((custom_type, args)) = self.custom_type(type_) {
                    match custom_type {
                        CustomType::ExternalI32 | CustomType::Enum { .. } => {
                            (self.int.int_const(&0.into()), self.int.val_type())
                        }
                        CustomType::Struct {
                            custom_type,
                            constructor,
                        } => {
                            let (type_index, _) = self.mono_struct_type_index(
                                type_,
                                &custom_type,
                                &constructor,
                                &args,
                            );
                            (
                                const_expr_ref_null(type_index),
                                self.val_type_ref_nullable(type_index),
                            )
                        }
                        CustomType::Union { .. } => todo!(),
                    }
                } else {
                    todo!()
                };
                let id = self.add_const(const_name, val_type, expr, export, true);
                self.consts.push(WasmConst::Var {
                    global_index: id.index,
                    name: name.clone(),
                });
                id
            }

            _ => panic!(),
        }
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
            Constant::Record { arguments, .. } => {
                for arg in arguments {
                    self.register_string_const(&arg.value)
                }
            }
            Constant::Var { .. } => {}
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
                let type_index = self.list_type_index(&item_type);
                let _ = instructions.list_null(type_index);
                for element in elements.iter().rev() {
                    let _ = instructions.constant(self, element).list_new(type_index);
                }
            }
            Constant::Tuple { elements, .. } => {
                let type_index = self.tuple_type_index(elements.iter().map(|e| e.type_()));
                let _ = instructions
                    .constants(self, elements)
                    .struct_new(type_index);
            }
            Constant::Record { name, type_, .. } if type_.is_bool() => {
                let _ = instructions.bool_const(name == "True");
            }
            _ => todo!("Constant not supported: {:#?}", const_),
        }
    }

    fn var(&mut self, name: &EcoString, required_type: &Arc<Type>) -> Id {
        if is_generic_type(required_type) {
            panic!("Required type is generic:\n{required_type:#?}");
        }

        for module_constant in &self.module.definitions.constants {
            if &module_constant.name == name {
                assert!(required_type.same_as(&module_constant.type_));
                return self.module_constant(module_constant);
            }
        }

        for function in &self.module.definitions.functions {
            if function_name(function) == name {
                let declared_type = function_type(function);
                return if is_generic_type(&declared_type) {
                    match self.find_global(&self.mangle(name, required_type)) {
                        Some(id) => id, // the function has already been monomorphized
                        None => {
                            let mut function = Monomorphizer::new(&declared_type, required_type)
                                .function(function);
                            let name = self.mangle(function_name(&function), required_type);
                            set_function_name(&mut function, name);

                            if is_generic_type(&function_type(&function))
                                || function
                                    .body
                                    .iter()
                                    .any(|statement| is_generic_type(&statement.type_()))
                            {
                                panic!(
                                    "Could not monomorphize:\n{required_type:#?}\n{function:#?}"
                                );
                            }
                            self.function(&function, function.publicity.is_public())
                        }
                    }
                } else {
                    if let Some((_, fname, _)) = &function.external_webassembly {
                        return self.find_global_expect(fname);
                    }
                    self.function(function, function.publicity.is_public())
                };
            }
        }

        todo!(
            "Name not found: {:?}. Are you using closures? They are not supporte yet.",
            name
        );
    }

    fn function_next_id(&mut self) -> u32 {
        let id = self.function_next_id;
        self.function_next_id += 1;
        id
    }

    fn function(&mut self, function: &TypedFunction, export: bool) -> Id {
        let name = function_name(function);

        if let Some(id) = self.find_global(name) {
            return id;
        }

        let index = self.function_next_id();
        let id = self.add_function_to_globals(name.clone(), index);
        let locals = Locals::new(self, &function.arguments, &function.body);
        let mut code = Function::new(locals.val_types());
        let mut instructions = code.extend_instructions(self);
        self.statements(
            &mut instructions,
            Scope::with_params(self.globals.clone(), &function.arguments),
            &locals,
            &function.body,
        );
        let _ = instructions.end();

        let type_index = self.function_type_index(
            function_params_types(function),
            Some(function.return_type.clone()),
        );
        let _ = self.functions.insert(WasmFunction {
            name: name.clone(),
            index,
            type_index,
            code: code.into_raw_body().into(),
            export,
            locals: locals.names(),
        });

        id
    }

    fn function_local(
        &mut self,
        name: EcoString,
        type_: &Arc<Type>,
        arguments: &[TypedArg],
        body: &[TypedStatement],
    ) -> Id {
        if let Some(id) = self.find_global(&name) {
            return id;
        }

        let index = self.function_next_id();
        let id = self.add_function_to_globals(name.clone(), index);
        let locals = Locals::new(self, arguments, body);
        let mut code = Function::new(locals.val_types());
        let mut instructions = code.extend_instructions(self);
        self.statements(
            &mut instructions,
            Scope::with_params(self.globals.clone(), arguments),
            &locals,
            body,
        );
        let _ = instructions.end();

        let (params, return_) = type_.fn_types().unwrap();
        let type_index = self.function_type_index(params, Some(return_));
        let _ = self.functions.insert(WasmFunction {
            name,
            index,
            type_index,
            code: code.into_raw_body().into(),
            export: false,
            locals: locals.names(),
        });

        id
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
                self.assert(instructions, &scope, locals, assert);
            }
            Statement::Use(use_) => {
                let _ = instructions.expression(self, locals, scope.clone(), &use_.call);
            }
        }
        scope
    }

    fn assert(
        &mut self,
        instructions: &mut ExtendedInstructionSink<'_>,
        scope: &Scope,
        locals: &Locals,
        assert: &crate::ast::Assert<TypedExpr>,
    ) {
        assert!(assert.value.type_().is_bool());
        let msg: EcoString = format!(
            "Assertion failed at src/{}.gleam:{}.\n",
            self.module.name,
            self.line_numbers.line_number(assert.location.start)
        )
        .into();
        let string_index = self.string_index(&msg);
        let string_to_memory =
            self.find_global_expect(BuiltinFunctionExternal::StringToMemory.name());
        let heap_base = self.find_global_expect(HEAP_BASE);
        let print = self.find_global_expect(PRINT);
        let exit = self.find_global_expect(EXIT);
        #[rustfmt::skip]
        let _ = instructions
            .expression(self, locals, scope.clone(), &assert.value)
            .if_(BlockType::Result(BOOL_VALTYPE))
              .bool_const(true)
            .else_()
              .show_error_message(string_index, string_to_memory, heap_base, print)
              .drop()
              .i32_const(1)
              .call(exit.index)
              .unreachable()
            .end();
    }

    fn _expression(
        &mut self,
        locals: &Locals,
        scope: Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        expression: &TypedExpr,
    ) {
        match expression {
            TypedExpr::Todo { message, .. } | TypedExpr::Panic { message, .. } => {
                self.expression_todo_panic(locals, scope, instructions, expression, message);
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
                let type_index = self.list_type_index(&item_type);
                if let Some(rest) = tail {
                    let _ = instructions.expression(self, locals, scope.clone(), rest);
                } else {
                    let _ = instructions.list_null(type_index);
                }
                for element in elements.iter().rev() {
                    let _ = instructions
                        .expression(self, locals, scope.clone(), element)
                        .list_new(type_index);
                }
            }
            TypedExpr::Tuple { elements, .. } => {
                let type_index = self.tuple_type_index(elements.iter().map(|e| e.type_()));
                let _ = instructions
                    .expressions(self, locals, scope.clone(), elements)
                    .struct_new(type_index);
            }
            TypedExpr::TupleIndex { index, tuple, .. } => {
                let type_index = self.tuple_type_index(tuple.type_().tuple_types().unwrap());
                let _ = instructions
                    .expression(self, locals, scope.clone(), tuple)
                    .struct_get(type_index, *index as u32);
            }
            TypedExpr::BinOp {
                name, left, right, ..
            } => {
                self.expression_bin_op(locals, scope, instructions, name, left, right);
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
                if fun.is_var() {
                    // the function can be evaluated after the parameters
                    let _ = instructions
                        .expressions(self, locals, scope.clone(), args)
                        .expression(self, locals, scope, fun)
                        .call_ref(self.function_type_index(args_types, Some(type_.clone())));
                } else {
                    let index = locals.for_call(fun);
                    let _ = instructions
                        .expression(self, locals, scope.clone(), fun)
                        .local_set(index)
                        .expressions(self, locals, scope, args)
                        .local_get(index)
                        .call_ref(self.function_type_index(args_types, Some(type_.clone())));
                }
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
                let id = self.function_local(name, type_, arguments, body);
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
            echo @ TypedExpr::Echo {
                expression,
                message,
                ..
            } => {
                self.expression_echo(locals, &scope, instructions, echo, expression, message);
            }
            TypedExpr::RecordAccess { index, record, .. } => {
                let (custom_type, args) = self.custom_type(&record.type_()).unwrap();
                match custom_type {
                    CustomType::Struct {
                        custom_type,
                        constructor,
                    } => {
                        let (type_index, _) = self.mono_struct_type_index(
                            &record.type_(),
                            &custom_type,
                            &constructor,
                            &args,
                        );
                        let _ = instructions
                            .expression(self, locals, scope, record)
                            .struct_get(type_index, *index as u32);
                    }
                    CustomType::Union { custom_type } => {
                        // FIXME: handle missing inferred variant (same name, position and type)
                        let variant = record.type_().custom_type_inferred_variant().unwrap();
                        let constructor = custom_type.constructors.get(variant as usize).unwrap();
                        let (_, type_index, _) = self.mono_union_subtype_index(
                            &record.type_(),
                            &custom_type,
                            constructor,
                            &args,
                        );
                        let _ = instructions
                            .expression(self, locals, scope, record)
                            .ref_cast_non_null(HeapType::Concrete(type_index))
                            .struct_get(type_index, *index as u32 + 1);
                    }
                    _ => panic!(),
                }
            }
            TypedExpr::RecordUpdate {
                type_,
                record_assignment,
                constructor,
                arguments,
                ..
            } => {
                let scope = if let Some(assignment) = record_assignment {
                    let scope = self.assignment(locals, scope, instructions, assignment);
                    let _ = instructions.drop();
                    scope
                } else {
                    scope
                };
                let (custom_type, _) = self.custom_type(type_).unwrap();
                match custom_type {
                    CustomType::Struct { .. } | CustomType::Union { .. } => {
                        let (params, return_) = constructor.type_().fn_types().unwrap();
                        let index = self.function_type_index(params, Some(return_));
                        let _ = instructions
                            .expressions(
                                self,
                                locals,
                                scope.clone(),
                                arguments.iter().map(|arg| &arg.value),
                            )
                            .expression(self, locals, scope, constructor)
                            .call_ref(index);
                    }
                    CustomType::ExternalI32 | CustomType::Enum { .. } => todo!(),
                }
            }
            _ => todo!("Expression not supported: {:#?}", expression),
        };
    }

    fn expression_todo_panic(
        &mut self,
        locals: &Locals,
        scope: Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        expression: &TypedExpr,
        message: &Option<Box<TypedExpr>>,
    ) {
        let msg: EcoString = format!(
            "{} at src/{}.gleam:{}{}",
            if expression.is_panic() {
                "panic"
            } else {
                "todo"
            },
            self.module.name,
            self.line_numbers.line_number(expression.location().start),
            if message.is_none() { ".\n" } else { "\n  " },
        )
        .into();

        let string_index = self.string_index(&msg);
        let string_to_memory =
            self.find_global_expect(&BuiltinFunctionExternal::StringToMemory.name());
        let heap_base = self.find_global_expect(HEAP_BASE);
        let print = self.find_global_expect(PRINT);
        let exit = self.find_global_expect(EXIT);

        let _ = instructions.show_error_message(
            string_index,
            string_to_memory.clone(),
            heap_base.clone(),
            print.clone(),
        );

        if let Some(message) = message {
            assert!(message.type_().is_string());
            let _ = instructions
                .i32_const(STDERR)
                .call(heap_base.index)
                .expression(self, locals, scope, message)
                .call(heap_base.index)
                .call(string_to_memory.index)
                .call(print.index)
                .call(heap_base.index)
                .byte_store(b'\n')
                .i32_const(STDERR)
                .call(heap_base.index)
                .i32_const(1)
                .call(print.index);
        }

        let _ = instructions
            .drop()
            .i32_const(1)
            .call(exit.index)
            .unreachable();
    }

    fn expression_bin_op(
        &mut self,
        locals: &Locals,
        scope: Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        name: &BinOp,
        left: &TypedExpr,
        right: &TypedExpr,
    ) {
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
                .if_(BlockType::Result(BOOL_VALTYPE))
                  .expression(self, locals, scope, right)
                .else_()
                  .bool_const(false)
                .end(),
            #[rustfmt::skip]
            BinOp::Or => instructions
                .expression(self, locals, scope.clone(), left)
                .if_(BlockType::Result(BOOL_VALTYPE))
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
            let _ = instructions.block(BlockType::Result(BOOL_VALTYPE));
            let mut scope = scope.clone();
            for patterns in iter::once(&clause.pattern).chain(&clause.alternative_patterns) {
                // block patterns
                let _ = instructions.block(BlockType::Result(BOOL_VALTYPE));
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
        let id = if let Some(id) = scope.find(name) {
            id
        } else if let Some(variant) = self.variants.get(name).cloned() {
            // Variant constructor
            let (params, return_) = if let Some((params, return_)) = type_.fn_types() {
                (params, return_)
            } else {
                (vec![], type_.clone())
            };

            let (_, _, args) = return_.named_type_information().unwrap();

            let mut name = self.type_pretty_name(&return_);
            if let CustomType::Union { .. } = &variant.custom_type {
                name += ".";
                name += variant.constructor.name.clone();
            }

            let builtin = BuiltinFunction::VariantConstructor(
                self.val_types(params.iter().cloned()),
                self.val_type(&return_),
                name.clone(),
            );

            let index = if let Some(index) = self.builtins.get(&builtin) {
                *index
            } else {
                let num_fields = params.len() as u32;
                let (type_index, tag) = match variant.custom_type {
                    CustomType::ExternalI32 | CustomType::Enum { .. } => panic!(),
                    CustomType::Struct {
                        custom_type,
                        constructor,
                        ..
                    } => (
                        self.mono_struct_type_index(&return_, &custom_type, &constructor, &args)
                            .0,
                        None,
                    ),
                    CustomType::Union { custom_type } => {
                        let (_, type_index, _) = self.mono_union_subtype_index(
                            &return_,
                            &custom_type,
                            &variant.constructor,
                            &args,
                        );
                        (type_index, variant.tag)
                    }
                };
                let function = self.code_variant_constructor(type_index, num_fields, tag);
                self.add_function_builtin(builtin, function).index
            };

            if params.is_empty() {
                // a variant with no args
                let _ = instructions.call(index);
                return;
            }

            Id::func(name.clone(), index)
        } else {
            self.var(name, type_)
        };

        let is_ref_non_null = |type_: &Arc<Type>| {
            type_.is_string() || type_.tuple_types().is_some() || {
                if let Some((custom_type, _)) = self.custom_type(type_) {
                    custom_type.is_ref_non_null()
                } else {
                    false
                }
            }
        };

        let _ = match id.kind {
            IdKind::Func => instructions.ref_func(id.index),
            IdKind::Global => {
                if is_ref_non_null(type_) {
                    instructions.global_as_non_null(id.index)
                } else {
                    instructions.global_get(id.index)
                }
            }
            IdKind::Local => instructions.local_get(id.index),
        };
    }

    fn expression_echo(
        &mut self,
        locals: &Locals,
        scope: &Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        echo: &TypedExpr,
        expression: &Option<Box<TypedExpr>>,
        message: &Option<Box<TypedExpr>>,
    ) {
        if let Some(expression) = expression {
            let print = self.find_global_expect(PRINT);
            let heap_base = self.find_global_expect(HEAP_BASE);
            let string_to_memory =
                self.find_global_expect(&BuiltinFunctionExternal::StringToMemory.name());
            let string_index = self.string_index(
                &format!(
                    "src/{}.gleam:{}{}",
                    self.module.name,
                    self.line_numbers.line_number(echo.location().start),
                    if message.is_some() { ' ' } else { '\n' }
                )
                .into(),
            );

            let (dest, expr) = locals.for_echo(echo, expression);

            let _ = instructions
                .expression(self, locals, scope.clone(), expression)
                .local_set(expr)
                // write the module name and line number
                .global_as_non_null(string_index)
                .call(heap_base.index)
                .local_tee(dest)
                .call(string_to_memory.index)
                // update end
                .local_get(dest)
                .i32_add()
                .local_set(dest);

            if let Some(message) = message {
                let _ = instructions
                    .expression(self, locals, scope.clone(), message)
                    .local_get(dest)
                    .call(self.function_repr(&message.type_()))
                    // update end
                    .local_get(dest)
                    .i32_add()
                    .local_tee(dest)
                    // add new line
                    .byte_store(b'\n')
                    .i32_inc(dest);
            }

            let _ = instructions
                .local_get(expr)
                .local_get(dest)
                .call(self.function_repr(&expression.type_()))
                // update end
                .local_get(dest)
                .i32_add()
                .local_tee(dest)
                // add new line
                .byte_store(b'\n')
                .i32_inc(dest)
                // call print
                .i32_const(STDERR)
                .call(heap_base.index)
                .local_get(dest)
                .call(heap_base.index)
                .i32_sub()
                .call(print.index)
                .drop()
                // recover expression value
                .local_get(expr);
        }
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
                let msg: EcoString = format!(
                    "Pattern match failed, no pattern matched the value at src/{}.gleam:{}.\n",
                    self.module.name,
                    self.line_numbers.line_number(assignment.location.start)
                )
                .into();
                let string_index = self.string_index(&msg);
                let string_to_memory =
                    self.find_global_expect(&BuiltinFunctionExternal::StringToMemory.name());
                let heap_base = self.find_global_expect(HEAP_BASE);
                let print = self.find_global_expect(PRINT);
                let exit = self.find_global_expect(EXIT);
                #[rustfmt::skip]
                let _ = instructions
                    .pattern(self, locals, &mut scope, &assignment.pattern)
                    .if_(BlockType::Result(self.val_type(&assignment.value.type_())))
                      .local_get(right)
                    .else_()
                      .show_error_message(string_index, string_to_memory, heap_base, print)
                      .drop()
                      .i32_const(1)
                      .call(exit.index)
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
                let index = self.string_index(value);
                let eq = self.function_eq(&type_::string());
                let _ = instructions.global_as_non_null(index).eq(eq);
            }
            Pattern::List {
                elements,
                tail,
                type_,
                ..
            } => {
                let item_type = type_.list_type().unwrap();
                let type_index = self.list_type_index(&item_type);
                let right = locals.for_pattern(pattern);
                let _ = instructions
                    .local_set(right)
                    .block(BlockType::Result(BOOL_VALTYPE));
                if !elements.is_empty() {
                    #[rustfmt::skip]
                    let _ = instructions
                        .local_get(right)
                        .ref_is_null()
                        .if_(BlockType::Empty)
                          .bool_const(false)
                          .br(1)
                        .end();
                }
                for element in elements {
                    #[rustfmt::skip]
                    let _ = instructions
                        .local_get(right)
                        .list_first(type_index)
                        .pattern(self, locals, &mut scope, element)
                        .bool_not()
                        .if_(BlockType::Empty)
                          .bool_const(false)
                          .br(1)
                        .end()
                        .local_get(right)
                        .list_rest(type_index)
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
                let type_index = self.tuple_type_index(elements.iter().map(|e| e.type_()));
                let _ = instructions.patterns(
                    self,
                    locals,
                    &mut scope,
                    (type_index, None),
                    pattern,
                    elements.iter(),
                );
            }
            Pattern::Constructor {
                name,
                type_,
                arguments,
                ..
            } => {
                let (custom_type, args) = self.custom_type(type_).unwrap();
                match custom_type {
                    CustomType::Enum { values } => {
                        let value = values.iter().position(|n| n == name).unwrap();
                        let _ = instructions.i32_const(value as i32).i32_eq();
                    }
                    CustomType::Struct {
                        custom_type,
                        constructor,
                    } => {
                        let custom_type = custom_type.clone();
                        let constructor = constructor.clone();
                        let (type_index, _) =
                            self.mono_struct_type_index(type_, &custom_type, &constructor, &args);
                        assert_eq!(constructor.arguments.len(), arguments.len());
                        let _ = instructions.patterns(
                            self,
                            locals,
                            &mut scope,
                            (type_index, None),
                            pattern,
                            arguments.iter().map(|arg| &arg.value),
                        );
                    }
                    CustomType::Union { custom_type } => {
                        let custom_type = custom_type.clone();
                        let (tag, constructor) = custom_type
                            .constructors
                            .iter()
                            .enumerate()
                            .find(|(_, c)| &c.name == name)
                            .unwrap();
                        let (supertype_index, type_index, _) =
                            self.mono_union_subtype_index(type_, &custom_type, constructor, &args);
                        let right = locals.for_pattern(pattern);
                        #[rustfmt::skip]
                        let _ = instructions
                            .local_tee(right)
                            .struct_get(supertype_index, 0)
                            .i32_const(tag as i32)
                            .i32_eq()
                            .if_(BlockType::Result(BOOL_VALTYPE))
                              .local_get(right)
                              .patterns(
                                self,
                                locals,
                                &mut scope,
                                (supertype_index, Some(type_index)),
                                pattern,
                                arguments.iter().map(|arg| &arg.value),
                              )
                            .else_()
                              .bool_const(false)
                            .end();
                    }
                    CustomType::ExternalI32 => panic!(),
                };
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

    fn _patterns<'b>(
        &mut self,
        locals: &Locals,
        scope: &mut Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        (type_index, subtype_index): (u32, Option<u32>),
        pattern: &Pattern<Arc<Type>>,
        elements: impl IntoIterator<Item = &'b Pattern<Arc<Type>>> + Clone,
    ) {
        let right = locals.for_pattern(pattern);
        let _ = instructions
            .local_set(right)
            .block(BlockType::Result(BOOL_VALTYPE));
        for (field_index, element) in elements.into_iter().enumerate() {
            let _ = instructions.local_get(right);
            if let Some(subtype_index) = subtype_index {
                // cast and skip tag
                let _ = instructions
                    .ref_cast_non_null(HeapType::Concrete(subtype_index))
                    .struct_get(subtype_index, field_index as u32 + 1);
            } else {
                let _ = instructions.struct_get(type_index, field_index as u32);
            }
            #[rustfmt::skip]
            let _ = instructions
                .pattern(self, locals, scope, element)
                .bool_not()
                .if_(BlockType::Empty)
                  .bool_const(false)
                  .br(1)
                .end();
        }
        let _ = instructions.bool_const(true).end();
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
                    .if_(BlockType::Result(BOOL_VALTYPE))
                      .bool_const(true)
                    .else_()
                      .clause_guard(self, locals, scope, right)
                    .end();
            }
            ClauseGuard::And { left, right, .. } => {
                #[rustfmt::skip]
                let _ = instructions
                    .clause_guard(self, locals, scope, left)
                    .if_(BlockType::Result(BOOL_VALTYPE))
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
            ClauseGuard::Equals { left, right, .. } => {
                let eq = self.function_eq(&left.type_());
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .eq(eq);
            }
            ClauseGuard::NotEquals { left, right, .. } => {
                let eq = self.function_eq(&left.type_());
                let _ = instructions
                    .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                    .eq(eq)
                    .bool_not();
            }
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
                let type_index = self.tuple_type_index(tuple.type_().tuple_types().unwrap());
                let _ = instructions
                    .clause_guard(self, locals, scope, tuple)
                    .struct_get(type_index, *index as u32);
            }
            ClauseGuard::FieldAccess {
                index, container, ..
            } => {
                let type_ = container.type_();
                let index = index.expect("FieldAccess index") as u32;
                let (custom_type, args) = self.custom_type(&type_).unwrap();
                match custom_type {
                    CustomType::ExternalI32 | CustomType::Enum { .. } => todo!(),
                    CustomType::Struct {
                        custom_type,
                        constructor,
                    } => {
                        let (type_index, _) =
                            self.mono_struct_type_index(&type_, &custom_type, &constructor, &args);
                        let _ = instructions
                            .clause_guard(self, locals, scope, container)
                            .struct_get(type_index, index);
                    }
                    CustomType::Union { custom_type } => {
                        let constructor =
                            custom_type_inferred_constructor(&custom_type, &type_).unwrap();
                        let (_, type_index, _) =
                            self.mono_union_subtype_index(&type_, &custom_type, constructor, &args);
                        let _ = instructions
                            .clause_guard(self, locals, scope, container)
                            .ref_cast_non_null(HeapType::Concrete(type_index))
                            .struct_get(type_index, index + 1);
                    }
                }
            }
            ClauseGuard::ModuleSelect { .. } => todo!("Guard\n{guard:#?}"),
        }
    }

    fn add_const(
        &mut self,
        name: &EcoString,
        val_type: ValType,
        expr: ConstExpr,
        export: bool,
        mutable: bool,
    ) -> Id {
        let index = self.global_section.len();
        let _ = self.global_section.global(
            GlobalType {
                val_type,
                mutable,
                shared: false,
            },
            &expr,
        );
        self.global_names.append(index, name);
        if export {
            let _ = self.export_section.export(name, ExportKind::Global, index);
        }
        let id = Id::global(name.clone(), index);
        self.globals.borrow_mut().push(id.clone());
        id
    }

    fn function_start(&mut self) -> u32 {
        let function = self.code_start();
        self.add_function(
            "$start".into(),
            true,
            vec![],
            vec![],
            function.into_raw_body(),
        )
        .index
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
                WasmConst::String { from, to } => {
                    let _ = instructions.global_get(from).global_set(to);
                }
                WasmConst::List {
                    global_index,
                    type_index,
                    elements,
                } => {
                    let _ = instructions.global_get(global_index);
                    for element in elements.iter().rev() {
                        let _ = instructions.constant(self, element).list_new(type_index);
                    }
                    let _ = instructions.global_set(global_index);
                }
                WasmConst::Struct {
                    global_index,
                    type_index,
                    elements,
                } => {
                    let _ = instructions
                        .constants(self, &elements)
                        .struct_new(type_index)
                        .global_set(global_index);
                }
                WasmConst::Var { global_index, name } => {
                    let _ = instructions
                        .global_get(self.find_global_expect(&name).index)
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
        let string = unescape(string);
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

    fn function_string_concat(&mut self) -> u32 {
        if let Some(index) = self.builtins.get(&BuiltinFunction::StringConcat) {
            return *index;
        }
        let function = self.code_string_concat();
        self.add_function_builtin(BuiltinFunction::StringConcat, function)
            .index
    }

    fn code_string_concat(&self) -> Function {
        let mut function = Function::new(vec![(3, ValType::I32), (1, self.string.val_type())]);
        let mut instructions = function.extend_instructions(self);
        // params
        let a = 0; // String
        let b = 1; // String
        // locals
        let len_a = 2; // I32
        let len_b = 3; // I32
        let i = 4; // I32
        // return
        let r = 5; // String
        #[rustfmt::skip]
        let _ = instructions
            // len_a = a.len; push len_a
            .local_get(a)
            .string_len()
            .local_tee(len_a)
            // len_b = b.len; push len_b
            .local_get(b)
            .string_len()
            .local_tee(len_b)
            // r = array.new_default(len_a + len_b)
            .i32_add()
            .string_new()
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
                .string_get()
                .string_set()
                .i32_inc(i)
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
                .string_get()
                .string_set()
                .i32_inc(i)
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
            return Eq::Int;
        } else if type_.is_float() {
            return Eq::Float;
        } else if type_.is_bool() {
            return Eq::Bool;
        } else if let Some((CustomType::ExternalI32 | CustomType::Enum { .. }, _)) =
            self.custom_type(type_)
        {
            return Eq::I32;
        }

        let eq = BuiltinFunction::Equal(self.val_type(type_), self.type_pretty_name(type_));

        if let Some(index) = self.builtins.get(&eq) {
            return Eq::Call(*index);
        }

        // We register an empty function to get the builtin index,
        // so we avoid problems with recursive types.
        let mut function = self.add_function_builtin(eq.clone(), Function::new(vec![]));

        let code = if type_.is_string() {
            self.code_string_eq()
        } else if let Some(item_type) = type_.list_type() {
            let type_index = self.list_type_index(&item_type);
            let item_eq = self.function_eq(&item_type);
            self.code_list_eq(type_index, item_eq)
        } else if let Some(types) = type_.tuple_types() {
            let type_index = self.tuple_type_index(types.clone());
            self.code_composite_eq(type_index, types)
        } else if let Some((custom_type, args)) = self.custom_type(type_) {
            match custom_type {
                CustomType::Struct {
                    custom_type,
                    constructor,
                } => {
                    let (type_index, types) =
                        self.mono_struct_type_index(type_, &custom_type, &constructor, &args);
                    self.code_composite_eq(type_index, types)
                }
                CustomType::Union { custom_type } => {
                    let supertype_index = self.mono_union_supertype_index(type_, &custom_type);
                    self.code_union_eq(type_, supertype_index, &custom_type, &args)
                }
                _ => panic!(),
            }
        } else {
            panic!()
        };

        // Now we update the function
        function.code = code.into_raw_body().into();
        let eq = Eq::Call(function.index);
        assert!(self.functions.replace(function).is_some());
        eq
    }

    fn code_string_eq(&self) -> Function {
        let mut function = Function::new(vec![(2, ValType::I32)]);
        let mut instructions = function.extend_instructions(self);
        // params
        let a = 0; // String
        let b = 1; // String
        // locals
        let i = 2; // I32
        let len = 3; // I32
        // return Bool
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
            .string_len()
            .local_tee(len)
            // b.len
            .local_get(b)
            .string_len()
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
              .string_get()
              // b[i]
              .local_get(b)
              .local_get(i)
              .string_get()
              .i32_ne()
              // if a[i] != b[i]
              .if_(BlockType::Empty)
                .bool_const(false)
                .return_()
              .end()
              .i32_inc(i)
              // loop
              .br(0)
            // end loop
            .end()
            .bool_const(true)
            // end function
            .end();
        function
    }

    fn code_list_eq(&self, type_index: u32, item_eq: Eq) -> Function {
        let mut function = Function::new(vec![]);
        let mut instructions = function.extend_instructions(self);
        // params
        let a = 0; // List(a)
        let b = 1; // List(a)
        // return Bool
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
                  .list_first(type_index)
                  // b.value
                  .local_get(b)
                  .list_first(type_index)
                  .eq(item_eq)
                  // if a.value == b.value
                  .if_(BlockType::Empty)
                    // a = a.rest
                    .local_get(a)
                    .list_rest(type_index)
                    .local_set(a)
                    // b = b.rest
                    .local_get(b)
                    .list_rest(type_index)
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

    fn code_composite_eq(
        &mut self,
        type_index: u32,
        types: impl IntoIterator<Item = Arc<Type>>,
    ) -> Function {
        let mut function = Function::new(vec![]);
        let mut instructions = function.extend_instructions(self);
        // params
        let a = 0; // composite
        let b = 1; // composite
        // return Bool
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

    fn code_union_eq(
        &mut self,
        type_: &Arc<Type>,
        supertype_index: u32,
        custom_type: &TypedCustomType,
        args: &[Arc<Type>],
    ) -> Function {
        let mut function = Function::new(vec![(1, ValType::I32)]);
        let mut instructions = function.extend_instructions(self);
        // params
        let a = 0; // union
        let b = 1; // union
        // locals
        let tag = 2;
        // return Bool
        #[rustfmt::skip]
        let _ = instructions
            .local_get(a)
            .local_get(b)
            .ref_eq()
            .if_(BlockType::Empty)
              .bool_const(true)
              .return_()
            .end()
            .local_get(a)
            .struct_get(supertype_index, 0)
            .local_tee(tag)
            .local_get(b)
            .struct_get(supertype_index, 0)
            .i32_ne()
            .if_(BlockType::Empty)
              .bool_const(false)
              .return_()
            .end()
            .block(BlockType::Empty);

        for (target_tag, constructor) in custom_type.constructors.iter().enumerate() {
            let (_, type_index, types) =
                self.mono_union_subtype_index(type_, custom_type, constructor, args);
            let name = self.type_pretty_name(type_) + "." + constructor.name.clone();
            let builtin = BuiltinFunction::Equal(self.val_type_ref(type_index), name);
            let index = if let Some(index) = self.builtins.get(&builtin) {
                *index
            } else {
                let code =
                    self.code_composite_eq(type_index, iter::once(type_::int()).chain(types));
                self.add_function_builtin(builtin, code).index
            };

            #[rustfmt::skip]
            let _ = instructions
                .local_get(tag)
                .i32_const(target_tag as i32)
                .i32_eq()
                .if_(BlockType::Empty)
                  .local_get(a)
                  .ref_cast_non_null(HeapType::Concrete(type_index))
                  .local_get(b)
                  .ref_cast_non_null(HeapType::Concrete(type_index))
                  .call(index)
                  .return_()
                .end();
        }
        let _ = instructions.end().unreachable().end();
        function
    }

    fn code_variant_constructor(
        &mut self,
        struct_index: u32,
        num_fields: u32,
        tag: Option<i32>,
    ) -> Function {
        let mut function = Function::new(vec![]);
        let mut instructions = function.extend_instructions(self);
        if let Some(tag) = tag {
            let _ = instructions.i32_const(tag);
        }
        for index in 0..num_fields {
            let _ = instructions.local_get(index);
        }
        let _ = instructions.struct_new(struct_index).end();
        function
    }

    fn code_i32_to_int(&self) -> Function {
        let mut function = Function::new(vec![]);
        let _ = function
            .extend_instructions(self)
            .local_get(0)
            .i32_to_int()
            .end();
        function
    }

    fn code_int_to_i32(&self) -> Function {
        let mut function = Function::new(vec![]);
        let _ = function
            .extend_instructions(self)
            .local_get(0)
            .int_to_i32()
            .end();
        function
    }

    fn function_repr(&mut self, type_: &Arc<Type>) -> u32 {
        if type_.is_int() {
            return self.get_function_builtin_external(BuiltinFunctionExternal::IntRepr);
        }
        if type_.is_float() {
            return self.get_function_builtin_external(BuiltinFunctionExternal::FloatRepr);
        }
        if type_.is_string() {
            return self.get_function_builtin_external(BuiltinFunctionExternal::StringRepr);
        }

        let repr = if type_.is_list() {
            BuiltinFunction::ListRepr(self.val_type(type_), self.type_pretty_name(type_))
        } else if type_.is_tuple() {
            BuiltinFunction::TupleRepr(
                self.val_type(type_),
                self.type_pretty_name(type_).replace("#", "Tuple"),
            )
        } else if type_.fn_types().is_some() {
            BuiltinFunction::FunctionRepr(self.val_type(type_), self.type_pretty_name(type_))
        } else if self.custom_type(type_).is_some() {
            BuiltinFunction::CustomTypeRepr(
                self.val_type(type_),
                self.type_pretty_name(type_),
                None,
            )
        } else {
            todo!("function_repr\n{type_:#?}");
        };

        if let Some(id) = self.builtins.get(&repr) {
            return *id;
        }

        // We register an empty function to get the builtin index,
        // so we avoid problems with recursive types.
        let mut function = self.add_function_builtin(repr.clone(), Function::new(vec![]));

        let code = if let Some(item_type) = type_.list_type() {
            self.code_list_repr(&item_type)
        } else if let Some(types) = type_.tuple_types() {
            self.code_tuple_repr(&types)
        } else if type_.fn_types().is_some() {
            self.code_function_repr(type_)
        } else if let Some((custom_type, args)) = self.custom_type(type_) {
            self.code_custom_type_repr(type_, &custom_type, &args)
        } else {
            todo!("function_repr\n{type_:#?}");
        };

        // Now we update the function
        function.code = code.into_raw_body().into();
        let index = function.index;
        assert!(self.functions.replace(function.clone()).is_some());
        index
    }

    fn code_int_repr(&self) -> Function {
        let mut function = Function::new(vec![]);
        let id = match self.int {
            IntType::I32 => self.find_global_expect(I32_TO_STR),
            IntType::I64 => self.find_global_expect(I64_TO_STR),
        };
        let _ = function
            .instructions()
            .local_get(0)
            .local_get(1)
            .call(id.index)
            .end();
        function
    }

    fn code_float_repr(&self) -> Function {
        let mut function = Function::new(vec![]);
        let id = match self.float {
            FloatType::F64 => self.find_global_expect(F64_TO_STR),
        };
        let _ = function
            .instructions()
            .local_get(0)
            .local_get(1)
            .call(id.index)
            .end();
        function
    }

    fn code_string_repr(&self) -> Function {
        let mut function = Function::new(vec![(4, ValType::I32)]);
        // params
        let s = 0; // String
        let ptr = 1; // I32
        // locals
        let len = 2; // I32
        let i = 3; // I32
        let s_i = 4; // I32
        let dest = 5; // I32
        // return I32 - number of written bytes
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(ptr)
            .local_tee(dest)
            .byte_store(b'"')
            .i32_inc(dest)
            .local_get(s)
            .string_len()
            .local_set(len)
            .i32_const(0)
            .local_set(i)
            // while i <= len
            .loop_(BlockType::Empty)
              .local_get(i)
              .local_get(len)
              .i32_lt_u()
              .if_(BlockType::Empty)
                // value = s[i]
                .local_get(s)
                .local_get(i)
                .string_get()
                .local_set(s_i)
                .block(BlockType::Empty)
                  .local_get(s_i)
                  .try_escape(dest, b'"', b'"')
                  .br_if(0)
                  .local_get(s_i)
                  .try_escape(dest, b'\\', b'\\')
                  .br_if(0)
                  .local_get(s_i)
                  .try_escape(dest, b'\x0C', b'f')
                  .br_if(0)
                  .local_get(s_i)
                  .try_escape(dest, b'\n', b'n')
                  .br_if(0)
                  .local_get(s_i)
                  .try_escape(dest, b'\r', b'r')
                  .br_if(0)
                  .local_get(s_i)
                  .try_escape(dest, b'\t', b't')
                  .br_if(0)
                  // default, do not escape
                  .local_get(dest)
                  .local_get(s_i)
                  .i32_store8(MemArg {
                      offset: 0,
                      align: 0,
                      memory_index: 0,
                  })
                // block
                .end()
                .i32_inc(dest)
                .i32_inc(i)
                .br(1)
              // if
              .end()
            // loop
            .end()
            .local_get(dest)
            .byte_store(b'"')
            .i32_inc(dest)
            .local_get(dest)
            .local_get(ptr)
            .i32_sub()
            .end();

        trait Escape {
            fn try_escape(&mut self, dest: u32, byte: u8, escape: u8) -> &mut Self;
        }

        impl<'a> Escape for ExtendedInstructionSink<'a> {
            #[rustfmt::skip]
            fn try_escape(&mut self, dest: u32, byte: u8, escape: u8) -> &mut Self {
                self.i32_const(byte as i32)
                    .i32_eq()
                    .if_(BlockType::Result(ValType::I32))
                      .local_get(dest)
                      .byte_store(b'\\')
                      .i32_inc(dest)
                      .local_get(dest)
                      .byte_store(escape)
                      .i32_inc(dest)
                      .i32_const(1)
                    .else_()
                      .i32_const(0)
                    .end()
            }
        }

        function
    }

    fn code_list_repr(&mut self, item_type: &Arc<Type>) -> Function {
        let struct_index = self.list_type_index(item_type);
        let mut function = Function::new(vec![(1, ValType::I32)]);
        // params
        let lst = 0; // List(a)
        let ptr = 1; // I32
        let dest = 2; // I32
        // return I32 - number of written bytes
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(ptr)
            .local_tee(dest)
            .byte_store(b'[')
            .i32_inc(dest)
            .local_get(lst)
            .ref_is_null()
            .if_(BlockType::Empty)
              .local_get(dest)
              .byte_store(b']')
              .i32_const(2)
              .return_()
            .else_()
              .local_get(lst)
              .struct_get(struct_index, 1)
              .local_get(dest)
              .call(self.function_repr(item_type))
              .local_get(dest)
              .i32_add()
              .local_set(dest)
            // if
            .end()
            .loop_(BlockType::Empty)
              .local_get(lst)
              .struct_get(struct_index, 0)
              .local_tee(lst)
              .ref_is_null()
              .if_(BlockType::Empty)
                .local_get(dest)
                .byte_store(b']')
                .i32_inc(dest)
              .else_()
                .local_get(dest)
                .byte_store(b',')
                .i32_inc(dest)
                .local_get(dest)
                .byte_store(b' ')
                .i32_inc(dest)
                .local_get(lst)
                .struct_get(struct_index, 1)
                .local_get(dest)
                .call(self.function_repr(item_type))
                .local_get(dest)
                .i32_add()
                .local_set(dest)
                .br(1)
              .end()
            // loop
            .end()
            .local_get(dest)
            .local_get(ptr)
            .i32_sub()
            // function
            .end();
        function
    }

    fn code_tuple_repr(&mut self, types: &[Arc<Type>]) -> Function {
        let type_index = self.tuple_type_index(types.iter().cloned());
        self.code_composite_repr(&"#".into(), false, type_index, types)
    }

    fn code_composite_repr(
        &mut self,
        name: &EcoString,
        is_union: bool,
        type_index: u32,
        types: &[Arc<Type>],
    ) -> Function {
        let mut function = Function::new(vec![(1, ValType::I32)]);
        // params
        let value = 0;
        let ptr = 1; // I32
        // local
        let dest = 2; // I32
        // return I32 - number of written bytes
        let string_to_memory =
            self.find_global_expect(&BuiltinFunctionExternal::StringToMemory.name());
        let string_index = self.string_index(name);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions
            .global_as_non_null(string_index)
            .local_get(ptr)
            .local_tee(dest)
            .call(string_to_memory.index)
            .local_get(dest)
            .i32_add()
            .local_set(dest);

        let first_field = if is_union { 1u32 } else { 0u32 };
        if let Some((first, rest)) = types
            .get(first_field as usize..)
            .and_then(|t| t.split_first())
        {
            let _ = instructions
                .local_get(dest)
                .byte_store(b'(')
                .i32_inc(dest)
                .local_get(value)
                .struct_get(type_index, first_field)
                .local_get(dest)
                .call(self.function_repr(first))
                .local_get(dest)
                .i32_add()
                .local_set(dest);

            for (type_, field_index) in rest.iter().zip(first_field + 1..) {
                let _ = instructions
                    .local_get(dest)
                    .byte_store(b',')
                    .i32_inc(dest)
                    .local_get(dest)
                    .byte_store(b' ')
                    .i32_inc(dest)
                    .local_get(value)
                    .struct_get(type_index, field_index)
                    .local_get(dest)
                    .call(self.function_repr(type_))
                    .local_get(dest)
                    .i32_add()
                    .local_set(dest);
            }

            let _ = instructions.local_get(dest).byte_store(b')').i32_inc(dest);
        }

        let _ = instructions.local_get(dest).local_get(ptr).i32_sub().end();

        function
    }

    fn code_function_repr(&mut self, type_: &Arc<Type>) -> Function {
        let mut function = Function::new(vec![]);
        // params
        let _func = 0; // Function
        let ptr = 1; // I32
        // return I32 - number of written bytes
        let repr = format!("//{} {{ ... }}", self.type_pretty_name(type_));
        let string_index = self.string_index(&repr.into());
        let _ = function
            .extend_instructions(self)
            .global_as_non_null(string_index)
            .local_get(ptr)
            .call(
                self.find_global_expect(&BuiltinFunctionExternal::StringToMemory.name())
                    .index,
            )
            .end();
        function
    }

    fn code_custom_type_repr(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &CustomType,
        args: &[Arc<Type>],
    ) -> Function {
        let mut function = Function::new(vec![]);
        // params
        let value = 0; // value
        let ptr = 1; // I32
        // return I32 - number of written bytes
        let string_to_memory =
            self.find_global_expect(&BuiltinFunctionExternal::StringToMemory.name());
        let mut instructions = function.extend_instructions(self);
        match custom_type {
            CustomType::ExternalI32 => {
                let i32_to_str = self.find_global_expect(I32_TO_STR);
                let _ = instructions
                    .local_get(value)
                    .local_get(ptr)
                    .call(i32_to_str.index)
                    .end();
            }
            CustomType::Enum { values } => {
                let _ = instructions.block(BlockType::Result(self.string.val_type()));
                for (enum_value, name) in values.iter().enumerate() {
                    let string_index = self.string_index(name);
                    #[rustfmt::skip]
                    let _ = instructions
                        .local_get(value)
                        .i32_const(enum_value as i32)
                        .i32_eq()
                        .if_(BlockType::Empty)
                          .global_as_non_null(string_index)
                          .br(1)
                        // if
                        .end();
                }
                let _ = instructions
                    .unreachable()
                    .end()
                    .local_get(ptr)
                    .call(string_to_memory.index)
                    .end();
            }
            CustomType::Struct {
                custom_type,
                constructor,
            } => {
                let (type_index, types) =
                    self.mono_struct_type_index(type_, custom_type, constructor, args);
                return self.code_composite_repr(&constructor.name, false, type_index, &types);
            }
            CustomType::Union { custom_type } => {
                let _ = instructions.block(BlockType::Result(ValType::I32));
                let supertype_index = self.mono_union_supertype_index(type_, custom_type);
                let name = self.type_pretty_name(type_);
                for (tag, constructor) in custom_type.constructors.iter().enumerate() {
                    let (type_index, types) = self.mono_union_type_index(
                        type_,
                        custom_type,
                        Some(constructor),
                        Some(supertype_index),
                        args,
                    );
                    let builtin = BuiltinFunction::CustomTypeRepr(
                        self.val_type_ref(type_index),
                        name.clone() + "." + constructor.name.clone(),
                        Some(constructor.name.clone()),
                    );
                    let repr_index = if let Some(index) = self.builtins.get(&builtin) {
                        *index
                    } else {
                        let code = self.code_composite_repr(
                            &constructor.name,
                            true,
                            type_index,
                            &iter::once(type_::int()).chain(types).collect_vec(),
                        );
                        self.add_function_builtin(builtin, code).index
                    };
                    #[rustfmt::skip]
                    let _ = instructions
                        .local_get(value)
                        .struct_get(supertype_index, 0)
                        .i32_const(tag as i32)
                        .i32_eq()
                        //.local_get(value)
                        //.ref_test_non_null(HeapType::Concrete(type_index))
                        //.i32_and()
                        .if_(BlockType::Empty)
                          .local_get(value)
                          .ref_cast_non_null(HeapType::Concrete(type_index))
                          .local_get(ptr)
                          .call(repr_index)
                          .br(1)
                        .end();
                }
                let _ = instructions.unreachable().end().end();
            }
        };

        function
    }

    fn code_string_to_memory(&self) -> Function {
        let mut function = Function::new(vec![(2, ValType::I32)]);
        // params
        let s = 0; // String
        let dest = 1; // I32
        // locals
        let len = 2; // I32
        let i = 3; // I32
        // result len - I32
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(s)
            .array_len()
            .local_set(len)
            .i32_const(0)
            .local_set(i)
            // while i <= len
            .loop_(BlockType::Empty)
              .local_get(i)
              .local_get(len)
              .i32_lt_u()
              .if_(BlockType::Empty)
                .local_get(dest)
                // value
                .local_get(s)
                .local_get(i)
                .string_get()
                // store
                .i32_store8(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                })
                .i32_inc(dest)
                .i32_inc(i)
                .br(1)
              // if
              .end()
            // loop
            .end()
            .local_get(len)
            .end();
        function
    }

    fn code_memory_to_string(&self) -> Function {
        let mut function = Function::new(vec![(1, ValType::I32), (1, self.string.val_type())]);
        // params
        let src = 0; // I32
        let len = 1; // I32
        // locals
        let i = 2; // I32
        // return String
        let s = 3;
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(len)
            .string_new()
            .local_set(s)
            .i32_const(0)
            .local_set(i)
            .loop_(BlockType::Empty)
              .local_get(i)
              .local_get(len)
              .i32_lt_u()
              .if_(BlockType::Empty)
                .local_get(s)
                .local_get(i)
                .local_get(src)
                .i32_load8_u(MemArg { offset: 0, align: 0, memory_index: 0 })
                .array_set(self.string.type_index)
                .i32_inc(src)
                .i32_inc(i)
                .br(1)
              // if
              .end()
            // loop
            .end()
            .local_get(s)
            .end();
        function
    }

    fn code_parse_int(&mut self) -> Function {
        let parse = match self.int {
            IntType::I32 => self.find_global_expect(PARSE_I32),
            IntType::I64 => self.find_global_expect(PARSE_I64),
        };
        self.code_parse(type_::int(), parse.index)
    }

    fn code_parse_float(&mut self) -> Function {
        let parse = match self.float {
            FloatType::F64 => self.find_global_expect(PARSE_F64),
        };
        self.code_parse(type_::float(), parse.index)
    }

    fn code_parse(&mut self, type_: Arc<Type>, parse: u32) -> Function {
        let mut function = Function::new(vec![(3, ValType::I32), (1, self.val_type(&type_))]);
        // params
        let s = 0; // String
        // locals
        let prt = 1; // I32
        let dest = 2; // I32
        let len = 3; // I32
        let r = 4; // type_
        // return Result(type, Nil)
        // FIX: create on demand
        let string_to_memory =
            self.find_global_expect(&BuiltinFunctionExternal::StringToMemory.name());
        let heap_base = self.find_global_expect(HEAP_BASE);
        let result = type_::result(type_.clone(), type_::nil());
        let params = vec![type_];
        let ok_type = type_::fn_(params.clone(), result.clone());
        let ok_index = self.function_type_index(params, Some(result.clone()));
        let params = vec![type_::nil()];
        let err_type = type_::fn_(params.clone(), result.clone());
        let err_index = self.function_type_index(params, Some(result.clone()));
        let scope = Scope::with_params(self.globals.clone(), &[]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions
            .local_get(s)
            .call(heap_base.index)
            .local_tee(prt)
            .local_set(dest)
            .i32_inc(dest)
            .local_get(dest)
            .call(string_to_memory.index)
            .local_set(len)
            .local_get(prt) // result
            .local_get(dest) // ptr
            .local_get(len)
            .call(parse)
            .local_set(r)
            .local_get(prt)
            .i32_load8_u(MemArg {
                offset: 0,
                align: 0,
                memory_index: 0,
            })
            .if_(BlockType::Result(self.val_type(&result)))
            .local_get(r);
        // FIXME: call variant_constructor instead of expression_var
        let _ = self.expression_var(&scope, &mut instructions, &"Ok".into(), &ok_type);
        let _ = instructions.call_ref(ok_index).else_().i32_const(0); // FIXME: get nil value
        let _ = self.expression_var(&scope, &mut instructions, &"Error".into(), &err_type);
        let _ = instructions.call_ref(err_index).end().end();
        function
    }

    fn add_function_builtin(
        &mut self,
        builtin: BuiltinFunction,
        function: Function,
    ) -> WasmFunction {
        let (params, results) = self.builtin_type(&builtin);
        let function = self.add_function(
            builtin.name(),
            false,
            params,
            results,
            function.into_raw_body(),
        );
        let _ = self.builtins.insert(builtin, function.index);
        function
    }

    fn get_function_builtin_external(&mut self, builtin: BuiltinFunctionExternal) -> u32 {
        if let Some(id) = self.builtins_external.get(&builtin) {
            return *id;
        }

        let function = match builtin {
            BuiltinFunctionExternal::I32ToInt => self.code_i32_to_int(),
            BuiltinFunctionExternal::IntToI32 => self.code_int_to_i32(),
            BuiltinFunctionExternal::IntRepr => self.code_int_repr(),
            BuiltinFunctionExternal::FloatRepr => self.code_float_repr(),
            BuiltinFunctionExternal::StringRepr => self.code_string_repr(),
            BuiltinFunctionExternal::StringToMemory => self.code_string_to_memory(),
            BuiltinFunctionExternal::MemoryToString => self.code_memory_to_string(),
            BuiltinFunctionExternal::ParseInt => self.code_parse_int(),
            BuiltinFunctionExternal::ParseFloat => self.code_parse_float(),
        };
        // FIXME: create an appropriated type
        let i32 = type_::named("", &self.module.name, "I32", Publicity::Private, vec![]);
        let (params, result) = builtin.type_(i32);
        let params = self.val_types(params);
        let result = self.val_type(&result);
        let function = self.add_function(
            builtin.name().into(),
            false,
            params,
            vec![result],
            function.into_raw_body(),
        );
        let _ = self.builtins_external.insert(builtin, function.index);
        function.index
    }

    fn add_function(
        &mut self,
        name: EcoString,
        export: bool,
        params: Vec<ValType>,
        results: Vec<ValType>,
        code: Vec<u8>,
    ) -> WasmFunction {
        let type_index = self.function_type_index_with_val_types(params, results);
        let index = self.function_next_id();
        let function = WasmFunction {
            name: name.clone(),
            index,
            type_index,
            code: code.into(),
            export,
            locals: vec![],
        };
        let _ = self.functions.insert(function.clone());
        function
    }
}

fn const_expr_ref_null(type_index: u32) -> ConstExpr {
    ConstExpr::ref_null(HeapType::Concrete(type_index))
}

fn is_main_funtion(function: &TypedFunction) -> bool {
    function
        .name
        .as_ref()
        .map(|name| name.1 == MAIN)
        .unwrap_or(false)
        && function.arguments.is_empty()
}

fn type_pretty_name(names: &Names, type_: &Arc<Type>) -> EcoString {
    Printer::new(names).print_type(type_)
}

fn custom_type_inferred_constructor<'a>(
    custom_type: &'a TypedCustomType,
    type_: &Arc<Type>,
) -> Option<&'a TypedRecordConstructor> {
    type_
        .custom_type_inferred_variant()
        .and_then(|index| custom_type.constructors.get(index as usize))
}

#[allow(unused)]
struct ExtendedInstructionSink<'a> {
    int: IntType,
    float: FloatType,
    string: StringType,
    instructions: InstructionSink<'a>,
}

#[derive(Clone, Copy)]
enum Eq {
    I32,
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
            int: generator.int,
            float: generator.float,
            string: generator.string,
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
    fn i32_inc(&mut self, local: u32) -> &mut Self {
        self.local_get(local)
            .i32_const(1)
            .i32_add()
            .local_set(local)
    }

    fn list_null(&mut self, type_index: u32) -> &mut Self {
        self.ref_null(HeapType::Concrete(type_index))
    }

    fn list_new(&mut self, type_index: u32) -> &mut Self {
        self.struct_new(type_index)
    }

    fn list_first(&mut self, type_index: u32) -> &mut Self {
        self.struct_get(type_index, 1)
    }

    fn list_rest(&mut self, type_index: u32) -> &mut Self {
        self.struct_get(type_index, 0)
    }

    fn string_new(&mut self) -> &mut Self {
        self.array_new_default(self.string.type_index)
    }

    fn string_get(&mut self) -> &mut Self {
        self.array_get_u(self.string.type_index)
    }

    fn string_set(&mut self) -> &mut Self {
        self.array_set(self.string.type_index)
    }

    fn string_len(&mut self) -> &mut Self {
        self.array_len()
    }

    fn byte_store(&mut self, byte: u8) -> &mut Self {
        self.i32_const(byte as i32).i32_store8(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        })
    }

    fn global_as_non_null(&mut self, index: u32) -> &mut Self {
        self.global_get(index).ref_as_non_null()
    }

    fn eq(&mut self, eq: Eq) -> &mut Self {
        match eq {
            Eq::I32 => {
                let _ = self.instructions.i32_eq();
                self
            }
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

    fn patterns<'b>(
        &mut self,
        generator: &mut Generator<'_>,
        locals: &Locals,
        scope: &mut Scope,
        (type_index, subtype_index): (u32, Option<u32>),
        pattern: &Pattern<Arc<Type>>,
        elements: impl IntoIterator<Item = &'b Pattern<Arc<Type>>> + Clone,
    ) -> &mut Self {
        generator._patterns(
            locals,
            scope,
            self,
            (type_index, subtype_index),
            pattern,
            elements,
        );
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

    fn show_error_message(
        &mut self,
        string_index: u32,
        string_to_memory: Id,
        heap_base: Id,
        print: Id,
    ) -> &mut Self {
        self.i32_const(STDERR)
            .call(heap_base.index)
            .global_as_non_null(string_index)
            .call(heap_base.index)
            .call(string_to_memory.index)
            .call(print.index)
    }

    delegate! {
        if_(bt: BlockType),
        else_(),
        end(),
        loop_(bt: BlockType),
        block(bt: BlockType),
        br(l: u32),
        br_if(l: u32),
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
        ref_cast_non_null(ht: HeapType),
        call_ref(index: u32),
        call(index: u32),
        array_new_default(type_index: u32),
        array_new_data(type_index: u32, data_segment: u32),
        array_len(),
        array_get_u(type_index: u32),
        array_set(type_index: u32),
        return_(),
        i32_const(x: i32),
        i32_eq(),
        i32_ne(),
        i32_ge_u(),
        i32_lt_u(),
        i32_add(),
        i32_sub(),
        i32_store8(m: MemArg),
        i32_load8_u(m: MemArg),
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
}

#[allow(unused)]
#[derive(Debug, Copy, Clone)]
enum IntType {
    I32,
    I64,
}

impl IntType {
    fn val_type(&self) -> ValType {
        match self {
            IntType::I32 => ValType::I32,
            IntType::I64 => ValType::I64,
        }
    }

    fn int_const(&self, value: &BigInt) -> ConstExpr {
        match self {
            IntType::I32 => ConstExpr::i32_const(value.try_into().unwrap()),
            IntType::I64 => ConstExpr::i64_const(value.try_into().unwrap()),
        }
    }
}

macro_rules! int_op {
    ($name:ident, $i32:ident, $i64:ident) => {
        fn $name(&mut self) -> &mut Self {
            let _ = match self.int {
                IntType::I32 => self.instructions.$i32(),
                IntType::I64 => self.instructions.$i64(),
            };
            self
        }
    };
}

impl<'a> ExtendedInstructionSink<'a> {
    fn int_const(&mut self, value: &BigInt) -> &mut Self {
        let _ = match self.int {
            IntType::I32 => self.instructions.i32_const(value.try_into().unwrap()),
            IntType::I64 => self.instructions.i64_const(value.try_into().unwrap()),
        };
        self
    }

    fn int_div(&mut self, dividend: u32, divisor: u32) -> &mut Self {
        #[rustfmt::skip]
        let _ = match self.int {
            IntType::I32 => self
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
            IntType::I64 => self
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

    fn i32_to_int(&mut self) -> &mut Self {
        if let IntType::I64 = self.int {
            let _ = self.instructions.i64_extend_i32_s();
        }
        self
    }

    fn int_to_i32(&mut self) -> &mut Self {
        if let IntType::I64 = self.int {
            let _ = self.instructions.i32_wrap_i64();
        }
        self
    }

    int_op!(int_add, i32_add, i64_add);
    int_op!(int_sub, i32_sub, i64_sub);
    int_op!(int_mul, i32_mul, i64_mul);
    int_op!(int_rem, i32_rem_s, i64_rem_s);
    int_op!(int_eq, i32_eq, i64_eq);
    int_op!(int_lt, i32_lt_s, i64_lt_s);
    int_op!(int_le, i32_le_s, i64_le_s);
    int_op!(int_gt, i32_gt_s, i64_gt_s);
    int_op!(int_ge, i32_ge_s, i64_ge_s);
}

#[derive(Debug, Copy, Clone)]
enum FloatType {
    F64,
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
    float_op!(float_lt, f64_lt);
    float_op!(float_le, f64_le);
    float_op!(float_gt, f64_gt);
    float_op!(float_ge, f64_ge);
}

#[derive(Clone, Copy)]
struct StringType {
    type_index: u32,
}

impl StringType {
    fn wasm_type() -> WasmType {
        WasmType::array(StorageType::I8)
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
    params: Vec<(u32, Option<EcoString>)>,
    locals: HashMap<u64, (u32, Option<EcoString>)>,
    names: HashMap<EcoString, usize>,
    val_types: Vec<ValType>,
}

impl Locals {
    fn new(
        generator: &mut Generator<'_>,
        arguments: &[TypedArg],
        statements: &[TypedStatement],
    ) -> Self {
        let mut locals = Locals {
            params: (0..)
                .zip(
                    arguments
                        .iter()
                        .map(|arg| arg.names.get_variable_name().cloned()),
                )
                .collect(),
            locals: HashMap::new(),
            names: HashMap::from_iter(
                arguments
                    .iter()
                    .flat_map(|arg| arg.names.get_variable_name().map(|name| (name.clone(), 1))),
            ),
            val_types: vec![],
        };

        let mut visit = LocalsVisit {
            locals: &mut locals,
            generator,
        };

        for statement in statements {
            visit.visit_typed_statement(statement);
        }

        locals
    }

    fn names(&self) -> Vec<(u32, EcoString)> {
        self.params
            .iter()
            .chain(self.locals.values())
            .filter_map(|(index, name)| name.as_ref().map(|n| (*index, n.clone())))
            .collect()
    }

    fn val_types(&self) -> Vec<(u32, ValType)> {
        self.val_types.iter().map(|e| (1, *e)).collect()
    }

    fn insert_assignment(&mut self, generator: &mut Generator<'_>, assignment: &TypedAssignment) {
        if matches!(assignment.kind, AssignmentKind::Let) && assignment.pattern.is_variable() {
            //we use the local variable
        } else {
            self._insert(generator, assignment, &assignment.type_());
        }
    }

    fn for_assigment(&self, assignment: &TypedAssignment) -> u32 {
        if matches!(assignment.kind, AssignmentKind::Let) && assignment.pattern.is_variable() {
            self._get(&assignment.pattern)
        } else {
            self._get(assignment)
        }
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
        if !fun.is_var() {
            self._insert(generator, fun, &fun.type_())
        } else {
            set_ubound_or_generic(&fun.type_(), &type_::nil());
        }
    }

    fn for_call(&self, fun: &TypedExpr) -> u32 {
        assert!(!fun.is_var());
        self._get(fun)
    }

    fn insert_subjects(&mut self, generator: &mut Generator<'_>, subjects: &[TypedExpr]) {
        for subject in subjects {
            // FIXME: do not create a local if the subject is var
            let name = format!(
                "subject@{}",
                generator.line_numbers.line_number(subject.location().start)
            );
            self._insert_named(generator, subject, &subject.type_(), Some(name.into()));
        }
    }

    fn for_subjects(&self, subjects: &[TypedExpr]) -> Vec<u32> {
        subjects.iter().map(|subject| self._get(subject)).collect()
    }

    fn insert_pattern(&mut self, generator: &mut Generator<'_>, pattern: &TypedPattern) {
        match pattern {
            TypedPattern::Variable { name, .. } => {
                self._insert_named(generator, pattern, &pattern.type_(), Some(name.clone()));
            }
            _ => {
                let name = format!(
                    "pattern@{}",
                    generator.line_numbers.line_number(pattern.location().start)
                );
                self._insert_named(generator, pattern, &pattern.type_(), Some(name.into()));
            }
        }
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

    fn insert_echo(
        &mut self,
        generator: &mut Generator<'_>,
        echo: &TypedExpr,
        expression: &TypedExpr,
    ) {
        // the number of written bytes
        self._insert_with_val_type(echo, ValType::I32);
        self._insert(generator, expression, &echo.type_());
    }

    fn for_echo(&self, echo: &TypedExpr, expression: &TypedExpr) -> (u32, u32) {
        (self._get(echo), self._get(expression))
    }

    fn _insert(&mut self, generator: &mut Generator<'_>, key: impl LocalHash, type_: &Arc<Type>) {
        self._insert_named(generator, key, type_, None);
    }

    fn _insert_named(
        &mut self,
        generator: &mut Generator<'_>,
        key: impl LocalHash,
        type_: &Arc<Type>,
        name: Option<EcoString>,
    ) {
        let index = self.locals.len() as u32 + self.params.len() as u32;
        let name = name.map(|name| {
            let count = self.names.entry(name.clone()).or_insert(0);
            *count += 1;
            if *count == 1 {
                name
            } else {
                name + "'".repeat(*count - 1).as_str()
            }
        });
        if self.locals.insert(key.hash(), (index, name)).is_some() {
            panic!("Locals collision.");
        }
        // Some local variable can still be unbound or generic,
        // like [], None, etc, so we choose arbitrarily to monormorphize
        // the types to nil. The locals are determined before code generation,
        // so we choose to do the monomorphization here to avoid doing a
        // another complete pass in the ast before the code generation.
        set_ubound_or_generic(type_, &type_::nil());
        self.val_types.push(generator.val_type(type_));
    }

    fn _insert_with_val_type(&mut self, key: impl LocalHash, val_type: ValType) {
        let index = self.locals.len() as u32 + self.params.len() as u32;
        if self.locals.insert(key.hash(), (index, None)).is_some() {
            panic!("Locals collision.");
        }
        self.val_types.push(val_type);
    }

    fn _get(&self, key: impl LocalHash) -> u32 {
        let id = key.hash();
        self.locals
            .get(&id)
            .unwrap_or_else(|| panic!("Local with id {id} not found."))
            .0
    }
}

struct LocalsVisit<'a, 'b, 'c> {
    locals: &'a mut Locals,
    generator: &'b mut Generator<'c>,
}

impl<'ast, 'a, 'b, 'c> Visit<'ast> for LocalsVisit<'a, 'b, 'c> {
    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        self.locals.insert_assignment(self.generator, assignment);
        visit_typed_assignment(self, assignment);
    }

    fn visit_typed_expr_bin_op(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        name: &'ast BinOp,
        name_location: &'ast SrcSpan,
        left: &'ast TypedExpr,
        right: &'ast TypedExpr,
    ) {
        if matches!(name, BinOp::DivInt | BinOp::DivFloat) {
            self.locals.insert_div(self.generator, left, right);
        }
        visit_typed_expr_bin_op(self, location, type_, name, name_location, left, right);
    }

    fn visit_typed_expr_call(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        fun: &'ast TypedExpr,
        arguments: &'ast [type_::TypedCallArg],
    ) {
        self.locals.insert_call(self.generator, fun);
        visit_typed_expr_call(self, location, type_, fun, arguments);
    }

    fn visit_typed_expr_case(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        subjects: &'ast [TypedExpr],
        clauses: &'ast [TypedClause],
        compiled_case: &'ast crate::exhaustiveness::CompiledCase,
    ) {
        self.locals.insert_subjects(self.generator, subjects);
        visit_typed_expr_case(self, location, type_, subjects, clauses, compiled_case);
    }

    fn visit_typed_clause_guard(&mut self, guard: &'ast TypedClauseGuard) {
        match guard {
            ClauseGuard::DivInt { left, right, .. } | ClauseGuard::DivFloat { left, right, .. } => {
                self.locals.insert_guard_div(self.generator, left, right);
            }
            _ => {}
        }
        visit_typed_clause_guard(self, guard);
    }

    fn visit_typed_pattern(&mut self, pattern: &'ast TypedPattern) {
        self.locals.insert_pattern(self.generator, pattern);
        visit_typed_pattern(self, pattern);
    }

    fn visit_typed_pipeline_assignment(&mut self, assignment: &'ast TypedPipelineAssignment) {
        self.locals
            .insert_pipeline_assignment(self.generator, assignment);
        visit_typed_pipeline_assignment(self, assignment);
    }

    fn visit_typed_expr(&mut self, expr: &'ast TypedExpr) {
        if let echo @ TypedExpr::Echo { expression, .. } = expr {
            self.locals
                .insert_echo(self.generator, echo, expression.as_ref().unwrap());
        }
        visit_typed_expr(self, expr);
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

    fn variant_constructor(
        custom_type: &TypedCustomType,
        constructor: &TypedRecordConstructor,
        args: &[Arc<Type>],
    ) -> Vec<Arc<Type>> {
        let mut mono = Monomorphizer {
            map: HashMap::new(),
        };

        for (from, to) in custom_type.typed_parameters.iter().zip(args) {
            let _ = mono.bound(from, to);
        }

        constructor
            .arguments
            .iter()
            .map(|arg| mono.type_(&arg.type_))
            .collect()
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
        } else if let Type::Named {
            publicity,
            package,
            module,
            name,
            arguments,
            inferred_variant,
        } = (**old).clone()
        {
            Type::Named {
                publicity,
                package,
                module,
                name,
                arguments: arguments
                    .iter()
                    .map(|argument| self.type_(argument))
                    .collect(),
                inferred_variant,
            }
            .into()
        } else if let Type::Var { type_ } = old.as_ref()
            && let TypeVar::Link { type_ } = &*type_.borrow()
        {
            self.type_(type_)
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
                self.assignment(assignment);
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

    fn assignment(&self, assignment: &mut TypedAssignment) {
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
                        self.guard(guard);
                    }
                }
            }
            TypedExpr::RecordAccess { type_, record, .. } => {
                *type_ = self.type_(type_);
                self.expression(record);
            }
            TypedExpr::RecordUpdate {
                type_,
                record_assignment,
                constructor,
                arguments,
                ..
            } => {
                *type_ = self.type_(type_);
                if let Some(record_assignment) = record_assignment {
                    self.assignment(record_assignment);
                }
                self.expression(constructor);
                for argument in arguments {
                    self.expression(&mut argument.value);
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

    fn guard(&self, guard: &mut TypedClauseGuard) {
        match guard {
            ClauseGuard::Constant(_) => {}
            ClauseGuard::Block { value, .. } => self.guard(value),
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
            | ClauseGuard::DivInt { left, right, .. }
            | ClauseGuard::DivFloat { left, right, .. }
            | ClauseGuard::RemainderInt { left, right, .. }
            | ClauseGuard::Or { left, right, .. }
            | ClauseGuard::And { left, right, .. } => {
                self.guard(left);
                self.guard(right);
            }
            ClauseGuard::Not { expression, .. } => self.guard(expression),
            ClauseGuard::Var { type_, .. } => *type_ = self.type_(type_),
            ClauseGuard::TupleIndex { type_, tuple, .. } => {
                *type_ = self.type_(type_);
                self.guard(tuple);
            }
            ClauseGuard::FieldAccess {
                type_, container, ..
            } => {
                *type_ = self.type_(type_);
                self.guard(container);
            }
            ClauseGuard::ModuleSelect { .. } => todo!(),
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

#[derive(Clone)]
struct ExternalFunction {
    used_names: HashSet<EcoString>,
    params: Vec<ValType>,
    results: Vec<ValType>,
}

struct Externals {
    externals: HashMap<EcoString, ExternalFunction>,
    builtins: HashMap<BuiltinFunctionExternal, Option<(EcoString, Arc<Type>)>>,
    // FIXME: use a reference
    custom_types: Vec<TypedCustomType>,
    visited_types: Vec<Arc<Type>>,
    todo_panic: bool,
    assert: bool,
    echo_any: bool,
    echo_int: bool,
    echo_float: bool,
    echo: usize,
}

impl Externals {
    fn new(generator: &mut Generator<'_>) -> Externals {
        let mut externals = Externals {
            externals: HashMap::new(),
            builtins: HashMap::new(),
            custom_types: generator.module.definitions.custom_types.clone(),
            visited_types: vec![],
            todo_panic: false,
            assert: false,
            echo_any: false,
            echo_int: false,
            echo_float: false,
            echo: 0,
        };

        // find available wasm functions
        let module = walrus::Module::from_buffer(BUILTINS_WASM).expect("Wasm module");
        for function in module.functions() {
            if let (Some(name), walrus::FunctionKind::Local(local_function)) =
                (&function.name, &function.kind)
            {
                let type_ = module.types.get(local_function.ty());
                let results = type_.results();
                externals.insert_available(
                    name.into(),
                    ExternalFunction {
                        used_names: HashSet::new(),
                        params: walrus_types_to_wasmencoder_types(type_.params()),
                        results: walrus_types_to_wasmencoder_types(results),
                    },
                );
            }
        }

        externals.functions(generator, &generator.module.definitions.functions);

        if externals.echo_any || externals.assert || externals.todo_panic {
            externals.insert_builtin(BuiltinFunctionExternal::StringToMemory, None);
            externals.insert_external(HEAP_BASE);
            externals.insert_external(PRINT);
        }

        if externals.assert || externals.todo_panic {
            externals.insert_external(EXIT);
        }

        if externals.echo_int {
            externals.insert_builtin(BuiltinFunctionExternal::IntRepr, None);
        }

        if externals.echo_float {
            externals.insert_builtin(BuiltinFunctionExternal::FloatRepr, None);
        }

        for builtin in externals.builtins.keys().cloned().collect_vec() {
            for dep in builtin.dependencies() {
                externals.insert_builtin(dep.clone(), None);
            }
            for dep in builtin.externals_dependencies(generator.int, generator.float) {
                externals.insert_external(dep);
            }
        }

        externals
    }

    fn insert_available(&mut self, name: EcoString, function: ExternalFunction) {
        let _ = self.externals.insert(name, function);
    }

    fn insert_external(&mut self, name: &str) {
        let _ = self
            .externals
            .get_mut(name)
            .unwrap()
            .used_names
            .insert(name.into());
    }

    fn insert_builtin(
        &mut self,
        builtin: BuiltinFunctionExternal,
        use_: Option<(EcoString, Arc<Type>)>,
    ) {
        let _ = self.builtins.insert(builtin, use_);
    }

    fn use_data_section(&self) -> bool {
        self.echo_any
        // FIXME: echo needs __heap_base, which is in data section,
        //        but we cannot easily get it without the other data,
        //        so we include all data section.
    }

    fn functions<'a>(
        &mut self,
        generator: &mut Generator<'_>,
        functions: impl IntoIterator<Item = &'a TypedFunction>,
    ) {
        for function in functions {
            if let Some((module, name, _)) = &function.external_webassembly {
                // FIXME: handle error
                assert_eq!(module, "builtins");
                if let Some(ExternalFunction {
                    params,
                    results,
                    used_names,
                }) = self.externals.get_mut(name)
                {
                    let _ = used_names.insert(name.clone());
                    let wasm_params =
                        generator.val_types(function.arguments.iter().map(|arg| arg.type_.clone()));
                    let wasm_results =
                        generator.val_types(iter::once(function.return_type.clone()));
                    if !function
                        .arguments
                        .iter()
                        .all(|arg| generator.is_external_type(&arg.type_))
                        || !generator.is_external_type(&function.return_type)
                        || (&wasm_params, &wasm_results) != (params, results)
                    {
                        // FIXME: improve error
                        panic!(
                            "Wrong type for {module}/{name}. Expected {:?}, but got ({:?}) -> ({:?}).",
                            generator.type_pretty_name(&function_type(function)),
                            params,
                            results
                        );
                    }
                } else if let Some(builtin) = BuiltinFunctionExternal::by_name(name) {
                    let _ =
                        self.insert_builtin(builtin, Some((name.into(), function_type(function))));
                } else {
                    panic!("There is no function {name} in {module} module.");
                }
            }
            self.visit_typed_function(function);
        }
    }

    fn visit_type(&mut self, type_: &Arc<Type>) {
        self.echo_int |= type_.is_int();
        self.echo_float |= type_.is_float();

        // FIXME: avoid linear search
        if self.visited_types.contains(type_) {
            return;
        }

        self.visited_types.push(type_.clone());

        match type_.as_ref() {
            Type::Named {
                name, arguments, ..
            } => {
                for argument in arguments {
                    self.visit_type(argument);
                }
                // FIXME: types in other modules?
                if let Some(custom_type) = self
                    .custom_types
                    .iter()
                    .find(|custom_type| &custom_type.name == name)
                {
                    let custom_type = custom_type.clone();
                    for type_ in custom_type
                        .constructors
                        .iter()
                        .flat_map(|constructor| &constructor.arguments)
                        .map(|arg| arg.type_.clone())
                    {
                        self.visit_type(&type_);
                    }
                }
            }
            Type::Var { type_ } => {
                if let TypeVar::Link { type_ } = &*type_.borrow() {
                    self.visit_type(type_);
                }
            }
            Type::Tuple { elements } => {
                for element in elements {
                    self.visit_type(element);
                }
            }
            Type::Fn { .. } => {}
        }
    }
}

impl<'ast> Visit<'ast> for Externals {
    fn visit_typed_expr_echo(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        expression: &'ast Option<Box<TypedExpr>>,
        message: &'ast Option<Box<TypedExpr>>,
    ) {
        self.echo_any = true;
        self.echo += 1;
        if let Some(expression) = expression {
            self.visit_type(&expression.type_());
        }
        if let Some(message) = message {
            self.visit_type(&message.type_());
        }
        visit_typed_expr_echo(self, location, type_, expression, message);
        self.echo -= 1;
    }

    fn visit_typed_expr_int(
        &mut self,
        _location: &'ast SrcSpan,
        _type_: &'ast Arc<Type>,
        _value: &'ast EcoString,
    ) {
        if self.echo > 0 {
            self.echo_int = true;
        }
    }

    fn visit_typed_expr_float(
        &mut self,
        _location: &'ast SrcSpan,
        _type_: &'ast Arc<Type>,
        _value: &'ast EcoString,
    ) {
        if self.echo > 0 {
            self.echo_float = true;
        }
    }

    fn visit_typed_assert(&mut self, assert: &'ast TypedAssert) {
        self.assert = true;
        visit_typed_assert(self, assert);
    }

    fn visit_typed_assignment(&mut self, assignment: &'ast TypedAssignment) {
        if assignment.kind.is_assert() {
            self.assert = true;
        }
        visit_typed_assignment(self, assignment);
    }

    fn visit_typed_expr_panic(
        &mut self,
        location: &'ast SrcSpan,
        message: &'ast Option<Box<TypedExpr>>,
        type_: &'ast Arc<Type>,
    ) {
        self.todo_panic = true;
        visit_typed_expr_panic(self, location, message, type_);
    }

    fn visit_typed_expr_todo(
        &mut self,
        location: &'ast SrcSpan,
        message: &'ast Option<Box<TypedExpr>>,
        kind: &'ast TodoKind,
        type_: &'ast Arc<Type>,
    ) {
        self.todo_panic = true;
        visit_typed_expr_todo(self, location, message, kind, type_);
    }
}

fn prepare_wasm_module<'a>(
    buffer: &[u8],
    externals: impl IntoIterator<Item = &'a EcoString>,
) -> Vec<u8> {
    let mut module = walrus::Module::from_buffer(buffer).unwrap();
    let mut roots = HashSet::new();
    'loop_: for external in externals {
        for func in module.funcs.iter() {
            if func.name.as_deref() == Some(external) {
                let _ = roots.insert(func.id());
                continue 'loop_;
            }
        }
        panic!("External not found: \"{external}\".");
    }

    // Find functions and globals used by root functions
    struct State<'a> {
        queue: &'a mut Vec<walrus::FunctionId>,
        used_globals: &'a mut HashSet<walrus::GlobalId>,
    }

    impl<'a> walrus::ir::Visitor<'a> for State<'a> {
        fn visit_function_id(&mut self, function: &walrus::FunctionId) {
            self.queue.push(*function);
        }

        fn visit_global_id(&mut self, global: &walrus::GlobalId) {
            let _ = self.used_globals.insert(*global);
        }
    }
    let mut used_functions = HashSet::new();
    let mut used_types = HashSet::new();
    let mut used_globals = HashSet::new();
    let mut queue = roots.into_iter().collect_vec();
    while let Some(id) = queue.pop() {
        let func = module.funcs.get(id);
        let _ = used_types.insert(func.ty());
        if used_functions.insert(func.id())
            && let walrus::FunctionKind::Local(local) = &func.kind
        {
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

    // Remove unused functions
    let unused = module
        .funcs
        .iter()
        .map(|f| f.id())
        .filter(|id| !used_functions.contains(id))
        .collect_vec();
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
    let unused = module
        .types
        .iter()
        .map(|t| t.id())
        .filter(|id| !used_types.contains(id))
        .collect_vec();
    for id in unused {
        module.types.delete(id);
    }

    // Remove unused globals
    let unused = module
        .globals
        .iter()
        .map(|g| g.id())
        .filter(|id| !used_globals.contains(id))
        .collect_vec();
    for id in unused {
        module.globals.delete(id);
        if let Some(export) = module.exports.get_exported_global(id) {
            module.exports.delete(export.id());
        }
    }

    module.emit_wasm()
}

fn walrus_types_to_wasmencoder_types(types: &[walrus::ValType]) -> Vec<ValType> {
    types.iter().map(walrus_type_to_wasmencoder_type).collect()
}

fn walrus_type_to_wasmencoder_type(type_: &walrus::ValType) -> ValType {
    match type_ {
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
    }
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
            op => panic!("Operator not expected: {:?}", op),
        }
    }
    ConstExpr::i32_const(i32_value)
}

pub fn unescape(s: &str) -> String {
    let mut r = String::new();
    let mut chars = s.chars();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            r.push(ch);
            continue;
        }
        // see https://tour.gleam.run/basics/strings/
        let ch = match chars.next() {
            Some('"') => '"',
            Some('\\') => '\\',
            Some('f') => '\x0C',
            Some('n') => '\n',
            Some('r') => '\r',
            Some('t') => '\t',
            Some('u') => unescape_unicode(&mut chars),
            _ => panic!(),
        };
        r.push(ch);
    }
    r
}

fn unescape_unicode(chars: &mut Chars<'_>) -> char {
    let s = chars.as_str();
    assert_eq!(chars.next(), Some('{'));
    let num = 1 + chars.take_while(|c| *c != '}').count();
    *chars = s[num + 1..].chars();
    char::from_u32(u32::from_str_radix(&s[1..num], 16).unwrap()).unwrap()
}

#[test]
fn string_unescape() {
    assert_eq!(
        unescape(r#"sure \\ \n \"its\" \t works! \u{263A}, \r or \f not..."#),
        "sure \\ \n \"its\" \t works! ☺, \r or \x0C not..."
    );
}
