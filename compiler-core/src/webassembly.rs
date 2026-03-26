#![allow(clippy::todo)]
mod builtins;
mod instructions;
mod monomorphize;
mod native;
mod scope;
#[cfg(test)]
mod tests;

pub use builtins::builtin_function_names;
use builtins::*;
use instructions::*;
use monomorphize::*;
use native::*;
use scope::*;

use ecow::EcoString;
use indexmap::IndexMap;
use itertools::Itertools;
use num_bigint::BigInt;
use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{BTreeSet, HashMap, HashSet},
    iter,
    ops::Deref,
    rc::Rc,
    str::Chars,
    sync::Arc,
};
use wasm_encoder::{
    BlockType, CodeSection, CompositeInnerType, CompositeType, ConstExpr, DataCountSection,
    DataSection, ElementSection, Elements, EntityType, ExportKind, ExportSection, FieldType,
    Function, FunctionSection, GlobalSection, GlobalType, HeapType, ImportSection, IndirectNameMap,
    InstructionSink, MemArg, MemorySection, MemoryType, Module, NameMap, NameSection, RefType,
    StartSection, StorageType, StructType, SubType, TypeSection, ValType,
};

use crate::{
    ast::{
        AssignName, AssignmentKind, BinOp, ClauseGuard, Constant, OperatorKind, Pattern, Publicity,
        SrcSpan, Statement, TypeAst, TypeAstVar, TypedArg, TypedAssignment, TypedClause,
        TypedClauseGuard, TypedConstant, TypedCustomType, TypedExpr, TypedFunction, TypedModule,
        TypedModuleConstant, TypedPattern, TypedRecordConstructor, TypedRecordConstructorArg,
        TypedStatement, visit::Visit,
    },
    line_numbers::LineNumbers,
    type_::{
        self, ModuleValueConstructor, PRELUDE_MODULE_NAME, Type, TypeVar, ValueConstructorVariant,
        printer::{Names, Printer},
    },
};

const BUILTINS_WASM: &[u8] =
    include_bytes!("../../builtins-wasm/target/wasm32-unknown-unknown/release/builtins_wasm.wasm");

const MAIN: &str = "main";
const BUILTINS_MODULE: &str = "builtins";
const INSPECT: &str = "_inspect";

const HEAP_BASE: &str = "_heap_base";
const EXIT: &str = "_exit";
const PRINT: &str = "_print";

const I32_TO_STR: &str = "_i32_to_str";
const I32_PARSE: &str = "_i32_parse";
const I32_IS_CODEPOINT: &str = "_i32_is_codepoint";

const I64_TO_STR: &str = "_i64_to_str";
const I64_PARSE: &str = "_i64_parse";
const I64_IS_CODEPOINT: &str = "_i64_is_codepoint";

const F32_TO_STR: &str = "_f32_to_str";
const F32_PARSE: &str = "_f32_parse";

const F64_TO_STR: &str = "_f64_to_str";
const F64_PARSE: &str = "_f64_parse";

const STDERR: i32 = 2;

const BOOL_VALTYPE: ValType = ValType::I32;

const OK_GENERIC_ID: u64 = u64::MAX;
const ERROR_GENERIC_ID: u64 = u64::MAX - 1;
const LIST_ITEM_GENERIC_ID: u64 = u64::MAX - 2;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Error {
    UnknownExternalType {
        location: SrcSpan,
        name: EcoString,
    },
    UnknownBuiltinFunction {
        location: SrcSpan,
        name: EcoString,
    },
    WrongBuiltinFunctionSignature {
        location: SrcSpan,
        name: EcoString,
        expected: EcoString,
        got: EcoString,
    },
    UnknownExternalModule {
        location: SrcSpan,
        module: EcoString,
    },
    UnsupportedFeature {
        location: SrcSpan,
        feature: EcoString,
    },
    IntLiteralOutOfRange {
        location: SrcSpan,
        value: EcoString,
        target: EcoString,
    },
    FloatLiteralOutOfRange {
        location: SrcSpan,
        value: EcoString,
        target: EcoString,
    },
}

#[cfg(test)]
pub(crate) fn validate_module(
    module: &TypedModule,
    all_modules: &HashMap<EcoString, &TypedModule>,
    int: &str,
    float: &str,
) -> Result<(), Error> {
    let line_numbers = LineNumbers::new("");
    let all_line_numbers = HashMap::new();
    let mut generator = Generator::new(module, &line_numbers, all_modules, &all_line_numbers);
    generator.int = match int {
        "I32" => IntType::I32,
        "I64" => IntType::I64,
        _ => panic!("unknown int type"),
    };
    generator.float = match float {
        "F32" => FloatType::F32,
        "F64" => FloatType::F64,
        _ => panic!("unknown float type"),
    };
    generator.validate_numeric_literals()
}

pub fn module(
    module: &TypedModule,
    line_numbers: &LineNumbers,
    all_modules: &HashMap<EcoString, &TypedModule>,
    all_line_numbers: &HashMap<EcoString, LineNumbers>,
) -> Result<Vec<u8>, Error> {
    module_with_config(
        module,
        line_numbers,
        all_modules,
        all_line_numbers,
        "I32",
        "F64",
    )
}

pub(crate) fn module_with_config(
    module: &TypedModule,
    line_numbers: &LineNumbers,
    all_modules: &HashMap<EcoString, &TypedModule>,
    all_line_numbers: &HashMap<EcoString, LineNumbers>,
    int: &str,
    float: &str,
) -> Result<Vec<u8>, Error> {
    let mut generator = Generator::new(module, line_numbers, all_modules, all_line_numbers);
    generator.int = match int {
        "I32" => IntType::I32,
        "I64" => IntType::I64,
        _ => panic!("unknown int type: {int}"),
    };
    generator.float = match float {
        "F32" => FloatType::F32,
        "F64" => FloatType::F64,
        _ => panic!("unknown float type: {float}"),
    };
    generator.compile()
}

fn eliminate_dead_code(wasm: Vec<u8>, builtin_data_names: &[String]) -> Vec<u8> {
    let mut module =
        walrus::Module::from_buffer(&wasm).expect("generated wasm to be a valid module");
    // Clear element segments so declared functions are not treated as roots.
    for elem in module.elements.iter().map(|e| e.id()).collect::<Vec<_>>() {
        module.elements.delete(elem);
    }
    // Remove memory export before GC so unused memory is not a root.
    let memory_export_items: Vec<_> = module
        .exports
        .iter()
        .filter(|e| matches!(e.item, walrus::ExportItem::Memory(_)))
        .map(|e| (e.id(), e.item))
        .collect();
    for &(id, _) in &memory_export_items {
        module.exports.delete(id);
    }
    // Convert active data to passive before GC so memory is not rooted.
    // Save the original kind to restore selectively after GC.
    let active_data: Vec<_> = module
        .data
        .iter()
        .filter(|d| !matches!(d.kind, walrus::DataKind::Passive))
        .enumerate()
        .map(|(idx, d)| {
            let kind = match &d.kind {
                walrus::DataKind::Active { memory, offset } => Some((*memory, offset.clone())),
                _ => None,
            };
            let name = builtin_data_names.get(idx).cloned();
            (d.id(), name, kind)
        })
        .collect();
    for &(id, _, _) in &active_data {
        module.data.get_mut(id).kind = walrus::DataKind::Passive;
    }
    walrus::passes::gc::run(&mut module);
    // Re-add memory export and restore only needed data segments.
    let surviving_mem = module.memories.iter().next().map(|m| m.id());
    if let Some(mem) = surviving_mem {
        for (_, item) in memory_export_items {
            let _ = module.exports.add("memory", item);
        }
        // Determine which builtin functions survived GC.
        let surviving_names: HashSet<&str> = module
            .funcs
            .iter()
            .filter_map(|f| f.name.as_deref())
            .collect();
        for (id, ref name, kind_info) in active_data {
            if let Some((_, offset)) = kind_info {
                // Only restore if the data segment survived GC and is
                // needed by a surviving function group.
                if module.data.iter().any(|d| d.id() == id)
                    && is_data_segment_needed(name, &surviving_names)
                {
                    module.data.get_mut(id).kind = walrus::DataKind::Active {
                        memory: mem,
                        offset,
                    };
                } else if module.data.iter().any(|d| d.id() == id) {
                    module.data.delete(id);
                }
            }
        }
        // If no active data segments remain and no imports need memory,
        // remove memory and its export.
        let has_active_data = module
            .data
            .iter()
            .any(|d| !matches!(d.kind, walrus::DataKind::Passive));
        if !has_active_data && module.imports.iter().next().is_none() {
            for id in module
                .exports
                .iter()
                .filter(|e| matches!(e.item, walrus::ExportItem::Memory(_)))
                .map(|e| e.id())
                .collect::<Vec<_>>()
            {
                module.exports.delete(id);
            }
            for id in module.memories.iter().map(|m| m.id()).collect::<Vec<_>>() {
                module.memories.delete(id);
            }
        }
    }
    module.emit_wasm()
}

/// Check if a data segment is needed based on its name and surviving functions.
fn is_data_segment_needed(name: &Option<String>, surviving: &HashSet<&str>) -> bool {
    let name = match name {
        Some(n) => n,
        // No name → be conservative, keep it.
        None => return true,
    };
    // Named dtoa segments (DEC_DIGITS_LUT is shared with itoa via LTO dedup)
    if name.contains("dtoa") {
        return surviving.contains("_f32_to_str")
            || surviving.contains("_f64_to_str")
            || surviving.contains("_i32_to_str")
            || surviving.contains("_i64_to_str");
    }
    // Named fast_float segments
    if name.contains("fast_float") {
        return surviving.contains("_f32_parse") || surviving.contains("_f64_parse");
    }
    // Named itoa segments
    if name.contains("itoa") {
        return surviving.contains("_i32_to_str") || surviving.contains("_i64_to_str");
    }
    // Named lexical_core segments
    if name.contains("lexical") {
        return surviving.contains("_i32_parse") || surviving.contains("_i64_parse");
    }
    // Anonymous segments — keep if any builtin using memory survived
    true
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
    Struct(Vec<(EcoString, ValType)>),
    Union(Vec<(EcoString, ValType)>, Option<u32>),
}

#[derive(Clone)]
enum WasmConst {
    String {
        dest: u32,
        src: u32,
    },
    Constant {
        global_index: u32,
        value: Box<TypedConstant>,
    },
    Struct {
        global_index: u32,
        type_index: u32,
        tag: Option<i32>,
        elements: Vec<TypedConstant>,
    },
    Function {
        dest: u32,
        src: u32,
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

#[derive(Debug, Clone)]
enum CustomType {
    External {
        val_type: ValType,
        to_str: &'static str,
    },
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
        match self {
            CustomType::Struct { .. } => true,
            CustomType::Union { custom_type } => Generator::null_variant_tag(custom_type).is_none(),
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
struct Variant {
    custom_type: CustomType,
    constructor: TypedRecordConstructor,
    tag: Option<i32>,
}

#[derive(Clone)]
struct LocalFunction {
    location: SrcSpan,
    parent_id: u32,
    type_: Arc<Type>,
    arguments: Vec<TypedArg>,
    body: Vec<TypedStatement>,
}

struct Generator<'a> {
    global_section: GlobalSection,
    import_section: ImportSection,
    export_section: ExportSection,
    data_section: DataSection,
    /// Names of builtin data segments, indexed by segment index.
    builtin_data_names: Vec<String>,
    global_names: NameMap,
    wasm_types: IndexMap<WasmType, u32>,
    types: HashMap<(EcoString, EcoString), CustomType>,
    variants: HashMap<(EcoString, EcoString), Variant>,
    functions: BTreeSet<WasmFunction>,
    function_next_id: u32,
    local_functions: HashMap<EcoString, LocalFunction>,
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
    all_modules: &'a HashMap<EcoString, &'a TypedModule>,
    all_line_numbers: &'a HashMap<EcoString, LineNumbers>,
    /// The module currently being compiled (may differ from self.module for imports).
    current_module_name: EcoString,
}

impl<'a> Generator<'a> {
    fn new(
        module: &'a TypedModule,
        line_numbers: &'a LineNumbers,
        all_modules: &'a HashMap<EcoString, &'a TypedModule>,
        all_line_numbers: &'a HashMap<EcoString, LineNumbers>,
    ) -> Self {
        Generator {
            global_section: GlobalSection::new(),
            import_section: ImportSection::new(),
            export_section: ExportSection::new(),
            data_section: DataSection::new(),
            builtin_data_names: Vec::new(),
            global_names: NameMap::new(),
            wasm_types: IndexMap::new(),
            types: HashMap::new(),
            variants: HashMap::new(),
            functions: BTreeSet::new(),
            function_next_id: 0,
            local_functions: HashMap::new(),
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
            all_modules,
            all_line_numbers,
            current_module_name: module.name.clone(),
        }
    }

    /// Get the source location for a byte offset, using the correct module
    /// name and line numbers for the currently-compiled module.
    fn source_location(&self, start: u32) -> (EcoString, u32) {
        if let Some(ln) = self.all_line_numbers.get(self.current_module_name.as_str()) {
            (self.current_module_name.clone(), ln.line_number(start))
        } else {
            (
                self.module.name.clone(),
                self.line_numbers.line_number(start),
            )
        }
    }

    fn validate_numeric_literals(&self) -> Result<(), Error> {
        struct Validator {
            int: IntType,
            float: FloatType,
            error: Option<Error>,
        }

        impl<'ast> Visit<'ast> for Validator {
            fn visit_typed_expr_int(
                &mut self,
                location: &'ast SrcSpan,
                _type_: &'ast Arc<Type>,
                value: &'ast EcoString,
            ) {
                if self.error.is_some() {
                    return;
                }
                let Some(int_value) = crate::parse::parse_int_value(value) else {
                    return;
                };
                let err = match self.int {
                    IntType::I32 => i32::try_from(&int_value).err().map(|_| ()),
                    IntType::I64 => i64::try_from(&int_value).err().map(|_| ()),
                };
                if err.is_some() {
                    self.error = Some(Error::IntLiteralOutOfRange {
                        location: *location,
                        value: int_value.to_string().into(),
                        target: match self.int {
                            IntType::I32 => "I32".into(),
                            IntType::I64 => "I64".into(),
                        },
                    });
                }
            }

            fn visit_typed_expr_float(
                &mut self,
                location: &'ast SrcSpan,
                _type_: &'ast Arc<Type>,
                value: &'ast EcoString,
            ) {
                if self.error.is_some() {
                    return;
                }
                let clean = value.replace("_", "");
                let err = match self.float {
                    FloatType::F32 => clean
                        .parse::<f32>()
                        .ok()
                        .filter(|f| !f.is_infinite())
                        .is_none(),
                    FloatType::F64 => clean
                        .parse::<f64>()
                        .ok()
                        .filter(|f| !f.is_infinite())
                        .is_none(),
                };
                if err {
                    self.error = Some(Error::FloatLiteralOutOfRange {
                        location: *location,
                        value: value.clone(),
                        target: match self.float {
                            FloatType::F32 => "F32".into(),
                            FloatType::F64 => "F64".into(),
                        },
                    });
                }
            }
        }

        let mut v = Validator {
            int: self.int,
            float: self.float,
            error: None,
        };
        v.visit_typed_module(self.module);
        match v.error {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }

    fn compile(mut self) -> Result<Vec<u8>, Error> {
        let start = self.generate()?;

        let mut module = Module::default();

        // type section
        let mut type_section = TypeSection::new();
        for (type_, _index) in &self.wasm_types {
            match &type_.kind {
                WasmTypeKind::Array(storage_type) => type_section.ty().array(storage_type, true),
                WasmTypeKind::Function(params, results) => {
                    type_section.ty().function(params.clone(), results.clone())
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
        let _ = module.section(&self.import_section);

        // function section
        let mut function_section = FunctionSection::new();
        for function in &self.functions {
            let _ = function_section.function(function.type_index);
        }
        let _ = module.section(&function_section);

        // memory section (only needed when builtins use linear memory)
        if !self.import_section.is_empty() {
            let mut memory_section = MemorySection::new();
            let _ = memory_section.memory(MemoryType {
                minimum: 17,
                maximum: None,
                memory64: false,
                shared: false,
                page_size_log2: None,
            });
            let _ = module.section(&memory_section);
        }

        // global section
        let _ = module.section(&self.global_section);

        // export section
        for function in self.functions.iter().filter(|f| f.export) {
            let _ = self
                .export_section
                .export(&function.name, ExportKind::Func, function.index);
        }
        let _ = module.section(&self.export_section);

        // start section
        let _ = module.section(&StartSection {
            function_index: start,
        });

        // element section
        let mut element_section = ElementSection::new();
        let _ = element_section.declared(Elements::Functions(
            self.functions.iter().map(|f| f.index).collect(),
        ));
        let _ = module.section(&element_section);

        // data count section
        let _ = module.section(&DataCountSection {
            count: self.data_section.len(),
        });

        // code section
        let mut codes_section = CodeSection::new();
        for function in &self.functions {
            let _ = codes_section.raw(&function.code);
        }
        let _ = module.section(&codes_section);

        // data section
        let _ = module.section(&self.data_section);

        // name section
        let mut names = NameSection::new();
        names.module(&self.module.name);

        // name section / function names
        let mut function_names = NameMap::new();
        for function in &self.functions {
            function_names.append(function.index, &function.name);
        }
        names.functions(&function_names);

        // name section / type names
        let mut type_names = NameMap::new();
        for (wasm_type, index) in &self.wasm_types {
            if let Some(name) = &wasm_type.name {
                type_names.append(*index, name);
            }
        }
        names.types(&type_names);

        // name section / global names
        names.globals(&self.global_names);

        // name section / local names
        let mut locals = IndirectNameMap::new();
        for function in &self.functions {
            let mut name_map = NameMap::new();
            for (index, name) in &function.locals {
                name_map.append(*index, name);
            }
            locals.append(function.index, &name_map);
        }
        names.locals(&locals);

        // name section / field names
        let mut fields = IndirectNameMap::new();
        for (type_, index) in &self.wasm_types {
            let mut name_map = NameMap::new();
            match &type_.kind {
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
        let wasm = module.finish();
        Ok(eliminate_dead_code(wasm, &self.builtin_data_names))
    }

    fn generate(&mut self) -> Result<u32, Error> {
        // Validate all numeric literals fit in the target types before codegen.
        self.validate_numeric_literals()?;

        // External types, like I32, must come first because of the external function type checking.
        // Externals functions come first because of the indexes in the builtin wasm file.
        self.types_external()?;
        let builtins = self.functions_external()?;
        self.types_prelude();
        self.functions_builtins(builtins);
        self.types();
        self.types_imported();
        self.constants();
        self.functions();
        Ok(self.function_start())
    }

    fn functions_builtins(
        &mut self,
        builtins: Vec<(BuiltinFunctionExternal, EcoString, Arc<Type>)>,
    ) {
        for (builtin, _name, type_) in &builtins {
            let i32 = self.extract_external_type(builtin, type_);
            let (arguments, result) = builtin.type_(i32);
            let expected = type_::fn_(arguments, result);
            if !expected.same_as(type_) {
                panic!(
                    "{expected:#?}\n{type_:#?}\n{} != {}",
                    self.type_pretty_name(&expected),
                    self.type_pretty_name(type_)
                );
            }
        }
        for (builtin, name, _) in builtins {
            let index = self.get_function_builtin_external(builtin);
            let _ = self.add_function_to_globals(name, index);
        }
    }

    /// Extract the actual external type from a builtin function's Gleam type signature.
    /// Returns a dummy type for builtins that don't use external types.
    fn extract_external_type(
        &self,
        builtin: &BuiltinFunctionExternal,
        fn_type: &Arc<Type>,
    ) -> Arc<Type> {
        let (params, return_) = fn_type.fn_types().expect("function type");
        match builtin {
            // For builtins that return I32, use the return type
            BuiltinFunctionExternal::IntToI32
            | BuiltinFunctionExternal::IntToI64
            | BuiltinFunctionExternal::FloatToF32
            | BuiltinFunctionExternal::FloatToF64
            | BuiltinFunctionExternal::IntRepr
            | BuiltinFunctionExternal::FloatRepr
            | BuiltinFunctionExternal::UtfCodepointRepr
            | BuiltinFunctionExternal::StringRepr
            | BuiltinFunctionExternal::StringToMemory => return_,
            // For builtins that take an external type as first param
            BuiltinFunctionExternal::I32ToInt
            | BuiltinFunctionExternal::I64ToInt
            | BuiltinFunctionExternal::F32ToFloat
            | BuiltinFunctionExternal::F64ToFloat
            | BuiltinFunctionExternal::MemoryToString => params
                .into_iter()
                .next()
                .expect("builtin to have at least one parameter"),
            // Builtins without I32 — return a dummy (unused by type_())
            _ => type_::int(),
        }
    }

    /// Find any External type from the types registry and construct an Arc<Type> for it.
    fn find_external_type(&self) -> Arc<Type> {
        self.types
            .iter()
            .find(|(_, ct)| matches!(ct, CustomType::External { .. }))
            .map(|((module, name), _)| type_::named("", module, name, Publicity::Private, vec![]))
            .unwrap_or_else(|| {
                type_::named("", &self.module.name, "I32", Publicity::Private, vec![])
            })
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
        self.find_global(name)
            .unwrap_or_else(|| panic!("global \"{name}\" to be registered"))
    }

    fn builtin_type(&self, function: &BuiltinFunction) -> (Vec<ValType>, Vec<ValType>) {
        match function {
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
            BuiltinFunction::Inspect(val_type, _) => {
                (vec![*val_type], vec![self.string.val_type()])
            }
            BuiltinFunction::StringStartsWith => {
                let s = self.string.val_type();
                (vec![s, s], vec![BOOL_VALTYPE])
            }
        }
    }

    fn null_variant_tag(custom_type: &TypedCustomType) -> Option<usize> {
        let zero_arg_count = custom_type
            .constructors
            .iter()
            .filter(|c| c.arguments.is_empty())
            .count();
        if zero_arg_count == 1 {
            custom_type
                .constructors
                .iter()
                .position(|c| c.arguments.is_empty())
        } else {
            None
        }
    }

    fn is_external_type(&self, type_: &Arc<Type>) -> bool {
        if let Some((module, name, _)) = type_.named_type_information() {
            matches!(
                self.types.get(&(module, name)),
                Some(CustomType::External { .. })
            )
        } else {
            false
        }
    }

    pub(super) fn type_pretty_name(&self, type_: &Arc<Type>) -> EcoString {
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

    fn types_external(&mut self) -> Result<(), Error> {
        for custom_type in &self.module.definitions.custom_types {
            let type_ = if let Some((module, name, span)) = &custom_type.external_webassembly {
                assert!(custom_type.constructors.is_empty());
                if module != BUILTINS_MODULE {
                    return Err(Error::UnknownExternalModule {
                        location: *span,
                        module: module.clone(),
                    });
                }
                match name.as_str() {
                    "I32" => CustomType::External {
                        val_type: ValType::I32,
                        to_str: I32_TO_STR,
                    },
                    "I64" => CustomType::External {
                        val_type: ValType::I64,
                        to_str: I64_TO_STR,
                    },
                    "F32" => CustomType::External {
                        val_type: ValType::F32,
                        to_str: F32_TO_STR,
                    },
                    "F64" => CustomType::External {
                        val_type: ValType::F64,
                        to_str: F64_TO_STR,
                    },
                    _ => {
                        return Err(Error::UnknownExternalType {
                            location: *span,
                            name: name.clone(),
                        });
                    }
                }
            } else if custom_type.name == "I32" {
                CustomType::External {
                    val_type: ValType::I32,
                    to_str: I32_TO_STR,
                }
            } else {
                continue;
            };
            let _ = self
                .types
                .insert((self.module.name.clone(), custom_type.name.clone()), type_);
        }
        Ok(())
    }

    fn functions_external(
        &mut self,
    ) -> Result<Vec<(BuiltinFunctionExternal, EcoString, Arc<Type>)>, Error> {
        let builtins = parse_builtins(BUILTINS_WASM);

        // Add imports
        for (module, name, type_index) in &builtins.imports {
            let (params, results) = builtins
                .types
                .get(*type_index)
                .expect("import type at index")
                .clone();
            let type_index = self.function_type_index_with_val_types(params, results);
            let _ = self
                .import_section
                .import(module, name, EntityType::Function(type_index));
            self.function_next_id += 1;
        }

        // Add globals
        for &(val_type, mutable, shared, init_value) in &builtins.globals {
            let _ = self.global_section.global(
                GlobalType {
                    val_type,
                    mutable,
                    shared,
                },
                &ConstExpr::i32_const(init_value),
            );
        }

        // Add exports (func and global exports are already excluded)
        for (name, kind, index) in &builtins.exports {
            if *kind == ExportKind::Memory && self.import_section.is_empty() {
                continue;
            }
            let _ = self.export_section.export(name, *kind, *index);
        }

        // Add data segments
        for segment in &builtins.data_segments {
            match segment {
                DataSegment::Passive(data) => {
                    let _ = self.data_section.passive(data.iter().cloned());
                }
                DataSegment::Active {
                    memory_index,
                    offset_i32,
                    data,
                } => {
                    let _ = self.data_section.active(
                        *memory_index,
                        &ConstExpr::i32_const(*offset_i32),
                        data.iter().cloned(),
                    );
                }
            }
        }

        // Add functions
        for (name, params, results, code, original_index) in &builtins.functions {
            let _ = self.add_function(
                name.clone(),
                false,
                params.clone(),
                results.clone(),
                code.clone(),
            );
            let _ = self.add_function_to_globals(name.clone(), *original_index);
        }

        // Add global names
        for (index, name) in &builtins.global_names {
            self.global_names.append(*index, name);
        }

        // Add data segment names
        self.builtin_data_names = builtins.data_names;

        // Validate external function signatures and collect needed builtins
        self.validate_externals(&builtins.available)
    }

    fn validate_externals(
        &mut self,
        available: &HashMap<EcoString, (Vec<ValType>, Vec<ValType>)>,
    ) -> Result<Vec<(BuiltinFunctionExternal, EcoString, Arc<Type>)>, Error> {
        let mut builtins = vec![];
        let functions: Vec<_> = self.module.definitions.functions.iter().collect();
        for function in functions {
            if let Some((module, name, _)) = &function.external_webassembly {
                if module != BUILTINS_MODULE {
                    return Err(Error::UnknownExternalModule {
                        location: function.location,
                        module: module.clone(),
                    });
                }
                if let Some((params, results)) = available.get(name) {
                    let wasm_params =
                        self.val_types(function.arguments.iter().map(|arg| arg.type_.clone()));
                    let wasm_results = self.val_types(iter::once(function.return_type.clone()));
                    if !function
                        .arguments
                        .iter()
                        .all(|arg| self.is_external_type(&arg.type_))
                        || !self.is_external_type(&function.return_type)
                        || (&wasm_params, &wasm_results) != (params, results)
                    {
                        return Err(Error::WrongBuiltinFunctionSignature {
                            location: function.location,
                            name: name.clone(),
                            expected: format!(
                                "fn({}) -> {}",
                                params.iter().map(|p| format!("{p:?}")).join(", "),
                                results.iter().map(|r| format!("{r:?}")).join(", "),
                            )
                            .into(),
                            got: self.type_pretty_name(&function_type(function)).clone(),
                        });
                    }
                } else if let Some(builtin) = BuiltinFunctionExternal::by_name(name) {
                    builtins.push((builtin, name.clone(), function_type(function)));
                } else if name == INSPECT {
                    if !(function.arguments.len() == 1
                        && function
                            .arguments
                            .first()
                            .is_some_and(|arg| is_generic_type(&arg.type_))
                        && function.return_type.is_string())
                    {
                        return Err(Error::WrongBuiltinFunctionSignature {
                            location: function.location,
                            name: name.clone(),
                            expected: "fn(a) -> String".into(),
                            got: self.type_pretty_name(&function_type(function)).clone(),
                        });
                    }
                } else {
                    return Err(Error::UnknownBuiltinFunction {
                        location: function.location,
                        name: name.clone(),
                    });
                }
            }
        }
        Ok(builtins)
    }

    fn types(&mut self) {
        self.add_module_types(self.module);
    }

    fn types_imported(&mut self) {
        let mut visited = HashSet::new();
        let _ = visited.insert(self.module.name.clone());
        let mut queue: Vec<EcoString> = self
            .module
            .definitions
            .imports
            .iter()
            .map(|i| i.module.clone())
            .collect();

        while let Some(name) = queue.pop() {
            if !visited.insert(name.clone()) {
                continue;
            }
            let module = self.module_imported(&name);
            let next_imports: Vec<EcoString> = module
                .definitions
                .imports
                .iter()
                .map(|i| i.module.clone())
                .collect();
            self.add_module_types(module);
            queue.extend(next_imports);
        }
    }

    fn module_imported(&self, name: &EcoString) -> &'a TypedModule {
        self.all_modules
            .get(name)
            .unwrap_or_else(|| panic!("imported module not found during code generation: {name}"))
    }

    fn add_module_types(&mut self, module: &TypedModule) {
        fn is_external(custom_type: &TypedCustomType) -> bool {
            custom_type.external_webassembly.is_some() || custom_type.constructors.is_empty()
        }

        self.add_custom_types(
            module
                .definitions
                .custom_types
                .iter()
                .filter(|t| !is_external(t))
                .map(|c| (c, module.name.as_str())),
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
                let export =
                    custom_type.publicity.is_public() && module_name != PRELUDE_MODULE_NAME;
                for (value, constructor) in custom_type.constructors.iter().enumerate() {
                    let _ = self.add_const(
                        &constructor.name,
                        ValType::I32,
                        ConstExpr::i32_const(value as i32),
                        export,
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
                    (module_name.into(), constructor.name.clone()),
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

        let type_ok = type_::generic_var(OK_GENERIC_ID);
        let type_err = type_::generic_var(ERROR_GENERIC_ID);
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

        let type_item = type_::generic_var(LIST_ITEM_GENERIC_ID);
        let list = custom_type(
            "List",
            vec![
                record_constructor("Empty", vec![]),
                record_constructor(
                    "Cons",
                    vec![
                        record_constructor_arg(ast_var("rest"), type_::list(type_item.clone())),
                        record_constructor_arg(ast_var("first"), type_item.clone()),
                    ],
                ),
            ],
            vec![(Default::default(), "a".into())],
            vec![type_item],
        );

        vec![nil, bool_, result, list]
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

    fn custom_type_expect(&self, type_: &Arc<Type>) -> (CustomType, Vec<Arc<Type>>) {
        self.custom_type(type_)
            .unwrap_or_else(|| panic!("custom type to be registered for {type_:?}"))
    }

    fn list_type_cons(
        &self,
        list_type: &Arc<Type>,
    ) -> (
        TypedCustomType,
        TypedRecordConstructor,
        Arc<Type>,
        Vec<Arc<Type>>,
    ) {
        let (custom_type, args) = self.custom_type_expect(list_type);
        let CustomType::Union { custom_type } = custom_type else {
            panic!("list type to be a union")
        };
        let cons = custom_type
            .constructors
            .get(1)
            .expect("Cons constructor for list type")
            .clone();
        let item_type = args.first().expect("list item type").clone();
        (custom_type, cons, item_type, args)
    }

    fn variant_expect(&self, module: EcoString, name: EcoString) -> Variant {
        self.variants
            .get(&(module, name))
            .expect("variant to be registered")
            .clone()
    }

    fn enum_value_index(values: &[EcoString], name: &EcoString) -> usize {
        values
            .iter()
            .position(|v| v == name)
            .expect("enum value to exist")
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
                let id = self.function(function, true, function_name(function).into());
                if is_main_funtion(function) {
                    self.main = Some(id.index)
                }
            }
            // Private, generic, and external functions are compiled elsewhere.
        }
    }

    fn val_type(&mut self, type_: &Arc<Type>) -> ValType {
        if type_.is_utf_codepoint() {
            ValType::I32
        } else if type_.is_int() {
            self.int.val_type()
        } else if type_.is_float() {
            self.float.val_type()
        } else if type_.is_string() {
            self.string.val_type()
        } else if let Some(types) = type_.tuple_types() {
            let type_index = self.tuple_type_index(types);
            self.composite_val_type(type_index)
        } else if let Some((params, return_)) = type_.fn_types() {
            let type_index = self.function_type_index(params, Some(return_));
            self.function_val_type(type_index)
        } else if let Some((custom_type, args)) = self.custom_type(type_) {
            self.custom_type_val_type(type_, &custom_type, args)
        } else if let Some((_, name)) = type_.named_type_name() {
            match name.as_str() {
                "I32" => ValType::I32,
                "I64" => ValType::I64,
                "F32" => ValType::F32,
                "F64" => ValType::F64,
                _ => panic!("unexpected named type should not reach code generation: {name}"),
            }
        } else if type_.is_bit_array() {
            todo!("BitArray is not yet supported");
        } else {
            panic!("unexpected type should not reach code generation: {type_:#?}");
        }
    }

    pub(super) fn val_types(&mut self, types: impl IntoIterator<Item = Arc<Type>>) -> Vec<ValType> {
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

    fn type_index(&mut self, type_: &Arc<Type>) -> u32 {
        if type_.is_string() {
            self.string.type_index
        } else if let Some(types) = type_.tuple_types() {
            self.tuple_type_index(types)
        } else if let Some((params, return_)) = type_.fn_types() {
            self.function_type_index(params, Some(return_))
        } else if let Some((custom_type, args)) = self.custom_type(type_) {
            match custom_type {
                CustomType::External { .. } | CustomType::Enum { .. } => {
                    panic!("external/enum types should not reach code generation")
                }
                CustomType::Struct {
                    custom_type,
                    constructor,
                } => {
                    let (type_index, _) =
                        self.mono_struct_type_index(type_, &custom_type, &constructor, &args);
                    type_index
                }
                CustomType::Union { custom_type } => {
                    if let Some(constructor) = custom_type_inferred_constructor(&custom_type, type_)
                    {
                        let (_, type_index, _) =
                            self.mono_union_subtype_index(type_, &custom_type, constructor, &args);
                        type_index
                    } else {
                        self.mono_union_supertype_index(type_, &custom_type)
                    }
                }
            }
        } else {
            panic!("unexpected type should not reach code generation: {type_:?}");
        }
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
        let (name, fields, types) = if let Some(constructor) = constructor {
            let mut name = self.type_pretty_name(type_);
            name += ".";
            name += constructor.name.clone();
            let types = Monomorphizer::variant_constructor(custom_type, constructor, args);
            (name, self.fields(constructor, &types), types)
        } else {
            // Supertype only has the discriminant (tag) — shared across
            // all monomorphizations of this generic union type.
            (custom_type.name.clone(), vec![], vec![])
        };

        let index = self.wasm_types.len() as u32;
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
            CustomType::External { val_type, .. } => *val_type,
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
                if Self::null_variant_tag(custom_type).is_some() {
                    self.val_type_ref_nullable(struct_index)
                } else {
                    self.composite_val_type(struct_index)
                }
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
                let src = self.string_index(value);
                self.consts.push(WasmConst::String {
                    dest: id.index,
                    src,
                });
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
                    tag: None,
                    elements: elements.clone(),
                });
                id
            }
            Constant::List { type_, .. } => {
                let (custom_type, _) = self.custom_type_expect(type_);
                let CustomType::Union { custom_type } = custom_type else {
                    panic!("list type should be a union")
                };
                let supertype_index = self.mono_union_supertype_index(type_, &custom_type);
                let val_type = self.val_type_ref_nullable(supertype_index);
                let id = self.add_const(
                    const_name,
                    val_type,
                    const_expr_ref_null(supertype_index),
                    export,
                    true,
                );
                self.consts.push(WasmConst::Constant {
                    global_index: id.index,
                    value: Box::new(module_constant.value.as_ref().clone()),
                });
                id
            }
            Constant::Record {
                type_,
                arguments,
                name,
                ..
            } => {
                let (custom_type, args) = self.custom_type_expect(type_);
                match custom_type {
                    CustomType::Enum { values } => {
                        assert!(arguments.is_empty());
                        let value = Self::enum_value_index(&values, name);
                        self.add_const(
                            const_name,
                            ValType::I32,
                            ConstExpr::i32_const(value as i32),
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
                            tag: None,
                            elements: arguments.iter().map(|e| &e.value).cloned().collect(),
                        });
                        id
                    }
                    CustomType::Union { custom_type } => {
                        let index = type_
                            .custom_type_inferred_variant()
                            .expect("inferred variant index");
                        let constructor = custom_type
                            .constructors
                            .get(index as usize)
                            .expect("constructor at index");
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
                            tag: Some(index.into()),
                            elements: arguments.iter().map(|e| &e.value).cloned().collect(),
                        });
                        id
                    }
                    CustomType::External { .. } => {
                        panic!("external types should not reach code generation")
                    }
                }
            }
            Constant::Var { name, type_, .. } => {
                let (expr, val_type) = if type_.is_int() {
                    (self.int.int_const(&0.into()), self.int.val_type())
                } else if type_.is_float() {
                    (self.float.float_const(&"0".into()), self.float.val_type())
                } else if let Some((CustomType::External { val_type, .. }, _)) =
                    self.custom_type(type_)
                {
                    (ConstExpr::i32_const(0), val_type)
                } else if let Some((CustomType::Enum { .. }, _)) = self.custom_type(type_) {
                    (ConstExpr::i32_const(0), ValType::I32)
                } else {
                    let type_index = self.type_index(type_);
                    (
                        const_expr_ref_null(type_index),
                        self.val_type_ref_nullable(type_index),
                    )
                };
                let id = self.add_const(const_name, val_type, expr, export, true);
                let var_id = self.var_id(&Scope::Global(self.globals.clone()), name, type_);
                if let IdKind::Func = var_id.kind {
                    self.consts.push(WasmConst::Function {
                        dest: id.index,
                        src: var_id.index,
                    });
                } else {
                    self.consts.push(WasmConst::Var {
                        global_index: id.index,
                        name: name.clone(),
                    });
                }
                id
            }
            Constant::BitArray { .. } => todo!("BitArray constants are not yet supported"),
            Constant::StringConcatenation { .. } => {
                let id = self.add_const(
                    const_name,
                    self.string.val_type_nullable(),
                    ConstExpr::ref_null(self.string.heap_type()),
                    export,
                    true,
                );
                self.consts.push(WasmConst::Constant {
                    global_index: id.index,
                    value: Box::new(module_constant.value.as_ref().clone()),
                });
                id
            }
            Constant::Invalid { .. } => {
                panic!("invalid constants should not reach code generation")
            }
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
            Constant::BitArray { .. } => todo!("BitArray constants are not yet supported"),
            Constant::StringConcatenation { left, right, .. } => {
                self.register_string_const(left);
                self.register_string_const(right);
            }
            Constant::Invalid { .. } => {
                panic!("invalid constants should not reach code generation")
            }
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
                let (custom_type, cons, item_type, _) = self.list_type_cons(type_);
                let supertype_index = self.mono_union_supertype_index(type_, &custom_type);
                let cons_variant =
                    self.variant_expect(PRELUDE_MODULE_NAME.into(), cons.name.clone());
                let cons_fn = self.variant_constructor(
                    vec![type_::list(item_type.clone()), item_type.clone()],
                    type_.clone(),
                    cons_variant,
                );
                let _ = instructions.ref_null(HeapType::Concrete(supertype_index));
                for element in elements.iter().rev() {
                    let _ = instructions.constant(self, element).call(cons_fn);
                }
            }
            Constant::Tuple { elements, .. } => {
                let type_index = self.tuple_type_index(elements.iter().map(|e| e.type_()));
                let _ = instructions
                    .constants(self, elements)
                    .struct_new(type_index);
            }
            Constant::Record {
                name,
                arguments,
                type_,
                ..
            } => {
                if let Some((CustomType::Enum { values }, _)) = self.custom_type(type_) {
                    let value = Self::enum_value_index(&values, name);
                    let _ = instructions.i32_const(value as i32);
                } else {
                    let (module, _, _) = type_
                        .named_type_information()
                        .expect("named type information");
                    let variant = self.variant_expect(module, name.clone());
                    let id = self.variant_constructor(
                        arguments.iter().map(|arg| arg.value.type_()).collect(),
                        type_.clone(),
                        variant,
                    );
                    let _ = instructions
                        .constants(self, arguments.iter().map(|arg| &arg.value))
                        .call(id);
                }
            }
            Constant::Var { name, type_, .. } => {
                let scope = Scope::Global(self.globals.clone());
                self.expression_var(&scope, instructions, name, type_);
            }
            Constant::BitArray { .. } => todo!("BitArray constants are not yet supported"),
            Constant::StringConcatenation { left, right, .. } => {
                let concat =
                    self.get_function_builtin_external(BuiltinFunctionExternal::StringConcat);
                let _ = instructions
                    .constant(self, left)
                    .constant(self, right)
                    .call(concat);
            }
            Constant::Invalid { .. } => {
                panic!("invalid constants should not reach code generation")
            }
        }
    }

    fn var(&mut self, name: &EcoString, required_type: &Arc<Type>) -> Id {
        if is_generic_type(required_type) {
            panic!("generic type should not reach code generation: {required_type:#?}");
        }

        for module_constant in &self.module.definitions.constants {
            if &module_constant.name == name {
                assert!(required_type.same_as(&module_constant.type_));
                return self.module_constant(module_constant);
            }
        }

        for function in &self.module.definitions.functions {
            if function_name(function) == name {
                if let Some((_, fname, _)) = &function.external_webassembly {
                    if fname == INSPECT {
                        return self.function_inspect(required_type);
                    }
                    return self.find_global_expect(fname);
                }
                let export = function.publicity.is_public();
                let base_name = function_name(function).into();
                return if is_generic_type(&function_type(function)) {
                    self.function_generic(function, required_type, export, base_name)
                } else {
                    self.function(function, export, base_name)
                };
            }
        }

        todo!(
            "Name not found: {:?}. Are you using closures? They are not supported yet.",
            name
        );
    }

    fn var_id(&mut self, scope: &Scope, name: &EcoString, type_: &Arc<Type>) -> Id {
        if let Some(id) = scope.find(name) {
            id
        } else if let Some(variant) = {
            let return_type = type_.fn_types().map_or(type_.clone(), |(_, r)| r);
            return_type
                .named_type_information()
                .and_then(|(module, _, _)| self.variants.get(&(module, name.clone())).cloned())
        } {
            let id = if let Some((params, return_)) = type_.fn_types() {
                self.variant_constructor(params, return_, variant)
            } else {
                self.variant_constructor(vec![], type_.clone(), variant)
            };
            Id::func(name.clone(), id)
        } else {
            self.var(name, type_)
        }
    }

    fn function_next_id(&mut self) -> u32 {
        let id = self.function_next_id;
        self.function_next_id += 1;
        id
    }

    fn _function(
        &mut self,
        function: TypedFunction,
        name: EcoString,
        export: bool,
        original_local_functions: HashMap<EcoString, LocalFunction>,
    ) -> Id {
        let index = self.function_next_id();
        let id = self.add_function_to_globals(name.clone(), index);
        let locals = Locals::new(self, &function.arguments, &function.body);
        let mut code = Function::new(locals.val_types());
        let mut instructions = code.extend_instructions(self);
        let saved_local_functions =
            std::mem::replace(&mut self.local_functions, original_local_functions);
        self.statements(
            &mut instructions,
            Scope::with_params(self.globals.clone(), &function.arguments),
            &locals,
            &function.body,
        );
        self.local_functions = saved_local_functions;
        let _ = instructions.end();

        let type_index = self.function_type_index(
            function_params_types(&function),
            Some(function.return_type.clone()),
        );
        let _ = self.functions.insert(WasmFunction {
            name,
            index,
            type_index,
            code: code.into_raw_body().into(),
            export,
            locals: locals.names(),
        });

        id
    }

    fn function(&mut self, function: &TypedFunction, export: bool, base_name: EcoString) -> Id {
        if let Some(id) = self.find_global(&base_name) {
            return id;
        }
        let original_local_functions =
            collect_local_functions(&function.body, self.function_next_id);
        // Resolve Generic type vars (from inner lambdas) to Nil for valid WASM types.
        let function = Monomorphizer::new().function(function);
        self._function(function, base_name, export, original_local_functions)
    }

    fn function_generic(
        &mut self,
        function: &TypedFunction,
        required_type: &Arc<Type>,
        export: bool,
        base_name: EcoString,
    ) -> Id {
        let name = self.mangle(&base_name, required_type);
        if let Some(id) = self.find_global(&name) {
            return id;
        }
        let original_local_functions =
            collect_local_functions(&function.body, self.function_next_id);
        let type_ = function_type(function);
        let mut function = Monomorphizer::with_bound(&type_, required_type).function(function);
        set_function_name(&mut function, name.clone());
        self._function(function, name, export, original_local_functions)
    }

    fn _function_local(
        &mut self,
        name: EcoString,
        type_: &Arc<Type>,
        arguments: &[TypedArg],
        body: &[TypedStatement],
    ) -> Id {
        let index = self.function_next_id();
        let id = self.add_function_to_globals(name.clone(), index);
        let locals = Locals::new(self, arguments, body);
        let mut code = Function::new(locals.val_types());
        let mut instructions = code.extend_instructions(self);
        let saved_local_functions = self.local_functions.clone();
        self.statements(
            &mut instructions,
            Scope::with_params(self.globals.clone(), arguments),
            &locals,
            body,
        );
        self.local_functions = saved_local_functions;
        let _ = instructions.end();

        let (params, return_) = type_.fn_types().expect("function type");
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
        self._function_local(name, type_, arguments, body)
    }

    fn function_local_generic(&mut self, info: &LocalFunction, required_type: &Arc<Type>) -> Id {
        let type_name = self.type_pretty_name(required_type).replace(" ", "");
        let name: EcoString = format!(
            "anonymous@{}-{}#{}:{type_name}",
            info.location.start, info.location.end, info.parent_id
        )
        .into();

        if let Some(id) = self.find_global(&name) {
            return id;
        }

        let (args, body) = Monomorphizer::function_local(info, required_type);
        self._function_local(name, required_type, &args, &body)
    }

    fn function_imported(
        &mut self,
        module_name: &EcoString,
        fn_name: &EcoString,
        required_type: &Arc<Type>,
    ) -> Id {
        let module = self.module_imported(module_name);

        for function in &module.definitions.functions {
            if function_name(function) == fn_name {
                if let Some((_, fname, _)) = &function.external_webassembly {
                    if fname == INSPECT {
                        return self.function_inspect(required_type);
                    }
                    return self.find_global_expect(fname);
                }
                // Track which module we're compiling for correct diagnostics.
                let prev_module =
                    std::mem::replace(&mut self.current_module_name, module_name.clone());
                // Build a module-qualified name to avoid collisions
                let base_name: EcoString = format!("{module_name}.{fn_name}").into();
                let id = if is_generic_type(&function_type(function)) {
                    self.function_generic(function, required_type, false, base_name)
                } else {
                    self.function(function, false, base_name)
                };
                self.current_module_name = prev_module;
                return id;
            }
        }

        panic!("imported function not found during code generation: {module_name}.{fn_name}");
    }

    fn statements(
        &mut self,
        instructions: &mut ExtendedInstructionSink<'_>,
        mut scope: Scope,
        locals: &Locals,
        statements: &[TypedStatement],
    ) {
        let mut remaining = statements;
        while let Some((statement, rest)) = remaining.split_first() {
            match statement {
                Statement::Assignment(assignment)
                    if matches!(assignment.kind, AssignmentKind::Assert { .. }) =>
                {
                    self.assignment_assert(locals, scope, instructions, assignment, rest);
                    return;
                }
                _ => {
                    scope = self.statement(instructions, scope, locals, statement);
                    if !rest.is_empty() {
                        let _ = instructions.drop();
                    }
                }
            }
            remaining = rest;
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
        let prefix = self.string_index(&"Assertion failed at ".into());
        let (mod_name, line) = self.source_location(assert.location.start);
        let location: EcoString = format!("src/{mod_name}.gleam:{line}.\n").into();
        let location = self.string_index(&location);
        let string_to_memory =
            self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
        let heap_base = self.find_global_expect(HEAP_BASE);
        let print = self.find_global_expect(PRINT);
        let exit = self.find_global_expect(EXIT);
        #[rustfmt::skip]
        let _ = instructions
            .expression(self, locals, scope.clone(), &assert.value)
            .if_(BlockType::Result(BOOL_VALTYPE))
              .bool_const(true)
            .else_()
              .show_error_message(prefix, location, string_to_memory, heap_base.index, print.index)
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
                let (custom_type, cons, item_type, args) = self.list_type_cons(type_);
                let supertype_index = self.mono_union_supertype_index(type_, &custom_type);
                let _ = self.mono_union_subtype_index(type_, &custom_type, &cons, &args);
                if let Some(rest) = tail {
                    let _ = instructions.expression(self, locals, scope.clone(), rest);
                } else {
                    let _ = instructions.ref_null(HeapType::Concrete(supertype_index));
                }
                let cons_variant =
                    self.variant_expect(PRELUDE_MODULE_NAME.into(), cons.name.clone());
                let cons_fn = self.variant_constructor(
                    vec![type_::list(item_type.clone()), item_type.clone()],
                    type_.clone(),
                    cons_variant,
                );
                for element in elements.iter().rev() {
                    // Stack: [rest]
                    // Cons(rest, first) — constructor takes (rest, first) and adds tag
                    let _ = instructions
                        .expression(self, locals, scope.clone(), element)
                        .call(cons_fn);
                }
            }
            TypedExpr::Tuple { elements, .. } => {
                let type_index = self.tuple_type_index(elements.iter().map(|e| e.type_()));
                let _ = instructions
                    .expressions(self, locals, scope.clone(), elements)
                    .struct_new(type_index);
            }
            TypedExpr::TupleIndex { index, tuple, .. } => {
                let type_index =
                    self.tuple_type_index(tuple.type_().tuple_types().expect("tuple type"));
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
            TypedExpr::Var {
                name, constructor, ..
            } => match &constructor.variant {
                ValueConstructorVariant::ModuleFn { module, name, .. }
                    if module.as_str() != self.module.name.as_str() =>
                {
                    let id = self.function_imported(module, name, &expression.type_());
                    let _ = instructions.ref_func(id.index);
                }
                ValueConstructorVariant::ModuleConstant { literal, .. } => {
                    self._constant(instructions, literal);
                }
                _ => {
                    self.expression_var(&scope, instructions, name, &expression.type_());
                }
            },
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
                let type_name = self.type_pretty_name(type_).replace(" ", "");
                let name: EcoString =
                    format!("anonymous@{}-{}:{type_name}", location.start, location.end).into();
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
                let (custom_type, args) = self.custom_type_expect(&record.type_());
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
                        let variant = record
                            .type_()
                            .custom_type_inferred_variant()
                            .expect("inferred variant index");
                        let constructor = custom_type
                            .constructors
                            .get(variant as usize)
                            .expect("constructor at index");
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
                    CustomType::External { .. } | CustomType::Enum { .. } => {
                        panic!("external/enum types should not reach code generation")
                    }
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
                let (custom_type, _) = self.custom_type_expect(type_);
                match custom_type {
                    CustomType::Struct { .. } | CustomType::Union { .. } => {
                        let (params, return_) =
                            constructor.type_().fn_types().expect("function type");
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
                    CustomType::External { .. } | CustomType::Enum { .. } => {
                        panic!("external/enum types should not reach code generation")
                    }
                }
            }
            TypedExpr::BitArray { .. } => todo!("BitArray expressions are not yet supported"),
            TypedExpr::ModuleSelect {
                constructor,
                module_name,
                label,
                ..
            } => match constructor {
                ModuleValueConstructor::Fn { .. } => {
                    let id = self.function_imported(module_name, label, &expression.type_());
                    let _ = instructions.ref_func(id.index);
                }
                ModuleValueConstructor::Record { name, .. } => {
                    self.expression_var(&scope, instructions, name, &expression.type_());
                }
                ModuleValueConstructor::Constant { literal, .. } => {
                    self._constant(instructions, literal);
                }
            },
            TypedExpr::Invalid { .. } => {
                panic!("invalid expressions should not reach code generation")
            }
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
        let prefix = self.string_index(&if expression.is_panic() {
            "panic at ".into()
        } else {
            "todo at ".into()
        });
        let (mod_name, line) = self.source_location(expression.location().start);
        let suffix = if message.is_none() { ".\n" } else { "\n  " };
        let location: EcoString = format!("src/{mod_name}.gleam:{line}{suffix}").into();
        let location = self.string_index(&location);
        let string_to_memory =
            self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
        let heap_base = self.find_global_expect(HEAP_BASE);
        let print = self.find_global_expect(PRINT);
        let exit = self.find_global_expect(EXIT);

        let _ = instructions.show_error_message(
            prefix,
            location,
            string_to_memory,
            heap_base.index,
            print.index,
        );

        if let Some(message) = message {
            assert!(message.type_().is_string());
            let _ = instructions
                .i32_const(STDERR)
                .call(heap_base.index)
                .expression(self, locals, scope, message)
                .call(heap_base.index)
                .call(string_to_memory)
                .call(print.index)
                .call(heap_base.index)
                .byte_store(b'\n')
                .i32_const(STDERR)
                .call(heap_base.index)
                .i32_const(1)
                .call(print.index);
        }

        let _ = instructions.i32_const(1).call(exit.index).unreachable();
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
                let (left, right) = locals.for_div(&scope, left, right);
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
                let (left, right) = locals.for_div(&scope, left, right);
                instructions.float_div(left, right)
            }
            BinOp::LtFloat => instructions.float_lt(),
            BinOp::LtEqFloat => instructions.float_le(),
            BinOp::GtFloat => instructions.float_gt(),
            BinOp::GtEqFloat => instructions.float_ge(),
            // String
            BinOp::Concatenate => {
                let concat =
                    self.get_function_builtin_external(BuiltinFunctionExternal::StringConcat);
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

    /// Generates a case expression. Each clause alternative is wrapped
    /// in a block that `_pattern` branches out of on failure.
    ///
    /// ```wat
    /// block case (result T):
    ///   block alt:
    ///     pattern(fail_depth=0)
    ///     guard check            ;; br_if 0 on failure
    ///     <body>
    ///     br 1                   ;; exit case with result
    ///   end alt
    ///   ... next clause ...
    ///   unreachable
    /// end case
    /// ```
    fn expression_case(
        &mut self,
        locals: &Locals,
        scope: Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        type_: &Arc<Type>,
        subjects: &[TypedExpr],
        clauses: &[TypedClause],
    ) {
        let mut subjects_locals = vec![];
        for subject in subjects {
            if let Some(name) = subject.var_name()
                && subject.is_local_var()
            {
                // Subject is a local variable — reuse its existing local
                let id = self.var_id(&scope, name, &subject.type_());
                subjects_locals.push(id.index);
            } else {
                let index = locals.for_subject(subject);
                // evaluate and save subject into a new local
                let _ = instructions
                    .expression(self, locals, scope.clone(), subject)
                    .local_set(index);
                subjects_locals.push(index);
            }
        }
        // block case
        let _ = instructions.block(BlockType::Result(self.val_type(type_)));
        for clause in clauses {
            let mut scope = scope.clone();
            for patterns in iter::once(&clause.pattern).chain(&clause.alternative_patterns) {
                // block alt — _pattern with Some(0) branches here on failure
                let _ = instructions.block(BlockType::Empty);
                for (pattern, subject_local) in patterns.iter().zip(&subjects_locals) {
                    let _ = instructions.local_get(*subject_local);
                    scope = self._pattern(locals, scope, instructions, pattern, 0);
                }
                // All patterns matched — check guard if present
                if let Some(guard) = &clause.guard {
                    let _ = instructions
                        .clause_guard(self, locals, &scope, guard)
                        .bool_not()
                        .br_if(0);
                }
                let _ = instructions
                    .expression(self, locals, scope.clone(), &clause.then)
                    // exit block case
                    .br(1);
                // end block alt
                let _ = instructions.end();
            }
        }
        // end block case
        let _ = instructions.unreachable().end();
    }

    fn is_ref_non_null(&self, type_: &Arc<Type>) -> bool {
        type_.is_string() || type_.tuple_types().is_some() || {
            if let Some((custom_type, _)) = self.custom_type(type_) {
                custom_type.is_ref_non_null()
            } else {
                false
            }
        }
    }

    fn expression_var(
        &mut self,
        scope: &Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        name: &EcoString,
        type_: &Arc<Type>,
    ) {
        if let Some(info) = self.local_functions.get(name).cloned() {
            if type_.fn_types().is_some() {
                let id = self.function_local_generic(&info, type_);
                let _ = instructions.ref_func(id.index);
                return;
            }
        }
        let id = self.var_id(scope, name, type_);

        let _ = match id.kind {
            IdKind::Func => {
                if type_.fn_types().is_none()
                    && type_
                        .named_type_information()
                        .is_some_and(|(module, _, _)| {
                            self.variants.contains_key(&(module, name.clone()))
                        })
                {
                    // variant with no args must be called
                    instructions.call(id.index)
                } else {
                    instructions.ref_func(id.index)
                }
            }
            IdKind::Global => {
                if self.is_ref_non_null(type_) {
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
                self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
            let (mod_name, line) = self.source_location(echo.location().start);
            let suffix = if message.is_some() { ' ' } else { '\n' };
            let string_index =
                self.string_index(&format!("src/{mod_name}.gleam:{line}{suffix}").into());

            let (dest, expr) = locals.for_echo(echo);

            let expr = match expr {
                Ok(expr) => expr,
                Err(name) => self.var_id(scope, name, &echo.type_()).index,
            };

            let _ = instructions
                .expression(self, locals, scope.clone(), expression)
                .local_set(expr)
                // write the module name and line number
                .global_as_non_null(string_index)
                .call(heap_base.index)
                .local_tee(dest)
                .call(string_to_memory)
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
            .local_tee(right)
            .pattern(self, locals, &mut scope, &assignment.pattern, 0)
            .local_get(right);
        scope
    }

    /// Generates a case-like structure for `let assert` assignments.
    /// Remaining statements are processed inside the success block so
    /// that pattern locals are proven initialized when used.
    ///
    /// ```wat
    /// block skip (result T):
    ///   block fail:
    ///     local.get right
    ///     pattern(fail_depth=0)     ;; br_if 0 on failure
    ///     <remaining statements>
    ///     br 1                      ;; exit skip with result
    ///   end fail
    ///   error_message + exit        ;; dead path
    ///   unreachable
    /// end skip
    /// ```
    fn assignment_assert(
        &mut self,
        locals: &Locals,
        scope: Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        assignment: &TypedAssignment,
        remaining: &[TypedStatement],
    ) {
        let right = locals.for_assigment(assignment);
        let _ = instructions
            .expression(self, locals, scope.clone(), &assignment.value)
            .local_set(right);

        let last_type = remaining
            .last()
            .map_or(assignment.value.type_(), |s| s.type_());
        let result_type = self.val_type(&last_type);

        let prefix =
            self.string_index(&"Pattern match failed, no pattern matched the value at ".into());
        let (mod_name, line) = self.source_location(assignment.location.start);
        let location: EcoString = format!("src/{mod_name}.gleam:{line}.\n").into();
        let location = self.string_index(&location);
        let string_to_memory =
            self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
        let heap_base = self.find_global_expect(HEAP_BASE);
        let print = self.find_global_expect(PRINT);
        let exit = self.find_global_expect(EXIT);

        let mut scope = scope;
        #[rustfmt::skip]
        let _ = instructions
            // block skip (result T): success exits here with the final result
            .block(BlockType::Result(result_type))
              // block fail: pattern failure branches here
              .block(BlockType::Empty)
                .local_get(right)
                .pattern(self, locals, &mut scope, &assignment.pattern, 0);
        // Pattern matched — remaining statements run here.
        if !remaining.is_empty() {
            self.statements(instructions, scope, locals, remaining);
        } else {
            let _ = instructions.local_get(right);
        }
        #[rustfmt::skip]
        let _ = instructions
                .br(1) // exit block skip with result
              .end() // end block fail
              // error handler (dead path — unreachable tells validator)
              .show_error_message(prefix, location, string_to_memory, heap_base.index, print.index)
              .i32_const(1)
              .call(exit.index)
              .unreachable()
            .end(); // end block skip
    }

    /// Checks if the subject (on the stack) matches the pattern. On
    /// failure, branches to `fail_depth`. On success, falls through
    /// with pattern locals set.
    ///
    /// ```wat
    /// block alt:
    ///   local.get subject
    ///   pattern(fail_depth=0)   ;; br_if 0 on failure
    ///   <body>                  ;; pattern locals are initialized here
    /// end alt
    /// ```
    fn _pattern(
        &mut self,
        locals: &Locals,
        mut scope: Scope,
        instructions: &mut ExtendedInstructionSink<'_>,
        pattern: &TypedPattern,
        fail_depth: u32,
    ) -> Scope {
        match pattern {
            Pattern::Discard { .. } => {
                let _ = instructions.drop();
            }
            Pattern::Int { int_value, .. } => {
                let _ = instructions.int_const(int_value).int_ne().br_if(fail_depth);
            }
            Pattern::Float { value, .. } => {
                let _ = instructions.float_const(value).float_ne().br_if(fail_depth);
            }
            Pattern::String { value, .. } => {
                let index = self.string_index(value);
                let eq = self.function_eq(&type_::string());
                let _ = instructions
                    .global_as_non_null(index)
                    .eq(eq)
                    .bool_not()
                    .br_if(fail_depth);
            }
            Pattern::List {
                elements,
                tail,
                type_,
                ..
            } => {
                let (custom_type, cons, _, args) = self.list_type_cons(type_);
                let (_, cons_index, _) =
                    self.mono_union_subtype_index(type_, &custom_type, &cons, &args);
                let right = locals.for_pattern(pattern);
                let _ = instructions.local_set(right);
                if !elements.is_empty() {
                    let _ = instructions
                        .local_get(right)
                        .ref_is_null()
                        .br_if(fail_depth);
                }
                for element in elements {
                    // Cons struct: {tag: 0, rest: 1, first: 2}
                    let _ = instructions
                        .local_get(right)
                        .ref_cast_non_null(HeapType::Concrete(cons_index))
                        .struct_get(cons_index, 2); // first
                    scope = self._pattern(locals, scope, instructions, element, fail_depth);
                    let _ = instructions
                        .local_get(right)
                        .ref_cast_non_null(HeapType::Concrete(cons_index))
                        .struct_get(cons_index, 1) // rest
                        .local_set(right);
                }
                if let Some(tail) = tail {
                    let _ = instructions.local_get(right);
                    scope = self._pattern(locals, scope, instructions, &tail.pattern, fail_depth);
                } else {
                    let _ = instructions
                        .local_get(right)
                        .ref_is_null()
                        .bool_not()
                        .br_if(fail_depth);
                }
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
                    fail_depth,
                );
            }
            Pattern::Constructor {
                name,
                type_,
                arguments,
                ..
            } => {
                let (custom_type, args) = self.custom_type_expect(type_);
                match custom_type {
                    CustomType::Enum { values } => {
                        let value = Self::enum_value_index(&values, name);
                        let _ = instructions
                            .i32_const(value as i32)
                            .i32_ne()
                            .br_if(fail_depth);
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
                            fail_depth,
                        );
                    }
                    CustomType::Union { custom_type } => {
                        let custom_type = custom_type.clone();
                        let null_tag = Self::null_variant_tag(&custom_type);
                        let (tag, constructor) = custom_type
                            .constructors
                            .iter()
                            .enumerate()
                            .find(|(_, c)| &c.name == name)
                            .expect("constructor at index");
                        let right = locals.for_pattern(pattern);
                        if null_tag == Some(tag) {
                            // Null-optimized variant: check ref_is_null
                            let _ = instructions
                                .local_tee(right)
                                .ref_is_null()
                                .bool_not()
                                .br_if(fail_depth);
                        } else {
                            let (supertype_index, type_index, _) = self.mono_union_subtype_index(
                                type_,
                                &custom_type,
                                constructor,
                                &args,
                            );
                            let _ = instructions.local_tee(right);
                            if null_tag.is_some() {
                                let _ = instructions.ref_is_null().br_if(fail_depth);
                                let _ = instructions
                                    .local_get(right)
                                    .struct_get(supertype_index, 0)
                                    .i32_const(tag as i32)
                                    .i32_ne()
                                    .br_if(fail_depth);
                            } else {
                                let _ = instructions
                                    .struct_get(supertype_index, 0)
                                    .i32_const(tag as i32)
                                    .i32_ne()
                                    .br_if(fail_depth);
                            }
                            let _ = instructions.local_get(right).patterns(
                                self,
                                locals,
                                &mut scope,
                                (supertype_index, Some(type_index)),
                                pattern,
                                arguments.iter().map(|arg| &arg.value),
                                fail_depth,
                            );
                        }
                    }
                    CustomType::External { .. } => {
                        panic!("external types should not reach code generation")
                    }
                };
            }
            Pattern::Variable { name, .. } => {
                let right = locals.for_pattern(pattern);
                scope = scope.insert_local(name.clone(), right);
                let _ = instructions.local_set(right);
            }
            Pattern::StringPrefix {
                left_side_string,
                left_side_assignment,
                left_location,
                right_location,
                right_side_assignment,
                ..
            } => {
                let subject = locals.for_pattern(pattern);
                let prefix_len = unescape(left_side_string).len() as i32;
                let starts_with = self.function_string_starts_with();
                let prefix_index = self.string_index(left_side_string);

                let _ = instructions
                    .local_tee(subject)
                    .global_as_non_null(prefix_index)
                    .call(starts_with)
                    .bool_not()
                    .br_if(fail_depth);

                if let Some((name, _)) = left_side_assignment {
                    let local = locals._get(left_location);
                    scope = scope.insert_local(name.clone(), local);
                    let _ = instructions
                        .global_as_non_null(prefix_index)
                        .local_set(local);
                }

                if let AssignName::Variable(name) = right_side_assignment {
                    let rest_local = locals._get(right_location);
                    scope = scope.insert_local(name.clone(), rest_local);
                    // rest = subject[prefix_len..]
                    let _ = instructions
                        .local_get(subject)
                        .string_len()
                        .i32_const(prefix_len)
                        .i32_sub()
                        .string_new()
                        .local_set(rest_local)
                        .local_get(rest_local)
                        .i32_const(0)
                        .local_get(subject)
                        .i32_const(prefix_len)
                        .local_get(subject)
                        .string_len()
                        .i32_const(prefix_len)
                        .i32_sub()
                        .string_copy();
                }
            }
            Pattern::BitArray { .. } | Pattern::BitArraySize(_) => {
                todo!("BitArray patterns are not yet supported")
            }
            Pattern::Assign {
                name,
                pattern: inner,
                ..
            } => {
                let right = locals.for_pattern(pattern);
                scope = scope.insert_local(name.clone(), right);
                let _ = instructions.local_tee(right);
                scope = self._pattern(locals, scope, instructions, inner, fail_depth);
            }
            Pattern::Invalid { .. } => {
                panic!("invalid patterns should not reach code generation")
            }
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
        fail_depth: u32,
    ) {
        let right = locals.for_pattern(pattern);
        let _ = instructions.local_set(right);
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
            *scope = self._pattern(locals, scope.clone(), instructions, element, fail_depth);
        }
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
                let (a, b) = locals.for_guard_div(scope, left, right);
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
                let (a, b) = locals.for_guard_div(scope, left, right);
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
                let type_index =
                    self.tuple_type_index(tuple.type_().tuple_types().expect("tuple type"));
                let _ = instructions
                    .clause_guard(self, locals, scope, tuple)
                    .struct_get(type_index, *index as u32);
            }
            ClauseGuard::FieldAccess {
                index, container, ..
            } => {
                let type_ = container.type_();
                let index = index.expect("FieldAccess index") as u32;
                let (custom_type, args) = self.custom_type_expect(&type_);
                match custom_type {
                    CustomType::External { .. } | CustomType::Enum { .. } => {
                        panic!("external/enum types should not reach code generation")
                    }
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
                        let constructor = custom_type_inferred_constructor(&custom_type, &type_)
                            .expect("inferred constructor");
                        let (_, type_index, _) =
                            self.mono_union_subtype_index(&type_, &custom_type, constructor, &args);
                        let _ = instructions
                            .clause_guard(self, locals, scope, container)
                            .ref_cast_non_null(HeapType::Concrete(type_index))
                            .struct_get(type_index, index + 1);
                    }
                }
            }
            ClauseGuard::ModuleSelect { literal, .. } => {
                let _ = instructions.constant(self, literal);
            }
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

    fn get_function_builtin_external(&mut self, builtin: BuiltinFunctionExternal) -> u32 {
        if let Some(id) = self.builtins_external.get(&builtin) {
            return *id;
        }

        let function = match builtin {
            BuiltinFunctionExternal::StringConcat => self.code_string_concat(),
            BuiltinFunctionExternal::StringNumBytes => self.code_string_num_bytes(),
            BuiltinFunctionExternal::StringGetByte => self.code_string_get_byte(),
            BuiltinFunctionExternal::I32ToInt => self.code_i32_to_int(),
            BuiltinFunctionExternal::IntToI32 => self.code_int_to_i32(),
            BuiltinFunctionExternal::I64ToInt => self.code_i64_to_int(),
            BuiltinFunctionExternal::IntToI64 => self.code_int_to_i64(),
            BuiltinFunctionExternal::F32ToFloat => self.code_f32_to_float(),
            BuiltinFunctionExternal::FloatToF32 => self.code_float_to_f32(),
            BuiltinFunctionExternal::F64ToFloat => self.code_f64_to_float(),
            BuiltinFunctionExternal::FloatToF64 => self.code_float_to_f64(),
            BuiltinFunctionExternal::IntToUtfCodepoint => self.code_int_to_utf_codepoint(),
            BuiltinFunctionExternal::IntRepr => self.code_int_repr(),
            BuiltinFunctionExternal::FloatRepr => self.code_float_repr(),
            BuiltinFunctionExternal::UtfCodepointRepr => self.code_utf_codepoint_repr(),
            BuiltinFunctionExternal::StringRepr => self.code_string_repr(),
            BuiltinFunctionExternal::StringToMemory => self.code_string_to_memory(),
            BuiltinFunctionExternal::MemoryToString => self.code_memory_to_string(),
            BuiltinFunctionExternal::ParseInt => self.code_parse_int(),
            BuiltinFunctionExternal::ParseFloat => self.code_parse_float(),
        };
        let (params, results) = if let Some(wasm_type) = builtin.wasm_type(self.int, self.float) {
            wasm_type
        } else {
            let i32 = self.find_external_type();
            let (gleam_params, gleam_result) = builtin.type_(i32);
            (
                self.val_types(gleam_params),
                vec![self.val_type(&gleam_result)],
            )
        };
        let function = self.add_function(
            builtin.name().into(),
            false,
            params,
            results,
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

fn function_type(function: &TypedFunction) -> Arc<Type> {
    Type::Fn {
        arguments: function_params_types(function),
        return_: function.return_type.clone(),
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

fn function_name(function: &TypedFunction) -> &EcoString {
    &function.name.as_ref().expect("function to have a name").1
}

fn set_function_name(function: &mut TypedFunction, name: EcoString) {
    function.name.as_mut().expect("function to have a name").1 = name;
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
            _ => panic!("invalid escape sequence during code generation"),
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
    char::from_u32(u32::from_str_radix(&s[1..num], 16).expect("valid hex escape"))
        .expect("valid unicode codepoint")
}

#[test]
fn string_unescape() {
    assert_eq!(
        unescape(r#"sure \\ \n \"its\" \t works! \u{263A}, \r or \f not..."#),
        "sure \\ \n \"its\" \t works! ☺, \r or \x0C not..."
    );
}
