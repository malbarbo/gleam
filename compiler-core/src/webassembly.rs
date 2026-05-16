#![allow(clippy::todo)]
mod builtins;
mod instructions;
mod monomorphize;
mod scope;
#[cfg(test)]
mod tests;

pub use builtins::builtin_function_names;
use builtins::*;
use instructions::*;
use monomorphize::*;
use scope::*;

use ecow::EcoString;
use indexmap::IndexMap;
use itertools::Itertools;
use num_bigint::BigInt;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    iter,
    ops::Deref,
    rc::Rc,
    str::Chars,
    sync::Arc,
};
use walrus::{
    ConstExpr, DataId, FieldType, FunctionBuilder, FunctionId, GlobalId, HeapType, InstrSeqBuilder,
    LocalId, MemoryId, RefType, StorageType, TypeId, ValType,
    ir::{
        BinaryOp, ExtendedLoad, InstrSeqId, InstrSeqType, LoadKind, MemArg, StoreKind, UnaryOp,
        Value,
    },
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
    int: crate::config::WasmInt,
    float: crate::config::WasmFloat,
) -> Result<(), Error> {
    let line_numbers = LineNumbers::new("");
    let all_line_numbers = HashMap::new();
    let mut generator = Generator::new(module, &line_numbers, all_modules, &all_line_numbers);
    generator.int = match int {
        crate::config::WasmInt::I32 => IntType::I32,
        crate::config::WasmInt::I64 => IntType::I64,
    };
    generator.float = match float {
        crate::config::WasmFloat::F32 => FloatType::F32,
        crate::config::WasmFloat::F64 => FloatType::F64,
    };
    generator.validate_numeric_literals()
}

pub fn module(
    module: &TypedModule,
    line_numbers: &LineNumbers,
    all_modules: &HashMap<EcoString, &TypedModule>,
    all_line_numbers: &HashMap<EcoString, LineNumbers>,
    int: crate::config::WasmInt,
    float: crate::config::WasmFloat,
) -> Result<Vec<u8>, Error> {
    let mut generator = Generator::new(module, line_numbers, all_modules, all_line_numbers);
    generator.int = match int {
        crate::config::WasmInt::I32 => IntType::I32,
        crate::config::WasmInt::I64 => IntType::I64,
    };
    generator.float = match float {
        crate::config::WasmFloat::F32 => FloatType::F32,
        crate::config::WasmFloat::F64 => FloatType::F64,
    };
    generator.compile()
}

fn eliminate_dead_code(module: &mut walrus::Module, builtin_data_names: &[String]) {
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
                walrus::DataKind::Passive => None,
            };
            let name = builtin_data_names.get(idx).cloned();
            (d.id(), name, kind)
        })
        .collect();
    for &(id, _, _) in &active_data {
        module.data.get_mut(id).kind = walrus::DataKind::Passive;
    }
    walrus::passes::gc::run(module);
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
        supertype: Option<TypeId>,
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
    Union(Vec<(EcoString, ValType)>, Option<TypeId>),
}

#[derive(Clone)]
enum WasmConst {
    String {
        dest: GlobalId,
        src: GlobalId,
    },
    Constant {
        global_index: GlobalId,
        value: Box<TypedConstant>,
    },
    Struct {
        global_index: GlobalId,
        type_index: TypeId,
        tag: Option<i32>,
        elements: Vec<TypedConstant>,
    },
    Function {
        dest: GlobalId,
        src: FunctionId,
    },
    Var {
        global_index: GlobalId,
        name: EcoString,
    },
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
        layout: UnionFieldLayout,
    },
}

impl CustomType {
    fn is_ref_non_null(&self) -> bool {
        match self {
            CustomType::Struct { .. } => true,
            CustomType::Union { custom_type, .. } => {
                Generator::null_variant_tag(custom_type).is_none()
            }
            CustomType::External { .. } | CustomType::Enum { .. } => false,
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

/// Maps Gleam field positions to wasm struct field indices for a union type.
/// Shared fields come first (after the tag), then variant-specific fields.
#[derive(Debug, Clone)]
struct UnionFieldLayout {
    shared_count: usize,
    field_indices: Vec<Vec<u32>>,
}

impl UnionFieldLayout {
    fn compute(custom_type: &TypedCustomType) -> UnionFieldLayout {
        let constructors = &custom_type.constructors;
        // Find shared fields: same position, same label, same type across ALL variants
        let min_fields = constructors
            .iter()
            .map(|c| c.arguments.len())
            .min()
            .unwrap_or(0);
        let first_constructor = constructors.first().expect("union to have constructors");
        let mut shared_positions = vec![];
        for (pos, first_arg) in first_constructor
            .arguments
            .iter()
            .enumerate()
            .take(min_fields)
        {
            let first_label = first_arg.label.as_ref().map(|(_, l)| l);
            let is_shared = constructors.iter().skip(1).all(|c| {
                c.arguments.get(pos).is_some_and(|arg| {
                    let label = arg.label.as_ref().map(|(_, l)| l);
                    label == first_label && arg.type_.same_as(&first_arg.type_)
                })
            });
            if is_shared {
                shared_positions.push(pos);
            }
        }

        let shared_count = shared_positions.len();

        let field_indices = constructors
            .iter()
            .map(|c| {
                let mut indices = vec![0u32; c.arguments.len()];
                let mut next_specific = shared_count as u32 + 1; // after tag + shared
                for (pos, slot) in indices.iter_mut().enumerate() {
                    if let Some(shared_idx) = shared_positions.iter().position(|&p| p == pos) {
                        *slot = shared_idx as u32 + 1; // after tag
                    } else {
                        *slot = next_specific;
                        next_specific += 1;
                    }
                }
                indices
            })
            .collect();

        UnionFieldLayout {
            shared_count,
            field_indices,
        }
    }

    fn fields(&self, variant: usize) -> &[u32] {
        self.field_indices
            .get(variant)
            .expect("variant field indices")
    }

    fn shared_field(&self, pos: u64) -> u32 {
        self.fields(0)
            .get(pos as usize)
            .copied()
            .expect("shared field index")
    }

    fn field(&self, variant: usize, pos: u64) -> u32 {
        self.fields(variant)
            .get(pos as usize)
            .copied()
            .expect("field index")
    }
}

// === User-type emission via Tarjan SCC + walrus `add_rec_group` ===
//
// Eagerly walks function signatures and constant types of the current module,
// recursively expanding via field types to find all reachable WASM type
// nodes (plain struct / union supertype + N subtypes / tuple / function).
// Builds a dependency graph between nodes, runs Tarjan to obtain SCCs in
// reverse topological order, and emits each SCC either as a singleton via
// `add_struct`/`add_composite`/`add` (preserving arena-level dedup) or via
// `add_rec_group` when the SCC has a self-loop or more than one node.
//
// Types referenced only from expression bodies (e.g., `Some(1)` inside a fn
// returning `Nil`) are NOT discovered here — the existing lazy emission path
// still handles them. Lazy emission is safe for those types because they do
// not form cycles (otherwise existing tests would already crash).

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TypeNodeKey {
    Plain {
        name: EcoString,
    },
    UnionSuper {
        name: EcoString,
        shared_field_types: Vec<EcoString>,
    },
    UnionSub {
        pretty: EcoString,
        constructor: EcoString,
    },
    Function {
        params: Vec<EcoString>,
        results: Vec<EcoString>,
    },
}

#[derive(Debug, Clone)]
enum TypeNodeKind {
    PlainStruct,
    Tuple,
    UnionSupertype,
    UnionSubtype,
    Function,
}

#[derive(Debug, Clone)]
struct TypeNode {
    key: TypeNodeKey,
    kind: TypeNodeKind,
    walrus_name: Option<EcoString>,
    is_final: bool,
    supertype_key: Option<TypeNodeKey>,
    field_types: Vec<Arc<Type>>,
    field_labels: Vec<EcoString>,
    /// For Function: number of result types at the tail of `field_types`.
    result_count: usize,
}

#[derive(Debug, Clone, Copy)]
struct TypeNodeFieldRef {
    descriptor_idx: Option<usize>,
    nullable: bool,
}

struct Generator<'a> {
    pub(super) wasm_module: walrus::Module,
    memory: Option<MemoryId>,
    /// Names of builtin data segments, indexed by segment index.
    builtin_data_names: Vec<String>,
    wasm_types: IndexMap<WasmType, TypeId>,
    /// High-level cache keyed by TypeNodeKey, populated by
    /// `pre_emit_user_types`. Bypasses the ValType chicken-and-egg during
    /// cyclic emission (the existing `wasm_types` cache's key includes
    /// resolved ValTypes, which require TypeIds we haven't allocated yet).
    type_node_cache: HashMap<TypeNodeKey, TypeId>,
    types: HashMap<(EcoString, EcoString), CustomType>,
    variants: HashMap<(EcoString, EcoString), Variant>,
    local_functions: HashMap<EcoString, LocalFunction>,
    builtins: HashMap<BuiltinFunction, FunctionId>,
    builtins_external: HashMap<BuiltinFunctionExternal, FunctionId>,
    main: Option<FunctionId>,
    // String literals and its index in the global section
    strings: HashMap<EcoString, GlobalId>,
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
        let mut wasm_module =
            walrus::Module::from_buffer(BUILTINS_WASM).expect("builtins wasm to be a valid module");
        let memory = wasm_module.memories.iter().next().map(|m| m.id());
        // String is a mutable i8 array. Add it once and reuse for all string literals.
        let string_type_index = wasm_module.types.add_array(StringType::field_type());
        wasm_module.types.get_mut(string_type_index).name = Some("String".to_string());
        let mut wasm_types = IndexMap::new();
        let _ = wasm_types.insert(StringType::wasm_type(), string_type_index);
        Generator {
            wasm_module,
            memory,
            builtin_data_names: Vec::new(),
            wasm_types,
            type_node_cache: HashMap::new(),
            types: HashMap::new(),
            variants: HashMap::new(),
            local_functions: HashMap::new(),
            builtins: HashMap::new(),
            builtins_external: HashMap::new(),
            main: None,
            strings: HashMap::new(),
            consts: vec![],
            globals: Rc::default(),
            int: IntType::I32,
            float: FloatType::F64,
            string: StringType {
                type_index: string_type_index,
            },
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
        self.wasm_module.name = Some(self.module.name.to_string());
        self.wasm_module.start = Some(start);
        eliminate_dead_code(&mut self.wasm_module, &self.builtin_data_names);
        Ok(self.wasm_module.emit_wasm())
    }

    fn generate(&mut self) -> Result<FunctionId, Error> {
        // Validate all numeric literals fit in the target types before codegen.
        self.validate_numeric_literals()?;

        // External types, like I32, must come first because of the external function type checking.
        // Externals functions come first because of the indexes in the builtin wasm file.
        self.types_external()?;
        let builtins = self.functions_external()?;
        self.types_prelude();
        self.types();
        self.types_imported();
        self.pre_emit_user_types();
        self.functions_builtins(builtins);
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
            BuiltinFunctionExternal::StringConcat
            | BuiltinFunctionExternal::StringNumBytes
            | BuiltinFunctionExternal::StringGetByte
            | BuiltinFunctionExternal::IntToUtfCodepoint
            | BuiltinFunctionExternal::ParseInt
            | BuiltinFunctionExternal::ParseFloat => type_::int(),
        }
    }

    fn add_function_to_globals(&mut self, name: EcoString, index: FunctionId) -> Id {
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
        // The walrus module was parsed from BUILTINS_WASM in Generator::new;
        // discover its functions and data segments directly.
        let mut available: HashMap<EcoString, (Vec<ValType>, Vec<ValType>)> = HashMap::new();
        let func_info: Vec<(EcoString, FunctionId, Vec<ValType>, Vec<ValType>)> = self
            .wasm_module
            .funcs
            .iter()
            .filter_map(|f| {
                let name = f.name.as_ref()?;
                let ty = self.wasm_module.types.get(f.ty());
                Some((
                    EcoString::from(name.as_str()),
                    f.id(),
                    ty.params().to_vec(),
                    ty.results().to_vec(),
                ))
            })
            .collect();
        for (name, id, params, results) in func_info {
            let _ = self.add_function_to_globals(name.clone(), id);
            let _ = available.insert(name, (params, results));
        }

        self.builtin_data_names = self
            .wasm_module
            .data
            .iter()
            .map(|d| d.name.clone().unwrap_or_default())
            .collect();

        // Validate external function signatures and collect needed builtins
        self.validate_externals(&available)
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
        // Add Nil, Bool and Result. String was registered in `new`.
        self.add_custom_types(
            Self::prelude_custom_types()
                .iter()
                .map(|c| (c, PRELUDE_MODULE_NAME)),
        );
    }

    // === User-type emission (Tarjan SCC + add_rec_group) ===

    fn pre_emit_user_types(&mut self) {
        let descriptors = self.discover_type_nodes();
        if descriptors.is_empty() {
            return;
        }
        self.emit_type_nodes(descriptors);
    }

    fn discover_type_nodes(&self) -> Vec<TypeNode> {
        let mut visited: HashSet<TypeNodeKey> = HashSet::new();
        let mut descriptors: Vec<TypeNode> = Vec::new();
        let mut to_visit: Vec<Arc<Type>> = Vec::new();

        for f in self.module.definitions.functions.iter() {
            if f.publicity.is_public()
                && !is_generic_type(&function_type(f))
                && f.external_webassembly.is_none()
            {
                for arg in &f.arguments {
                    to_visit.push(arg.type_.clone());
                }
                to_visit.push(f.return_type.clone());
            }
        }
        for c in &self.module.definitions.constants {
            if !is_generic_type(&c.type_) {
                to_visit.push(c.type_.clone());
            }
        }

        while let Some(t) = to_visit.pop() {
            self.visit_type_node(&t, &mut visited, &mut descriptors, &mut to_visit);
        }

        descriptors
    }

    fn visit_type_node(
        &self,
        t: &Arc<Type>,
        visited: &mut HashSet<TypeNodeKey>,
        descriptors: &mut Vec<TypeNode>,
        to_visit: &mut Vec<Arc<Type>>,
    ) {
        if t.is_int() || t.is_float() || t.is_bool() || t.is_string() || t.is_utf_codepoint() {
            return;
        }
        if let Some((_, n)) = t.named_type_name()
            && n.as_str() == "Nil"
        {
            return;
        }

        if let Some(types) = t.tuple_types() {
            let pretty: EcoString = self.type_pretty_name(t).replace("#", "Tuple").into();
            let key = TypeNodeKey::Plain {
                name: pretty.clone(),
            };
            if !visited.insert(key.clone()) {
                return;
            }
            let labels: Vec<EcoString> = (0..types.len())
                .map(|i| EcoString::from(i.to_string()))
                .collect();
            descriptors.push(TypeNode {
                key,
                kind: TypeNodeKind::Tuple,
                walrus_name: Some(pretty),
                is_final: true,
                supertype_key: None,
                field_types: types.clone(),
                field_labels: labels,
                result_count: 0,
            });
            for el in types {
                to_visit.push(el);
            }
            return;
        }

        if let Some((params, result)) = t.fn_types() {
            let params_names: Vec<EcoString> =
                params.iter().map(|p| self.type_pretty_name(p)).collect();
            let result_name = self.type_pretty_name(&result);
            let key = TypeNodeKey::Function {
                params: params_names,
                results: vec![result_name],
            };
            if !visited.insert(key.clone()) {
                return;
            }
            let mut field_types = params.clone();
            field_types.push(result.clone());
            let labels: Vec<EcoString> = (0..field_types.len())
                .map(|i| EcoString::from(i.to_string()))
                .collect();
            descriptors.push(TypeNode {
                key,
                kind: TypeNodeKind::Function,
                walrus_name: None,
                is_final: true,
                supertype_key: None,
                field_types: field_types.clone(),
                field_labels: labels,
                result_count: 1,
            });
            for ft in field_types {
                to_visit.push(ft);
            }
            return;
        }

        if let Some((custom_type, args)) = self.custom_type(t) {
            match custom_type {
                CustomType::External { .. } | CustomType::Enum { .. } => {}
                CustomType::Struct {
                    custom_type: ct,
                    constructor,
                } => {
                    let pretty = self.type_pretty_name(t);
                    let key = TypeNodeKey::Plain {
                        name: pretty.clone(),
                    };
                    if !visited.insert(key.clone()) {
                        return;
                    }
                    let field_types = Monomorphizer::variant_constructor(&ct, &constructor, &args);
                    let labels = self.record_field_labels(&constructor);
                    descriptors.push(TypeNode {
                        key,
                        kind: TypeNodeKind::PlainStruct,
                        walrus_name: Some(pretty),
                        is_final: true,
                        supertype_key: None,
                        field_types: field_types.clone(),
                        field_labels: labels,
                        result_count: 0,
                    });
                    for ft in field_types {
                        to_visit.push(ft);
                    }
                }
                CustomType::Union {
                    custom_type: ct,
                    layout,
                } => {
                    self.visit_union_type_nodes(
                        t,
                        &ct,
                        &layout,
                        &args,
                        visited,
                        descriptors,
                        to_visit,
                    );
                }
            }
        }
    }

    fn visit_union_type_nodes(
        &self,
        t: &Arc<Type>,
        ct: &TypedCustomType,
        layout: &UnionFieldLayout,
        args: &[Arc<Type>],
        visited: &mut HashSet<TypeNodeKey>,
        descriptors: &mut Vec<TypeNode>,
        to_visit: &mut Vec<Arc<Type>>,
    ) {
        let first = ct.constructors.first().expect("union to have constructors");
        let first_types = Monomorphizer::variant_constructor(ct, first, args);
        let field_indices_0 = layout.fields(0);
        let mut shared_field_types: Vec<Arc<Type>> = Vec::new();
        let mut shared_field_labels: Vec<EcoString> = Vec::new();
        for (pos, (arg, ftype)) in first.arguments.iter().zip(first_types.iter()).enumerate() {
            let widx = field_indices_0.get(pos).copied().expect("field index");
            if widx > layout.shared_count as u32 {
                continue;
            }
            let label = arg
                .label
                .as_ref()
                .map(|(_, l)| l.clone())
                .unwrap_or_else(|| EcoString::from(pos.to_string()));
            shared_field_labels.push(label);
            shared_field_types.push(ftype.clone());
        }

        let supertype_fp: Vec<EcoString> = shared_field_types
            .iter()
            .map(|t| self.type_pretty_name(t))
            .collect();
        let supertype_key = TypeNodeKey::UnionSuper {
            name: ct.name.clone(),
            shared_field_types: supertype_fp,
        };

        if visited.insert(supertype_key.clone()) {
            descriptors.push(TypeNode {
                key: supertype_key.clone(),
                kind: TypeNodeKind::UnionSupertype,
                walrus_name: Some(ct.name.clone()),
                is_final: false,
                supertype_key: None,
                field_types: shared_field_types.clone(),
                field_labels: shared_field_labels,
                result_count: 0,
            });
            for ft in &shared_field_types {
                to_visit.push(ft.clone());
            }
        }

        let pretty = self.type_pretty_name(t);
        for (variant_idx, ctor) in ct.constructors.iter().enumerate() {
            let subtype_name: EcoString = format!("{}.{}", pretty, ctor.name).into();
            let subtype_key = TypeNodeKey::UnionSub {
                pretty: pretty.clone(),
                constructor: ctor.name.clone(),
            };
            if !visited.insert(subtype_key.clone()) {
                continue;
            }

            let types_v = Monomorphizer::variant_constructor(ct, ctor, args);
            let field_indices = layout.fields(variant_idx);

            let mut ordered: Vec<(u32, EcoString, Arc<Type>)> = ctor
                .arguments
                .iter()
                .enumerate()
                .zip(types_v.iter())
                .map(|((pos, arg), ftype)| {
                    let label = arg
                        .label
                        .as_ref()
                        .map(|(_, l)| l.clone())
                        .unwrap_or_else(|| EcoString::from(pos.to_string()));
                    let widx = field_indices.get(pos).copied().expect("field idx");
                    (widx, label, ftype.clone())
                })
                .collect();
            ordered.sort_by_key(|(w, _, _)| *w);

            let labels: Vec<EcoString> = ordered.iter().map(|(_, l, _)| l.clone()).collect();
            let field_types: Vec<Arc<Type>> = ordered.into_iter().map(|(_, _, t)| t).collect();

            descriptors.push(TypeNode {
                key: subtype_key,
                kind: TypeNodeKind::UnionSubtype,
                walrus_name: Some(subtype_name),
                is_final: true,
                supertype_key: Some(supertype_key.clone()),
                field_types: field_types.clone(),
                field_labels: labels,
                result_count: 0,
            });
            for ft in field_types {
                to_visit.push(ft);
            }
        }
    }

    fn record_field_labels(&self, constructor: &TypedRecordConstructor) -> Vec<EcoString> {
        constructor
            .arguments
            .iter()
            .enumerate()
            .map(|(index, c)| {
                if let Some((_, label)) = &c.label {
                    label.clone()
                } else {
                    EcoString::from(index.to_string())
                }
            })
            .collect()
    }

    fn type_node_key_and_nullable(&self, t: &Arc<Type>) -> (Option<TypeNodeKey>, bool) {
        if t.is_int() || t.is_float() || t.is_bool() || t.is_string() || t.is_utf_codepoint() {
            return (None, false);
        }
        if let Some((_, n)) = t.named_type_name()
            && n.as_str() == "Nil"
        {
            return (None, false);
        }

        if t.tuple_types().is_some() {
            let pretty: EcoString = self.type_pretty_name(t).replace("#", "Tuple").into();
            return (Some(TypeNodeKey::Plain { name: pretty }), false);
        }
        if let Some((params, result)) = t.fn_types() {
            let params_names: Vec<EcoString> =
                params.iter().map(|p| self.type_pretty_name(p)).collect();
            let result_name = self.type_pretty_name(&result);
            return (
                Some(TypeNodeKey::Function {
                    params: params_names,
                    results: vec![result_name],
                }),
                false,
            );
        }

        if let Some((custom_type, _args)) = self.custom_type(t) {
            match custom_type {
                CustomType::External { .. } | CustomType::Enum { .. } => (None, false),
                CustomType::Struct { .. } => {
                    let pretty = self.type_pretty_name(t);
                    (Some(TypeNodeKey::Plain { name: pretty }), false)
                }
                CustomType::Union {
                    custom_type: ct, ..
                } => {
                    let args = t
                        .named_type_information()
                        .map(|(_, _, a)| a)
                        .unwrap_or_default();
                    let first = ct.constructors.first().expect("union to have constructors");
                    let first_types = Monomorphizer::variant_constructor(&ct, first, &args);
                    let layout = UnionFieldLayout::compute(&ct);
                    let field_indices_0 = layout.fields(0);
                    let mut shared_field_types: Vec<Arc<Type>> = Vec::new();
                    for (pos, ftype) in first_types.iter().enumerate() {
                        let widx = field_indices_0.get(pos).copied().expect("idx");
                        if widx > layout.shared_count as u32 {
                            continue;
                        }
                        shared_field_types.push(ftype.clone());
                    }
                    let supertype_fp: Vec<EcoString> = shared_field_types
                        .iter()
                        .map(|t| self.type_pretty_name(t))
                        .collect();
                    let nullable = Generator::null_variant_tag(&ct).is_some();
                    (
                        Some(TypeNodeKey::UnionSuper {
                            name: ct.name.clone(),
                            shared_field_types: supertype_fp,
                        }),
                        nullable,
                    )
                }
            }
        } else {
            (None, false)
        }
    }

    fn emit_type_nodes(&mut self, descriptors: Vec<TypeNode>) {
        let n = descriptors.len();
        if n == 0 {
            return;
        }

        let mut key_to_idx: HashMap<TypeNodeKey, usize> = HashMap::new();
        for (i, d) in descriptors.iter().enumerate() {
            let _ = key_to_idx.insert(d.key.clone(), i);
        }

        let mut field_refs: Vec<Vec<TypeNodeFieldRef>> = Vec::with_capacity(n);
        for d in &descriptors {
            let mut refs = Vec::with_capacity(d.field_types.len());
            for ft in &d.field_types {
                let (key, nullable) = self.type_node_key_and_nullable(ft);
                let descriptor_idx = key.as_ref().and_then(|k| key_to_idx.get(k).copied());
                refs.push(TypeNodeFieldRef {
                    descriptor_idx,
                    nullable,
                });
            }
            field_refs.push(refs);
        }

        // Build edges: field refs + supertype refs (subtype → its supertype).
        let mut edges: Vec<Vec<usize>> = vec![Vec::new(); n];
        for (i, d) in descriptors.iter().enumerate() {
            if let Some(slot) = edges.get_mut(i) {
                if let Some(refs) = field_refs.get(i) {
                    for r in refs {
                        if let Some(idx) = r.descriptor_idx {
                            slot.push(idx);
                        }
                    }
                }
                if let Some(super_key) = &d.supertype_key
                    && let Some(&super_idx) = key_to_idx.get(super_key)
                {
                    slot.push(super_idx);
                }
            }
        }

        let sccs = tarjan_scc(n, &edges);

        let int = self.int;
        let float = self.float;
        let string_idx = self.string.type_index;
        let mut assigned: Vec<Option<TypeId>> = vec![None; n];

        for scc in &sccs {
            let only_global = scc.first().copied();
            let has_self_loop = scc.len() == 1
                && only_global
                    .and_then(|i| edges.get(i).map(|e| e.contains(&i)))
                    .unwrap_or(false);

            if scc.len() == 1 && !has_self_loop {
                let global = only_global.expect("non-empty SCC");
                let d = descriptors.get(global).expect("descriptor");
                let refs = field_refs.get(global).expect("refs");
                let val_types: Vec<ValType> = d
                    .field_types
                    .iter()
                    .zip(refs.iter())
                    .map(|(ft, r)| {
                        resolve_val_type_assigned(ft, *r, &assigned, int, float, string_idx)
                    })
                    .collect();
                let supertype_id = d
                    .supertype_key
                    .as_ref()
                    .and_then(|k| key_to_idx.get(k))
                    .and_then(|si| assigned.get(*si))
                    .copied()
                    .flatten();
                let id = self.emit_singleton_type_node(d, &val_types, supertype_id);
                if let Some(slot) = assigned.get_mut(global) {
                    *slot = Some(id);
                }
                self.register_type_node(d, id, &val_types, supertype_id);
            } else {
                let scc_indices: Vec<usize> = scc.clone();
                let local_of: HashMap<usize, usize> = scc_indices
                    .iter()
                    .enumerate()
                    .map(|(local, &global)| (global, local))
                    .collect();

                let scc_descriptors: Vec<TypeNode> = scc_indices
                    .iter()
                    .filter_map(|&g| descriptors.get(g).cloned())
                    .collect();
                let scc_refs: Vec<Vec<TypeNodeFieldRef>> = scc_indices
                    .iter()
                    .filter_map(|&g| field_refs.get(g).cloned())
                    .collect();
                let assigned_snapshot = assigned.clone();
                let key_to_idx_snapshot = key_to_idx.clone();
                let local_of_for_closure = local_of.clone();
                let scc_descriptors_for_closure = scc_descriptors.clone();
                let scc_refs_for_closure = scc_refs.clone();

                let scc_size = scc_indices.len();
                let ids = self.wasm_module.types.add_rec_group(scc_size, move |ids| {
                    scc_descriptors_for_closure
                        .iter()
                        .zip(scc_refs_for_closure.iter())
                        .map(|(d, refs)| {
                            let val_types: Vec<ValType> = d
                                .field_types
                                .iter()
                                .zip(refs.iter())
                                .map(|(ft, r)| {
                                    resolve_val_type_in_scc(
                                        ft,
                                        *r,
                                        ids,
                                        &assigned_snapshot,
                                        &local_of_for_closure,
                                        int,
                                        float,
                                        string_idx,
                                    )
                                })
                                .collect();
                            let composite =
                                build_composite_for_node(&d.kind, val_types, d.result_count);
                            let supertype_id = d.supertype_key.as_ref().and_then(|k| {
                                let global = key_to_idx_snapshot.get(k)?;
                                if let Some(local) = local_of_for_closure.get(global) {
                                    ids.get(*local).copied()
                                } else {
                                    assigned_snapshot.get(*global).copied().flatten()
                                }
                            });
                            (composite, d.is_final, supertype_id)
                        })
                        .collect()
                });

                for (local, &global) in scc_indices.iter().enumerate() {
                    let id = *ids.get(local).expect("rec_group returned id");
                    if let Some(slot) = assigned.get_mut(global) {
                        *slot = Some(id);
                    }
                }
                for (local, &global) in scc_indices.iter().enumerate() {
                    let id = *ids.get(local).expect("rec_group returned id");
                    let d = descriptors.get(global).expect("descriptor");
                    let refs = field_refs.get(global).expect("refs");
                    let val_types: Vec<ValType> = d
                        .field_types
                        .iter()
                        .zip(refs.iter())
                        .map(|(ft, r)| {
                            resolve_val_type_in_scc(
                                ft, *r, &ids, &assigned, &local_of, int, float, string_idx,
                            )
                        })
                        .collect();
                    let supertype_id = d.supertype_key.as_ref().and_then(|k| {
                        let global_super = key_to_idx.get(k)?;
                        if let Some(local) = local_of.get(global_super) {
                            ids.get(*local).copied()
                        } else {
                            assigned.get(*global_super).copied().flatten()
                        }
                    });
                    if let Some(name) = &d.walrus_name {
                        self.wasm_module.types.get_mut(id).name = Some(name.to_string());
                    }
                    self.register_type_node(d, id, &val_types, supertype_id);
                }
            }
        }
    }

    fn emit_singleton_type_node(
        &mut self,
        d: &TypeNode,
        val_types: &[ValType],
        supertype_id: Option<TypeId>,
    ) -> TypeId {
        let id = match &d.kind {
            TypeNodeKind::PlainStruct | TypeNodeKind::Tuple => {
                let walrus_fields: Vec<FieldType> = val_types
                    .iter()
                    .map(|vt| FieldType {
                        element_type: StorageType::Val(*vt),
                        mutable: false,
                    })
                    .collect();
                self.wasm_module.types.add_struct(walrus_fields)
            }
            TypeNodeKind::UnionSupertype | TypeNodeKind::UnionSubtype => {
                let mut walrus_fields: Vec<FieldType> = vec![FieldType {
                    element_type: StorageType::Val(ValType::I32),
                    mutable: false,
                }];
                walrus_fields.extend(val_types.iter().map(|vt| FieldType {
                    element_type: StorageType::Val(*vt),
                    mutable: false,
                }));
                let comp = walrus::CompositeType::Struct(walrus::StructType {
                    fields: walrus_fields.into_boxed_slice(),
                });
                self.wasm_module
                    .types
                    .add_composite(comp, d.is_final, supertype_id)
            }
            TypeNodeKind::Function => {
                let total = val_types.len();
                let param_count = total.saturating_sub(d.result_count);
                let (params_slice, results_slice) = val_types.split_at(param_count);
                self.wasm_module.types.add(params_slice, results_slice)
            }
        };
        if let Some(name) = &d.walrus_name {
            self.wasm_module.types.get_mut(id).name = Some(name.to_string());
        }
        id
    }

    fn register_type_node(
        &mut self,
        d: &TypeNode,
        id: TypeId,
        val_types: &[ValType],
        supertype_id: Option<TypeId>,
    ) {
        let _ = self.type_node_cache.insert(d.key.clone(), id);
        match &d.kind {
            TypeNodeKind::PlainStruct | TypeNodeKind::Tuple => {
                let name = d.walrus_name.clone().unwrap_or_default();
                let fields: Vec<(EcoString, ValType)> = d
                    .field_labels
                    .iter()
                    .cloned()
                    .zip(val_types.iter().copied())
                    .collect();
                let _ = self.wasm_types.insert(WasmType::struct_(name, fields), id);
            }
            TypeNodeKind::UnionSupertype => {
                let name = d.walrus_name.clone().unwrap_or_default();
                let fields: Vec<(EcoString, ValType)> = d
                    .field_labels
                    .iter()
                    .cloned()
                    .zip(val_types.iter().copied())
                    .collect();
                let _ = self
                    .wasm_types
                    .insert(WasmType::union(name, fields, None), id);
            }
            TypeNodeKind::UnionSubtype => {
                let name = d.walrus_name.clone().unwrap_or_default();
                let fields: Vec<(EcoString, ValType)> = d
                    .field_labels
                    .iter()
                    .cloned()
                    .zip(val_types.iter().copied())
                    .collect();
                let _ = self
                    .wasm_types
                    .insert(WasmType::union(name, fields, supertype_id), id);
            }
            TypeNodeKind::Function => {
                let total = val_types.len();
                let param_count = total.saturating_sub(d.result_count);
                let (params_slice, results_slice) = val_types.split_at(param_count);
                let _ = self.wasm_types.insert(
                    WasmType::function(params_slice.to_vec(), results_slice.to_vec()),
                    id,
                );
            }
        }
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
                        ConstExpr::Value(Value::I32(value as i32)),
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
                    layout: UnionFieldLayout::compute(custom_type),
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
        let CustomType::Union { custom_type, .. } = custom_type else {
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
                    self.main = Some(id.func_id())
                }
            }
            // Private, generic, and external functions are compiled elsewhere.
        }
    }

    fn lookup_node_val_type(&self, type_: &Arc<Type>) -> Option<ValType> {
        let (key, nullable) = self.type_node_key_and_nullable(type_);
        let key = key?;
        let id = *self.type_node_cache.get(&key)?;
        Some(ValType::Ref(RefType {
            heap_type: HeapType::Concrete(id),
            nullable,
        }))
    }

    fn lookup_node_type_index(&self, type_: &Arc<Type>) -> Option<TypeId> {
        if let Some((custom_type, _)) = self.custom_type(type_)
            && let CustomType::Union {
                custom_type: ct, ..
            } = &custom_type
            && let Some(ctor) = custom_type_inferred_constructor(ct, type_)
        {
            let pretty = self.type_pretty_name(type_);
            let key = TypeNodeKey::UnionSub {
                pretty,
                constructor: ctor.name.clone(),
            };
            return self.type_node_cache.get(&key).copied();
        }
        let (key, _) = self.type_node_key_and_nullable(type_);
        let key = key?;
        self.type_node_cache.get(&key).copied()
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
        } else if let Some(vt) = self.lookup_node_val_type(type_) {
            vt
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

    fn val_type_ref(&self, type_index: TypeId) -> ValType {
        ValType::Ref(RefType {
            heap_type: HeapType::Concrete(type_index),
            nullable: false,
        })
    }

    fn val_type_ref_nullable(&self, type_index: TypeId) -> ValType {
        ValType::Ref(RefType {
            heap_type: HeapType::Concrete(type_index),
            nullable: true,
        })
    }

    fn type_index(&mut self, type_: &Arc<Type>) -> TypeId {
        if type_.is_string() {
            self.string.type_index
        } else if let Some(id) = self.lookup_node_type_index(type_) {
            id
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
                CustomType::Union { custom_type, .. } => {
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
    ) -> TypeId {
        let params = self.val_types(arguments);
        let results = self.val_types(return_);
        self.function_type_index_with_val_types(params, results)
    }

    fn function_type_index_with_val_types(
        &mut self,
        params: Vec<ValType>,
        result: Vec<ValType>,
    ) -> TypeId {
        let key = WasmType::function(params.clone(), result.clone());
        if let Some(id) = self.wasm_types.get(&key) {
            return *id;
        }
        let id = self.wasm_module.types.add(&params, &result);
        let _ = self.wasm_types.insert(key, id);
        id
    }

    fn function_val_type(&self, type_index: TypeId) -> ValType {
        self.val_type_ref(type_index)
    }

    fn tuple_type_index(&mut self, types: impl IntoIterator<Item = Arc<Type>>) -> TypeId {
        let types = types.into_iter().collect_vec();
        let val_types = self.val_types(types.iter().cloned());
        let name = self
            .type_pretty_name(&type_::tuple(types))
            .replace("#", "Tuple");
        let fields: Vec<(EcoString, ValType)> = val_types
            .into_iter()
            .enumerate()
            .map(|(index, v)| (index.to_string().into(), v))
            .collect();
        self.struct_type_index(name, fields)
    }

    fn struct_type_index(&mut self, name: EcoString, fields: Vec<(EcoString, ValType)>) -> TypeId {
        let key = WasmType::struct_(name.clone(), fields.clone());
        if let Some(id) = self.wasm_types.get(&key) {
            return *id;
        }
        let walrus_fields: Vec<FieldType> = fields
            .iter()
            .map(|(_, vt)| FieldType {
                element_type: StorageType::Val(*vt),
                mutable: false,
            })
            .collect();
        let id = self.wasm_module.types.add_struct(walrus_fields);
        self.wasm_module.types.get_mut(id).name = Some(name.to_string());
        let _ = self.wasm_types.insert(key, id);
        id
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
    ) -> (TypeId, Vec<Arc<Type>>) {
        let types = Monomorphizer::variant_constructor(custom_type, constructor, args);
        let fields = self.fields(constructor, &types);
        let name = self.type_pretty_name(type_);
        (self.struct_type_index(name, fields), types)
    }

    fn union_layout(&self, type_: &Arc<Type>) -> UnionFieldLayout {
        let (module, name, _) = type_
            .named_type_information()
            .expect("named type information");
        match self.types.get(&(module, name)).expect("union type") {
            CustomType::Union { layout, .. } => layout.clone(),
            CustomType::External { .. } | CustomType::Enum { .. } | CustomType::Struct { .. } => {
                panic!("expected union type")
            }
        }
    }

    fn mono_union_supertype_index(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &TypedCustomType,
    ) -> TypeId {
        let layout = self.union_layout(type_);
        let first = custom_type
            .constructors
            .first()
            .expect("union to have constructors");
        let args = type_
            .named_type_information()
            .map(|(_, _, a)| a)
            .unwrap_or_default();
        let types = Monomorphizer::variant_constructor(custom_type, first, &args);

        // Build shared fields (those with wasm index <= shared_count, i.e. in supertype)
        let field_indices = layout.fields(0);
        let mut shared_fields = Vec::with_capacity(layout.shared_count);
        for (pos, (arg, type_)) in first.arguments.iter().zip(types.iter()).enumerate() {
            if field_indices.get(pos).copied().expect("field index") > layout.shared_count as u32 {
                continue;
            }
            let label = if let Some((_, label)) = &arg.label {
                label.clone()
            } else {
                pos.to_string().into()
            };
            shared_fields.push((label, self.val_type(type_)));
        }

        let name = custom_type.name.clone();
        let key = WasmType::union(name.clone(), shared_fields.clone(), None);
        if let Some(id) = self.wasm_types.get(&key) {
            return *id;
        }
        let id = self.union_type_add(&name, &shared_fields, None);
        let _ = self.wasm_types.insert(key, id);
        id
    }

    fn mono_union_subtype_index(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &TypedCustomType,
        constructor: &TypedRecordConstructor,
        args: &[Arc<Type>],
    ) -> (TypeId, TypeId, Vec<Arc<Type>>) {
        let layout = self.union_layout(type_);
        let supertype_index = self.mono_union_supertype_index(type_, custom_type);
        let types = Monomorphizer::variant_constructor(custom_type, constructor, args);
        let variant_index = custom_type
            .constructors
            .iter()
            .position(|c| c.name == constructor.name)
            .expect("constructor in union");

        let field_indices = layout.fields(variant_index);
        let val_types = self.val_types(types.iter().cloned());
        let mut fields: Vec<(u32, EcoString, ValType)> = constructor
            .arguments
            .iter()
            .zip(val_types.iter())
            .enumerate()
            .map(|(pos, (arg, val_type))| {
                let label = if let Some((_, label)) = &arg.label {
                    label.clone()
                } else {
                    pos.to_string().into()
                };
                let idx = field_indices.get(pos).copied().expect("field index");
                (idx, label, *val_type)
            })
            .collect();
        fields.sort_by_key(|(idx, _, _)| *idx);
        let fields: Vec<(EcoString, ValType)> = fields
            .into_iter()
            .map(|(_, label, val_type)| (label, val_type))
            .collect();

        let mut name = self.type_pretty_name(type_);
        name += ".";
        name += constructor.name.clone();

        let key = WasmType::union(name.clone(), fields.clone(), Some(supertype_index));
        let type_index = if let Some(id) = self.wasm_types.get(&key) {
            *id
        } else {
            let id = self.union_type_add(&name, &fields, Some(supertype_index));
            let _ = self.wasm_types.insert(key, id);
            id
        };
        (supertype_index, type_index, types)
    }

    fn union_type_add(
        &mut self,
        name: &EcoString,
        fields: &[(EcoString, ValType)],
        supertype: Option<TypeId>,
    ) -> TypeId {
        let tag_field: (EcoString, ValType) = ("tag".into(), ValType::I32);
        let walrus_fields: Vec<FieldType> = iter::once(&tag_field)
            .chain(fields)
            .map(|(_, vt)| FieldType {
                element_type: StorageType::Val(*vt),
                mutable: false,
            })
            .collect();
        // Supertypes are not final (open for subtyping). Subtypes are final.
        let is_final = supertype.is_some();
        let comp = walrus::CompositeType::Struct(walrus::StructType {
            fields: walrus_fields.into_boxed_slice(),
        });
        let id = self
            .wasm_module
            .types
            .add_composite(comp, is_final, supertype);
        self.wasm_module.types.get_mut(id).name = Some(name.to_string());
        id
    }

    fn composite_val_type(&self, type_index: TypeId) -> ValType {
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
            CustomType::Union { custom_type, .. } => {
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
                    dest: id.global_id(),
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
                    global_index: id.global_id(),
                    type_index,
                    tag: None,
                    elements: elements.clone(),
                });
                id
            }
            Constant::List { type_, .. } => {
                let (custom_type, _) = self.custom_type_expect(type_);
                let CustomType::Union { custom_type, .. } = custom_type else {
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
                    global_index: id.global_id(),
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
                            ConstExpr::Value(Value::I32(value as i32)),
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
                            global_index: id.global_id(),
                            type_index,
                            tag: None,
                            elements: arguments.iter().map(|e| &e.value).cloned().collect(),
                        });
                        id
                    }
                    CustomType::Union { custom_type, .. } => {
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
                            global_index: id.global_id(),
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
                    (self.float.float_const("0"), self.float.val_type())
                } else if let Some((CustomType::External { val_type, .. }, _)) =
                    self.custom_type(type_)
                {
                    (ConstExpr::Value(Value::I32(0)), val_type)
                } else if let Some((CustomType::Enum { .. }, _)) = self.custom_type(type_) {
                    (ConstExpr::Value(Value::I32(0)), ValType::I32)
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
                        dest: id.global_id(),
                        src: var_id.func_id(),
                    });
                } else {
                    self.consts.push(WasmConst::Var {
                        global_index: id.global_id(),
                        name: name.clone(),
                    });
                }
                id
            }
            Constant::BitArray { .. } => todo!("BitArray constants are not yet supported"),
            Constant::RecordUpdate { .. } => {
                panic!("record update constants should not reach code generation")
            }
            Constant::StringConcatenation { .. } => {
                let id = self.add_const(
                    const_name,
                    self.string.val_type_nullable(),
                    const_expr_ref_null(self.string.type_index),
                    export,
                    true,
                );
                self.consts.push(WasmConst::Constant {
                    global_index: id.global_id(),
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
            Constant::RecordUpdate { .. } => {
                panic!("record update constants should not reach code generation")
            }
            Constant::StringConcatenation { left, right, .. } => {
                self.register_string_const(left);
                self.register_string_const(right);
            }
            Constant::Invalid { .. } => {
                panic!("invalid constants should not reach code generation")
            }
        }
    }

    fn _constant(&mut self, instructions: &mut Instructions<'_, '_>, const_: &TypedConstant) {
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
            Constant::RecordUpdate { .. } => {
                panic!("record update constants should not reach code generation")
            }
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

    /// Returns a u32 unique to each function emitted by the compiler. Used
    /// to disambiguate anonymous inner functions sharing a source location.
    fn allocate_parent_id(&self) -> u32 {
        self.wasm_module.funcs.iter().count() as u32
    }

    fn allocate_placeholder_function(
        &mut self,
        name: &str,
        params: &[ValType],
        results: &[ValType],
    ) -> FunctionId {
        let fb = FunctionBuilder::new(&mut self.wasm_module.types, params, results);
        let id = fb.finish(vec![], &mut self.wasm_module.funcs);
        self.wasm_module.funcs.get_mut(id).name = Some(name.to_string());
        id
    }

    fn _function(
        &mut self,
        function: TypedFunction,
        name: EcoString,
        export: bool,
        original_local_functions: HashMap<EcoString, LocalFunction>,
    ) -> Id {
        let params = self.val_types(function_params_types(&function));
        let results = self.val_types(iter::once(function.return_type.clone()));
        let index = self.allocate_placeholder_function(&name, &params, &results);
        let id = self.add_function_to_globals(name.clone(), index);
        let locals = Locals::new(self, &function.arguments, &function.body);
        let param_ids = locals.param_ids();
        let mut code = Function::new(self, &name, &param_ids, &results);
        let mut instructions = code.extend_instructions(self);
        let saved_local_functions =
            std::mem::replace(&mut self.local_functions, original_local_functions);
        self.statements(
            &mut instructions,
            Scope::with_params(self.globals.clone(), &function.arguments, &param_ids),
            &locals,
            &function.body,
        );
        self.local_functions = saved_local_functions;
        let _ = instructions.end();

        for (local_id, lname) in locals.names() {
            self.wasm_module.locals.get_mut(local_id).name = Some(lname.to_string());
        }
        let real_id = code.finish(self);
        replace_function_body(&mut self.wasm_module, index, real_id);
        if export {
            let _ = self.wasm_module.exports.add(&name, index);
        }
        id
    }

    fn function(&mut self, function: &TypedFunction, export: bool, base_name: EcoString) -> Id {
        if let Some(id) = self.find_global(&base_name) {
            return id;
        }
        let original_local_functions =
            collect_local_functions(&function.body, self.allocate_parent_id());
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
            collect_local_functions(&function.body, self.allocate_parent_id());
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
        let (gleam_params, return_) = type_.fn_types().expect("function type");
        let params = self.val_types(gleam_params);
        let results = self.val_types(iter::once(return_));
        let index = self.allocate_placeholder_function(&name, &params, &results);
        let id = self.add_function_to_globals(name.clone(), index);
        let locals = Locals::new(self, arguments, body);
        let param_ids = locals.param_ids();
        let mut code = Function::new(self, &name, &param_ids, &results);
        let mut instructions = code.extend_instructions(self);
        let saved_local_functions = self.local_functions.clone();
        self.statements(
            &mut instructions,
            Scope::with_params(self.globals.clone(), arguments, &param_ids),
            &locals,
            body,
        );
        self.local_functions = saved_local_functions;
        let _ = instructions.end();

        for (local_id, lname) in locals.names() {
            self.wasm_module.locals.get_mut(local_id).name = Some(lname.to_string());
        }
        let real_id = code.finish(self);
        replace_function_body(&mut self.wasm_module, index, real_id);
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
        instructions: &mut Instructions<'_, '_>,
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
                Statement::Expression(_)
                | Statement::Assignment(_)
                | Statement::Use(_)
                | Statement::Assert(_) => {
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
        instructions: &mut Instructions<'_, '_>,
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
        instructions: &mut Instructions<'_, '_>,
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
        let heap_base = self.find_global_expect(HEAP_BASE).func_id();
        let print = self.find_global_expect(PRINT).func_id();
        let exit = self.find_global_expect(EXIT).func_id();
        #[rustfmt::skip]
        let _ = instructions
            .expression(self, locals, scope.clone(), &assert.value)
            .if_else(
                BOOL_VALTYPE,
                |then_s| {
                    let _ = then_s.bool_const(true);
                },
                |else_s| {
                    let _ = else_s
                        .show_error_message(prefix, location, string_to_memory, heap_base, print)
                        .i32_const(1)
                        .call(exit)
                        .unreachable();
                },
            );
    }

    fn _expression(
        &mut self,
        locals: &Locals,
        scope: Scope,
        instructions: &mut Instructions<'_, '_>,
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
            TypedExpr::PositionalAccess { index, record, .. } => {
                // Same as RecordAccess but by position
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
                    CustomType::Union { custom_type, .. } => {
                        let layout = self.union_layout(&record.type_()).clone();
                        if let Some(variant) = record.type_().custom_type_inferred_variant() {
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
                            let field_index = layout.field(variant as usize, *index);
                            let _ = instructions
                                .expression(self, locals, scope, record)
                                .ref_cast_non_null(HeapType::Concrete(type_index))
                                .struct_get(type_index, field_index);
                        } else {
                            let supertype_index =
                                self.mono_union_supertype_index(&record.type_(), &custom_type);
                            let field_index = layout.shared_field(*index);
                            let _ = instructions
                                .expression(self, locals, scope, record)
                                .struct_get(supertype_index, field_index);
                        }
                    }
                    CustomType::External { .. } | CustomType::Enum { .. } => {
                        panic!("external/enum types should not reach code generation")
                    }
                }
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
                    let _ = instructions.ref_func(id.func_id());
                }
                ValueConstructorVariant::ModuleConstant { literal, .. } => {
                    self._constant(instructions, literal);
                }
                ValueConstructorVariant::LocalVariable { .. }
                | ValueConstructorVariant::ModuleFn { .. }
                | ValueConstructorVariant::Record { .. } => {
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
                let _ = instructions.ref_func(id.func_id());
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
                    CustomType::Union { custom_type, .. } => {
                        let layout = self.union_layout(&record.type_()).clone();
                        if let Some(variant) = record.type_().custom_type_inferred_variant() {
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
                            let field_index = layout.field(variant as usize, *index);
                            let _ = instructions
                                .expression(self, locals, scope, record)
                                .ref_cast_non_null(HeapType::Concrete(type_index))
                                .struct_get(type_index, field_index);
                        } else {
                            // Shared field — access via supertype
                            let supertype_index =
                                self.mono_union_supertype_index(&record.type_(), &custom_type);
                            let field_index = layout.shared_field(*index);
                            let _ = instructions
                                .expression(self, locals, scope, record)
                                .struct_get(supertype_index, field_index);
                        }
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
                    let _ = instructions.ref_func(id.func_id());
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
        instructions: &mut Instructions<'_, '_>,
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
        let heap_base = self.find_global_expect(HEAP_BASE).func_id();
        let print = self.find_global_expect(PRINT).func_id();
        let exit = self.find_global_expect(EXIT).func_id();

        let _ =
            instructions.show_error_message(prefix, location, string_to_memory, heap_base, print);

        if let Some(message) = message {
            assert!(message.type_().is_string());
            let _ = instructions
                .i32_const(STDERR)
                .call(heap_base)
                .expression(self, locals, scope, message)
                .call(heap_base)
                .call(string_to_memory)
                .call(print)
                .call(heap_base)
                .byte_store(b'\n')
                .i32_const(STDERR)
                .call(heap_base)
                .i32_const(1)
                .call(print);
        }

        let _ = instructions.i32_const(1).call(exit).unreachable();
    }

    fn expression_bin_op(
        &mut self,
        locals: &Locals,
        scope: Scope,
        instructions: &mut Instructions<'_, '_>,
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
            BinOp::And => {
                let _ = instructions.expression(self, locals, scope.clone(), left);
                instructions.if_else(
                    BOOL_VALTYPE,
                    |then_s| {
                        let _ = then_s.expression(self, locals, scope.clone(), right);
                    },
                    |else_s| {
                        let _ = else_s.bool_const(false);
                    },
                )
            }
            BinOp::Or => {
                let _ = instructions.expression(self, locals, scope.clone(), left);
                instructions.if_else(
                    BOOL_VALTYPE,
                    |then_s| {
                        let _ = then_s.bool_const(true);
                    },
                    |else_s| {
                        let _ = else_s.expression(self, locals, scope.clone(), right);
                    },
                )
            }
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
    ///     pattern(fail_target=alt_id)
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
        instructions: &mut Instructions<'_, '_>,
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
                subjects_locals.push(id.local_id());
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
        let case_type = self.val_type(type_);
        let _ = instructions.block_(case_type, |case_b| {
            let case_id = case_b.id();
            for clause in clauses {
                let mut scope = scope.clone();
                for patterns in iter::once(&clause.pattern).chain(&clause.alternative_patterns) {
                    // block alt — _pattern branches here on failure
                    let _ = case_b.block_(InstrSeqType::Simple(None), |alt_b| {
                        let alt_id = alt_b.id();
                        for (pattern, subject_local) in patterns.iter().zip(&subjects_locals) {
                            let _ = alt_b.local_get(*subject_local);
                            scope = self._pattern(locals, scope.clone(), alt_b, pattern, alt_id);
                        }
                        // All patterns matched — check guard if present
                        if let Some(guard) = &clause.guard {
                            let _ = alt_b
                                .clause_guard(self, locals, &scope, guard)
                                .bool_not()
                                .br_if(alt_id);
                        }
                        let _ = alt_b
                            .expression(self, locals, scope.clone(), &clause.then)
                            // exit block case
                            .br(case_id);
                    });
                }
            }
            // end block case
            let _ = case_b.unreachable();
        });
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
        instructions: &mut Instructions<'_, '_>,
        name: &EcoString,
        type_: &Arc<Type>,
    ) {
        if let Some(info) = self.local_functions.get(name).cloned()
            && type_.fn_types().is_some()
        {
            let id = self.function_local_generic(&info, type_);
            let _ = instructions.ref_func(id.func_id());
            return;
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
                    instructions.call(id.func_id())
                } else {
                    instructions.ref_func(id.func_id())
                }
            }
            IdKind::Global => {
                if self.is_ref_non_null(type_) {
                    instructions.global_as_non_null(id.global_id())
                } else {
                    instructions.global_get(id.global_id())
                }
            }
            IdKind::Local => instructions.local_get(id.local_id()),
        };
    }

    fn expression_echo(
        &mut self,
        locals: &Locals,
        scope: &Scope,
        instructions: &mut Instructions<'_, '_>,
        echo: &TypedExpr,
        expression: &Option<Box<TypedExpr>>,
        message: &Option<Box<TypedExpr>>,
    ) {
        if let Some(expression) = expression {
            let print = self.find_global_expect(PRINT).func_id();
            let heap_base = self.find_global_expect(HEAP_BASE).func_id();
            let string_to_memory =
                self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
            let (mod_name, line) = self.source_location(echo.location().start);
            let suffix = if message.is_some() { ' ' } else { '\n' };
            let string_index =
                self.string_index(&format!("src/{mod_name}.gleam:{line}{suffix}").into());

            let (dest, expr) = locals.for_echo(echo);

            let expr = match expr {
                Ok(expr) => expr,
                Err(name) => self.var_id(scope, name, &echo.type_()).local_id(),
            };

            let _ = instructions
                .expression(self, locals, scope.clone(), expression)
                .local_set(expr)
                // write the module name and line number
                .global_as_non_null(string_index)
                .call(heap_base)
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
                .call(heap_base)
                .local_get(dest)
                .call(heap_base)
                .i32_sub()
                .call(print)
                .drop()
                // recover expression value
                .local_get(expr);
        }
    }

    fn assignment(
        &mut self,
        locals: &Locals,
        mut scope: Scope,
        instructions: &mut Instructions<'_, '_>,
        assignment: &TypedAssignment,
    ) -> Scope {
        let right = locals.for_assigment(assignment);
        let body_id = instructions.id();
        let _ = instructions
            .expression(self, locals, scope.clone(), &assignment.value)
            .local_tee(right)
            .pattern(self, locals, &mut scope, &assignment.pattern, body_id)
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
    ///     pattern(fail_target=alt_id)     ;; br_if 0 on failure
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
        instructions: &mut Instructions<'_, '_>,
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
        let heap_base = self.find_global_expect(HEAP_BASE).func_id();
        let print = self.find_global_expect(PRINT).func_id();
        let exit = self.find_global_expect(EXIT).func_id();

        let mut scope = scope;
        let _ = instructions.block_(result_type, |skip_b| {
            let skip_id = skip_b.id();
            let _ = skip_b.block_(InstrSeqType::Simple(None), |fail_b| {
                let fail_id = fail_b.id();
                let _ = fail_b.local_get(right).pattern(
                    self,
                    locals,
                    &mut scope,
                    &assignment.pattern,
                    fail_id,
                );
                // Pattern matched — remaining statements run here.
                if !remaining.is_empty() {
                    self.statements(fail_b, scope.clone(), locals, remaining);
                } else {
                    let _ = fail_b.local_get(right);
                }
                let _ = fail_b.br(skip_id);
            });
            // error handler (dead path — unreachable tells validator)
            let _ = skip_b
                .show_error_message(prefix, location, string_to_memory, heap_base, print)
                .i32_const(1)
                .call(exit)
                .unreachable();
        });
    }

    /// Checks if the subject (on the stack) matches the pattern. On
    /// failure, branches to `fail_target`. On success, falls through
    /// with pattern locals set.
    ///
    /// ```wat
    /// block alt:
    ///   local.get subject
    ///   pattern(fail_target=alt_id)   ;; br_if 0 on failure
    ///   <body>                  ;; pattern locals are initialized here
    /// end alt
    /// ```
    fn _pattern(
        &mut self,
        locals: &Locals,
        mut scope: Scope,
        instructions: &mut Instructions<'_, '_>,
        pattern: &TypedPattern,
        fail_target: InstrSeqId,
    ) -> Scope {
        match pattern {
            Pattern::Discard { .. } => {
                let _ = instructions.drop();
            }
            Pattern::Int { int_value, .. } => {
                let _ = instructions
                    .int_const(int_value)
                    .int_ne()
                    .br_if(fail_target);
            }
            Pattern::Float { value, .. } => {
                let _ = instructions
                    .float_const(value)
                    .float_ne()
                    .br_if(fail_target);
            }
            Pattern::String { value, .. } => {
                let index = self.string_index(value);
                let eq = self.function_eq(&type_::string());
                let _ = instructions
                    .global_as_non_null(index)
                    .eq(eq)
                    .bool_not()
                    .br_if(fail_target);
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
                        .br_if(fail_target);
                }
                for element in elements {
                    // Cons struct: {tag: 0, rest: 1, first: 2}
                    let _ = instructions
                        .local_get(right)
                        .ref_cast_non_null(HeapType::Concrete(cons_index))
                        .struct_get(cons_index, 2); // first
                    scope = self._pattern(locals, scope, instructions, element, fail_target);
                    let _ = instructions
                        .local_get(right)
                        .ref_cast_non_null(HeapType::Concrete(cons_index))
                        .struct_get(cons_index, 1) // rest
                        .local_set(right);
                }
                if let Some(tail) = tail {
                    let _ = instructions.local_get(right);
                    scope = self._pattern(locals, scope, instructions, &tail.pattern, fail_target);
                } else {
                    let _ = instructions
                        .local_get(right)
                        .ref_is_null()
                        .bool_not()
                        .br_if(fail_target);
                }
            }
            Pattern::Tuple { elements, .. } => {
                let type_index = self.tuple_type_index(elements.iter().map(|e| e.type_()));
                let _ = instructions.patterns(
                    self,
                    locals,
                    &mut scope,
                    (type_index, None),
                    None,
                    pattern,
                    elements.iter(),
                    fail_target,
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
                            .br_if(fail_target);
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
                            None,
                            pattern,
                            arguments.iter().map(|arg| &arg.value),
                            fail_target,
                        );
                    }
                    CustomType::Union { custom_type, .. } => {
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
                                .br_if(fail_target);
                        } else {
                            let (supertype_index, type_index, _) = self.mono_union_subtype_index(
                                type_,
                                &custom_type,
                                constructor,
                                &args,
                            );
                            let _ = instructions.local_tee(right);
                            if null_tag.is_some() {
                                let _ = instructions.ref_is_null().br_if(fail_target);
                                let _ = instructions
                                    .local_get(right)
                                    .struct_get(supertype_index, 0)
                                    .i32_const(tag as i32)
                                    .i32_ne()
                                    .br_if(fail_target);
                            } else {
                                let _ = instructions
                                    .struct_get(supertype_index, 0)
                                    .i32_const(tag as i32)
                                    .i32_ne()
                                    .br_if(fail_target);
                            }
                            let layout = self.union_layout(type_).clone();
                            let field_mapping = layout.fields(tag);
                            let _ = instructions.local_get(right).patterns(
                                self,
                                locals,
                                &mut scope,
                                (supertype_index, Some(type_index)),
                                Some(field_mapping),
                                pattern,
                                arguments.iter().map(|arg| &arg.value),
                                fail_target,
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
                    .br_if(fail_target);

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
                scope = self._pattern(locals, scope, instructions, inner, fail_target);
            }
            Pattern::Invalid { .. } => {
                panic!("invalid patterns should not reach code generation")
            }
        }
        scope
    }

    #[allow(clippy::too_many_arguments)]
    fn _patterns<'b>(
        &mut self,
        locals: &Locals,
        scope: &mut Scope,
        instructions: &mut Instructions<'_, '_>,
        (type_index, subtype_index): (TypeId, Option<TypeId>),
        field_mapping: Option<&[u32]>,
        pattern: &Pattern<Arc<Type>>,
        elements: impl IntoIterator<Item = &'b Pattern<Arc<Type>>> + Clone,
        fail_target: InstrSeqId,
    ) {
        let right = locals.for_pattern(pattern);
        let _ = instructions.local_set(right);
        for (field_index, element) in elements.into_iter().enumerate() {
            let _ = instructions.local_get(right);
            if let Some(subtype_index) = subtype_index {
                let field_index = field_mapping
                    .and_then(|m| m.get(field_index).copied())
                    .unwrap_or(field_index as u32 + 1);
                let _ = instructions
                    .ref_cast_non_null(HeapType::Concrete(subtype_index))
                    .struct_get(subtype_index, field_index);
            } else {
                let _ = instructions.struct_get(type_index, field_index as u32);
            }
            *scope = self._pattern(locals, scope.clone(), instructions, element, fail_target);
        }
    }

    fn _clause_guard(
        &mut self,
        locals: &Locals,
        scope: &Scope,
        instructions: &mut Instructions<'_, '_>,
        guard: &TypedClauseGuard,
    ) {
        match guard {
            ClauseGuard::BinaryOperator {
                operator,
                left,
                right,
                ..
            } => match operator {
                BinOp::Or => {
                    let _ = instructions
                        .clause_guard(self, locals, scope, left)
                        .if_else(
                            BOOL_VALTYPE,
                            |then_s| {
                                let _ = then_s.bool_const(true);
                            },
                            |else_s| {
                                let _ = else_s.clause_guard(self, locals, scope, right);
                            },
                        );
                }
                BinOp::And => {
                    let _ = instructions
                        .clause_guard(self, locals, scope, left)
                        .if_else(
                            BOOL_VALTYPE,
                            |then_s| {
                                let _ = then_s.clause_guard(self, locals, scope, right);
                            },
                            |else_s| {
                                let _ = else_s.bool_const(false);
                            },
                        );
                }
                BinOp::AddInt => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .int_add();
                }
                BinOp::SubInt => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .int_sub();
                }
                BinOp::MultInt => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .int_mul();
                }
                BinOp::RemainderInt => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .int_rem();
                }
                BinOp::DivInt => {
                    let (a, b) = locals.for_guard_div(scope, left, right);
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .int_div(a, b);
                }
                BinOp::GtInt => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .int_gt();
                }
                BinOp::GtEqInt => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .int_ge();
                }
                BinOp::LtInt => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .int_lt();
                }
                BinOp::LtEqInt => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .int_le();
                }
                BinOp::AddFloat => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .float_add();
                }
                BinOp::SubFloat => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .float_sub();
                }
                BinOp::MultFloat => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .float_mul();
                }
                BinOp::DivFloat => {
                    let (a, b) = locals.for_guard_div(scope, left, right);
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .float_div(a, b);
                }
                BinOp::GtFloat => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .float_gt();
                }
                BinOp::GtEqFloat => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .float_ge();
                }
                BinOp::LtFloat => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .float_lt();
                }
                BinOp::LtEqFloat => {
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .float_le();
                }
                BinOp::Eq => {
                    let eq = self.function_eq(&left.type_());
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .eq(eq);
                }
                BinOp::NotEq => {
                    let eq = self.function_eq(&left.type_());
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .eq(eq)
                        .bool_not();
                }
                BinOp::Concatenate => {
                    let concat =
                        self.get_function_builtin_external(BuiltinFunctionExternal::StringConcat);
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .call(concat);
                }
            },
            ClauseGuard::Not { expression, .. } => {
                let _ = instructions
                    .clause_guard(self, locals, scope, expression)
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
                    CustomType::Union { custom_type, .. } => {
                        let layout = self.union_layout(&type_).clone();
                        let constructor = custom_type_inferred_constructor(&custom_type, &type_)
                            .expect("inferred constructor");
                        let variant = custom_type
                            .constructors
                            .iter()
                            .position(|c| c.name == constructor.name)
                            .expect("constructor in union");
                        let (_, type_index, _) =
                            self.mono_union_subtype_index(&type_, &custom_type, constructor, &args);
                        let field_index = layout.field(variant, index as u64);
                        let _ = instructions
                            .clause_guard(self, locals, scope, container)
                            .ref_cast_non_null(HeapType::Concrete(type_index))
                            .struct_get(type_index, field_index);
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
        let index = self
            .wasm_module
            .globals
            .add_local(val_type, mutable, false, expr);
        self.wasm_module.globals.get_mut(index).name = Some(name.to_string());
        if export {
            let _ = self.wasm_module.exports.add(name, index);
        }
        let id = Id::global(name.clone(), index);
        self.globals.borrow_mut().push(id.clone());
        id
    }

    fn get_function_builtin_external(&mut self, builtin: BuiltinFunctionExternal) -> FunctionId {
        if let Some(id) = self.builtins_external.get(&builtin) {
            return *id;
        }

        let index = match builtin {
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
        self.wasm_module.funcs.get_mut(index).name = Some(builtin.name().to_string());
        let _ = self.builtins_external.insert(builtin, index);
        index
    }

    pub(super) fn wasm_local(&mut self, val_type: ValType) -> LocalId {
        self.wasm_module.locals.add(val_type)
    }
}

fn const_expr_ref_null(type_index: TypeId) -> ConstExpr {
    ConstExpr::RefNull(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(type_index),
    })
}

/// Replace the body (kind/name) of `dest` with the body of `src`, then
/// delete `src`. Used to honour pre-allocated FunctionIds for recursive
/// references when building functions and types.
fn replace_function_body(module: &mut walrus::Module, dest: FunctionId, src: FunctionId) {
    let dest_ty = module.funcs.get(dest).ty();
    let placeholder = walrus::FunctionKind::Uninitialized(dest_ty);
    let src_kind = std::mem::replace(&mut module.funcs.get_mut(src).kind, placeholder);
    let src_name = module.funcs.get_mut(src).name.take();
    module.funcs.get_mut(dest).kind = src_kind;
    if let Some(name) = src_name {
        module.funcs.get_mut(dest).name = Some(name);
    }
    module.funcs.delete(src);
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

/// Tarjan's SCC algorithm. Returns SCCs in reverse topological order
/// (post-order: leaves first, roots last).
fn tarjan_scc(node_count: usize, edges: &[Vec<usize>]) -> Vec<Vec<usize>> {
    struct State {
        index_of: Vec<Option<usize>>,
        lowlink: Vec<usize>,
        on_stack: Vec<bool>,
        stack: Vec<usize>,
        next_index: usize,
        sccs: Vec<Vec<usize>>,
    }

    fn strongconnect(v: usize, edges: &[Vec<usize>], state: &mut State) {
        if let Some(slot) = state.index_of.get_mut(v) {
            *slot = Some(state.next_index);
        }
        if let Some(slot) = state.lowlink.get_mut(v) {
            *slot = state.next_index;
        }
        state.next_index += 1;
        state.stack.push(v);
        if let Some(slot) = state.on_stack.get_mut(v) {
            *slot = true;
        }

        let neighbors = edges.get(v).cloned().unwrap_or_default();
        for w in neighbors {
            if state.index_of.get(w).copied().flatten().is_none() {
                strongconnect(w, edges, state);
                let w_low = state.lowlink.get(w).copied().unwrap_or(0);
                if let Some(slot) = state.lowlink.get_mut(v) {
                    *slot = (*slot).min(w_low);
                }
            } else if state.on_stack.get(w).copied().unwrap_or(false) {
                let w_idx = state.index_of.get(w).copied().flatten().unwrap_or(0);
                if let Some(slot) = state.lowlink.get_mut(v) {
                    *slot = (*slot).min(w_idx);
                }
            }
        }

        let v_low = state.lowlink.get(v).copied().unwrap_or(0);
        let v_idx = state.index_of.get(v).copied().flatten().unwrap_or(0);
        if v_low == v_idx {
            let mut scc = Vec::new();
            loop {
                let w = state
                    .stack
                    .pop()
                    .expect("tarjan: stack non-empty at SCC pop");
                if let Some(slot) = state.on_stack.get_mut(w) {
                    *slot = false;
                }
                let stop = w == v;
                scc.push(w);
                if stop {
                    break;
                }
            }
            state.sccs.push(scc);
        }
    }

    let mut state = State {
        index_of: vec![None; node_count],
        lowlink: vec![0; node_count],
        on_stack: vec![false; node_count],
        stack: Vec::new(),
        next_index: 0,
        sccs: Vec::new(),
    };

    for v in 0..node_count {
        if state.index_of.get(v).copied().flatten().is_none() {
            strongconnect(v, edges, &mut state);
        }
    }

    state.sccs
}

#[cfg(test)]
#[test]
fn test_tarjan_scc() {
    // Simple cycle: 0 → 1 → 0
    let sccs = tarjan_scc(2, &[vec![1], vec![0]]);
    assert_eq!(sccs.len(), 1);
    assert_eq!(sccs[0].len(), 2);

    // DAG: 0 → 1 → 2 (3 singleton SCCs in reverse topo order: 2, 1, 0)
    let sccs = tarjan_scc(3, &[vec![1], vec![2], vec![]]);
    assert_eq!(sccs, vec![vec![2], vec![1], vec![0]]);

    // Self-loop: 0 → 0
    let sccs = tarjan_scc(1, &[vec![0]]);
    assert_eq!(sccs, vec![vec![0]]);

    // Mixed: 0 → 1, 1 → 2, 2 → 1 (SCCs: {1,2}, {0})
    let sccs = tarjan_scc(3, &[vec![1], vec![2], vec![1]]);
    assert_eq!(sccs.len(), 2);
    let mut first: Vec<usize> = sccs[0].clone();
    first.sort();
    assert_eq!(first, vec![1, 2]);
    assert_eq!(sccs[1], vec![0]);
}

/// ValType for a field whose referenced type (if any) is already emitted
/// (its TypeId is in `assigned`). Used for singleton-non-cyclic SCC emission.
fn resolve_val_type_assigned(
    t: &Arc<Type>,
    info: TypeNodeFieldRef,
    assigned: &[Option<TypeId>],
    int: IntType,
    float: FloatType,
    string_idx: TypeId,
) -> ValType {
    if let Some(v) = primitive_val_type(t, int, float, string_idx) {
        return v;
    }
    let idx = info
        .descriptor_idx
        .expect("emit: field type not discovered");
    let id = assigned
        .get(idx)
        .copied()
        .flatten()
        .expect("emit: ref to type emitted in later SCC");
    ValType::Ref(RefType {
        heap_type: HeapType::Concrete(id),
        nullable: info.nullable,
    })
}

/// ValType for a field inside an SCC being emitted via `add_rec_group`.
/// Resolves via the SCC's pre-allocated `ids` for cyclic refs and `assigned`
/// for external refs.
fn resolve_val_type_in_scc(
    t: &Arc<Type>,
    info: TypeNodeFieldRef,
    ids: &[TypeId],
    assigned: &[Option<TypeId>],
    local_of: &HashMap<usize, usize>,
    int: IntType,
    float: FloatType,
    string_idx: TypeId,
) -> ValType {
    if let Some(v) = primitive_val_type(t, int, float, string_idx) {
        return v;
    }
    let idx = info
        .descriptor_idx
        .expect("emit: field type not discovered");
    let id = if let Some(local) = local_of.get(&idx) {
        *ids.get(*local).expect("scc rec_group id")
    } else {
        assigned
            .get(idx)
            .copied()
            .flatten()
            .expect("scc: external ref not yet emitted")
    };
    ValType::Ref(RefType {
        heap_type: HeapType::Concrete(id),
        nullable: info.nullable,
    })
}

fn primitive_val_type(
    t: &Arc<Type>,
    int: IntType,
    float: FloatType,
    string_idx: TypeId,
) -> Option<ValType> {
    if t.is_int() {
        return Some(int.val_type());
    }
    if t.is_float() {
        return Some(float.val_type());
    }
    if t.is_string() {
        return Some(ValType::Ref(RefType {
            heap_type: HeapType::Concrete(string_idx),
            nullable: false,
        }));
    }
    if t.is_bool() || t.is_utf_codepoint() {
        return Some(ValType::I32);
    }
    if let Some((_, n)) = t.named_type_name()
        && n.as_str() == "Nil"
    {
        return Some(ValType::I32);
    }
    None
}

fn build_composite_for_node(
    kind: &TypeNodeKind,
    val_types: Vec<ValType>,
    result_count: usize,
) -> walrus::CompositeType {
    match kind {
        TypeNodeKind::PlainStruct | TypeNodeKind::Tuple => {
            let fields: Vec<FieldType> = val_types
                .into_iter()
                .map(|vt| FieldType {
                    element_type: StorageType::Val(vt),
                    mutable: false,
                })
                .collect();
            walrus::CompositeType::Struct(walrus::StructType {
                fields: fields.into_boxed_slice(),
            })
        }
        TypeNodeKind::UnionSupertype | TypeNodeKind::UnionSubtype => {
            let mut fields: Vec<FieldType> = vec![FieldType {
                element_type: StorageType::Val(ValType::I32),
                mutable: false,
            }];
            fields.extend(val_types.into_iter().map(|vt| FieldType {
                element_type: StorageType::Val(vt),
                mutable: false,
            }));
            walrus::CompositeType::Struct(walrus::StructType {
                fields: fields.into_boxed_slice(),
            })
        }
        TypeNodeKind::Function => {
            let total = val_types.len();
            let param_count = total.saturating_sub(result_count);
            let (params_slice, results_slice) = val_types.split_at(param_count);
            walrus::CompositeType::Function(walrus::FunctionType::new(
                params_slice.to_vec().into_boxed_slice(),
                results_slice.to_vec().into_boxed_slice(),
            ))
        }
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
