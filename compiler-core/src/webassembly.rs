// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

#![allow(clippy::todo)]
mod array;
mod builtins;
mod captures;
mod dynamic;
mod glue;
mod hash;
mod instructions;
mod link;
mod liveness;
mod monomorphize;
mod runtime;
mod scope;
mod specialization;
mod stdlib;
#[cfg(test)]
mod tests;

use array::*;
pub use builtins::builtin_function_names;
use builtins::*;
use captures::{Capture, Variables};
use dynamic::*;
use glue::*;
use hash::*;
use instructions::*;
use monomorphize::*;
use runtime::{Hooks, RUNTIME_PACKAGE};
pub use runtime::{RuntimeModule, modules as runtime_modules};
use scope::*;
use specialization::*;
use stdlib::*;

use ecow::EcoString;
use indexmap::IndexMap;
use itertools::Itertools;
use num_bigint::BigInt;
use std::{
    cell::{OnceCell, RefCell},
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
    STDLIB_PACKAGE_NAME,
    ast::{
        AssignName, AssignmentKind, BinOp, ClauseGuard, Constant, PIPE_VARIABLE, Pattern,
        Publicity, Statement, StringPrefixLeftSideAssignment, TypeAst, TypeAstVar, TypedArg,
        TypedAssignment, TypedClause, TypedClauseGuard, TypedConstant, TypedCustomType, TypedExpr,
        TypedFunction, TypedModule, TypedModuleConstant, TypedPattern, TypedRecordConstructor,
        TypedRecordConstructorArg, TypedStatement, visit::Visit,
    },
    type_::{
        self, Deprecation, ModuleValueConstructor, PRELUDE_MODULE_NAME, Type, TypeVar,
        ValueConstructor, ValueConstructorVariant,
        printer::{Names, Printer},
    },
};
use src_span::{LineNumbers, SrcSpan};

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

const UTF_CODEPOINT_REPR: &str = "_utf_codepoint_repr";

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
    /// An external function or type outside the standard library, which
    /// alone reaches the builtins.
    ExternalOutsideStdlib {
        location: SrcSpan,
    },
    /// A module of the program has the name of a module of the runtime.
    ReservedModuleName {
        name: EcoString,
    },
    WrongExternalTypeArity {
        location: SrcSpan,
        name: EcoString,
        expected: usize,
        got: usize,
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
    /// A type is too large to be generated, see `val_type`. `definition` is
    /// the module and the location of the outermost custom type in it.
    TypeTooLarge {
        type_: EcoString,
        definition: Option<(EcoString, SrcSpan)>,
    },
    /// A local variable used with different types, see
    /// `Monomorphizer::conflicting_reference`. `location` is a reference in
    /// `module`.
    GenericLocalVariable {
        module: EcoString,
        location: SrcSpan,
        name: EcoString,
    },
}

impl Error {
    /// The module of the location of the error, when it may not be the
    /// module being generated.
    pub fn module(&self) -> Option<&EcoString> {
        match self {
            Error::TypeTooLarge {
                definition: Some((module, _)),
                ..
            }
            | Error::GenericLocalVariable { module, .. } => Some(module),
            Error::UnknownExternalType { .. }
            | Error::UnknownBuiltinFunction { .. }
            | Error::WrongBuiltinFunctionSignature { .. }
            | Error::UnknownExternalModule { .. }
            | Error::ExternalOutsideStdlib { .. }
            | Error::ReservedModuleName { .. }
            | Error::WrongExternalTypeArity { .. }
            | Error::UnsupportedFeature { .. }
            | Error::IntLiteralOutOfRange { .. }
            | Error::FloatLiteralOutOfRange { .. }
            | Error::TypeTooLarge {
                definition: None, ..
            } => None,
        }
    }
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
    runtime: &[RuntimeModule],
    int: crate::config::WasmInt,
    float: crate::config::WasmFloat,
) -> Result<Vec<u8>, Error> {
    let mut all_modules = all_modules.clone();
    for runtime_module in runtime {
        let name = &runtime_module.module.name;
        if all_modules
            .insert(name.clone(), &runtime_module.module)
            .is_some()
        {
            return Err(Error::ReservedModuleName { name: name.clone() });
        }
    }
    let mut generator = Generator::new(module, line_numbers, &all_modules, all_line_numbers);
    generator.runtime = runtime;
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

fn eliminate_dead_code(
    module: &mut walrus::Module,
    builtin_exports: &[walrus::ExportId],
    builtin_names: &HashSet<String>,
) {
    // The functions exported by the builtins are not exports of the program:
    // only those reached from its code survive.
    for &id in builtin_exports {
        module.exports.delete(id);
    }
    // Clear declarative element segments so declared functions are not
    // treated as roots. Active segments fill the table of the Rust builtins
    // and must stay for their indirect calls.
    for elem in module
        .elements
        .iter()
        .filter(|e| matches!(e.kind, walrus::ElementKind::Declared))
        .map(|e| e.id())
        .collect::<Vec<_>>()
    {
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
        .map(|d| {
            let kind = match &d.kind {
                walrus::DataKind::Active { memory, offset } => Some((*memory, offset.clone())),
                walrus::DataKind::Passive => None,
            };
            (d.id(), kind)
        })
        .collect();
    for &(id, _) in &active_data {
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
        let data_needed = is_builtin_data_needed(&surviving_names, builtin_names);
        for (id, kind_info) in active_data {
            if let Some((_, offset)) = kind_info {
                // Only restore if the data segment survived GC and is
                // needed by a surviving function group.
                if module.data.iter().any(|d| d.id() == id) && data_needed {
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
        let uses_builtins = surviving_names
            .iter()
            .any(|name| builtin_names.contains(*name));
        if !has_active_data && !uses_builtins && module.imports.iter().next().is_none() {
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

/// Builtins that never read the data segments of builtins-wasm.
const DATA_FREE_BUILTINS: &[&str] = &[
    CONFIG,
    ARENA_MARK,
    ARENA_RELEASE,
    CABI_REALLOC,
    HEAP_BASE,
    EXIT,
    PRINT,
];

/// The data of builtins-wasm is needed when a builtin that may read it
/// survived. Its segments can't be told apart: after LTO they mix several
/// crates and most are anonymous.
fn is_builtin_data_needed(surviving: &HashSet<&str>, builtin_names: &HashSet<String>) -> bool {
    surviving
        .iter()
        .any(|name| builtin_names.contains(*name) && !DATA_FREE_BUILTINS.contains(name))
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

    fn remap(self, remap: &impl Fn(TypeId) -> TypeId) -> WasmType {
        let val_types = |val_types: Vec<ValType>| {
            val_types
                .iter()
                .map(|val_type| remap_val_type(val_type, remap))
                .collect()
        };
        let fields = |fields: Vec<(EcoString, ValType)>| {
            fields
                .into_iter()
                .map(|(name, val_type)| (name, remap_val_type(&val_type, remap)))
                .collect()
        };
        let kind = match self.kind {
            WasmTypeKind::Array(storage_type) => {
                WasmTypeKind::Array(remap_storage_type(storage_type, remap))
            }
            WasmTypeKind::Closure(params, results) => {
                WasmTypeKind::Closure(val_types(params), val_types(results))
            }
            WasmTypeKind::Struct(fields_) => WasmTypeKind::Struct(fields(fields_)),
            WasmTypeKind::Union(fields_, supertype) => {
                WasmTypeKind::Union(fields(fields_), supertype.map(remap))
            }
        };
        WasmType {
            name: self.name,
            kind,
        }
    }
}

#[derive(Hash, PartialEq, Eq)]
enum WasmTypeKind {
    Array(StorageType),
    /// The closure struct of functions with these params (without the
    /// closure itself) and results.
    Closure(Vec<ValType>, Vec<ValType>),
    Struct(Vec<(EcoString, ValType)>),
    Union(Vec<(EcoString, ValType)>, Option<TypeId>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ClosureType {
    type_index: TypeId,
    code_type_index: TypeId,
}

enum CustomTypeId {
    /// Its definition is in progress, at this position of
    /// `custom_type_definitions`.
    Defining(usize),
    Defined(TypeId),
}

/// A custom type definition in progress. Definitions that reference each
/// other form a cycle, and the types of a cycle are added to a rec group
/// when the outermost definition of the cycle is finished.
struct CustomTypeDefinition {
    /// The id reserved for the type when its definition references it.
    reserved: Option<TypeId>,
    /// The position of the outermost definition in progress that this
    /// definition, or the definitions inside it, references.
    outermost: usize,
    /// The first id reserved by this definition or the definitions inside it.
    first_reserved: Option<TypeId>,
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
    Var {
        global_index: GlobalId,
        name: EcoString,
    },
}

#[derive(Debug, Clone)]
enum CustomType {
    External {
        val_type: ValType,
        /// The builtin that writes the text of a value, when it has one.
        to_str: Option<&'static str>,
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
    /// An external type represented by the public type `name` of the runtime
    /// module `module`, with the same arguments, see `runtime`.
    Runtime {
        module: EcoString,
        name: EcoString,
    },
    /// The external type `Array(a)`, see `array`.
    Array,
}

impl CustomType {
    fn is_ref_non_null(&self) -> bool {
        match self {
            CustomType::Struct { .. } | CustomType::Array => true,
            CustomType::Union { custom_type, .. } => {
                Generator::null_variant_tag(custom_type).is_none()
            }
            CustomType::External { .. } | CustomType::Enum { .. } => false,
            CustomType::Runtime { .. } => {
                panic!("runtime types should be replaced by their representation")
            }
        }
    }
}

#[derive(Clone, Debug)]
struct Variant {
    custom_type: CustomType,
    constructor: TypedRecordConstructor,
    tag: Option<i32>,
}

struct LocalFunction {
    name: EcoString,
    /// Where the function is bound.
    definition: SrcSpan,
    /// Where the function literal is.
    location: SrcSpan,
    specialization: u32,
    enclosing: Monomorphizer,
    type_: Arc<Type>,
    arguments: Vec<TypedArg>,
    body: Vec<TypedStatement>,
    /// See `Generator::local_function_captures`.
    captures: OnceCell<Rc<[Capture]>>,
    /// Whether the function is used as a value, and not only called. With
    /// captures, the definition then creates its closure, bound to the hidden
    /// name `closure_name`, so every use is the same value.
    is_value: bool,
}

impl LocalFunction {
    /// The type of the function, which is used with a single type, see
    /// `Monomorphizer::bind_lets`.
    fn type_(&self) -> Arc<Type> {
        self.enclosing.type_(&self.type_)
    }
}

/// The name of the scope entry with the capture `capture` of the local
/// function `info`.
fn hidden_name(info: &LocalFunction, capture: &str) -> EcoString {
    format!("{}@{}.{capture}", info.name, info.definition.start).into()
}

/// The locals of `scope` with the captured variables `names`.
fn scope_locals(scope: &Scope, names: impl IntoIterator<Item = EcoString>) -> Vec<LocalId> {
    names
        .into_iter()
        .map(|name| {
            scope
                .find_local(&name)
                .unwrap_or_else(|| panic!("captured variable {name} should be in scope"))
        })
        .collect()
}

/// The name of the scope entry with the closure of the local function `info`.
fn closure_name(info: &LocalFunction) -> EcoString {
    hidden_name(info, "#closure")
}

/// Maps Gleam field positions to wasm struct field indices for an
/// instantiation of a union type. Shared fields come first (after the tag,
/// when there is one), then variant-specific fields. A field of a unit type is
/// not stored, see `Generator::is_unit`.
#[derive(Debug, Clone)]
struct UnionFieldLayout {
    /// Whether the structs start with a field with the index of the variant.
    /// A union whose variants are a null one and one other has no tag, since
    /// a value that is not null can only be of the other variant.
    tagged: bool,
    shared_count: usize,
    field_indices: Vec<Vec<Option<u32>>>,
}

/// A matched tuple or record, held in the local `value`, whose fields are
/// matched against sub-patterns. `field_indices` maps each element to its
/// field in the struct, see `Generator::field_indices`.
#[derive(Clone, Copy)]
enum Fields<'a> {
    Struct {
        value: LocalId,
        type_index: TypeId,
        field_indices: &'a [Option<u32>],
    },
    /// A union variant, referenced as its supertype.
    Variant {
        value: LocalId,
        type_index: TypeId,
        field_indices: &'a [Option<u32>],
    },
}

impl UnionFieldLayout {
    /// The layout of `custom_type`, where `units` tells, for each argument of
    /// each constructor, whether it has a unit type.
    fn compute(custom_type: &TypedCustomType, units: &[Vec<bool>]) -> UnionFieldLayout {
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
            let is_unit = units
                .first()
                .and_then(|units| units.get(pos))
                .is_some_and(|unit| *unit);
            if is_shared && !is_unit {
                shared_positions.push(pos);
            }
        }

        let shared_count = shared_positions.len();
        let tagged =
            !(Generator::null_variant_tag(custom_type).is_some() && constructors.len() == 2);
        let first_field = u32::from(tagged);

        let field_indices = units
            .iter()
            .map(|units| {
                let mut next_specific = first_field + shared_count as u32;
                units
                    .iter()
                    .enumerate()
                    .map(|(pos, unit)| {
                        if *unit {
                            None
                        } else if let Some(shared_idx) =
                            shared_positions.iter().position(|&p| p == pos)
                        {
                            Some(first_field + shared_idx as u32)
                        } else {
                            next_specific += 1;
                            Some(next_specific - 1)
                        }
                    })
                    .collect()
            })
            .collect();

        UnionFieldLayout {
            tagged,
            shared_count,
            field_indices,
        }
    }

    /// The tag field, followed by `fields`, the fields of a struct of the
    /// union.
    fn with_tag(&self, fields: Vec<(EcoString, ValType)>) -> Vec<(EcoString, ValType)> {
        if self.tagged {
            iter::once(("tag".into(), ValType::I32))
                .chain(fields)
                .collect()
        } else {
            fields
        }
    }

    fn fields(&self, variant: usize) -> &[Option<u32>] {
        self.field_indices
            .get(variant)
            .expect("variant field indices")
    }

    fn shared_field(&self, pos: u64) -> Option<u32> {
        self.fields(0)
            .get(pos as usize)
            .copied()
            .expect("shared field index")
    }

    fn field(&self, variant: usize, pos: u64) -> Option<u32> {
        self.fields(variant)
            .get(pos as usize)
            .copied()
            .expect("field index")
    }
}

/// How the functions of builtins-wasm get into the generated module.
#[derive(Debug)]
enum Linking {
    /// The module is built on builtins-wasm, parsed by walrus, see
    /// `eliminate_dead_code`.
    Walrus {
        /// The functions exported by builtins-wasm, removed before the dead
        /// code elimination.
        exports: Vec<walrus::ExportId>,
        /// The names of the functions of builtins-wasm.
        names: HashSet<String>,
    },
    /// The module refers to builtins-wasm through stubs, see `link`.
    Stubs(link::Linker),
}

struct Generator<'a> {
    pub(super) wasm_module: walrus::Module,
    memory: Option<MemoryId>,
    /// How the functions of builtins-wasm get into the module.
    linking: Linking,
    wasm_types: IndexMap<WasmType, TypeId>,
    /// The type ids of the custom types instantiations.
    custom_type_ids: HashMap<EcoString, CustomTypeId>,
    custom_type_definitions: Vec<CustomTypeDefinition>,
    types: HashMap<(EcoString, EcoString), CustomType>,
    variants: HashMap<(EcoString, EcoString), Variant>,
    local_functions: HashMap<SrcSpan, Rc<LocalFunction>>,
    specializations: Specializations,
    /// The closure singleton of each code function.
    closure_globals: HashMap<FunctionId, GlobalId>,
    /// The closure singleton of each function used as a value, by the
    /// qualified name of its type: an external with a generic signature has
    /// one function for all its types.
    function_closures: HashMap<(FunctionId, EcoString), GlobalId>,
    /// The closure code of each function used as a value, by the qualified
    /// name of its type and the number of its first params that are captures.
    adapters: HashMap<(FunctionId, EcoString, usize), FunctionId>,
    /// The closure type of each function type, by its qualified name, see
    /// `closure_type`.
    closure_types: HashMap<EcoString, TypeId>,
    /// Anonymous functions are named after the specialization that contains
    /// them, as their code depends on its substitution.
    specialization: u32,
    next_specialization: u32,
    builtins: HashMap<BuiltinFunction, FunctionId>,
    /// The indexes of the constructors with values of each custom type, by
    /// the qualified name of its representation, see `constructed`.
    constructed: HashMap<EcoString, HashSet<usize>>,
    /// The functions of constructors without values yet, by the qualified
    /// name of the representation of their type and the index of the
    /// constructor, see `get_function_constructor`.
    pending: HashMap<(EcoString, usize), Vec<PendingFunction<'a>>>,
    builtins_external: HashMap<BuiltinFunctionExternal, FunctionId>,
    /// The functions of builtins-wasm, by name.
    rust_functions: HashMap<EcoString, FunctionId>,
    main: Option<FunctionId>,
    /// The first error found while generating the code, see `val_type`.
    error: Option<Error>,
    /// Interned string literals and the global each one owns. Insertion
    /// ordered: `code_start` walks this map to lay out the data segments, so
    /// a `HashMap` here would make the emitted module differ between runs of
    /// the compiler over the same source.
    strings: IndexMap<EcoString, GlobalId>,
    consts: Vec<WasmConst>,
    globals: Rc<RefCell<Vec<Id>>>,
    int: IntType,
    float: FloatType,
    string: StringType,
    module: &'a TypedModule,
    line_numbers: &'a LineNumbers,
    all_modules: &'a HashMap<EcoString, &'a TypedModule>,
    /// The functions of every module, by module and function name.
    functions: HashMap<(EcoString, EcoString), &'a TypedFunction>,
    all_line_numbers: &'a HashMap<EcoString, LineNumbers>,
    /// The modules of the runtime, see `runtime`.
    runtime: &'a [RuntimeModule],
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
        let (mut wasm_module, memory, linking) = if link::walrus() {
            let wasm_module = walrus::Module::from_buffer(BUILTINS_WASM)
                .expect("builtins wasm to be a valid module");
            let memory = wasm_module.memories.iter().next().map(|m| m.id());
            let exports = wasm_module
                .exports
                .iter()
                .filter(|e| matches!(e.item, walrus::ExportItem::Function(_)))
                .map(|e| e.id())
                .collect();
            let linking = Linking::Walrus {
                exports,
                names: HashSet::new(),
            };
            (wasm_module, memory, linking)
        } else {
            let mut wasm_module = walrus::Module::default();
            let linker = link::Linker::new(&mut wasm_module);
            let memory = Some(linker.memory());
            (wasm_module, memory, Linking::Stubs(linker))
        };
        let string = StringType::new(&mut wasm_module);
        let mut wasm_types = IndexMap::new();
        let _ = wasm_types.insert(StringType::wasm_type(), string.bytes_index);
        Generator {
            wasm_module,
            memory,
            linking,
            wasm_types,
            custom_type_ids: HashMap::new(),
            custom_type_definitions: Vec::new(),
            types: HashMap::new(),
            variants: HashMap::new(),
            local_functions: HashMap::new(),
            specializations: Specializations::default(),
            closure_globals: HashMap::new(),
            function_closures: HashMap::new(),
            adapters: HashMap::new(),
            closure_types: HashMap::new(),
            specialization: 0,
            next_specialization: 0,
            builtins: HashMap::new(),
            constructed: HashMap::new(),
            pending: HashMap::new(),
            builtins_external: HashMap::new(),
            rust_functions: HashMap::new(),
            main: None,
            error: None,
            strings: IndexMap::new(),
            consts: vec![],
            globals: Rc::default(),
            int: IntType::I32,
            float: FloatType::F64,
            string,
            module,
            line_numbers,
            all_modules,
            functions: iter::once(module)
                .chain(all_modules.values().copied())
                .flat_map(|module| {
                    module.definitions.functions.iter().map(|function| {
                        (
                            (module.name.clone(), function_name(function).clone()),
                            function,
                        )
                    })
                })
                .collect(),
            all_line_numbers,
            runtime: &[],
            current_module_name: module.name.clone(),
        }
    }

    /// Get the source location for a byte offset, using the correct module
    /// name and line numbers for the currently-compiled module.
    fn source_location(&self, start: u32) -> (EcoString, u32) {
        if let Some(ln) = self.all_line_numbers.get(self.current_module_name.as_str()) {
            (self.current_module_name.clone(), ln.line_number(start))
        } else if let Some(runtime_module) = self
            .runtime
            .iter()
            .find(|m| m.module.name == self.current_module_name)
        {
            (
                self.current_module_name.clone(),
                runtime_module.line_numbers.line_number(start),
            )
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
                _value: &'ast EcoString,
                int_value: &'ast BigInt,
            ) {
                if self.error.is_some() {
                    return;
                }
                let err = match self.int {
                    IntType::I32 => i32::try_from(int_value).err().map(|_| ()),
                    IntType::I64 => i64::try_from(int_value).err().map(|_| ()),
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
        match self.linking {
            Linking::Walrus { exports, names } => {
                eliminate_dead_code(&mut self.wasm_module, &exports, &names);
                liveness::clear_dead_locals(&mut self.wasm_module);
                Ok(self.wasm_module.emit_wasm())
            }
            Linking::Stubs(linker) => {
                let link = linker.eliminate_dead_code(&mut self.wasm_module);
                liveness::clear_dead_locals(&mut self.wasm_module);
                Ok(link::emit(&mut self.wasm_module, link))
            }
        }
    }

    fn generate(&mut self) -> Result<FunctionId, Error> {
        // Validate all numeric literals fit in the target types before codegen.
        self.validate_numeric_literals()?;

        // External types, like I32, must come first because of the external function type checking.
        // Externals functions come first because of the indexes in the builtin wasm file.
        self.types_external()?;
        let builtins = self.functions_external()?;
        self.types_prelude();
        self.types_runtime();
        self.functions_builtins(builtins);
        self.types();
        self.types_imported();
        self.constants();
        self.functions();
        let constants = self.function_constants();
        let start = self.function_start(constants);
        match self.error.take() {
            Some(error) => Err(error),
            None => Ok(start),
        }
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
            | BuiltinFunctionExternal::ParseFloat
            | BuiltinFunctionExternal::IntBitwiseAnd
            | BuiltinFunctionExternal::IntBitwiseOr
            | BuiltinFunctionExternal::IntBitwiseExclusiveOr
            | BuiltinFunctionExternal::IntBitwiseNot
            | BuiltinFunctionExternal::IntBitwiseShiftLeft
            | BuiltinFunctionExternal::IntBitwiseShiftRight
            | BuiltinFunctionExternal::IntToFloat
            | BuiltinFunctionExternal::FloatFloor
            | BuiltinFunctionExternal::FloatCeiling
            | BuiltinFunctionExternal::FloatTruncate
            | BuiltinFunctionExternal::FloatRound
            | BuiltinFunctionExternal::FloatSquareRoot
            | BuiltinFunctionExternal::StringByteSize
            | BuiltinFunctionExternal::StringByteSlice
            | BuiltinFunctionExternal::StringPopCodeunit
            | BuiltinFunctionExternal::StringStartsWith
            | BuiltinFunctionExternal::StringEndsWith
            | BuiltinFunctionExternal::StringRemovePrefix
            | BuiltinFunctionExternal::StringRemoveSuffix
            | BuiltinFunctionExternal::StringLessThan
            | BuiltinFunctionExternal::StringConcatList
            | BuiltinFunctionExternal::StringJoin
            | BuiltinFunctionExternal::UtfCodepointToInt
            | BuiltinFunctionExternal::UnsafeIntToUtfCodepoint
            | BuiltinFunctionExternal::StringPopGrapheme => type_::int(),
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
            BuiltinFunction::Hash(val_type, _) => (vec![*val_type], vec![ValType::I32]),
            BuiltinFunction::HashInto(val_type, _) => {
                (vec![*val_type, ValType::I32], vec![ValType::I32])
            }
            BuiltinFunction::HashExternal(val_type, _) => {
                (vec![*val_type], vec![self.int.val_type()])
            }
            BuiltinFunction::HashMix => (vec![ValType::I32], vec![ValType::I32]),
            BuiltinFunction::FoldI64 => (vec![ValType::I64], vec![ValType::I32]),
            BuiltinFunction::DynamicCast(val_type, dynamic, _) => (vec![*val_type], vec![*dynamic]),
            BuiltinFunction::DynamicEqual(_) => (vec![ANYREF, ANYREF], vec![BOOL_VALTYPE]),
            BuiltinFunction::DynamicHash(int, _) => (vec![ANYREF], vec![*int]),
            BuiltinFunction::DynamicInspect(string, _) => (vec![ANYREF], vec![*string]),
            BuiltinFunction::Array { params, result, .. } => (params.clone(), vec![*result]),
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

    /// Registers the external types of every module. Only those of the
    /// module being generated are checked: an invalid external type of
    /// another module is left out, and fails where it is used.
    fn types_external(&mut self) -> Result<(), Error> {
        let modules: Vec<&'a TypedModule> = iter::once(self.module)
            .chain(self.all_modules.values().copied())
            .collect();
        for module in modules {
            let is_main = module.name == self.module.name;
            for custom_type in &module.definitions.custom_types {
                let type_ = if let Some((external_module, name, span)) =
                    &custom_type.external_webassembly
                {
                    assert!(custom_type.constructors.is_empty());
                    match self.external_type(
                        custom_type,
                        &module.type_info.package,
                        external_module,
                        name,
                        *span,
                    ) {
                        Ok(type_) => type_,
                        Err(error) if is_main => return Err(error),
                        Err(_) => continue,
                    }
                } else if is_main && custom_type.name == "I32" {
                    CustomType::External {
                        val_type: ValType::I32,
                        to_str: Some(I32_TO_STR),
                    }
                } else {
                    continue;
                };
                let _ = self
                    .types
                    .insert((module.name.clone(), custom_type.name.clone()), type_);
            }
        }
        Ok(())
    }

    /// The representation of the external type `custom_type` of `package`,
    /// declared as `@external(webassembly, module, name)`.
    fn external_type(
        &self,
        custom_type: &TypedCustomType,
        package: &EcoString,
        module: &EcoString,
        name: &EcoString,
        location: SrcSpan,
    ) -> Result<CustomType, Error> {
        if !reaches_builtins(package) {
            return Err(Error::ExternalOutsideStdlib { location });
        }
        if module != BUILTINS_MODULE {
            return Err(Error::UnknownExternalModule {
                location,
                module: module.clone(),
            });
        }
        let (val_type, to_str) = match name.as_str() {
            "I32" => (ValType::I32, Some(I32_TO_STR)),
            "I64" => (ValType::I64, Some(I64_TO_STR)),
            "F32" => (ValType::F32, Some(F32_TO_STR)),
            "F64" => (ValType::F64, Some(F64_TO_STR)),
            "AnyRef" => (ANYREF, None),
            // Only for the runtime, which reads the positions it knows.
            "Array" if package == RUNTIME_PACKAGE => {
                if custom_type.parameters.len() != 1 {
                    return Err(Error::WrongExternalTypeArity {
                        location,
                        name: name.clone(),
                        expected: 1,
                        got: custom_type.parameters.len(),
                    });
                }
                return Ok(CustomType::Array);
            }
            _ => {
                let Some((runtime_module, runtime_type)) = self.runtime_type(name) else {
                    return Err(Error::UnknownExternalType {
                        location,
                        name: name.clone(),
                    });
                };
                if runtime_type.parameters.len() != custom_type.parameters.len() {
                    return Err(Error::WrongExternalTypeArity {
                        location,
                        name: name.clone(),
                        expected: runtime_type.parameters.len(),
                        got: custom_type.parameters.len(),
                    });
                }
                return Ok(CustomType::Runtime {
                    module: runtime_module,
                    name: name.clone(),
                });
            }
        };
        Ok(CustomType::External { val_type, to_str })
    }

    /// The public type `name` of the runtime and its module.
    fn runtime_type(&self, name: &str) -> Option<(EcoString, &'a TypedCustomType)> {
        self.runtime.iter().find_map(|runtime_module| {
            let module = &runtime_module.module;
            module
                .definitions
                .custom_types
                .iter()
                .find(|t| {
                    t.name == name
                        && t.publicity.is_importable()
                        && t.external_webassembly.is_none()
                })
                .map(|t| (module.name.clone(), t))
        })
    }

    /// The public function `name` of the runtime and its module.
    fn runtime_function(&self, name: &str) -> Option<(EcoString, &'a TypedFunction)> {
        self.runtime.iter().find_map(|runtime_module| {
            let module = &runtime_module.module;
            module
                .definitions
                .functions
                .iter()
                .find(|f| function_name(f) == name && f.publicity.is_importable())
                .map(|f| (module.name.clone(), f))
        })
    }

    /// The type of the runtime that represents `type_`, when `type_` is an
    /// external type represented by the runtime.
    fn representation(&self, type_: &Arc<Type>) -> Option<Arc<Type>> {
        let (CustomType::Runtime { module, name }, arguments) = self.custom_type(type_)? else {
            return None;
        };
        Some(Arc::new(Type::Named {
            publicity: Publicity::Public,
            package: RUNTIME_PACKAGE.into(),
            module,
            name,
            arguments,
            inferred_variant: None,
        }))
    }

    /// The module and the hooks of `type_`, when it is a type of the runtime
    /// with hooks.
    fn type_hooks(&self, type_: &Arc<Type>) -> Option<(EcoString, &'static Hooks)> {
        let (module, name, _) = type_.named_type_information()?;
        runtime::hooks(&module, &name).map(|hooks| (module, hooks))
    }

    /// The hook `name` of the runtime module `module`, with type `type_`.
    fn function_hook(&mut self, module: &EcoString, name: &str, type_: Arc<Type>) -> FunctionId {
        self.function_imported(module, &name.into(), &type_)
            .func_id()
    }

    /// `type_` with each external type represented by the runtime replaced
    /// by its representation, at any depth.
    fn represent(&self, type_: &Arc<Type>) -> Arc<Type> {
        match type_.as_ref() {
            Type::Named {
                publicity,
                package,
                module,
                name,
                arguments,
                inferred_variant,
            } => {
                let arguments = arguments.iter().map(|a| self.represent(a)).collect();
                if let Some((CustomType::Runtime { module, name }, _)) = self.custom_type(type_) {
                    Arc::new(Type::Named {
                        publicity: Publicity::Public,
                        package: RUNTIME_PACKAGE.into(),
                        module,
                        name,
                        arguments,
                        inferred_variant: None,
                    })
                } else {
                    Arc::new(Type::Named {
                        publicity: *publicity,
                        package: package.clone(),
                        module: module.clone(),
                        name: name.clone(),
                        arguments,
                        inferred_variant: *inferred_variant,
                    })
                }
            }
            Type::Fn { arguments, return_ } => Arc::new(Type::Fn {
                arguments: arguments.iter().map(|a| self.represent(a)).collect(),
                return_: self.represent(return_),
            }),
            Type::Tuple { elements } => Arc::new(Type::Tuple {
                elements: elements.iter().map(|e| self.represent(e)).collect(),
            }),
            Type::Var { type_: var } => match &*var.borrow() {
                TypeVar::Link { type_ } => self.represent(type_),
                TypeVar::Unbound { .. } | TypeVar::Generic { .. } => type_.clone(),
            },
        }
    }

    /// The external function `name` of the runtime, with type `type_` in the
    /// types of the program.
    fn function_runtime(&mut self, module: &EcoString, name: &EcoString, type_: &Arc<Type>) -> Id {
        let type_ = self.represent(type_);
        self.function_imported(module, name, &type_)
    }

    /// Registers the types of the runtime, which programs don't import.
    fn types_runtime(&mut self) {
        for runtime_module in self.runtime {
            self.add_module_types(&runtime_module.module);
        }
    }

    fn functions_external(
        &mut self,
    ) -> Result<Vec<(BuiltinFunctionExternal, EcoString, Arc<Type>)>, Error> {
        let mut available: HashMap<EcoString, (Vec<ValType>, Vec<ValType>)> = HashMap::new();
        let func_info: Vec<(EcoString, FunctionId, Vec<ValType>, Vec<ValType>)> =
            match &self.linking {
                // The walrus module was parsed from BUILTINS_WASM in
                // Generator::new; discover its functions directly.
                Linking::Walrus { .. } => self
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
                    .collect(),
                Linking::Stubs(linker) => linker
                    .exports()
                    .map(|(name, id, params, results)| {
                        (name.into(), id, params.to_vec(), results.to_vec())
                    })
                    .collect(),
            };
        for (name, id, params, results) in func_info {
            // Gleam names can't start with `_`, so these don't shadow Gleam
            // functions; the others are only reached through glue.
            if name.starts_with('_') {
                let _ = self.add_function_to_globals(name.clone(), id);
            }
            let _ = self.rust_functions.insert(name.clone(), id);
            let _ = available.insert(name, (params, results));
        }

        if let Linking::Walrus { names, .. } = &mut self.linking {
            *names = self.rust_functions.keys().map(|n| n.to_string()).collect();
        }

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
                if !reaches_builtins(&self.module.type_info.package) {
                    return Err(Error::ExternalOutsideStdlib {
                        location: function.location,
                    });
                }
                if module != BUILTINS_MODULE {
                    return Err(Error::UnknownExternalModule {
                        location: function.location,
                        module: module.clone(),
                    });
                }
                if let Some((_, runtime_function)) = self.runtime_function(name) {
                    self.validate_runtime_external(function, name, runtime_function)?;
                    continue;
                }
                if is_glued(name) {
                    self.validate_rust_external(function, name, available)?;
                    continue;
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
                            got: self.type_pretty_name(&function_type(function)),
                        });
                    }
                } else if let Some(builtin) = BuiltinFunctionExternal::by_name(name) {
                    builtins.push((builtin, name.clone(), function_type(function)));
                } else if let Some(array_function) = ArrayFunction::by_name(name)
                    && self.module.type_info.package == RUNTIME_PACKAGE
                {
                    self.validate_array_external(function, name, array_function)?;
                } else if name == HASH {
                    if !(function.arguments.len() == 1
                        && function
                            .arguments
                            .first()
                            .is_some_and(|arg| is_generic_type(&arg.type_))
                        && function.return_type.is_int())
                    {
                        return Err(Error::WrongBuiltinFunctionSignature {
                            location: function.location,
                            name: name.clone(),
                            expected: "fn(a) -> Int".into(),
                            got: self.type_pretty_name(&function_type(function)),
                        });
                    }
                } else if name == DYNAMIC_CAST {
                    if !(function.arguments.len() == 1
                        && function
                            .arguments
                            .first()
                            .is_some_and(|arg| is_generic_type(&arg.type_))
                        && self
                            .represent(&function.return_type)
                            .same_as(&self.runtime_named_type(DYNAMIC_MODULE, "Dynamic", vec![])))
                    {
                        return Err(Error::WrongBuiltinFunctionSignature {
                            location: function.location,
                            name: name.clone(),
                            expected: "fn(a) -> Dynamic".into(),
                            got: self.type_pretty_name(&function_type(function)),
                        });
                    }
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
                            got: self.type_pretty_name(&function_type(function)),
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

    /// Checks that the type of the external `function` is an instance of
    /// the type of the function of the runtime that implements it, once its
    /// external types are replaced by their representation.
    fn validate_runtime_external(
        &self,
        function: &TypedFunction,
        name: &EcoString,
        runtime_function: &TypedFunction,
    ) -> Result<(), Error> {
        let external = self.represent(&function_type(function));
        let runtime = function_type(runtime_function);
        if is_instance(&runtime, &external, &mut HashMap::new()) {
            Ok(())
        } else {
            Err(Error::WrongBuiltinFunctionSignature {
                location: function.location,
                name: name.clone(),
                expected: self.type_pretty_name(&runtime),
                got: self.type_pretty_name(&function_type(function)),
            })
        }
    }

    fn validate_rust_external(
        &self,
        function: &TypedFunction,
        name: &EcoString,
        available: &HashMap<EcoString, (Vec<ValType>, Vec<ValType>)>,
    ) -> Result<(), Error> {
        let params = function
            .arguments
            .iter()
            .map(|arg| arg.type_.clone())
            .collect_vec();
        let Some(expected) = rust_signature(&params, &function.return_type) else {
            return Err(Error::UnsupportedFeature {
                location: function.location,
                feature: format!(
                    "the type {} in a Rust builtin",
                    self.type_pretty_name(&function_type(function))
                )
                .into(),
            });
        };
        let Some(got) = available.get(name) else {
            return Err(Error::UnknownBuiltinFunction {
                location: function.location,
                name: name.clone(),
            });
        };
        if &expected != got {
            let show = |(params, results): &(Vec<ValType>, Vec<ValType>)| {
                format!(
                    "fn({}) -> {}",
                    params.iter().map(|p| format!("{p:?}")).join(", "),
                    results.iter().map(|r| format!("{r:?}")).join(", "),
                )
            };
            return Err(Error::WrongBuiltinFunctionSignature {
                location: function.location,
                name: name.clone(),
                expected: show(got).into(),
                got: show(&expected).into(),
            });
        }
        Ok(())
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
                label: Option::default(),
                ast,
                location: SrcSpan::default(),
                type_,
                doc: Option::default(),
            }
        }

        fn record_constructor(
            name: &str,
            arguments: Vec<TypedRecordConstructorArg>,
        ) -> TypedRecordConstructor {
            TypedRecordConstructor {
                location: SrcSpan::default(),
                name_location: SrcSpan::default(),
                name: name.into(),
                arguments,
                documentation: Option::default(),
                deprecation: Deprecation::default(),
            }
        }

        fn ast_var(name: &str) -> TypeAst {
            TypeAst::Var(TypeAstVar {
                location: SrcSpan::default(),
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
                location: SrcSpan::default(),
                end_position: Default::default(),
                name: name.into(),
                name_location: SrcSpan::default(),
                publicity: Publicity::Public,
                constructors,
                documentation: Option::default(),
                deprecation: Deprecation::default(),
                opaque: Default::default(),
                parameters,
                typed_parameters,
                external_erlang: Option::default(),
                external_javascript: Option::default(),
                external_webassembly: Option::default(),
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
                (SrcSpan::default(), "ok".into()),
                (SrcSpan::default(), "err".into()),
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
            vec![(SrcSpan::default(), "a".into())],
            vec![type_item],
        );

        vec![nil, bool_, result, list]
    }

    fn custom_type(&self, type_: &Arc<Type>) -> Option<(CustomType, Vec<Arc<Type>>)> {
        if let Some((module, name, args)) = type_.named_type_information()
            && let Some(custom_type) = self.types.get(&(module, name))
        {
            Some((custom_type.clone(), args))
        } else {
            None
        }
    }

    /// Whether `type_` is a unit type, a custom type with a single constructor
    /// without arguments, like `Nil`. Its only value is `0` (see `Enum`), so a
    /// field of a unit type is not stored, and reading it pushes `0`.
    fn is_unit(&self, type_: &Arc<Type>) -> bool {
        type_
            .named_type_information()
            .is_some_and(|(module, name, _)| {
                matches!(
                    self.types.get(&(module, name)),
                    Some(CustomType::Enum { values }) if values.len() == 1
                )
            })
    }

    /// The index of the struct field of each value of `types`, the fields of a
    /// tuple or a record. A value of a unit type is not stored.
    fn field_indices(&self, types: &[Arc<Type>]) -> Vec<Option<u32>> {
        let mut next = 0;
        types
            .iter()
            .map(|type_| {
                if self.is_unit(type_) {
                    None
                } else {
                    next += 1;
                    Some(next - 1)
                }
            })
            .collect()
    }

    /// The index of the struct field of the value at `index` of `types`, see
    /// `field_indices`.
    fn field_index(&self, types: &[Arc<Type>], index: u64) -> Option<u32> {
        self.field_indices(types)
            .get(index as usize)
            .copied()
            .expect("field index")
    }

    /// The module and the location of the definition of the outermost custom
    /// type in `type_`, looking first at `type_` and then at its arguments.
    fn custom_type_definition(&self, type_: &Arc<Type>) -> Option<(EcoString, SrcSpan)> {
        if let Some((module, _, _)) = type_.named_type_information()
            && module != PRELUDE_MODULE_NAME
            && let Some((
                CustomType::Struct { custom_type, .. } | CustomType::Union { custom_type, .. },
                _,
            )) = self.custom_type(type_)
        {
            return Some((module, custom_type.location));
        }
        type_arguments(type_)
            .iter()
            .find_map(|argument| self.custom_type_definition(argument))
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

    /// The indices of the fields `rest` and `first` in the struct of the
    /// `Cons` of `list_type`. `first` is not stored when the elements are of a
    /// unit type.
    fn list_cons_fields(&self, list_type: &Arc<Type>) -> (u32, Option<u32>) {
        let layout = self.union_layout(list_type);
        let [Some(rest), first] = layout.fields(1) else {
            panic!("Cons to have two fields")
        };
        (*rest, *first)
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
            // A generic constant is specialized by each use, see
            // `constant_value`.
            if is_generic_type(&module_constant.type_) {
                continue;
            }
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

    /// The wasm type of `type_`. A type deeper than `TYPE_DEPTH_LIMIT` or
    /// larger than `TYPE_SIZE_LIMIT` is an error: it is the sign of a type
    /// that grows without bound, and generating it would not terminate. The
    /// type is replaced by `anyref`, and no more function bodies are
    /// generated (see `_function`), so the generation stops and reports the
    /// error.
    fn val_type(&mut self, type_: &Arc<Type>) -> ValType {
        let mut size = TYPE_SIZE_LIMIT;
        if is_type_too_large(type_, TYPE_DEPTH_LIMIT, &mut size) {
            if self.error.is_none() {
                let name = self.type_pretty_name(type_);
                self.error = Some(Error::TypeTooLarge {
                    type_: truncate(&name, 60),
                    definition: self.custom_type_definition(type_),
                });
            }
            return ANYREF;
        }
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
            let closure = self.closure_type(params, return_);
            self.val_type_ref(closure.type_index)
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
        if let Some(representation) = self.representation(type_) {
            self.type_index(&representation)
        } else if type_.is_string() {
            self.string.type_index
        } else if let Some(types) = type_.tuple_types() {
            self.tuple_type_index(types)
        } else if let Some((params, return_)) = type_.fn_types() {
            self.closure_type(params, return_).type_index
        } else if let Some((custom_type, args)) = self.custom_type(type_) {
            match custom_type {
                CustomType::External { .. }
                | CustomType::Runtime { .. }
                | CustomType::Enum { .. } => {
                    panic!("external/enum types should not reach code generation")
                }
                CustomType::Array => {
                    let element = args.first().expect("array element type");
                    self.array_type_index(element)
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

    /// The closure types of functions with these arguments and return. They
    /// are created once for each signature, in their own rec group:
    ///
    /// ```wat
    /// (rec
    ///   (type $closure (sub (struct (field (ref $code)))))
    ///   (type $code (func (param (ref $closure) A...) (result R))))
    /// ```
    ///
    /// `$code` is final and has no supertype, so the type that
    /// `FunctionBuilder::new` adds for a code function is `$code`.
    fn closure_type(
        &mut self,
        arguments: impl IntoIterator<Item = Arc<Type>>,
        return_: Arc<Type>,
    ) -> ClosureType {
        self.named_closure_type(arguments, return_).1
    }

    /// The closure type of `closure_type`, with the qualified name of the
    /// function type, which does not change when the closure type moves.
    fn named_closure_type(
        &mut self,
        arguments: impl IntoIterator<Item = Arc<Type>>,
        return_: Arc<Type>,
    ) -> (EcoString, ClosureType) {
        let arguments = arguments.into_iter().collect_vec();
        let name = qualified_function_type_name(&arguments, &return_);
        let closure = self.closure_type_by_name(name.clone(), arguments, return_);
        (name, closure)
    }

    fn closure_type_by_name(
        &mut self,
        name: EcoString,
        arguments: Vec<Arc<Type>>,
        return_: Arc<Type>,
    ) -> ClosureType {
        if let Some(type_index) = self.closure_types.get(&name) {
            return self.closure_type_of(*type_index);
        }
        // Computing the val types may define recursive types that move the
        // closure types, so the cache is checked after it.
        let params = self.val_types(arguments.iter().cloned());
        let results = self.val_types([return_]);
        let key = WasmType {
            name: None,
            kind: WasmTypeKind::Closure(params.clone(), results.clone()),
        };
        if let Some(type_index) = self.wasm_types.get(&key) {
            let _ = self.closure_types.insert(name, *type_index);
            return self.closure_type_of(*type_index);
        }
        let ref_ = |id| {
            ValType::Ref(RefType {
                heap_type: HeapType::Concrete(id),
                nullable: false,
            })
        };
        let ids = self.wasm_module.types.add_rec_group(2, |ids| {
            let &[type_index, code_type_index] = ids else {
                panic!("two closure types");
            };
            let closure_definition = walrus::CompositeType::Struct(walrus::StructType {
                fields: Box::new([FieldType {
                    element_type: StorageType::Val(ref_(code_type_index)),
                    mutable: false,
                }]),
            });
            let code_params = iter::once(ref_(type_index)).chain(params).collect_vec();
            let code_definition = walrus::CompositeType::Function(walrus::FunctionType::new(
                code_params.into_boxed_slice(),
                results.into_boxed_slice(),
            ));
            vec![
                (closure_definition, false, None),
                (code_definition, true, None),
            ]
        });
        let [type_index, code_type_index] = ids[..] else {
            panic!("two closure types");
        };
        let types = &mut self.wasm_module.types;
        let wasm_name = name.replace(" ", "");
        types.get_mut(code_type_index).name = Some(format!("{wasm_name}::code"));
        types.get_mut(type_index).name = Some(wasm_name.to_string());
        let _ = self.wasm_types.insert(key, type_index);
        let _ = self.closure_types.insert(name, type_index);
        ClosureType {
            type_index,
            code_type_index,
        }
    }

    fn closure_type_of(&self, type_index: TypeId) -> ClosureType {
        let walrus::CompositeType::Struct(struct_) = self.wasm_module.types.get(type_index).kind()
        else {
            panic!("closure type should be a struct");
        };
        let [
            FieldType {
                element_type:
                    StorageType::Val(ValType::Ref(RefType {
                        heap_type: HeapType::Concrete(code_type_index),
                        ..
                    })),
                ..
            },
        ] = *struct_.fields
        else {
            panic!("closure type field should be a code reference");
        };
        ClosureType {
            type_index,
            code_type_index,
        }
    }

    /// The closure of the code function `code`, a singleton global.
    fn closure_global(&mut self, code: FunctionId, closure: &ClosureType) -> GlobalId {
        if let Some(global) = self.closure_globals.get(&code) {
            return *global;
        }
        let name = self.wasm_module.funcs.get(code).name.clone();
        let global = self.wasm_module.globals.add_local(
            self.val_type_ref(closure.type_index),
            false,
            false,
            ConstExpr::Extended(vec![
                walrus::ConstOp::RefFunc(code),
                walrus::ConstOp::StructNew(closure.type_index),
            ]),
        );
        self.wasm_module.globals.get_mut(global).name = name;
        let _ = self.closure_globals.insert(code, global);
        global
    }

    /// The closure that calls `function`, whose type is `type_`. Its code is
    /// an adapter that drops the closure param.
    fn closure_of(&mut self, function: FunctionId, type_: &Arc<Type>) -> GlobalId {
        let (arguments, return_) = type_.fn_types().expect("function type");
        let (name, closure) = self.named_closure_type(arguments, return_);
        let key = (function, name);
        if let Some(global) = self.function_closures.get(&key) {
            return *global;
        }
        let code = self.closure_adapter(function, type_, &[]);
        let global = self.closure_global(code, &closure);
        let _ = self.function_closures.insert(key, global);
        global
    }

    /// The code of the closures that call `function`, whose type is `type_`
    /// and whose first params are `captures`. It reads the captures from the
    /// environment of the closure and passes the other params along.
    fn closure_adapter(
        &mut self,
        function: FunctionId,
        type_: &Arc<Type>,
        captures: &[Capture],
    ) -> FunctionId {
        let (arguments, return_) = type_.fn_types().expect("function type");
        let (name, closure) = self.named_closure_type(arguments, return_);
        let key = (function, name, captures.len());
        if let Some(adapter) = self.adapters.get(&key) {
            return *adapter;
        }
        let function_type = self.wasm_module.funcs.get(function).ty();
        let function_type = self.wasm_module.types.get(function_type);
        let (params, results) = (
            function_type
                .params()
                .iter()
                .skip(captures.len())
                .copied()
                .collect_vec(),
            function_type.results().to_vec(),
        );
        let name = format!(
            "{}@closure",
            self.wasm_module
                .funcs
                .get(function)
                .name
                .as_deref()
                .unwrap_or("function")
        );
        let closure_param = self.wasm_local(self.val_type_ref(closure.type_index));
        let params = params
            .into_iter()
            .map(|param| self.wasm_local(param))
            .collect_vec();
        let all_params = iter::once(closure_param)
            .chain(params.iter().copied())
            .collect_vec();
        let mut adapter = Function::new(self, &name, &all_params, &results);
        let mut instructions = adapter.extend_instructions(self);
        self.closure_environment_values(&mut instructions, &closure, closure_param, captures);
        for param in params {
            let _ = instructions.local_get(param);
        }
        let _ = instructions.return_call(function).end();
        let code = adapter.finish(self);
        let _ = self.adapters.insert(key, code);
        if let Some(known) = self.specializations.returns.get(&function).copied() {
            let _ = self.specializations.returns.insert(code, known);
        }
        code
    }

    /// The environment type of the closures of `closure` that capture
    /// `captures`. Closures with the same signature and capture val types
    /// share it, and its name comes from the captures of the first one.
    fn closure_environment_type(&mut self, closure: &ClosureType, captures: &[Capture]) -> TypeId {
        // A capture of a unit type is not stored, see `closure_new`.
        let captures_val_types = self.val_types(
            captures
                .iter()
                .filter(|capture| !self.is_unit(&capture.type_))
                .map(|capture| capture.type_.clone())
                .collect_vec(),
        );
        let fields = iter::once(self.val_type_ref(closure.code_type_index))
            .chain(captures_val_types)
            .map(|val_type| FieldType {
                element_type: StorageType::Val(val_type),
                mutable: false,
            })
            .collect_vec();
        let comp = walrus::CompositeType::Struct(walrus::StructType {
            fields: fields.into_boxed_slice(),
        });
        let types = &mut self.wasm_module.types;
        let id = types.add_composite(comp, true, Some(closure.type_index));
        if types.get(id).name.is_none() {
            let closure_name = types
                .get(closure.type_index)
                .name
                .clone()
                .expect("closure type name");
            let captures_names = captures
                .iter()
                .map(|capture| qualified_type_name(&capture.type_).replace(" ", ""))
                .join(",");
            types.get_mut(id).name = Some(format!("{closure_name}::environment({captures_names})"));
        }
        id
    }

    /// Pushes a new closure of `closure` with the code `code` and an
    /// environment with `captures`, whose values are in `locals`. The
    /// captures of unit types are not stored.
    fn closure_new(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        closure: &ClosureType,
        code: FunctionId,
        captures: &[Capture],
        locals: impl IntoIterator<Item = LocalId>,
    ) {
        let environment = self.closure_environment_type(closure, captures);
        let _ = instructions.ref_func(code);
        for (capture, local) in captures.iter().zip(locals) {
            if !self.is_unit(&capture.type_) {
                let _ = instructions.local_get(local);
            }
        }
        let _ = instructions.struct_new(environment);
    }

    /// Pushes the values of `captures` from the environment of the closure
    /// `closure_local`, created by `closure_new`.
    fn closure_environment_values(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        closure: &ClosureType,
        closure_local: LocalId,
        captures: &[Capture],
    ) {
        if captures.is_empty() {
            return;
        }
        let environment_type = self.closure_environment_type(closure, captures);
        let environment = self.wasm_local(self.val_type_ref(environment_type));
        let _ = instructions
            .local_get(closure_local)
            .ref_cast_non_null(HeapType::Concrete(environment_type))
            .local_set(environment);
        let mut field = 1;
        for capture in captures {
            if self.is_unit(&capture.type_) {
                let _ = instructions.i32_const(0);
            } else {
                let _ = instructions
                    .local_get(environment)
                    .struct_get(environment_type, field);
                field += 1;
            }
        }
    }

    fn tuple_type_index(&mut self, types: impl IntoIterator<Item = Arc<Type>>) -> TypeId {
        let types = types.into_iter().collect_vec();
        let val_types = self.val_types(types.iter().cloned());
        let name = self
            .type_pretty_name(&type_::tuple(types.clone()))
            .replace("#", "Tuple");
        let fields: Vec<(EcoString, ValType)> = val_types
            .into_iter()
            .zip(self.field_indices(&types))
            .enumerate()
            .filter(|(_, (_, field))| field.is_some())
            .map(|(index, (v, _))| (index.to_string().into(), v))
            .collect();
        self.struct_type_index(name, fields)
    }

    fn struct_type_index(&mut self, name: EcoString, fields: Vec<(EcoString, ValType)>) -> TypeId {
        self.struct_type_index_reserved(name, fields, None)
    }

    fn struct_type_index_reserved(
        &mut self,
        name: EcoString,
        fields: Vec<(EcoString, ValType)>,
        reserved: Option<TypeId>,
    ) -> TypeId {
        let key = WasmType::struct_(name.clone(), fields.clone());
        if reserved.is_none()
            && let Some(id) = self.wasm_types.get(&key)
        {
            return *id;
        }
        let walrus_fields: Vec<FieldType> = fields
            .iter()
            .map(|(_, vt)| FieldType {
                element_type: StorageType::Val(*vt),
                mutable: false,
            })
            .collect();
        let comp = walrus::CompositeType::Struct(walrus::StructType {
            fields: walrus_fields.into_boxed_slice(),
        });
        let id = self.composite_type_add(comp, true, None, reserved);
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
            .zip(self.field_indices(types))
            .filter(|(_, field)| field.is_some())
            .map(|(field, _)| field)
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
        let key = qualified_type_name(type_);
        if let Some(id) = self.custom_type_begin(&key) {
            return (id, types);
        }
        let fields = self.fields(constructor, &types);
        let name = self.type_pretty_name(type_);
        let id = self.custom_type_end(key, |this, reserved| {
            this.struct_type_index_reserved(name, fields, reserved)
        });
        (id, types)
    }

    /// Returns the id of the custom type `key` if it is defined or its
    /// definition is in progress, reserving the id in this case. Otherwise
    /// starts its definition, which is finished by `custom_type_end`.
    fn custom_type_begin(&mut self, key: &EcoString) -> Option<TypeId> {
        let position = match self.custom_type_ids.get(key) {
            Some(CustomTypeId::Defined(id)) => return Some(*id),
            Some(CustomTypeId::Defining(position)) => *position,
            None => {
                let position = self.custom_type_definitions.len();
                let _ = self
                    .custom_type_ids
                    .insert(key.clone(), CustomTypeId::Defining(position));
                self.custom_type_definitions.push(CustomTypeDefinition {
                    reserved: None,
                    outermost: position,
                    first_reserved: None,
                });
                return None;
            }
        };
        let current = self
            .custom_type_definitions
            .last_mut()
            .expect("definition in progress");
        current.outermost = current.outermost.min(position);
        let definition = self
            .custom_type_definitions
            .get_mut(position)
            .expect("definition in progress");
        if let Some(id) = definition.reserved {
            return Some(id);
        }
        // A placeholder that references itself, so no other type is
        // deduplicated to it.
        let placeholder = |id: &TypeId| {
            let field = FieldType {
                element_type: StorageType::Val(ValType::Ref(RefType {
                    heap_type: HeapType::Concrete(*id),
                    nullable: true,
                })),
                mutable: false,
            };
            let comp = walrus::CompositeType::Struct(walrus::StructType {
                fields: vec![field].into_boxed_slice(),
            });
            (comp, true, None)
        };
        let id = *self
            .wasm_module
            .types
            .add_rec_group(1, |ids| ids.iter().map(placeholder).collect())
            .first()
            .expect("reserved type");
        definition.reserved = Some(id);
        let _ = definition.first_reserved.get_or_insert(id);
        Some(id)
    }

    /// Finishes the definition of the custom type `key`, adding its type with
    /// `add`, which receives the reserved id.
    fn custom_type_end(
        &mut self,
        key: EcoString,
        add: impl FnOnce(&mut Self, Option<TypeId>) -> TypeId,
    ) -> TypeId {
        let reserved = self
            .custom_type_definitions
            .last()
            .expect("definition in progress")
            .reserved;
        let id = add(self, reserved);
        let definition = self
            .custom_type_definitions
            .pop()
            .expect("definition in progress");
        let _ = self.custom_type_ids.insert(key, CustomTypeId::Defined(id));
        let position = self.custom_type_definitions.len();
        if let Some(enclosing) = self.custom_type_definitions.last_mut()
            && definition.outermost < position
        {
            enclosing.outermost = enclosing.outermost.min(definition.outermost);
            enclosing.first_reserved = enclosing
                .first_reserved
                .into_iter()
                .chain(definition.first_reserved)
                .min_by_key(|id| id.index());
            return id;
        }
        match definition.first_reserved {
            Some(first) => self.rec_group_from(first, id),
            None => id,
        }
    }

    fn composite_type_add(
        &mut self,
        comp: walrus::CompositeType,
        is_final: bool,
        supertype: Option<TypeId>,
        reserved: Option<TypeId>,
    ) -> TypeId {
        let Some(id) = reserved else {
            return self
                .wasm_module
                .types
                .add_composite(comp, is_final, supertype);
        };
        let type_ = self.wasm_module.types.get_mut(id);
        *type_.kind_mut() = comp;
        type_.is_final = is_final;
        type_.supertype = supertype;
        id
    }

    /// Moves the types created since `first` to a new rec group, so they can
    /// reference each other. No rec group may contain both these types and
    /// types created before `first`. Returns the new id of `id`.
    fn rec_group_from(&mut self, first: TypeId, id: TypeId) -> TypeId {
        let types = &mut self.wasm_module.types;
        let old: Vec<TypeId> = types
            .iter()
            .map(|type_| type_.id())
            .skip_while(|id| id.index() < first.index())
            .collect();
        let definitions: Vec<_> = old
            .iter()
            .map(|id| {
                let type_ = types.get(*id);
                (
                    type_.kind().clone(),
                    type_.is_final,
                    type_.supertype,
                    type_.name.clone(),
                )
            })
            .collect();
        let new = types.add_rec_group(old.len(), |new| {
            let remap: HashMap<TypeId, TypeId> =
                old.iter().copied().zip(new.iter().copied()).collect();
            let remap = |id: TypeId| remap.get(&id).copied().unwrap_or(id);
            definitions
                .iter()
                .map(|(kind, is_final, supertype, _)| {
                    (
                        remap_composite_type(kind, &remap),
                        *is_final,
                        supertype.map(remap),
                    )
                })
                .collect()
        });
        for ((old, new), (.., name)) in old.iter().zip(&new).zip(definitions) {
            types.delete(*old);
            types.get_mut(*new).name = name;
        }
        let remap: HashMap<TypeId, TypeId> = old.into_iter().zip(new).collect();
        let remap = |id: TypeId| remap.get(&id).copied().unwrap_or(id);
        self.wasm_types = std::mem::take(&mut self.wasm_types)
            .into_iter()
            .map(|(key, id)| (key.remap(&remap), remap(id)))
            .collect();
        for custom_type_id in self.custom_type_ids.values_mut() {
            if let CustomTypeId::Defined(id) = custom_type_id {
                *id = remap(*id);
            }
        }
        for id in self.closure_types.values_mut() {
            *id = remap(*id);
        }
        remap(id)
    }

    fn union_layout(&self, type_: &Arc<Type>) -> UnionFieldLayout {
        let (module, name, args) = type_
            .named_type_information()
            .expect("named type information");
        match self.types.get(&(module, name)).expect("union type") {
            CustomType::Union { custom_type } => {
                let units = custom_type
                    .constructors
                    .iter()
                    .map(|constructor| {
                        Monomorphizer::variant_constructor(custom_type, constructor, &args)
                            .iter()
                            .map(|type_| self.is_unit(type_))
                            .collect()
                    })
                    .collect_vec();
                UnionFieldLayout::compute(custom_type, &units)
            }
            CustomType::External { .. }
            | CustomType::Runtime { .. }
            | CustomType::Array
            | CustomType::Enum { .. }
            | CustomType::Struct { .. } => {
                panic!("expected union type")
            }
        }
    }

    fn mono_union_supertype_index(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &TypedCustomType,
    ) -> TypeId {
        let custom_type_key = qualified_type_name(type_);
        if let Some(id) = self.custom_type_begin(&custom_type_key) {
            return id;
        }
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
            let field_index = field_indices.get(pos).copied().expect("field index");
            if field_index.is_none_or(|field_index| {
                field_index >= u32::from(layout.tagged) + layout.shared_count as u32
            }) {
                continue;
            }
            let label = if let Some((_, label)) = &arg.label {
                label.clone()
            } else {
                pos.to_string().into()
            };
            shared_fields.push((label, self.val_type(type_)));
        }

        let shared_fields = layout.with_tag(shared_fields);
        let name = custom_type.name.clone();
        self.custom_type_end(custom_type_key, |this, reserved| {
            let key = WasmType::union(name.clone(), shared_fields.clone(), None);
            if reserved.is_none()
                && let Some(id) = this.wasm_types.get(&key)
            {
                return *id;
            }
            let id = this.union_type_add(&name, &shared_fields, None, reserved);
            let _ = this.wasm_types.insert(key, id);
            id
        })
    }

    /// The supertype and subtype of the constructor `constructor` of the
    /// union `type_`, and the types of its fields. Records that the program
    /// constructs values of `constructor`, see `constructed`.
    fn mono_union_subtype_index(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &TypedCustomType,
        constructor: &TypedRecordConstructor,
        args: &[Arc<Type>],
    ) -> (TypeId, TypeId, Vec<Arc<Type>>) {
        let result = self.union_subtype_index(type_, custom_type, constructor, args);
        let variant_index = custom_type
            .constructors
            .iter()
            .position(|c| c.name == constructor.name)
            .expect("constructor in union");
        self.constructed(type_, variant_index);
        result
    }

    /// Like `mono_union_subtype_index`, without recording that the program
    /// constructs values of `constructor`, for the functions of a
    /// constructor, which only read its values (see
    /// `get_function_constructor`).
    fn union_subtype_index(
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
            .filter_map(|(pos, (arg, val_type))| {
                let label = if let Some((_, label)) = &arg.label {
                    label.clone()
                } else {
                    pos.to_string().into()
                };
                let idx = field_indices.get(pos).copied().expect("field index")?;
                Some((idx, label, *val_type))
            })
            .collect();
        fields.sort_by_key(|(idx, _, _)| *idx);
        let fields = layout.with_tag(
            fields
                .into_iter()
                .map(|(_, label, val_type)| (label, val_type))
                .collect(),
        );

        let mut name = self.type_pretty_name(type_);
        name += ".";
        name += constructor.name.clone();

        let key = WasmType::union(name.clone(), fields.clone(), Some(supertype_index));
        let type_index = if let Some(id) = self.wasm_types.get(&key) {
            *id
        } else {
            let id = self.union_type_add(&name, &fields, Some(supertype_index), None);
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
        reserved: Option<TypeId>,
    ) -> TypeId {
        let walrus_fields: Vec<FieldType> = fields
            .iter()
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
        let id = self.composite_type_add(comp, is_final, supertype, reserved);
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
            CustomType::Runtime { .. } => {
                let representation = self.representation(type_).expect("runtime type");
                self.val_type(&representation)
            }
            CustomType::Enum { .. } => ValType::I32,
            CustomType::Array => {
                let element = args.first().expect("array element type");
                let type_index = self.array_type_index(element);
                self.val_type_ref(type_index)
            }
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
        if let Constant::Var {
            constructor: Some(constructor),
            ..
        } = &*module_constant.value
            && let ValueConstructorVariant::ModuleConstant {
                literal, module, ..
            } = &constructor.variant
            && (*module != self.module.name || is_generic_type(&literal.type_()))
        {
            // A constant that names a constant of another module, or a generic
            // one, has the value of its literal, specialized to its type.
            let literal = if is_generic_type(&literal.type_()) {
                Monomorphizer::with_bound(&literal.type_(), &module_constant.type_)
                    .constant(literal)
            } else {
                literal.clone()
            };
            return self.module_constant(&TypedModuleConstant {
                value: Box::new(literal),
                ..module_constant.clone()
            });
        }

        self.register_string_const(&module_constant.value);

        let const_name = &module_constant.name;
        let export = module_constant.publicity.is_public();

        let type_ = &module_constant.type_;
        if type_.fn_types().is_some() {
            // A function constant names the global of the closure.
            let function = self
                .constant_function(&module_constant.value, type_)
                .expect("function constant to name a function");
            let global = self.closure_of(function, type_);
            if export {
                let _ = self.wasm_module.exports.add(const_name, global);
            }
            let id = Id::global(const_name.clone(), global);
            self.globals.borrow_mut().push(id.clone());
            return id;
        }

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
                let types = elements.iter().map(|e| e.type_()).collect_vec();
                let field_indices = self.field_indices(&types);
                let type_index = self.tuple_type_index(types);
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
                    elements: elements
                        .iter()
                        .zip(field_indices)
                        .filter(|(_, field)| field.is_some())
                        .map(|(element, _)| element.clone())
                        .collect(),
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
                record_constructor,
                ..
            } => {
                let name = record_constant_name(name, record_constructor);
                let (custom_type, args) = self.custom_type_expect(type_);
                match custom_type {
                    CustomType::Enum { values } => {
                        assert!(arguments.as_ref().is_none_or(Vec::is_empty));
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
                        let (type_index, types) =
                            self.mono_struct_type_index(type_, &custom_type, &constructor, &args);
                        let field_indices = self.field_indices(&types);
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
                            elements: arguments
                                .iter()
                                .flatten()
                                .zip(field_indices)
                                .filter(|(_, field)| field.is_some())
                                .map(|(e, _)| e.value.clone())
                                .collect(),
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
                        let layout = self.union_layout(type_);
                        // The elements go in the order of the struct fields,
                        // where the shared fields come first.
                        let elements = arguments
                            .iter()
                            .flatten()
                            .zip(layout.fields(index as usize))
                            .filter_map(|(e, field)| Some((e, (*field)?)))
                            .sorted_by_key(|(_, field)| *field)
                            .map(|(e, _)| e.value.clone())
                            .collect();
                        self.consts.push(WasmConst::Struct {
                            global_index: id.global_id(),
                            type_index,
                            tag: layout.tagged.then_some(index.into()),
                            elements,
                        });
                        id
                    }
                    CustomType::External { .. }
                    | CustomType::Runtime { .. }
                    | CustomType::Array => {
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
                // Generates the named constant if it is not yet.
                let _ = self.var_id(&Scope::Global(self.globals.clone()), name, type_);
                self.consts.push(WasmConst::Var {
                    global_index: id.global_id(),
                    name: name.clone(),
                });
                id
            }
            Constant::BitArray { .. } => todo!("BitArray constants are not yet supported"),
            Constant::RecordUpdate { .. } => {
                panic!("record update constants should not reach code generation")
            }
            Constant::BinaryOperator { operator, .. } => {
                if *operator != BinOp::Concatenate {
                    todo!("{operator:?} constants are not yet supported");
                }
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
            Constant::Todo { .. } => {
                panic!("todo constants should not reach code generation")
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
                for arg in arguments.iter().flatten() {
                    self.register_string_const(&arg.value)
                }
            }
            Constant::Var { .. } => {}
            Constant::BitArray { .. } => todo!("BitArray constants are not yet supported"),
            Constant::RecordUpdate { .. } => {
                panic!("record update constants should not reach code generation")
            }
            Constant::BinaryOperator { left, right, .. } => {
                self.register_string_const(left);
                self.register_string_const(right);
            }
            Constant::Todo { .. } => {
                panic!("todo constants should not reach code generation")
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
                let cons_variant = self.variant_expect(PRELUDE_MODULE_NAME.into(), cons.name);
                let cons_fn = self.variant_constructor(
                    vec![type_::list(item_type.clone()), item_type],
                    type_.clone(),
                    cons_variant,
                );
                let _ = instructions.ref_null(HeapType::Concrete(supertype_index));
                for element in elements.iter().rev() {
                    let _ = instructions.constant(self, element).call(cons_fn);
                }
            }
            Constant::Tuple { elements, .. } => {
                let types = elements.iter().map(|e| e.type_()).collect_vec();
                let field_indices = self.field_indices(&types);
                let type_index = self.tuple_type_index(types);
                let elements = elements
                    .iter()
                    .zip(field_indices)
                    .filter(|(_, field)| field.is_some())
                    .map(|(element, _)| element);
                let _ = instructions
                    .constants(self, elements)
                    .struct_new(type_index);
            }
            Constant::Record {
                name,
                arguments,
                type_,
                record_constructor,
                ..
            } => {
                let name = record_constant_name(name, record_constructor);
                if let Some(function) = self.constant_function(const_, type_) {
                    let closure = self.closure_of(function, type_);
                    let _ = instructions.global_get(closure);
                } else if let Some((CustomType::Enum { values }, _)) = self.custom_type(type_) {
                    let value = Self::enum_value_index(&values, name);
                    let _ = instructions.i32_const(value as i32);
                } else {
                    let (module, _, _) = type_
                        .named_type_information()
                        .expect("named type information");
                    let variant = self.variant_expect(module, name.clone());
                    let id = self.variant_constructor(
                        arguments
                            .iter()
                            .flatten()
                            .map(|arg| arg.value.type_())
                            .collect(),
                        type_.clone(),
                        variant,
                    );
                    let _ = instructions
                        .constants(self, arguments.iter().flatten().map(|arg| &arg.value))
                        .call(id);
                }
            }
            Constant::Var {
                name,
                type_,
                constructor,
                ..
            } => {
                if let Some(function) = self.constant_function(const_, type_) {
                    let closure = self.closure_of(function, type_);
                    let _ = instructions.global_get(closure);
                } else if let Some(ValueConstructorVariant::ModuleConstant {
                    literal, module, ..
                }) = constructor
                    .as_deref()
                    .map(|constructor| &constructor.variant)
                    && (*module != self.module.name || is_generic_type(&literal.type_()))
                {
                    self.constant_value(instructions, literal, type_);
                } else {
                    let scope = Scope::Global(self.globals.clone());
                    self.expression_var(&scope, instructions, name, type_, None);
                }
            }
            Constant::BitArray { .. } => todo!("BitArray constants are not yet supported"),
            Constant::RecordUpdate { .. } => {
                panic!("record update constants should not reach code generation")
            }
            Constant::BinaryOperator {
                operator,
                left,
                right,
                ..
            } => {
                if *operator != BinOp::Concatenate {
                    todo!("{operator:?} constants are not yet supported");
                }
                let concat =
                    self.get_function_builtin_external(BuiltinFunctionExternal::StringConcat);
                let _ = instructions
                    .constant(self, left)
                    .constant(self, right)
                    .call(concat);
            }
            Constant::Todo { .. } => {
                panic!("todo constants should not reach code generation")
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
                    if let Some((module, _)) = self.runtime_function(fname) {
                        return self.function_runtime(&module, fname, required_type);
                    }
                    if is_glued(fname) {
                        return self.function_rust(fname, &function_type(function));
                    }
                    if fname == INSPECT {
                        return self.function_inspect(required_type);
                    }
                    if fname == HASH {
                        return self.function_hash_external(required_type);
                    }
                    if fname == DYNAMIC_CAST {
                        return self.function_dynamic_cast_external(required_type);
                    }
                    if let Some(array_function) = ArrayFunction::by_name(fname) {
                        return self.function_array_external(array_function, required_type);
                    }
                    if let Some(builtin) = BuiltinFunctionExternal::by_name(fname) {
                        // Created on first use, also for externals declared in
                        // imported modules.
                        let index = self.get_function_builtin_external(builtin);
                        return Id::func(fname.clone(), index);
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

    /// The function that the constant `constant` names, with type `type_`. A
    /// function constant is an alias of the function, so a generic one is
    /// specialized by each use.
    fn constant_function(
        &mut self,
        constant: &TypedConstant,
        type_: &Arc<Type>,
    ) -> Option<FunctionId> {
        let _ = type_.fn_types()?;
        let target = constant_target(constant)?;
        let global = Scope::Global(self.globals.clone());
        let id = self.static_target_id(&global, target, type_);
        matches!(id.kind, IdKind::Func).then(|| id.func_id())
    }

    /// The id of the function `target` used with type `type_`.
    fn static_target_id(
        &mut self,
        scope: &Scope,
        target: StaticTarget<'_>,
        type_: &Arc<Type>,
    ) -> Id {
        match target {
            StaticTarget::Function { module, name } if *module != self.module.name => {
                self.function_imported(module, name, type_)
            }
            StaticTarget::Function { name, .. } | StaticTarget::Constructor(name) => {
                self.var_id(scope, name, type_)
            }
        }
    }

    /// Pushes the value of the constant `constant` used with type `type_`. A
    /// generic constant is specialized to the type of each use.
    fn constant_value(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        constant: &TypedConstant,
        type_: &Arc<Type>,
    ) {
        if let Some(function) = self.constant_function(constant, type_) {
            let closure = self.closure_of(function, type_);
            let _ = instructions.global_get(closure);
        } else if is_generic_type(&constant.type_()) {
            let constant = Monomorphizer::with_bound(&constant.type_(), type_).constant(constant);
            self.register_string_const(&constant);
            self._constant(instructions, &constant);
        } else {
            self._constant(instructions, constant);
        }
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

    fn allocate_specialization(&mut self) -> u32 {
        self.next_specialization += 1;
        self.next_specialization
    }

    fn allocate_placeholder_function(
        &mut self,
        name: &str,
        params: &[ValType],
        results: &[ValType],
    ) -> FunctionId {
        let mut fb = FunctionBuilder::new(&mut self.wasm_module.types, params, results);
        let _ = fb.func_body().unreachable();
        let id = fb.finish(vec![], &mut self.wasm_module.funcs);
        self.wasm_module.funcs.get_mut(id).name = Some(name.to_string());
        id
    }

    fn _function(
        &mut self,
        function: TypedFunction,
        name: EcoString,
        export: bool,
        specialization: u32,
        local_functions: HashMap<SrcSpan, Rc<LocalFunction>>,
        known_arguments: HashMap<SrcSpan, Known>,
    ) -> Id {
        let params = self.val_types(function_params_types(&function));
        let results = self.val_types(iter::once(function.return_type.clone()));
        let index = self.allocate_placeholder_function(&name, &params, &results);
        let id = self.add_function_to_globals(name.clone(), index);
        if self.error.is_some() {
            // The code is not used, see `val_type`.
            return id;
        }
        let saved_local_functions = std::mem::replace(&mut self.local_functions, local_functions);
        let saved_specialization = std::mem::replace(&mut self.specialization, specialization);
        let saved_known_arguments =
            std::mem::replace(&mut self.specializations.known_arguments, known_arguments);
        let locals = Locals::new(self, &function.arguments, &function.body);
        let param_ids = locals.param_ids();
        let mut code = Function::new(self, &name, &param_ids, &results);
        let mut instructions = code.extend_instructions(self);
        let scope = Scope::with_params(
            Scope::Global(self.globals.clone()),
            &function.arguments,
            &param_ids,
        );
        self.statements(
            &mut instructions,
            scope.clone(),
            &locals,
            &function.body,
            true,
        );
        self.record_return(&scope, index, &function.return_type, &function.body);
        self.local_functions = saved_local_functions;
        self.specialization = saved_specialization;
        self.specializations.known_arguments = saved_known_arguments;
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
        let specialization = self.allocate_specialization();
        let mut mono = Monomorphizer::new();
        let variables = Variables::of(&function.arguments, &function.body);
        let module = self.current_module_name.clone();
        self.bind_lets(&mut mono, &variables, &module);
        let local_functions = collect_local_functions(&variables, specialization, &mono);
        // Resolve Generic type vars (from inner lambdas) to Nil for valid WASM types.
        let function = mono.function(function);
        self._function(
            function,
            base_name,
            export,
            specialization,
            local_functions,
            HashMap::new(),
        )
    }

    fn function_generic(
        &mut self,
        function: &TypedFunction,
        required_type: &Arc<Type>,
        export: bool,
        base_name: EcoString,
    ) -> Id {
        let name = mangle(&base_name, required_type);
        if let Some(id) = self.find_global(&name) {
            return id;
        }
        let specialization = self.allocate_specialization();
        let mut mono = Monomorphizer::with_bound(&function_type(function), required_type);
        let variables = Variables::of(&function.arguments, &function.body);
        let module = self.current_module_name.clone();
        self.bind_lets(&mut mono, &variables, &module);
        let local_functions = collect_local_functions(&variables, specialization, &mono);
        let mut function = mono.function(function);
        set_function_name(&mut function, name.clone());
        self._function(
            function,
            name,
            export,
            specialization,
            local_functions,
            HashMap::new(),
        )
    }

    /// Generates a local function that uses `captures`. With `closure`, it is
    /// the code of a closure: its first param is the closure, which
    /// `arguments` don't include, and the captures are read from its
    /// environment. Without it, the captures are the first params.
    fn _function_local(
        &mut self,
        name: EcoString,
        type_: &Arc<Type>,
        arguments: &[TypedArg],
        body: &[TypedStatement],
        closure: Option<ClosureType>,
        captures: &[Capture],
    ) -> Id {
        let (gleam_params, return_) = type_.fn_types().expect("function type");
        let captures_val_types =
            self.val_types(captures.iter().map(|capture| capture.type_.clone()));
        let closure_param = closure.map(|closure| self.val_type_ref(closure.type_index));
        let captures_params = if closure.is_some() {
            vec![]
        } else {
            captures_val_types.clone()
        };
        let params = closure_param
            .into_iter()
            .chain(captures_params)
            .chain(self.val_types(gleam_params))
            .collect_vec();
        let results = self.val_types(iter::once(return_.clone()));
        let index = self.allocate_placeholder_function(&name, &params, &results);
        let id = self.add_function_to_globals(name.clone(), index);
        let locals = Locals::new(self, arguments, body);
        let param_ids = locals.param_ids();
        let closure_local = closure_param.map(|val_type| self.wasm_local(val_type));
        let captures_locals = captures_val_types
            .into_iter()
            .map(|val_type| self.wasm_local(val_type))
            .collect_vec();
        let captures_param_ids = if closure.is_some() {
            vec![]
        } else {
            captures_locals.clone()
        };
        let all_param_ids = closure_local
            .into_iter()
            .chain(captures_param_ids)
            .chain(param_ids.iter().copied())
            .collect_vec();
        let mut code = Function::new(self, &name, &all_param_ids, &results);
        let mut instructions = code.extend_instructions(self);
        if let (Some(closure), Some(closure_local)) = (closure, closure_local) {
            // Copy the captures from the environment to their locals
            self.closure_environment_values(&mut instructions, &closure, closure_local, captures);
            for local in captures_locals.iter().rev() {
                let _ = instructions.local_set(*local);
            }
        }
        // The variables bound in the body shadow the captures
        let mut scope = Scope::Global(self.globals.clone());
        for (capture, local) in captures.iter().zip(captures_locals) {
            scope = scope.insert_local(capture.name.clone(), local);
            self.wasm_module.locals.get_mut(local).name = Some(capture.name.to_string());
        }
        let scope = Scope::with_params(scope, arguments, &param_ids);
        self.statements(&mut instructions, scope.clone(), &locals, body, true);
        self.record_return(&scope, index, &return_, body);
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
        closure: Option<ClosureType>,
        captures: &[Capture],
    ) -> Id {
        if let Some(id) = self.find_global(&name) {
            return id;
        }
        self._function_local(name, type_, arguments, body, closure, captures)
    }

    /// The name of the code of the function literal at `location`.
    fn function_literal_name(&self, location: SrcSpan, type_: &Arc<Type>) -> EcoString {
        function_literal_name(location, type_, self.specialization)
    }

    /// The captures of a function literal.
    fn function_literal_captures(
        &self,
        location: SrcSpan,
        arguments: &[TypedArg],
        body: &[TypedStatement],
    ) -> Vec<Capture> {
        self.expand_captures(captures::captures(location, arguments, body))
    }

    /// Replaces the captured local functions by their captures, named by
    /// `hidden_name`, as they are called directly, and by their closures,
    /// named by `closure_name`, when they are used as values.
    fn expand_captures(&self, captures: Vec<Capture>) -> Vec<Capture> {
        let mut names = HashSet::new();
        let mut expanded: Vec<Capture> = vec![];
        for capture in captures {
            let captures = if let Some(info) = self.local_functions.get(&capture.definition) {
                let captures = self.local_function_captures(info);
                let closure = (capture.is_value && !captures.is_empty()).then(|| Capture {
                    name: closure_name(info),
                    type_: info.type_(),
                    definition: info.definition,
                    is_value: true,
                });
                captures
                    .iter()
                    .map(|inner| Capture {
                        name: hidden_name(info, &inner.name),
                        ..inner.clone()
                    })
                    .chain(closure)
                    .collect()
            } else {
                vec![capture]
            };
            for capture in captures {
                if names.insert(capture.name.clone()) {
                    expanded.push(capture);
                }
            }
        }
        expanded
    }

    /// The captures of the local function `info`, typed by the enclosing
    /// function. The definition of the local function binds each one to a
    /// hidden name, so the calls far from it find the right variable.
    fn local_function_captures(&self, info: &LocalFunction) -> Rc<[Capture]> {
        info.captures
            .get_or_init(|| {
                let captures = captures::captures(info.location, &info.arguments, &info.body)
                    .into_iter()
                    .map(|capture| Capture {
                        type_: info.enclosing.type_(&capture.type_),
                        ..capture
                    })
                    .collect();
                self.expand_captures(captures).into()
            })
            .clone()
    }

    /// The locals with the captures of the local function `info`, bound by
    /// its definition in `scope`.
    fn local_function_captures_locals(&self, scope: &Scope, info: &LocalFunction) -> Vec<LocalId> {
        let captures = self.local_function_captures(info);
        let names = captures
            .iter()
            .map(|capture| hidden_name(info, &capture.name));
        scope_locals(scope, names)
    }

    /// Pushes the value of the local function `info`. With captures, it is
    /// the closure created by the definition, or a new one when the function
    /// is not used as a value, but is the result of the block that defines
    /// it.
    fn local_function_value(
        &mut self,
        scope: &Scope,
        instructions: &mut Instructions<'_, '_>,
        info: &LocalFunction,
    ) {
        if let Some(local) = scope.find_local(&closure_name(info)) {
            let _ = instructions.local_get(local);
            return;
        }
        let type_ = info.type_();
        let id = self.local_function_code(info);
        let captures = self.local_function_captures(info);
        if captures.is_empty() {
            let closure = self.closure_of(id, &type_);
            let _ = instructions.global_get(closure);
        } else {
            let (params, return_) = type_.fn_types().expect("function type");
            let closure = self.closure_type(params, return_);
            let adapter = self.closure_adapter(id, &type_, &captures);
            let locals = self.local_function_captures_locals(scope, info);
            self.closure_new(instructions, &closure, adapter, &captures, locals);
        }
    }

    /// The locals of `scope` with the values of `captures`.
    fn captures_locals(&self, scope: &Scope, captures: &[Capture]) -> Vec<LocalId> {
        scope_locals(scope, captures.iter().map(|capture| capture.name.clone()))
    }

    /// Calls the function literal `fun` directly, without creating its
    /// closure: its captures are the first arguments.
    fn function_literal_call<'e>(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        locals: &Locals,
        scope: Scope,
        fun: &TypedExpr,
        arguments: impl IntoIterator<Item = &'e TypedExpr>,
        tail: bool,
    ) {
        let TypedExpr::Fn {
            location,
            type_,
            arguments: parameters,
            body,
            ..
        } = fun
        else {
            panic!("function literal");
        };
        let name = self.function_literal_name(*location, type_) + "@direct";
        let captures = self.function_literal_captures(*location, parameters, body);
        let id = self.function_local(name, type_, parameters, body, None, &captures);
        for local in self.captures_locals(&scope, &captures) {
            let _ = instructions.local_get(local);
        }
        let _ = instructions.expressions(self, locals, scope, arguments);
        let _ = instructions.call_maybe_tail(id.func_id(), tail);
    }

    /// The code of the local function `info`, whose first params are its
    /// captures.
    fn local_function_code(&mut self, info: &LocalFunction) -> FunctionId {
        let type_ = info.type_();
        let name = function_literal_name(info.location, &type_, info.specialization);
        if let Some(id) = self.find_global(&name) {
            return id.func_id();
        }
        let variables = Variables::of(&info.arguments, &info.body);
        let nested = collect_local_functions(&variables, info.specialization, &info.enclosing);
        let (args, body) = info.enclosing.function_local(info);
        let captures = self.local_function_captures(info);
        let nested_locations: Vec<_> = nested.keys().copied().collect();
        self.local_functions.extend(nested);
        let id = self._function_local(name, &type_, &args, &body, None, &captures);
        for location in nested_locations {
            let _ = self.local_functions.remove(&location);
        }
        id.func_id()
    }

    /// Binds the type variables of the variables of a function used with a
    /// single type (see `Monomorphizer::bind_lets`), and records an error for
    /// a variable used with different types. `module` is the module of the
    /// function.
    fn bind_lets(
        &mut self,
        mono: &mut Monomorphizer,
        variables: &Variables<'_>,
        module: &EcoString,
    ) {
        mono.bind_lets(variables);
        if self.error.is_none()
            && let Some(reference) = mono.conflicting_reference(variables)
        {
            self.error = Some(Error::GenericLocalVariable {
                module: module.clone(),
                location: reference.location,
                name: reference.name.clone(),
            });
        }
    }

    fn function_imported(
        &mut self,
        module_name: &EcoString,
        fn_name: &EcoString,
        required_type: &Arc<Type>,
    ) -> Id {
        let function = self.module_function_definition(module_name, fn_name);
        if let Some((_, fname, _)) = &function.external_webassembly {
            if let Some((module, _)) = self.runtime_function(fname) {
                return self.function_runtime(&module, fname, required_type);
            }
            if is_glued(fname) {
                return self.function_rust(fname, &function_type(function));
            }
            if fname == INSPECT {
                return self.function_inspect(required_type);
            }
            if fname == HASH {
                return self.function_hash_external(required_type);
            }
            if fname == DYNAMIC_CAST {
                return self.function_dynamic_cast_external(required_type);
            }
            if let Some(array_function) = ArrayFunction::by_name(fname) {
                return self.function_array_external(array_function, required_type);
            }
            if let Some(builtin) = BuiltinFunctionExternal::by_name(fname) {
                // Created on first use, also for externals declared in
                // imported modules.
                let index = self.get_function_builtin_external(builtin);
                return Id::func(fname.clone(), index);
            }
            return self.find_global_expect(fname);
        }
        // Track which module we're compiling for correct diagnostics.
        let prev_module = std::mem::replace(&mut self.current_module_name, module_name.clone());
        let base_name = imported_base_name(module_name, fn_name);
        let id = if is_generic_type(&function_type(function)) {
            self.function_generic(function, required_type, false, base_name)
        } else {
            self.function(function, false, base_name)
        };
        self.current_module_name = prev_module;
        id
    }

    /// The definition of the function `name` of the module `module`.
    fn module_function_definition(
        &self,
        module: &EcoString,
        name: &EcoString,
    ) -> &'a TypedFunction {
        self.functions
            .get(&(module.clone(), name.clone()))
            .copied()
            .unwrap_or_else(|| panic!("function not found during code generation: {module}.{name}"))
    }

    fn statements(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        mut scope: Scope,
        locals: &Locals,
        statements: &[TypedStatement],
        tail: bool,
    ) {
        let mut remaining = statements;
        while let Some((statement, rest)) = remaining.split_first() {
            match statement {
                Statement::Assignment(assignment)
                    if let Some(info) = self.local_function_definition(assignment) =>
                {
                    // The code of a local function is generated where it is
                    // used, the definition binds its captures to hidden names
                    // and creates its closure when it is used as a value, or
                    // when it is the result of the block.
                    let info = info.clone();
                    let captures = self.local_function_captures(&info);
                    for capture in captures.iter() {
                        let local = scope.find_local(&capture.name).unwrap_or_else(|| {
                            panic!("captured variable {} is not in scope", capture.name)
                        });
                        scope = scope.insert_local(hidden_name(&info, &capture.name), local);
                    }
                    if info.is_value && !captures.is_empty() {
                        self.local_function_value(&scope, instructions, &info);
                        let val_type = self.val_type(&info.type_());
                        let local = self.wasm_local(val_type);
                        let _ = instructions.local_set(local);
                        scope = scope.insert_local(closure_name(&info), local);
                    }
                    if rest.is_empty() {
                        self.local_function_value(&scope, instructions, &info);
                    }
                }
                Statement::Assignment(assignment)
                    if matches!(assignment.kind, AssignmentKind::Assert { .. }) =>
                {
                    self.assignment_assert(locals, scope, instructions, assignment, rest, tail);
                    return;
                }
                Statement::Assignment(assignment) => {
                    scope =
                        self.assignment(locals, scope, instructions, assignment, rest.is_empty());
                }
                Statement::Expression(_) | Statement::Use(_) | Statement::Assert(_) => {
                    let tail = tail && rest.is_empty();
                    self.statement(instructions, &scope, locals, statement, tail);
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
        scope: &Scope,
        locals: &Locals,
        statement: &TypedStatement,
        tail: bool,
    ) {
        match statement {
            Statement::Expression(expression) => {
                let _ = instructions.tail_expression(self, locals, scope.clone(), expression, tail);
            }
            Statement::Assignment(_) => {
                panic!("assignments are generated by `statements`")
            }
            Statement::Assert(assert) => {
                self.assert(instructions, scope, locals, assert);
            }
            Statement::Use(use_) => {
                let _ = instructions.tail_expression(self, locals, scope.clone(), &use_.call, tail);
            }
        }
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
        tail: bool,
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
                    vec![type_::list(item_type.clone()), item_type],
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
                let types = elements.iter().map(|e| e.type_()).collect_vec();
                let field_indices = self.field_indices(&types);
                let type_index = self.tuple_type_index(types);
                for (element, field) in elements.iter().zip(field_indices) {
                    let _ = instructions.expression(self, locals, scope.clone(), element);
                    if field.is_none() {
                        let _ = instructions.drop();
                    }
                }
                let _ = instructions.struct_new(type_index);
            }
            TypedExpr::TupleIndex { index, tuple, .. } => {
                let types = tuple.type_().tuple_types().expect("tuple type");
                let field = self.field_index(&types, *index);
                let type_index = self.tuple_type_index(types);
                let _ = instructions
                    .expression(self, locals, scope.clone(), tuple)
                    .field_get(type_index, field);
            }
            TypedExpr::PositionalAccess { index, record, .. } => {
                // Same as RecordAccess but by position
                let (custom_type, args) = self.custom_type_expect(&record.type_());
                match custom_type {
                    CustomType::Struct {
                        custom_type,
                        constructor,
                    } => {
                        let (type_index, types) = self.mono_struct_type_index(
                            &record.type_(),
                            &custom_type,
                            &constructor,
                            &args,
                        );
                        let field = self.field_index(&types, *index);
                        let _ = instructions
                            .expression(self, locals, scope, record)
                            .field_get(type_index, field);
                    }
                    CustomType::Union { custom_type, .. } => {
                        let layout = self.union_layout(&record.type_());
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
                                .variant_field_get(type_index, field_index);
                        } else {
                            let supertype_index =
                                self.mono_union_supertype_index(&record.type_(), &custom_type);
                            let field_index = layout.shared_field(*index);
                            let _ = instructions
                                .expression(self, locals, scope, record)
                                .field_get(supertype_index, field_index);
                        }
                    }
                    CustomType::External { .. }
                    | CustomType::Runtime { .. }
                    | CustomType::Array
                    | CustomType::Enum { .. } => {
                        panic!("external/enum types should not reach code generation")
                    }
                }
            }
            TypedExpr::BinOp {
                operator: BinOp::And,
                left,
                right,
                ..
            } => {
                let _ = instructions
                    .expression(self, locals, scope.clone(), left)
                    .if_else(
                        BOOL_VALTYPE,
                        |then_s| {
                            let _ = then_s.tail_expression(self, locals, scope, right, tail);
                        },
                        |else_s| {
                            let _ = else_s.bool_const(false);
                        },
                    );
            }
            TypedExpr::BinOp {
                operator: BinOp::Or,
                left,
                right,
                ..
            } => {
                let _ = instructions
                    .expression(self, locals, scope.clone(), left)
                    .if_else(
                        BOOL_VALTYPE,
                        |then_s| {
                            let _ = then_s.bool_const(true);
                        },
                        |else_s| {
                            let _ = else_s.tail_expression(self, locals, scope, right, tail);
                        },
                    );
            }
            TypedExpr::BinOp {
                operator,
                left,
                right,
                ..
            } => {
                self.expression_bin_op(locals, scope, instructions, operator, left, right);
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
                self.statements(instructions, scope, locals, statements, tail);
            }
            TypedExpr::Pipeline {
                first_value,
                assignments,
                finally,
                ..
            } => {
                let mut scope = scope;
                // The references to a pipe variable are defined at its value
                self.bind(&scope, first_value.value.location(), &first_value.value);
                let first_index = locals.for_pipeline_assignment(first_value);
                let _ = instructions
                    .expression(self, locals, scope.clone(), &first_value.value)
                    .local_set(first_index);
                scope = scope.insert_local(first_value.name.clone(), first_index);
                for (assignment, _) in assignments {
                    self.bind(&scope, assignment.value.location(), &assignment.value);
                    let assign_index = locals.for_pipeline_assignment(assignment);
                    let _ = instructions
                        .expression(self, locals, scope.clone(), &assignment.value)
                        .local_set(assign_index);
                    scope = scope.insert_local(assignment.name.clone(), assign_index);
                }
                let _ = instructions.tail_expression(self, locals, scope, finally, tail);
            }
            TypedExpr::Var {
                name, constructor, ..
            } => match &constructor.variant {
                ValueConstructorVariant::ModuleFn { module, name, .. }
                    if module.as_str() != self.module.name.as_str() =>
                {
                    let type_ = expression.type_();
                    let id = self.function_imported(module, name, &type_);
                    let closure = self.closure_of(id.func_id(), &type_);
                    let _ = instructions.global_get(closure);
                }
                ValueConstructorVariant::ModuleConstant { literal, .. } => {
                    self.constant_value(instructions, literal, &expression.type_());
                }
                ValueConstructorVariant::LocalVariable { location, .. } => {
                    let type_ = expression.type_();
                    self.expression_var(&scope, instructions, name, &type_, Some(*location));
                }
                ValueConstructorVariant::ModuleFn { .. }
                | ValueConstructorVariant::Record { .. } => {
                    let name = constructor_name(name, constructor);
                    self.expression_var(&scope, instructions, name, &expression.type_(), None);
                }
            },
            TypedExpr::Call { fun, arguments, .. } => {
                // `return_call*` requires the callee results to match the
                // current function results. They do: both have the same Gleam
                // type, and equal Gleam types map to equal wasm types (module
                // validation fails otherwise). Constructors don't grow the
                // stack, so a tail call gains nothing there.
                let tail = tail && !fun.is_record_constructor_function();
                let args = arguments.iter().map(|arg| &arg.value);
                let target = self.call_target(&scope, fun, arguments);
                let direct = match target {
                    CallTarget::Literal => {
                        self.function_literal_call(instructions, locals, scope, fun, args, tail);
                        return;
                    }
                    // A callee whose identity is known, see `specialization`
                    CallTarget::Known(Known::Code(id)) => {
                        // The closure is the first argument
                        let _ = instructions.expression(self, locals, scope.clone(), fun);
                        Some((id, tail))
                    }
                    CallTarget::Known(known @ (Known::Function(id) | Known::Constructor(id))) => {
                        if !fun.is_var() {
                            // Evaluated for its effects
                            let _ = instructions
                                .expression(self, locals, scope.clone(), fun)
                                .drop();
                        }
                        Some((id, tail && !matches!(known, Known::Constructor(_))))
                    }
                    CallTarget::Direct(id) => Some((id, tail)),
                    CallTarget::Local(ref info, id) => {
                        // The captures are the first arguments
                        for local in self.local_function_captures_locals(&scope, info) {
                            let _ = instructions.local_get(local);
                        }
                        Some((id, tail))
                    }
                    CallTarget::Dynamic => None,
                };
                if let Some((id, tail)) = direct {
                    let _ = instructions
                        .expressions(self, locals, scope, args)
                        .call_maybe_tail(id, tail);
                } else {
                    // The closure is the first argument, and its field 0 is
                    // the code to call.
                    if fun.is_var() {
                        // Evaluating a variable again has no effects.
                        let _ = instructions
                            .expression(self, locals, scope.clone(), fun)
                            .expressions(self, locals, scope.clone(), args)
                            .expression(self, locals, scope, fun);
                    } else {
                        let index = locals.for_call(fun);
                        let _ = instructions
                            .expression(self, locals, scope.clone(), fun)
                            .local_tee(index)
                            .expressions(self, locals, scope, args)
                            .local_get(index);
                    }
                    let (params, return_) = fun.type_().fn_types().expect("function type");
                    let closure = self.closure_type(params, return_);
                    let _ = instructions
                        .struct_get(closure.type_index, 0)
                        .call_ref_maybe_tail(closure.code_type_index, tail);
                }
            }
            TypedExpr::Fn {
                location,
                type_,
                arguments,
                body,
                ..
            } => {
                let name = self.function_literal_name(*location, type_);
                let captures = self.function_literal_captures(*location, arguments, body);
                let (params, return_) = type_.fn_types().expect("function type");
                let closure = self.closure_type(params, return_);
                let id =
                    self.function_local(name, type_, arguments, body, Some(closure), &captures);
                if captures.is_empty() {
                    let closure = self.closure_global(id.func_id(), &closure);
                    let _ = instructions.global_get(closure);
                } else {
                    let locals = self.captures_locals(&scope, &captures);
                    self.closure_new(instructions, &closure, id.func_id(), &captures, locals);
                }
            }
            TypedExpr::Case {
                type_,
                subjects,
                clauses,
                ..
            } => {
                let subjects = self.case_subjects(locals, &scope, instructions, subjects);
                let case_type = self.val_type(type_);
                let _ = instructions.block_(case_type, |case_b| {
                    self.case_clauses(locals, scope, case_b, &subjects, clauses, tail);
                });
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
                        let (type_index, types) = self.mono_struct_type_index(
                            &record.type_(),
                            &custom_type,
                            &constructor,
                            &args,
                        );
                        let field = self.field_index(&types, *index);
                        let _ = instructions
                            .expression(self, locals, scope, record)
                            .field_get(type_index, field);
                    }
                    CustomType::Union { custom_type, .. } => {
                        let layout = self.union_layout(&record.type_());
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
                                .variant_field_get(type_index, field_index);
                        } else {
                            // Shared field — access via supertype
                            let supertype_index =
                                self.mono_union_supertype_index(&record.type_(), &custom_type);
                            let field_index = layout.shared_field(*index);
                            let _ = instructions
                                .expression(self, locals, scope, record)
                                .field_get(supertype_index, field_index);
                        }
                    }
                    CustomType::External { .. }
                    | CustomType::Runtime { .. }
                    | CustomType::Array
                    | CustomType::Enum { .. } => {
                        panic!("external/enum types should not reach code generation")
                    }
                }
            }
            TypedExpr::RecordUpdate {
                type_,
                updated_record,
                updated_record_assigned_name,
                constructor,
                arguments,
                ..
            } => {
                let scope = if let Some(name) = updated_record_assigned_name {
                    let local = locals.for_record_update(updated_record);
                    let _ = instructions
                        .expression(self, locals, scope.clone(), updated_record)
                        .local_set(local);
                    scope.insert_local(name.clone(), local)
                } else {
                    scope
                };
                let (custom_type, _) = self.custom_type_expect(type_);
                match custom_type {
                    CustomType::Struct { .. } | CustomType::Union { .. } => {
                        let id = self
                            .static_callee(&scope, constructor)
                            .expect("record constructor");
                        let _ = instructions
                            .expressions(
                                self,
                                locals,
                                scope,
                                arguments.iter().map(|arg| &arg.value),
                            )
                            .call(id);
                    }
                    CustomType::External { .. }
                    | CustomType::Runtime { .. }
                    | CustomType::Array
                    | CustomType::Enum { .. } => {
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
                    let type_ = expression.type_();
                    let id = self.function_imported(module_name, label, &type_);
                    let closure = self.closure_of(id.func_id(), &type_);
                    let _ = instructions.global_get(closure);
                }
                ModuleValueConstructor::Record { name, .. } => {
                    self.expression_var(&scope, instructions, name, &expression.type_(), None);
                }
                ModuleValueConstructor::Constant { literal, .. } => {
                    self.constant_value(instructions, literal, &expression.type_());
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
        let _ = instructions
            .expression(self, locals, scope.clone(), left)
            .expression(self, locals, scope.clone(), right);

        let _ = match name {
            BinOp::And | BinOp::Or => {
                unreachable!("short-circuit operators are generated in _expression")
            }
            // Int
            BinOp::AddInt => instructions.int_add(),
            BinOp::SubInt => instructions.int_sub(),
            BinOp::MultInt => instructions.int_mul(),
            BinOp::DivInt => {
                let (left, right) = locals.for_div(&scope, left, right);
                instructions.int_div(left, right)
            }
            BinOp::RemainderInt => {
                let (left, right) = locals.for_div(&scope, left, right);
                instructions.int_rem(left, right)
            }
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

    /// Returns the locals holding the values of the case `subjects`.
    fn case_subjects(
        &mut self,
        locals: &Locals,
        scope: &Scope,
        instructions: &mut Instructions<'_, '_>,
        subjects: &[TypedExpr],
    ) -> Vec<LocalId> {
        let mut subjects_locals = vec![];
        for subject in subjects {
            if let Some(name) = subject.var_name()
                && subject.is_local_var()
                && !self.is_local_function(subject)
            {
                // Subject is a local variable — reuse its existing local
                let id = self.var_id(scope, name, &subject.type_());
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
        subjects_locals
    }

    /// Generates the clauses of a case expression in the `case` block. Each
    /// clause alternative is wrapped in a block that `_pattern` branches out
    /// of on failure.
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
    fn case_clauses(
        &mut self,
        locals: &Locals,
        scope: Scope,
        case_b: &mut Instructions<'_, '_>,
        subjects_locals: &[LocalId],
        clauses: &[TypedClause],
        tail: bool,
    ) {
        let case_id = case_b.id();
        for clause in clauses {
            let mut scope = scope.clone();
            for patterns in iter::once(&clause.pattern).chain(&clause.alternative_patterns) {
                // block alt — _pattern branches here on failure
                let _ = case_b.block_(InstrSeqType::Simple(None), |alt_b| {
                    let alt_id = alt_b.id();
                    for (pattern, subject_local) in patterns
                        .iter()
                        .zip(subjects_locals)
                        .filter(|(pattern, _)| !pattern.is_discard())
                    {
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
                        .tail_expression(self, locals, scope.clone(), &clause.then, tail)
                        // exit block case
                        .br(case_id);
                });
            }
        }
        // end block case
        let _ = case_b.unreachable();
    }

    fn is_ref_non_null(&self, type_: &Arc<Type>) -> bool {
        if let Some(representation) = self.representation(type_) {
            return self.is_ref_non_null(&representation);
        }
        type_.is_string() || type_.tuple_types().is_some() || {
            if let Some((custom_type, _)) = self.custom_type(type_) {
                custom_type.is_ref_non_null()
            } else {
                false
            }
        }
    }

    fn local_function_definition(
        &self,
        assignment: &TypedAssignment,
    ) -> Option<&Rc<LocalFunction>> {
        if let Pattern::Variable { location, .. } = &assignment.pattern {
            self.local_functions.get(location)
        } else {
            None
        }
    }

    /// The local function that `expr` names, if any.
    fn local_function_of(&self, expr: &TypedExpr) -> Option<&Rc<LocalFunction>> {
        captures::local_variable_definition(expr)
            .and_then(|location| self.local_functions.get(&location))
    }

    /// Whether `expr` is a variable bound to a local function, which must be
    /// monomorphized for the type of each use instead of read from its local.
    pub(super) fn is_local_function(&self, expr: &TypedExpr) -> bool {
        self.local_function_of(expr).is_some()
    }

    /// Returns the function `fun` statically names, if any, so a call to it
    /// can be emitted as a direct `call` instead of a `call_ref` on a
    /// function reference.
    fn static_callee(&mut self, scope: &Scope, fun: &TypedExpr) -> Option<FunctionId> {
        // Local functions take their captures, see `local_function_of`
        let target = static_target(fun)?;
        let id = self.static_target_id(scope, target, &fun.type_());
        matches!(id.kind, IdKind::Func).then(|| id.func_id())
    }

    fn expression_var(
        &mut self,
        scope: &Scope,
        instructions: &mut Instructions<'_, '_>,
        name: &EcoString,
        type_: &Arc<Type>,
        definition: Option<SrcSpan>,
    ) {
        if let Some(info) =
            definition.and_then(|location| self.local_functions.get(&location).cloned())
        {
            assert!(
                info.is_value,
                "local function {name} used as a value is not marked as one"
            );
            self.local_function_value(scope, instructions, &info);
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
                    let closure = self.closure_of(id.func_id(), type_);
                    instructions.global_get(closure)
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

        if let Some(expression) = expression {
            let _ = instructions.expression(self, locals, scope.clone(), expression);
        } else {
            // `|> echo` prints the value of the previous step
            let previous = scope
                .find_local(&PIPE_VARIABLE.into())
                .expect("echo with no previous step in a pipe");
            let _ = instructions.local_get(previous);
        }
        let _ = instructions
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
            .call(self.function_repr(&echo.type_()))
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

    fn assignment(
        &mut self,
        locals: &Locals,
        mut scope: Scope,
        instructions: &mut Instructions<'_, '_>,
        assignment: &TypedAssignment,
        result: bool,
    ) -> Scope {
        if let Pattern::Variable { location, .. } = &assignment.pattern {
            self.bind(&scope, *location, &assignment.value);
        }
        let body_id = instructions.id();
        let _ = instructions.expression(self, locals, scope.clone(), &assignment.value);
        if result {
            // The value of the assignment is the value of its expression
            let right = locals.for_assigment(assignment);
            let _ = instructions
                .local_tee(right)
                .pattern(self, locals, &mut scope, &assignment.pattern, body_id)
                .local_get(right);
        } else {
            let _ = instructions.pattern(self, locals, &mut scope, &assignment.pattern, body_id);
        }
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
        tail: bool,
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
                    self.statements(fail_b, scope.clone(), locals, remaining, tail);
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
                let (rest_field, first_field) = self.list_cons_fields(type_);
                let right = locals.for_pattern(pattern);
                let _ = instructions.local_set(right);
                // The rest after the last element is needed only to check or bind it.
                let rest = tail.as_ref().is_none_or(|tail| !tail.pattern.is_discard());
                for (i, element) in elements.iter().enumerate() {
                    let _ = instructions
                        .local_get(right)
                        .ref_is_null()
                        .br_if(fail_target);
                    if !element.is_discard() {
                        let _ = instructions
                            .local_get(right)
                            .variant_field_get(cons_index, first_field);
                        scope = self._pattern(locals, scope, instructions, element, fail_target);
                    }
                    if rest || i + 1 < elements.len() {
                        let _ = instructions
                            .local_get(right)
                            .ref_cast_non_null(HeapType::Concrete(cons_index))
                            .struct_get(cons_index, rest_field)
                            .local_set(right);
                    }
                }
                if let Some(tail) = tail {
                    if !tail.pattern.is_discard() {
                        let _ = instructions.local_get(right);
                        scope =
                            self._pattern(locals, scope, instructions, &tail.pattern, fail_target);
                    }
                } else {
                    let _ = instructions
                        .local_get(right)
                        .ref_is_null()
                        .bool_not()
                        .br_if(fail_target);
                }
            }
            Pattern::Tuple { elements, .. } => {
                let types = elements.iter().map(|e| e.type_()).collect_vec();
                let field_indices = self.field_indices(&types);
                let type_index = self.tuple_type_index(types);
                let value = locals.for_pattern(pattern);
                let _ = instructions.local_set(value).patterns(
                    self,
                    locals,
                    &mut scope,
                    Fields::Struct {
                        value,
                        type_index,
                        field_indices: &field_indices,
                    },
                    elements.iter(),
                    fail_target,
                );
            }
            Pattern::Constructor {
                constructor,
                type_,
                arguments,
                ..
            } => {
                // The name as written can be an alias of an imported constructor.
                let name = &constructor.expect_ref("inferred pattern constructor").name;
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
                        let custom_type = custom_type;
                        let constructor = constructor;
                        let (type_index, types) =
                            self.mono_struct_type_index(type_, &custom_type, &constructor, &args);
                        assert_eq!(constructor.arguments.len(), arguments.len());
                        let field_indices = self.field_indices(&types);
                        let value = locals.for_pattern(pattern);
                        let _ = instructions.local_set(value).patterns(
                            self,
                            locals,
                            &mut scope,
                            Fields::Struct {
                                value,
                                type_index,
                                field_indices: &field_indices,
                            },
                            arguments.iter().map(|arg| &arg.value),
                            fail_target,
                        );
                    }
                    CustomType::Union { custom_type, .. } => {
                        let custom_type = custom_type;
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
                            let layout = self.union_layout(type_);
                            let _ = instructions.local_tee(right);
                            if null_tag.is_some() {
                                let _ = instructions.ref_is_null().br_if(fail_target);
                                if layout.tagged {
                                    let _ = instructions.local_get(right);
                                }
                            }
                            if layout.tagged {
                                let _ = instructions
                                    .struct_get(supertype_index, 0)
                                    .i32_const(tag as i32)
                                    .i32_ne()
                                    .br_if(fail_target);
                            }
                            let fields = Fields::Variant {
                                value: right,
                                type_index,
                                field_indices: layout.fields(tag),
                            };
                            let _ = instructions.patterns(
                                self,
                                locals,
                                &mut scope,
                                fields,
                                arguments.iter().map(|arg| &arg.value),
                                fail_target,
                            );
                        }
                    }
                    CustomType::External { .. }
                    | CustomType::Runtime { .. }
                    | CustomType::Array => {
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

                if let Some(StringPrefixLeftSideAssignment { name, .. }) = left_side_assignment {
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
                        .i32_const(prefix_len)
                        .local_get(subject)
                        .string_len()
                        .i32_const(prefix_len)
                        .i32_sub()
                        .string_slice()
                        .local_set(rest_local);
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

    fn _patterns<'b>(
        &mut self,
        locals: &Locals,
        scope: &mut Scope,
        instructions: &mut Instructions<'_, '_>,
        fields: Fields<'_>,
        elements: impl IntoIterator<Item = &'b Pattern<Arc<Type>>>,
        fail_target: InstrSeqId,
    ) {
        for (index, element) in elements
            .into_iter()
            .enumerate()
            .filter(|(_, element)| !element.is_discard())
        {
            let (Fields::Struct { field_indices, .. } | Fields::Variant { field_indices, .. }) =
                fields;
            let field = field_indices.get(index).copied().expect("field index");
            let _ = match (fields, field) {
                (_, None) => instructions.i32_const(0),
                (
                    Fields::Struct {
                        value, type_index, ..
                    },
                    Some(field),
                ) => instructions.local_get(value).struct_get(type_index, field),
                (
                    Fields::Variant {
                        value, type_index, ..
                    },
                    Some(field),
                ) => instructions
                    .local_get(value)
                    .ref_cast_non_null(HeapType::Concrete(type_index))
                    .struct_get(type_index, field),
            };
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
                    let (a, b) = locals.for_guard_div(scope, left, right);
                    let _ = instructions
                        .clause_guards(self, locals, scope, [left.as_ref(), right.as_ref()])
                        .int_rem(a, b);
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
            ClauseGuard::Var {
                name,
                type_,
                definition_location,
                ..
            } => {
                self.expression_var(scope, instructions, name, type_, Some(*definition_location));
            }
            ClauseGuard::TupleIndex { tuple, index, .. } => {
                let types = tuple.type_().tuple_types().expect("tuple type");
                let field = self.field_index(&types, *index);
                let type_index = self.tuple_type_index(types);
                let _ = instructions
                    .clause_guard(self, locals, scope, tuple)
                    .field_get(type_index, field);
            }
            ClauseGuard::FieldAccess {
                index, container, ..
            } => {
                let type_ = container.type_();
                let index = index.expect("FieldAccess index") as u32;
                let (custom_type, args) = self.custom_type_expect(&type_);
                match custom_type {
                    CustomType::External { .. }
                    | CustomType::Runtime { .. }
                    | CustomType::Array
                    | CustomType::Enum { .. } => {
                        panic!("external/enum types should not reach code generation")
                    }
                    CustomType::Struct {
                        custom_type,
                        constructor,
                    } => {
                        let (type_index, types) =
                            self.mono_struct_type_index(&type_, &custom_type, &constructor, &args);
                        let field = self.field_index(&types, index.into());
                        let _ = instructions
                            .clause_guard(self, locals, scope, container)
                            .field_get(type_index, field);
                    }
                    CustomType::Union { custom_type, .. } => {
                        let layout = self.union_layout(&type_);
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
                            .variant_field_get(type_index, field_index);
                    }
                }
            }
            ClauseGuard::ModuleSelect { literal, .. } => {
                let _ = instructions.constant(self, literal);
            }
            ClauseGuard::Invalid { .. } => {
                panic!("invalid guards should not reach code generation")
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
            BuiltinFunctionExternal::IntBitwiseAnd => self.code_int_bitwise_binop(IntBitwise::And),
            BuiltinFunctionExternal::IntBitwiseOr => self.code_int_bitwise_binop(IntBitwise::Or),
            BuiltinFunctionExternal::IntBitwiseExclusiveOr => {
                self.code_int_bitwise_binop(IntBitwise::ExclusiveOr)
            }
            BuiltinFunctionExternal::IntBitwiseNot => self.code_int_bitwise_not(),
            BuiltinFunctionExternal::IntBitwiseShiftLeft => self.code_int_shift(true),
            BuiltinFunctionExternal::IntBitwiseShiftRight => self.code_int_shift(false),
            BuiltinFunctionExternal::IntToFloat => self.code_int_to_float(),
            BuiltinFunctionExternal::FloatFloor => self.code_float_unop(FloatUnary::Floor),
            BuiltinFunctionExternal::FloatCeiling => self.code_float_unop(FloatUnary::Ceiling),
            BuiltinFunctionExternal::FloatTruncate => self.code_float_truncate(),
            BuiltinFunctionExternal::FloatRound => self.code_float_round(),
            BuiltinFunctionExternal::FloatSquareRoot => {
                self.code_float_unop(FloatUnary::SquareRoot)
            }
            BuiltinFunctionExternal::StringByteSize => self.code_string_num_bytes(),
            BuiltinFunctionExternal::StringByteSlice => self.code_string_byte_slice(),
            BuiltinFunctionExternal::StringPopCodeunit => self.code_string_pop_codeunit(),
            BuiltinFunctionExternal::StringStartsWith => self.function_string_starts_with(),
            BuiltinFunctionExternal::StringEndsWith => self.code_string_ends_with(),
            BuiltinFunctionExternal::StringRemovePrefix => self.code_string_remove_affix(true),
            BuiltinFunctionExternal::StringRemoveSuffix => self.code_string_remove_affix(false),
            BuiltinFunctionExternal::StringLessThan => self.code_string_less_than(),
            BuiltinFunctionExternal::StringConcatList => self.code_string_join(false),
            BuiltinFunctionExternal::StringJoin => self.code_string_join(true),
            BuiltinFunctionExternal::UtfCodepointToInt => self.code_utf_codepoint_to_int(),
            BuiltinFunctionExternal::UnsafeIntToUtfCodepoint => {
                self.code_unsafe_int_to_utf_codepoint()
            }
            BuiltinFunctionExternal::StringPopGrapheme => self.code_string_pop_grapheme(),
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

/// The largest depth of a type, as `List(List(Int))` has depth 3, and the
/// largest number of types in it, counting the type and each of its
/// arguments, see `val_type`. Types of real programs are much smaller. A type
/// that grows without bound passes the depth when it grows by one level each
/// time, as `List(a)`, and the size first when it doubles, as `#(a, a)`. The
/// depth also bounds the recursion of the generation, which goes one level
/// deeper for each larger type.
const TYPE_DEPTH_LIMIT: usize = 20;
const TYPE_SIZE_LIMIT: usize = 1000;

/// Whether `type_` is deeper than `depth` or has more than `size` types. It
/// stops at `size`: a type that grows without bound usually shares its
/// arguments, as `#(a, a)`, and its tree is exponentially large.
fn is_type_too_large(type_: &Arc<Type>, depth: usize, size: &mut usize) -> bool {
    if let Type::Var { type_ } = type_.as_ref() {
        return match &*type_.borrow() {
            TypeVar::Link { type_ } => is_type_too_large(type_, depth, size),
            TypeVar::Unbound { .. } | TypeVar::Generic { .. } => false,
        };
    }
    if depth == 0 || *size == 0 {
        return true;
    }
    *size -= 1;
    type_arguments(type_)
        .iter()
        .any(|argument| is_type_too_large(argument, depth - 1, size))
}

/// The types in `type_`: the arguments of a named type, the params and the
/// return of a function, and the elements of a tuple.
fn type_arguments(type_: &Arc<Type>) -> Vec<Arc<Type>> {
    match type_.as_ref() {
        Type::Named { arguments, .. } => arguments.clone(),
        Type::Fn { arguments, return_ } => arguments
            .iter()
            .chain(iter::once(return_))
            .cloned()
            .collect(),
        Type::Tuple { elements } => elements.clone(),
        Type::Var { type_ } => match &*type_.borrow() {
            TypeVar::Link { type_ } => type_arguments(type_),
            TypeVar::Unbound { .. } | TypeVar::Generic { .. } => vec![],
        },
    }
}

/// The first `length` characters of `text`, followed by `…` if it is longer.
fn truncate(text: &str, length: usize) -> EcoString {
    match text.char_indices().nth(length) {
        Some((index, _)) => format!("{}…", &text[..index]).into(),
        None => text.into(),
    }
}

fn type_pretty_name(names: &Names, type_: &Arc<Type>) -> EcoString {
    Printer::new(names).print_type(type_)
}

fn mangle(name: &EcoString, type_: &Arc<Type>) -> EcoString {
    let type_name = qualified_type_name(type_);
    if type_.fn_types().is_some() {
        name.clone() + &type_name[2..]
    } else {
        name.clone() + "::" + type_name.replace(" ", "")
    }
}

/// The name of the code of the function literal at `location` in the
/// function generated with `specialization`.
fn function_literal_name(location: SrcSpan, type_: &Arc<Type>, specialization: u32) -> EcoString {
    let type_name = qualified_type_name(type_).replace(" ", "");
    format!(
        "anonymous@{}-{}#{specialization}:{type_name}",
        location.start, location.end
    )
    .into()
}

/// The name of `type_` with the types qualified by their module, except
/// the prelude ones, so different types have different names.
/// Whether `specific` is `general` with its type variables replaced by some
/// types, given the replacements already made in `bound`.
fn is_instance(
    general: &Arc<Type>,
    specific: &Arc<Type>,
    bound: &mut HashMap<u64, Arc<Type>>,
) -> bool {
    if let Type::Var { type_ } = specific.as_ref()
        && let TypeVar::Link { type_ } = &*type_.borrow()
    {
        return is_instance(general, type_, bound);
    }
    match general.as_ref() {
        Type::Var { type_ } => match &*type_.borrow() {
            TypeVar::Link { type_ } => is_instance(type_, specific, bound),
            TypeVar::Unbound { id } | TypeVar::Generic { id } => match bound.get(id) {
                Some(type_) => type_.same_as(specific),
                None => {
                    let _ = bound.insert(*id, specific.clone());
                    true
                }
            },
        },
        Type::Named {
            module,
            name,
            arguments,
            ..
        } => match specific.as_ref() {
            Type::Named {
                module: specific_module,
                name: specific_name,
                arguments: specific_arguments,
                ..
            } => {
                module == specific_module
                    && name == specific_name
                    && are_instances(arguments, specific_arguments, bound)
            }
            Type::Fn { .. } | Type::Tuple { .. } | Type::Var { .. } => false,
        },
        Type::Fn { arguments, return_ } => match specific.as_ref() {
            Type::Fn {
                arguments: specific_arguments,
                return_: specific_return,
            } => {
                are_instances(arguments, specific_arguments, bound)
                    && is_instance(return_, specific_return, bound)
            }
            Type::Named { .. } | Type::Tuple { .. } | Type::Var { .. } => false,
        },
        Type::Tuple { elements } => match specific.as_ref() {
            Type::Tuple {
                elements: specific_elements,
            } => are_instances(elements, specific_elements, bound),
            Type::Named { .. } | Type::Fn { .. } | Type::Var { .. } => false,
        },
    }
}

fn are_instances(
    general: &[Arc<Type>],
    specific: &[Arc<Type>],
    bound: &mut HashMap<u64, Arc<Type>>,
) -> bool {
    general.len() == specific.len()
        && general
            .iter()
            .zip(specific)
            .all(|(general, specific)| is_instance(general, specific, bound))
}

fn qualified_type_name(type_: &Arc<Type>) -> EcoString {
    let mut name = EcoString::new();
    write_qualified_type_name(&mut name, &mut vec![], type_);
    name
}

/// The qualified name of the function type with `arguments` and `return_`,
/// without creating the type.
fn qualified_function_type_name(arguments: &[Arc<Type>], return_: &Arc<Type>) -> EcoString {
    let mut name = EcoString::new();
    let mut type_vars = vec![];
    write_qualified_function_type_name(&mut name, &mut type_vars, arguments, return_);
    name
}

fn write_qualified_type_name(name: &mut EcoString, type_vars: &mut Vec<u64>, type_: &Arc<Type>) {
    match &**type_ {
        Type::Named {
            module,
            name: type_name,
            arguments,
            ..
        } => {
            if module != PRELUDE_MODULE_NAME {
                name.push_str(module);
                name.push('.');
            }
            name.push_str(type_name);
            if !arguments.is_empty() {
                write_qualified_type_names(name, type_vars, arguments);
            }
        }
        Type::Fn { arguments, return_ } => {
            write_qualified_function_type_name(name, type_vars, arguments, return_);
        }
        Type::Tuple { elements } => {
            name.push('#');
            write_qualified_type_names(name, type_vars, elements);
        }
        Type::Var { type_ } => match type_.borrow().deref() {
            TypeVar::Link { type_ } => write_qualified_type_name(name, type_vars, type_),
            TypeVar::Unbound { id } | TypeVar::Generic { id } => {
                let index = type_vars
                    .iter()
                    .position(|var| var == id)
                    .unwrap_or_else(|| {
                        type_vars.push(*id);
                        type_vars.len() - 1
                    });
                name.push_str(&format!("t{index}"));
            }
        },
    }
}

fn write_qualified_function_type_name(
    name: &mut EcoString,
    type_vars: &mut Vec<u64>,
    arguments: &[Arc<Type>],
    return_: &Arc<Type>,
) {
    name.push_str("fn");
    write_qualified_type_names(name, type_vars, arguments);
    name.push_str(" -> ");
    write_qualified_type_name(name, type_vars, return_);
}

fn write_qualified_type_names(name: &mut EcoString, type_vars: &mut Vec<u64>, types: &[Arc<Type>]) {
    name.push('(');
    for (index, type_) in types.iter().enumerate() {
        if index > 0 {
            name.push_str(", ");
        }
        write_qualified_type_name(name, type_vars, type_);
    }
    name.push(')');
}

fn remap_val_type(val_type: &ValType, remap: &impl Fn(TypeId) -> TypeId) -> ValType {
    match *val_type {
        ValType::Ref(RefType {
            nullable,
            heap_type,
        }) => ValType::Ref(RefType {
            nullable,
            heap_type: match heap_type {
                HeapType::Concrete(id) => HeapType::Concrete(remap(id)),
                HeapType::Exact(id) => HeapType::Exact(remap(id)),
                HeapType::Abstract(_) => heap_type,
                _ => panic!("unexpected heap type {heap_type:?}"),
            },
        }),
        ValType::I32 | ValType::I64 | ValType::F32 | ValType::F64 | ValType::V128 => *val_type,
    }
}

fn remap_storage_type(storage_type: StorageType, remap: &impl Fn(TypeId) -> TypeId) -> StorageType {
    match storage_type {
        StorageType::Val(val_type) => StorageType::Val(remap_val_type(&val_type, remap)),
        StorageType::I8 | StorageType::I16 => storage_type,
    }
}

fn remap_composite_type(
    type_: &walrus::CompositeType,
    remap: &impl Fn(TypeId) -> TypeId,
) -> walrus::CompositeType {
    let val_type = |val_type: &ValType| remap_val_type(val_type, remap);
    let field = |field: &FieldType| FieldType {
        element_type: remap_storage_type(field.element_type, remap),
        mutable: field.mutable,
    };
    match type_ {
        walrus::CompositeType::Function(function) => {
            walrus::CompositeType::Function(walrus::FunctionType::new(
                function.params().iter().map(val_type).collect(),
                function.results().iter().map(val_type).collect(),
            ))
        }
        walrus::CompositeType::Struct(struct_) => {
            walrus::CompositeType::Struct(walrus::StructType {
                fields: struct_.fields.iter().map(field).collect(),
            })
        }
        walrus::CompositeType::Array(array) => walrus::CompositeType::Array(walrus::ArrayType {
            field: field(&array.field),
        }),
    }
}

fn custom_type_inferred_constructor<'a>(
    custom_type: &'a TypedCustomType,
    type_: &Arc<Type>,
) -> Option<&'a TypedRecordConstructor> {
    type_
        .custom_type_inferred_variant()
        .and_then(|index| custom_type.constructors.get(index as usize))
}

/// The base name of the function `name` imported from the module `module`,
/// qualified to avoid collisions.
fn imported_base_name(module: &EcoString, name: &EcoString) -> EcoString {
    format!("{module}.{name}").into()
}

/// The name of the constructor `constructor`, written as `name`: an imported
/// constructor can be written with an alias.
fn constructor_name<'a>(name: &'a EcoString, constructor: &'a ValueConstructor) -> &'a EcoString {
    match &constructor.variant {
        ValueConstructorVariant::Record { name, .. } => name,
        ValueConstructorVariant::LocalVariable { .. }
        | ValueConstructorVariant::ModuleConstant { .. }
        | ValueConstructorVariant::ModuleFn { .. } => name,
    }
}

/// The name of the constructor of the record constant written as `name`.
fn record_constant_name<'a>(
    name: &'a EcoString,
    record_constructor: &'a Option<Box<ValueConstructor>>,
) -> &'a EcoString {
    record_constructor
        .as_deref()
        .map_or(name, |constructor| constructor_name(name, constructor))
}

/// What a function value statically names.
#[derive(Debug, Clone, Copy)]
enum StaticTarget<'a> {
    /// The function `name` of the module `module`.
    Function {
        module: &'a EcoString,
        name: &'a EcoString,
    },
    /// The record constructor with the real name `name`.
    Constructor(&'a EcoString),
}

/// What the expression `fun` statically names, following constant aliases.
fn static_target(fun: &TypedExpr) -> Option<StaticTarget<'_>> {
    if let TypedExpr::Var { constructor, .. } = fun {
        variant_target(&constructor.variant)
    } else if let TypedExpr::ModuleSelect {
        constructor,
        module_name,
        label,
        ..
    } = fun
    {
        match constructor {
            ModuleValueConstructor::Fn { .. } => Some(StaticTarget::Function {
                module: module_name,
                name: label,
            }),
            ModuleValueConstructor::Record { name, .. } => Some(StaticTarget::Constructor(name)),
            ModuleValueConstructor::Constant { literal, .. } => constant_target(literal),
        }
    } else {
        None
    }
}

/// What the constant `constant` statically names, following constant aliases.
fn constant_target(constant: &TypedConstant) -> Option<StaticTarget<'_>> {
    match constant {
        Constant::Var {
            constructor: Some(constructor),
            ..
        } => variant_target(&constructor.variant),
        Constant::Record {
            name,
            arguments: None,
            record_constructor,
            ..
        } => Some(StaticTarget::Constructor(record_constant_name(
            name,
            record_constructor,
        ))),
        Constant::Int { .. }
        | Constant::Float { .. }
        | Constant::String { .. }
        | Constant::Tuple { .. }
        | Constant::List { .. }
        | Constant::Record { .. }
        | Constant::RecordUpdate { .. }
        | Constant::BitArray { .. }
        | Constant::Var { .. }
        | Constant::BinaryOperator { .. }
        | Constant::Invalid { .. }
        | Constant::Todo { .. } => None,
    }
}

/// What a value with the variant `variant` statically names. A local
/// variable names nothing statically.
fn variant_target(variant: &ValueConstructorVariant) -> Option<StaticTarget<'_>> {
    match variant {
        ValueConstructorVariant::ModuleFn { module, name, .. } => {
            Some(StaticTarget::Function { module, name })
        }
        ValueConstructorVariant::Record { name, .. } => Some(StaticTarget::Constructor(name)),
        ValueConstructorVariant::ModuleConstant { literal, .. } => constant_target(literal),
        ValueConstructorVariant::LocalVariable { .. } => None,
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

/// A few characters of a string literal, safe to put in a wasm name. The
/// name section is read by hand and by `wasm-tools print`, so this only has
/// to be legible, not injective -- the index in `string@{i}<{tag}>` is what
/// tells two literals apart.
fn string_tag(s: &str) -> String {
    s.chars()
        .take(16)
        .map(|c| match c.is_ascii_alphanumeric() {
            true => c,
            false => '_',
        })
        .collect()
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

/// Whether the modules of `package` may declare external functions and types
/// of the builtins: the standard library and the runtime, which build on them
/// the API of programs.
fn reaches_builtins(package: &str) -> bool {
    package == STDLIB_PACKAGE_NAME || package == RUNTIME_PACKAGE
}
