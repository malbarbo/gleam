//! Links the functions of builtins-wasm into a generated module.
//!
//! The generated module refers to the exported functions of builtins-wasm
//! through stubs, functions with their signature and an `unreachable` body.
//! After the dead code elimination of walrus, the stubs that survived and the
//! builtins they reach are declared in the walrus module, with the memory,
//! table, global and data they need, and the emitted binary gets the bodies of
//! builtins-wasm in place of the stubs. builtins-wasm is parsed only once,
//! and its code never goes through walrus.

// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

// The matches on sections and instructions only care about a few of them.
#![allow(clippy::wildcard_enum_match_arm)]

use super::BUILTINS_WASM;
use itertools::Itertools;
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    convert::Infallible,
    ops::Range,
    sync::{Arc, LazyLock, OnceLock},
};
use walrus::{
    ConstExpr, DataKind, ElementItems, ElementKind, ExportItem, FunctionBuilder, FunctionId,
    GlobalId, IdsToIndices, MemoryId, RefType, TableId, TypeId, ValType, ir::Value,
};
use wasm_encoder::reencode::{Error as ReencodeError, Reencode};
use wasmparser::{BlockType, FunctionBody, Operator, Payload, TypeRef};

/// The name of the custom section that records the indexes of the items of
/// builtins-wasm in the emitted module, removed when linking.
const INDEXES_SECTION: &str = "gleam:builtins";

/// The custom sections of builtins-wasm that the generated module keeps.
const CUSTOM_SECTIONS: &[&str] = &["producers", "target_features"];

/// builtins-wasm, parsed once.
static BUILTINS: LazyLock<Builtins> = LazyLock::new(|| Builtins::parse(BUILTINS_WASM));

struct Builtins {
    types: Vec<(Vec<ValType>, Vec<ValType>)>,
    /// The imported functions, first in the index space of the functions.
    imports: Vec<Import>,
    /// The defined functions, after the imported ones.
    functions: Vec<Function>,
    memory: u64,
    table: (u64, Option<u64>),
    /// The offset and the functions of the active element segments.
    elements: Vec<(i32, Vec<u32>)>,
    /// The initial value of the stack pointer, the only global.
    global: i32,
    /// The offset and the bytes of the active data segments.
    data: Vec<(i32, Range<usize>)>,
    /// The name and the data of the custom sections that are kept.
    customs: Vec<(&'static str, &'static [u8])>,
}

struct Import {
    module: String,
    name: String,
    type_: u32,
}

struct Function {
    name: Option<String>,
    export: Option<String>,
    type_: u32,
    body: FunctionBody<'static>,
    /// The functions it calls or references.
    calls: Vec<u32>,
    /// The types its code refers to.
    types: Vec<u32>,
    indirect: bool,
    global: bool,
    /// Whether its code refers to an address of the data segments.
    data: bool,
}

impl Builtins {
    fn parse(wasm: &'static [u8]) -> Self {
        let mut builtins = Builtins {
            types: vec![],
            imports: vec![],
            functions: vec![],
            memory: 0,
            table: (0, None),
            elements: vec![],
            global: 0,
            data: vec![],
            customs: vec![],
        };
        let mut function_types = vec![];
        let mut exports = HashMap::new();
        let mut names = HashMap::new();
        for payload in wasmparser::Parser::new(0).parse_all(wasm) {
            match payload.expect("valid builtins wasm") {
                Payload::TypeSection(reader) => {
                    for type_ in reader.into_iter_err_on_gc_types() {
                        let type_ = type_.expect("valid builtins wasm");
                        builtins.types.push((
                            type_.params().iter().map(val_type).collect(),
                            type_.results().iter().map(val_type).collect(),
                        ));
                    }
                }
                Payload::ImportSection(reader) => {
                    for import in reader.into_imports() {
                        let import = import.expect("valid builtins wasm");
                        let TypeRef::Func(type_) = import.ty else {
                            panic!("builtins wasm to import only functions");
                        };
                        builtins.imports.push(Import {
                            module: import.module.into(),
                            name: import.name.into(),
                            type_,
                        });
                    }
                }
                Payload::FunctionSection(reader) => {
                    function_types = reader
                        .into_iter()
                        .collect::<Result<_, _>>()
                        .expect("valid builtins wasm");
                }
                Payload::TableSection(reader) => {
                    let table = reader
                        .into_iter()
                        .exactly_one()
                        .ok()
                        .expect("builtins wasm to have one table")
                        .expect("valid builtins wasm");
                    builtins.table = (table.ty.initial, table.ty.maximum);
                }
                Payload::MemorySection(reader) => {
                    let memory = reader
                        .into_iter()
                        .exactly_one()
                        .ok()
                        .expect("builtins wasm to have one memory")
                        .expect("valid builtins wasm");
                    builtins.memory = memory.initial;
                }
                Payload::GlobalSection(reader) => {
                    let global = reader
                        .into_iter()
                        .exactly_one()
                        .ok()
                        .expect("builtins wasm to have one global")
                        .expect("valid builtins wasm");
                    builtins.global = const_i32(&global.init_expr);
                }
                Payload::ExportSection(reader) => {
                    for export in reader {
                        let export = export.expect("valid builtins wasm");
                        if export.kind == wasmparser::ExternalKind::Func {
                            let _ = exports.insert(export.index, export.name.to_string());
                        }
                    }
                }
                Payload::ElementSection(reader) => {
                    for element in reader {
                        let element = element.expect("valid builtins wasm");
                        let wasmparser::ElementKind::Active {
                            table_index: None | Some(0),
                            offset_expr,
                        } = element.kind
                        else {
                            panic!("builtins wasm to have only active elements");
                        };
                        let wasmparser::ElementItems::Functions(functions) = element.items else {
                            panic!("builtins wasm to have only function elements");
                        };
                        builtins.elements.push((
                            const_i32(&offset_expr),
                            functions
                                .into_iter()
                                .collect::<Result<_, _>>()
                                .expect("valid builtins wasm"),
                        ));
                    }
                }
                Payload::DataSection(reader) => {
                    for data in reader {
                        let data = data.expect("valid builtins wasm");
                        let wasmparser::DataKind::Active {
                            memory_index: 0,
                            offset_expr,
                        } = data.kind
                        else {
                            panic!("builtins wasm to have only active data");
                        };
                        let start = data.data.as_ptr() as usize - wasm.as_ptr() as usize;
                        builtins
                            .data
                            .push((const_i32(&offset_expr), start..start + data.data.len()));
                    }
                }
                Payload::CodeSectionEntry(body) => {
                    let type_ = function_types
                        .get(builtins.functions.len())
                        .copied()
                        .expect("valid builtins wasm");
                    builtins.functions.push(Function::parse(body, type_));
                }
                Payload::CustomSection(section) => {
                    if CUSTOM_SECTIONS.contains(&section.name()) {
                        builtins.customs.push((section.name(), section.data()));
                    } else if let wasmparser::KnownCustom::Name(reader) = section.as_known() {
                        for subsection in reader {
                            if let Ok(wasmparser::Name::Function(map)) = subsection {
                                for naming in map.into_iter().flatten() {
                                    let _ = names.insert(naming.index, naming.name.to_string());
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        let imports = builtins.imports.len() as u32;
        for (index, function) in builtins.functions.iter_mut().enumerate() {
            let index = imports + index as u32;
            function.name = names.remove(&index);
            function.export = exports.remove(&index);
            function.data = addresses(&function.body).any(|address| {
                builtins.data.iter().any(|(offset, range)| {
                    let start = *offset as u32 as u64;
                    (start..start + range.len() as u64).contains(&address)
                })
            });
        }
        builtins
    }

    /// The defined function `index`, `None` for an imported one.
    fn function(&self, index: u32) -> Option<&Function> {
        let index = index.checked_sub(self.imports.len() as u32)?;
        Some(self.functions.get(index as usize).expect("function index"))
    }

    fn import(&self, index: u32) -> &Import {
        self.imports.get(index as usize).expect("import index")
    }

    fn type_(&self, index: u32) -> &(Vec<ValType>, Vec<ValType>) {
        self.types.get(index as usize).expect("type index")
    }
}

impl Function {
    fn parse(body: FunctionBody<'static>, type_: u32) -> Self {
        let mut function = Function {
            name: None,
            export: None,
            type_,
            body: body.clone(),
            calls: vec![],
            types: vec![],
            indirect: false,
            global: false,
            data: false,
        };
        let mut reader = body.get_operators_reader().expect("valid builtins wasm");
        while !reader.eof() {
            match reader.read().expect("valid builtins wasm") {
                Operator::Call { function_index }
                | Operator::ReturnCall { function_index }
                | Operator::RefFunc { function_index } => function.calls.push(function_index),
                Operator::CallIndirect { type_index, .. }
                | Operator::ReturnCallIndirect { type_index, .. } => {
                    function.indirect = true;
                    function.types.push(type_index);
                }
                Operator::Block { blockty }
                | Operator::Loop { blockty }
                | Operator::If { blockty } => {
                    if let BlockType::FuncType(type_index) = blockty {
                        function.types.push(type_index);
                    }
                }
                Operator::GlobalGet { .. } | Operator::GlobalSet { .. } => function.global = true,
                Operator::MemoryInit { .. } | Operator::DataDrop { .. } => {
                    panic!("builtins wasm to not refer to data segments")
                }
                _ => {}
            }
        }
        function
    }
}

/// The constant addresses that the code of a function can read or write: its
/// `i32.const` and the offsets of its loads and stores. A static of Rust is at
/// an address in the data segments that the code has as one of them.
fn addresses(body: &FunctionBody<'static>) -> impl Iterator<Item = u64> {
    let mut reader = body.get_operators_reader().expect("valid builtins wasm");
    std::iter::from_fn(move || {
        while !reader.eof() {
            match reader.read().expect("valid builtins wasm") {
                Operator::I32Const { value } => return Some(value as u32 as u64),
                Operator::I32Load { memarg }
                | Operator::I64Load { memarg }
                | Operator::F32Load { memarg }
                | Operator::F64Load { memarg }
                | Operator::I32Load8S { memarg }
                | Operator::I32Load8U { memarg }
                | Operator::I32Load16S { memarg }
                | Operator::I32Load16U { memarg }
                | Operator::I64Load8S { memarg }
                | Operator::I64Load8U { memarg }
                | Operator::I64Load16S { memarg }
                | Operator::I64Load16U { memarg }
                | Operator::I64Load32S { memarg }
                | Operator::I64Load32U { memarg }
                | Operator::I32Store { memarg }
                | Operator::I64Store { memarg }
                | Operator::F32Store { memarg }
                | Operator::F64Store { memarg }
                | Operator::I32Store8 { memarg }
                | Operator::I32Store16 { memarg }
                | Operator::I64Store8 { memarg }
                | Operator::I64Store16 { memarg }
                | Operator::I64Store32 { memarg } => return Some(memarg.offset),
                _ => {}
            }
        }
        None
    })
}

fn val_type(type_: &wasmparser::ValType) -> ValType {
    match type_ {
        wasmparser::ValType::I32 => ValType::I32,
        wasmparser::ValType::I64 => ValType::I64,
        wasmparser::ValType::F32 => ValType::F32,
        wasmparser::ValType::F64 => ValType::F64,
        _ => panic!("builtins wasm to use only numeric types"),
    }
}

fn const_i32(expr: &wasmparser::ConstExpr<'_>) -> i32 {
    match expr.get_operators_reader().read() {
        Ok(Operator::I32Const { value }) => value,
        _ => panic!("builtins wasm to use only i32 constant expressions"),
    }
}

/// The stubs of the exported functions of builtins-wasm in a generated
/// module.
#[derive(Debug)]
pub(super) struct Linker {
    memory: MemoryId,
    /// The stubs and the index of their function in builtins-wasm.
    stubs: Vec<(u32, FunctionId)>,
}

impl Linker {
    /// Adds to `module` the memory and the custom sections of builtins-wasm
    /// and a stub for each of its exported functions.
    pub(super) fn new(module: &mut walrus::Module) -> Self {
        let builtins = &*BUILTINS;
        for &(name, data) in &builtins.customs {
            let _ = module.customs.add(walrus::RawCustomSection {
                name: name.into(),
                data: data.into(),
            });
        }
        let memory = module
            .memories
            .add_local(false, false, builtins.memory, None, None);
        let imports = builtins.imports.len() as u32;
        let stubs = builtins
            .functions
            .iter()
            .enumerate()
            .filter(|(_, function)| function.export.is_some())
            .map(|(index, function)| (imports + index as u32, stub(module, function)))
            .collect();
        Linker { memory, stubs }
    }

    pub(super) fn memory(&self) -> MemoryId {
        self.memory
    }

    /// The exported functions of builtins-wasm: their name, stub, params and
    /// results.
    pub(super) fn exports(
        &self,
    ) -> impl Iterator<Item = (&str, FunctionId, &[ValType], &[ValType])> {
        let builtins = &*BUILTINS;
        self.stubs.iter().map(|&(index, id)| {
            let function = builtins.function(index).expect("defined function");
            let (params, results) = builtins.type_(function.type_);
            (
                function.export.as_deref().expect("exported function"),
                id,
                params.as_slice(),
                results.as_slice(),
            )
        })
    }

    /// Eliminates the dead code of `module` and declares in it the items of
    /// builtins-wasm that the surviving stubs reach, see `emit`.
    pub(super) fn eliminate_dead_code(self, module: &mut walrus::Module) -> Link {
        let builtins = &*BUILTINS;
        walrus::passes::gc::run(module);
        let alive: HashSet<FunctionId> = module.funcs.iter().map(|f| f.id()).collect();
        let mut functions: HashMap<u32, FunctionId> = self
            .stubs
            .into_iter()
            .filter(|(_, id)| alive.contains(id))
            .collect();

        // The functions of builtins-wasm that the stubs reach, sorted so that
        // the emitted module is the same in every run.
        let mut reached: Vec<u32> = functions.keys().copied().collect();
        let mut seen: HashSet<u32> = reached.iter().copied().collect();
        let mut pending = reached.clone();
        let mut indirect = false;
        while let Some(index) = pending.pop() {
            let Some(function) = builtins.function(index) else {
                continue;
            };
            let mut callees = function.calls.clone();
            if function.indirect && !indirect {
                indirect = true;
                for (_, elements) in &builtins.elements {
                    callees.extend(elements.iter().copied());
                }
            }
            for callee in callees {
                if seen.insert(callee) {
                    reached.push(callee);
                    pending.push(callee);
                }
            }
        }
        reached.sort_unstable();

        let memory_alive = module.memories.iter().any(|m| m.id() == self.memory);
        if reached.is_empty() {
            if memory_alive {
                let _ = module
                    .exports
                    .add("memory", ExportItem::Memory(self.memory));
            }
            return Link(None);
        }
        let memory = if memory_alive {
            self.memory
        } else {
            module
                .memories
                .add_local(false, false, builtins.memory, None, None)
        };
        let _ = module.exports.add("memory", ExportItem::Memory(memory));

        let mut types = HashMap::new();
        let mut type_id = |module: &mut walrus::Module, index: u32| {
            *types.entry(index).or_insert_with(|| {
                let (params, results) = builtins.type_(index);
                module.types.add(params, results)
            })
        };
        let mut global = false;
        let mut data = false;
        for &index in &reached {
            match builtins.function(index) {
                Some(function) => {
                    for &type_ in &function.types {
                        let _ = type_id(module, type_);
                    }
                    global |= function.global;
                    data |= function.data;
                    if let std::collections::hash_map::Entry::Vacant(entry) = functions.entry(index)
                    {
                        let _ = entry.insert(stub(module, function));
                    }
                }
                None => {
                    let import = builtins.import(index);
                    let type_ = type_id(module, import.type_);
                    let (id, _) = module.add_import_func(&import.module, &import.name, type_);
                    let _ = functions.insert(index, id);
                }
            }
        }
        let global = global.then(|| {
            module.globals.add_local(
                ValType::I32,
                true,
                false,
                ConstExpr::Value(Value::I32(builtins.global)),
            )
        });
        let table = indirect.then(|| {
            let table = module.tables.add_local(
                false,
                builtins.table.0,
                builtins.table.1,
                RefType::FUNCREF,
            );
            for (offset, elements) in &builtins.elements {
                let _ = module.elements.add(
                    ElementKind::Active {
                        table,
                        offset: ConstExpr::Value(Value::I32(*offset)),
                    },
                    ElementItems::Functions(
                        elements
                            .iter()
                            .map(|e| *functions.get(e).expect("reached element"))
                            .collect(),
                    ),
                );
            }
            table
        });
        if data {
            for (offset, range) in &builtins.data {
                let _ = module.data.add(
                    DataKind::Active {
                        memory,
                        offset: ConstExpr::Value(Value::I32(*offset)),
                    },
                    BUILTINS_WASM
                        .get(range.clone())
                        .expect("data range")
                        .to_vec(),
                );
            }
        }
        let indexes = Arc::new(OnceLock::new());
        let _ = module.customs.add(IndexesSection {
            ids: Ids {
                functions,
                types,
                global,
                table,
                memory,
            },
            indexes: Arc::clone(&indexes),
        });
        Link(Some(indexes))
    }
}

/// The indexes of the items of builtins-wasm, known when the module is
/// emitted.
#[derive(Debug)]
pub(super) struct Link(Option<Arc<OnceLock<Indexes>>>);

/// Adds to `module` a function with the signature of `function` and an
/// `unreachable` body.
fn stub(module: &mut walrus::Module, function: &Function) -> FunctionId {
    let (params, results) = BUILTINS.type_(function.type_);
    let mut builder = FunctionBuilder::new(&mut module.types, params, results);
    if let Some(name) = &function.name {
        let _ = builder.name(name.clone());
    }
    let _ = builder.func_body().unreachable();
    let args = params.iter().map(|&t| module.locals.add(t)).collect();
    builder.finish(args, &mut module.funcs)
}

#[derive(Debug)]
struct Ids {
    functions: HashMap<u32, FunctionId>,
    types: HashMap<u32, TypeId>,
    global: Option<GlobalId>,
    table: Option<TableId>,
    memory: MemoryId,
}

/// The indexes in the emitted module of the items of builtins-wasm.
#[derive(Debug)]
pub(super) struct Indexes {
    functions: HashMap<u32, u32>,
    types: HashMap<u32, u32>,
    global: u32,
    table: u32,
    memory: u32,
}

/// A custom section that records the indexes of the items of builtins-wasm
/// when walrus emits the module.
#[derive(Debug)]
struct IndexesSection {
    ids: Ids,
    indexes: Arc<OnceLock<Indexes>>,
}

impl walrus::CustomSection for IndexesSection {
    fn name(&self) -> &str {
        INDEXES_SECTION
    }

    fn data(&self, ids: &IdsToIndices) -> Cow<'_, [u8]> {
        let _ = self.indexes.set(Indexes {
            functions: self
                .ids
                .functions
                .iter()
                .map(|(&index, &id)| (index, ids.get_func_index(id)))
                .collect(),
            types: self
                .ids
                .types
                .iter()
                .map(|(&index, &id)| (index, ids.get_type_index(id)))
                .collect(),
            global: self.ids.global.map_or(0, |id| ids.get_global_index(id)),
            table: self.ids.table.map_or(0, |id| ids.get_table_index(id)),
            memory: ids.get_memory_index(self.ids.memory),
        });
        Cow::Borrowed(&[])
    }
}

/// Emits `module`, after `Linker::eliminate_dead_code`, with the bodies of
/// builtins-wasm in place of their stubs.
pub(super) fn emit(module: &mut walrus::Module, link: Link) -> Vec<u8> {
    let Link(Some(indexes)) = link else {
        return module.emit_wasm();
    };
    let imports = module
        .imports
        .iter()
        .filter(|i| matches!(i.kind, walrus::ImportKind::Function(_)))
        .count() as u32;
    let wasm = module.emit_wasm();
    let indexes = indexes.get().expect("indexes set when emitting");
    let stubs: HashMap<u32, u32> = indexes
        .functions
        .iter()
        .map(|(&builtin, &index)| (index, builtin))
        .collect();
    let mut relocation = Relocation { indexes };

    let mut output = wasm_encoder::Module::new();
    let mut code = None;
    let mut index = imports;
    for payload in wasmparser::Parser::new(0).parse_all(&wasm) {
        let payload = payload.expect("valid emitted wasm");
        match &payload {
            Payload::CodeSectionStart { .. } => code = Some(wasm_encoder::CodeSection::new()),
            Payload::CodeSectionEntry(body) => {
                let code = code.as_mut().expect("code section");
                match stubs.get(&index) {
                    Some(&builtin) => {
                        let function = BUILTINS.function(builtin).expect("defined function");
                        relocation
                            .parse_function_body(code, function.body.clone())
                            .expect("relocatable builtins wasm");
                    }
                    None => {
                        let _ = code.raw(wasm.get(body.range()).expect("body range"));
                    }
                }
                index += 1;
            }
            Payload::CustomSection(section) if section.name() == INDEXES_SECTION => {}
            _ => {
                if let Some(code) = code.take() {
                    let _ = output.section(&code);
                }
                if let Some((id, range)) = payload.as_section() {
                    let _ = output.section(&wasm_encoder::RawSection {
                        id,
                        data: wasm.get(range).expect("section range"),
                    });
                }
            }
        }
    }
    if let Some(code) = code.take() {
        let _ = output.section(&code);
    }
    output.finish()
}

struct Relocation<'a> {
    indexes: &'a Indexes,
}

impl Reencode for Relocation<'_> {
    type Error = Infallible;

    fn function_index(&mut self, func: u32) -> Result<u32, ReencodeError<Infallible>> {
        Ok(*self.indexes.functions.get(&func).expect("reached function"))
    }

    fn type_index(&mut self, ty: u32) -> Result<u32, ReencodeError<Infallible>> {
        Ok(*self.indexes.types.get(&ty).expect("reached type"))
    }

    fn global_index(&mut self, _global: u32) -> Result<u32, ReencodeError<Infallible>> {
        Ok(self.indexes.global)
    }

    fn table_index(&mut self, _table: u32) -> Result<u32, ReencodeError<Infallible>> {
        Ok(self.indexes.table)
    }

    fn memory_index(&mut self, _memory: u32) -> Result<u32, ReencodeError<Infallible>> {
        Ok(self.indexes.memory)
    }
}
