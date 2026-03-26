use std::collections::HashMap;

use ecow::EcoString;
use wasm_encoder::{AbstractHeapType, ExportKind, HeapType, RefType, ValType};

pub(super) enum DataSegment {
    Passive(Vec<u8>),
    Active {
        memory_index: u32,
        offset_i32: i32,
        data: Vec<u8>,
    },
}

/// All data extracted from the builtins wasm binary.
pub(super) struct Builtins {
    /// Imports: (module, name, type_index_in_builtins)
    pub imports: Vec<(String, String, usize)>,
    /// Types from TypeSection: (params, results)
    pub types: Vec<(Vec<ValType>, Vec<ValType>)>,
    /// Globals: (val_type, mutable, shared, init_value_i32)
    pub globals: Vec<(ValType, bool, bool, i32)>,
    /// Exports: (name, kind, index). Func and Global exports are excluded.
    pub exports: Vec<(String, ExportKind, u32)>,
    /// Named functions: (name, params, results, code, original_index)
    pub functions: Vec<(EcoString, Vec<ValType>, Vec<ValType>, Vec<u8>, u32)>,
    /// Data segments
    pub data_segments: Vec<DataSegment>,
    /// Global names: (index, name)
    pub global_names: Vec<(u32, String)>,
    /// Data segment names, indexed by segment index
    pub data_names: Vec<String>,
    /// Function name → (params, results) for signature validation
    pub available: HashMap<EcoString, (Vec<ValType>, Vec<ValType>)>,
}

/// Parse the builtins wasm binary and extract all sections.
pub(super) fn parse_builtins(builtins_wasm: &[u8]) -> Builtins {
    let mut types = vec![];
    let mut imports = vec![];
    let mut functions_types = vec![];
    let mut functions_code: Vec<(Vec<ValType>, Vec<ValType>, Vec<u8>)> = vec![];
    let mut globals = vec![];
    let mut exports = vec![];
    let mut data_segments = vec![];
    let mut global_names = vec![];
    let mut data_names: Vec<String> = vec![];
    let mut named_functions = vec![];
    let mut available = HashMap::new();

    for payload in wasmparser::Parser::new(0).parse_all(builtins_wasm) {
        match payload.expect("valid wasm payload") {
            wasmparser::Payload::TypeSection(section) => {
                for item in section.into_iter_with_offsets() {
                    let (_, group) = item.expect("Type entry");
                    for type_ in group.into_types() {
                        let func_type = type_.composite_type.unwrap_func();
                        let params = wasmparser_types_to_wasmencoder_types(func_type.params());
                        let results = wasmparser_types_to_wasmencoder_types(func_type.results());
                        types.push((params, results))
                    }
                }
            }
            wasmparser::Payload::ImportSection(section) => {
                for item in section.into_iter_with_offsets() {
                    let (_, import) = item.expect("Import entry");
                    let type_index = match import.ty {
                        wasmparser::TypeRef::Func(index) => index as usize,
                        other => panic!("unexpected import type in builtins module: {other:?}"),
                    };
                    imports.push((
                        import.module.to_string(),
                        import.name.to_string(),
                        type_index,
                    ));
                }
            }
            wasmparser::Payload::FunctionSection(section) => {
                for function in section.into_iter_with_offsets() {
                    let (_, type_index) = function.expect("Function entry");
                    functions_types.push(
                        types
                            .get(type_index as usize)
                            .expect("function type at index")
                            .clone(),
                    );
                }
            }
            wasmparser::Payload::GlobalSection(section) => {
                for entry in section.into_iter_with_offsets() {
                    let (_, global) = entry.expect("Global entry");
                    globals.push((
                        wasmparser_type_to_wasmencoder_type(&global.ty.content_type),
                        global.ty.mutable,
                        global.ty.shared,
                        parse_i32_const_expr(&global.init_expr),
                    ));
                }
            }
            wasmparser::Payload::ExportSection(section) => {
                for export in section.into_iter_with_offsets() {
                    let (_, export) = export.expect("Export entry");
                    let kind = match export.kind {
                        wasmparser::ExternalKind::Func | wasmparser::ExternalKind::Global => {
                            continue;
                        }
                        wasmparser::ExternalKind::Table => ExportKind::Table,
                        wasmparser::ExternalKind::Memory => ExportKind::Memory,
                        wasmparser::ExternalKind::Tag => ExportKind::Tag,
                    };
                    exports.push((export.name.to_string(), kind, export.index));
                }
            }
            wasmparser::Payload::CodeSectionEntry(section) => {
                let (params, results) = functions_types
                    .get(functions_code.len())
                    .expect("Function type")
                    .clone();
                functions_code.push((params, results, section.as_bytes().to_vec()));
            }
            wasmparser::Payload::DataSection(section) => {
                for entry in section.into_iter_with_offsets() {
                    let (_, entry) = entry.expect("Data entry");
                    match &entry.kind {
                        wasmparser::DataKind::Passive => {
                            data_segments.push(DataSegment::Passive(entry.data.to_vec()));
                        }
                        wasmparser::DataKind::Active {
                            memory_index,
                            offset_expr,
                        } => {
                            data_segments.push(DataSegment::Active {
                                memory_index: *memory_index,
                                offset_i32: parse_i32_const_expr(offset_expr),
                                data: entry.data.to_vec(),
                            });
                        }
                    }
                }
            }
            wasmparser::Payload::CustomSection(section) => {
                if let wasmparser::KnownCustom::Name(section) = section.as_known() {
                    for sub in section {
                        match sub.expect("Name section") {
                            wasmparser::Name::Function(section_limited) => {
                                let import_count = imports.len() as u32;
                                for item in section_limited.into_iter_with_offsets() {
                                    let (_, name) = item.expect("Name entry");
                                    if name.index < import_count {
                                        continue;
                                    }
                                    let index = (name.index - import_count) as usize;
                                    if let Some((params, results, code)) = functions_code.get(index)
                                    {
                                        let _ = available.insert(
                                            name.name.into(),
                                            (params.clone(), results.clone()),
                                        );
                                        named_functions.push((
                                            EcoString::from(name.name),
                                            params.clone(),
                                            results.clone(),
                                            code.clone(),
                                            name.index,
                                        ));
                                    }
                                }
                            }
                            wasmparser::Name::Global(section_limited) => {
                                for item in section_limited.into_iter_with_offsets() {
                                    let (_, name) = item.expect("Name entry");
                                    global_names.push((name.index, name.name.to_string()));
                                }
                            }
                            wasmparser::Name::Data(section_limited) => {
                                for item in section_limited.into_iter_with_offsets() {
                                    let (_, name) = item.expect("Name entry");
                                    assert_eq!(
                                        name.index as usize,
                                        data_names.len(),
                                        "data segment names must be in order"
                                    );
                                    data_names.push(name.name.to_string());
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

    Builtins {
        imports,
        types,
        globals,
        exports,
        functions: named_functions,
        data_segments,
        global_names,
        data_names,
        available,
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
                heap_type: HeapType::Concrete(
                    unpacked_index
                        .as_module_index()
                        .expect("concrete type to have a module index"),
                ),
            }),
        },
    }
}

fn parse_i32_const_expr(const_: &wasmparser::ConstExpr<'_>) -> i32 {
    let mut i32_value = 0;
    for op in const_.get_operators_reader().into_iter_with_offsets() {
        match op.expect("valid wasm operator").0 {
            wasmparser::Operator::I32Const { value } => {
                assert_eq!(i32_value, 0, "Too much ops");
                i32_value = value;
            }
            wasmparser::Operator::End => {}
            op => panic!("unexpected operator during code generation: {op:?}"),
        }
    }
    i32_value
}
