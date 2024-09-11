use wasm_encoder::CodeSection;

use wasm_encoder::ElementSection;
use wasm_encoder::FunctionSection;

use super::WasmPrimitive;
use super::WasmType;

use wasm_encoder::TypeSection;

use super::WasmModule;

pub(crate) fn emit(wasm_module: WasmModule) -> Vec<u8> {
    let mut module = wasm_encoder::Module::new();

    // types
    let mut types = TypeSection::new();
    for type_ in &wasm_module.types {
        match type_ {
            WasmType::FunctionType {
                parameters,
                returns,
            } => {
                let parameters: Vec<_> = parameters
                    .into_iter()
                    .copied()
                    .map(WasmPrimitive::to_val_type)
                    .collect();
                let returns = [returns.to_val_type()];
                _ = types.function(parameters, returns);
            }
            WasmType::SumType => {
                _ = types.subtype(&wasm_encoder::SubType {
                    is_final: false,
                    supertype_idx: None,
                    composite_type: wasm_encoder::CompositeType {
                        inner: wasm_encoder::CompositeInnerType::Struct(wasm_encoder::StructType {
                            fields: Box::new([]),
                        }),
                        shared: false,
                    },
                });
            }
            WasmType::ProductType {
                supertype_index,
                fields,
            } => {
                let mut field_list = vec![];
                for field in fields {
                    field_list.push(wasm_encoder::FieldType {
                        element_type: wasm_encoder::StorageType::Val(field.to_val_type()),
                        mutable: false,
                    });
                }

                _ = types.subtype(&wasm_encoder::SubType {
                    is_final: true,
                    supertype_idx: Some(*supertype_index),
                    composite_type: wasm_encoder::CompositeType {
                        inner: wasm_encoder::CompositeInnerType::Struct(wasm_encoder::StructType {
                            fields: field_list.into_boxed_slice(),
                        }),
                        shared: false,
                    },
                });
            }
        }
    }
    _ = module.section(&types);

    // functions
    let mut functions = FunctionSection::new();
    for func in &wasm_module.functions {
        _ = functions.function(func.type_index);
    }
    _ = module.section(&functions);

    // elems
    let mut elems = ElementSection::new();
    // emit references to functions
    let indices: Vec<_> = (0..(wasm_module.functions.len() as u32))
        .into_iter()
        .collect();
    _ = elems.segment(wasm_encoder::ElementSegment {
        mode: wasm_encoder::ElementMode::Declared,
        elements: wasm_encoder::Elements::Functions(&indices[..]),
    });
    _ = module.section(&elems);

    // code
    let mut codes = CodeSection::new();
    for func in wasm_module.functions.iter() {
        let locals = func
            .locals
            .iter()
            .copied()
            .map(|typ| (1, typ.to_val_type()));
        let mut f = wasm_encoder::Function::new(locals);
        for inst in &func.instructions.lst {
            _ = f.instruction(inst);
        }
        _ = codes.function(&f);
    }
    _ = module.section(&codes);

    module.finish()
}
