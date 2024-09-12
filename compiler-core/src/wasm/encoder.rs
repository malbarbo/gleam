#![allow(clippy::let_unit_value)]

use wasm_encoder::CodeSection;

use wasm_encoder::ElementSection;
use wasm_encoder::FunctionSection;
use wasm_encoder::GlobalSection;
use wasm_encoder::IndirectNameMap;
use wasm_encoder::NameMap;
use wasm_encoder::NameSection;
use wasm_encoder::StartSection;

use super::WasmPrimitive;
use super::WasmType;
use super::WasmTypeDefinition;

use wasm_encoder::TypeSection;

use super::WasmModule;

pub(crate) fn emit(wasm_module: WasmModule) -> Vec<u8> {
    let mut module = wasm_encoder::Module::new();

    // types
    let mut types = TypeSection::new();
    for type_ in &wasm_module.types {
        let WasmType {
            definition: type_, ..
        } = type_;
        match type_ {
            WasmTypeDefinition::Function {
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
            WasmTypeDefinition::Sum => {
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
            WasmTypeDefinition::Product {
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
    // also declare the start function type
    let init_function_type_idx = wasm_module.types.len() as u32;
    _ = types.function(vec![], vec![]);
    _ = module.section(&types);

    // functions
    let mut functions = FunctionSection::new();
    for func in &wasm_module.functions {
        _ = functions.function(func.type_index);
    }

    // create start function as well
    let init_function_idx = wasm_module.functions.len() as u32;
    _ = functions.function(init_function_type_idx);
    _ = module.section(&functions);

    // globals
    let mut globals = GlobalSection::new();
    for global in &wasm_module.constants {
        let heap_type = wasm_encoder::HeapType::Concrete(global.type_index);
        _ = globals.global(
            wasm_encoder::GlobalType {
                val_type: wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                    nullable: true, // this is so we can initialize it later
                    heap_type: heap_type.clone(),
                }),
                mutable: true, // this is so we can initialize it later
                shared: false,
            },
            &wasm_encoder::ConstExpr::ref_null(heap_type),
        );
    }
    _ = module.section(&globals);

    // declare a start function
    let start = StartSection {
        function_index: init_function_idx,
    };
    _ = module.section(&start);

    // elems
    let mut elems = ElementSection::new();
    let indices: Vec<_> = (0..(wasm_module.functions.len() as u32)).collect();
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
            .map(|(_, typ)| typ)
            .copied()
            .map(|typ| (1, typ.to_val_type()));
        let mut f = wasm_encoder::Function::new(locals);
        for inst in &func.instructions.lst {
            _ = f.instruction(inst);
        }
        _ = codes.function(&f);
    }
    // for the start function as well
    {
        let mut instructions = vec![];
        for global in wasm_module.constants.iter() {
            for inst in global.initializer.lst.iter() {
                instructions.push(inst.clone());
            }
            instructions.push(wasm_encoder::Instruction::GlobalSet(global.global_index));
        }
        instructions.push(wasm_encoder::Instruction::End);
        let mut f = wasm_encoder::Function::new(vec![]);
        for inst in instructions {
            _ = f.instruction(&inst);
        }
        _ = codes.function(&f);
    }
    _ = module.section(&codes);

    // names
    let mut names = NameSection::new();

    // modules, functions, locals, types

    // functions
    let mut function_names = NameMap::new();
    for func in wasm_module.functions.iter() {
        _ = function_names.append(func.function_index, &func.name);
    }
    _ = function_names.append(init_function_idx, "init@");
    _ = names.functions(&function_names);

    // locals
    let mut local_names = IndirectNameMap::new();
    for func in wasm_module.functions.iter() {
        let mut locals = NameMap::new();
        // first the arguments
        for (i, name) in func
            .argument_names
            .iter()
            .enumerate()
            .filter(|(_, name)| name.is_some())
        {
            _ = locals.append(i as u32, name.as_ref().map(|s| s.as_str()).unwrap());
        }
        for (i, (name, _)) in func.locals.iter().enumerate() {
            _ = locals.append((i + func.argument_names.len()) as u32, name);
        }
        _ = local_names.append(func.function_index, &locals);
    }
    _ = local_names.append(init_function_idx, &NameMap::new());
    _ = names.locals(&local_names);

    // types
    let mut type_names = NameMap::new();
    for type_ in wasm_module.types.iter() {
        _ = type_names.append(type_.id, &type_.name);
    }
    _ = type_names.append(init_function_type_idx, "typ@init");
    _ = names.types(&type_names);

    _ = module.section(&names);

    module.finish()
}
