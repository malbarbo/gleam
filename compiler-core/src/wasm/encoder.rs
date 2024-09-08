use wasm_encoder::CodeSection;

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
                let _ = types.function(parameters, returns);
            }
        }
    }
    let _ = module.section(&types);

    // functions
    let mut functions = FunctionSection::new();
    for func in &wasm_module.functions {
        let _ = functions.function(func.type_index);
    }
    let _ = module.section(&functions);

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
            let _ = f.instruction(inst);
        }
        let _ = codes.function(&f);
    }
    let _ = module.section(&codes);

    module.finish()
}
