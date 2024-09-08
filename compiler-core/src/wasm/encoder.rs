use wasm_encoder::CodeSection;

use wasm_encoder::FunctionSection;

use super::WasmPrimitive;

use wasm_encoder::TypeSection;

use super::WasmModule;

pub(crate) fn emit(wasm_module: WasmModule) -> Vec<u8> {
    let mut module = wasm_encoder::Module::new();

    // types
    let mut types = TypeSection::new();
    let mut type_idx = 0;

    let function_offset = type_idx;
    // function types
    for func in wasm_module.functions.iter() {
        let _ = types.function(
            func.parameters
                .iter()
                .copied()
                .map(WasmPrimitive::to_val_type),
            std::iter::once(func.returns.to_val_type()),
        );
        type_idx += 1;
    }
    let _ = module.section(&types);

    // functions
    let mut functions = FunctionSection::new();
    for i in 0..wasm_module.functions.len() {
        let _ = functions.function((function_offset as usize + i) as _);
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
