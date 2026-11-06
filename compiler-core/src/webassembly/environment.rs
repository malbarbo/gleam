// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! The functions of the environment, declared as
//! `@external(webassembly, "env", "name")` with a module of a plain name,
//! which whoever instantiates the program provides, like WASI. They receive
//! and return wasm values: an `Int`, a `Float`, a `Bool` as an `i32`, and the
//! values of `I32`, `I64`, `F32` and `F64` as they are. A `Nil` result is no
//! result. The analyser rejects any other type, see `is_environment_param`.

use super::*;

impl<'a> Generator<'a> {
    /// The function `name` of the module `module` of the environment,
    /// declared by `function`. It is imported once, with the wasm types of
    /// the Gleam types of `function`, and called through a function that
    /// makes the result a Gleam value when the result is a `Bool` or `Nil`.
    pub(super) fn function_environment(
        &mut self,
        module: &EcoString,
        name: &EcoString,
        function: &TypedFunction,
    ) -> Id {
        let global_name = ecow::eco_format!("{module}.{name}");
        if let Some(id) = self.find_global(&global_name) {
            return id;
        }
        self.imports_environment = true;
        let (params, return_) = function_type(function)
            .fn_types()
            .expect("an external function");
        let param_types: Vec<_> = params.iter().map(|param| self.val_type(param)).collect();
        let results = if return_.is_nil() {
            vec![]
        } else {
            vec![self.val_type(&return_)]
        };
        let import = self.host_import(module, name, &param_types, &results);
        let index = if return_.is_bool() || return_.is_nil() {
            self.code_environment_call(&global_name, import, &param_types, &return_)
        } else {
            import
        };
        self.add_function_to_globals(global_name, index)
    }

    /// Calls `import` with the arguments, and makes its result a `Bool` or
    /// `Nil`: a `Bool` is any `i32`, where 0 is `False`, and `Nil` is no
    /// result.
    fn code_environment_call(
        &mut self,
        name: &EcoString,
        import: FunctionId,
        param_types: &[ValType],
        return_: &Arc<Type>,
    ) -> FunctionId {
        let return_type = self.val_type(return_);
        let params: Vec<_> = param_types.iter().map(|vt| self.wasm_local(*vt)).collect();
        let mut function = Function::new(self, name, &params, &[return_type]);
        let mut instructions = function.extend_instructions(self);
        for param in &params {
            let _ = instructions.local_get(*param);
        }
        let _ = instructions.call(import);
        let _ = if return_.is_nil() {
            instructions.nil_const()
        } else {
            instructions.i32_const(0).i32_ne()
        };
        let _ = instructions.end();
        function.finish(self)
    }
}
