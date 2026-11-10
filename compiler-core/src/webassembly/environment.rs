// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! The functions of the environment, declared as
//! `@external(webassembly, "env", "name")` with a module of a plain name,
//! which whoever instantiates the program provides, like WASI. They receive
//! and return wasm values: an `Int`, a `Float`, a `Bool` as an `i32`, and the
//! values of `I32`, `I64`, `F32` and `F64` as they are. A `String` param is
//! the address and the size of a copy of its UTF-8 bytes in the arena, which
//! the function only borrows during the call. A `Nil` result is no result.
//! The analyser rejects any other type, see `is_environment_param`.

use super::*;
use glue::{ARENA_MARK, ARENA_RELEASE};

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
        // A function of a Gleam module is `module.name`, see
        // `imported_base_name`.
        let global_name = ecow::eco_format!("{module}#{name}");
        if let Some(id) = self.find_global(&global_name) {
            return id;
        }
        self.imports_environment = true;
        let (params, return_) = function_type(function)
            .fn_types()
            .expect("an external function");
        let import_params: Vec<_> = params
            .iter()
            .flat_map(|param| {
                if param.is_string() {
                    vec![ValType::I32, ValType::I32]
                } else {
                    vec![self.val_type(param)]
                }
            })
            .collect();
        let results = if return_.is_nil() {
            vec![]
        } else {
            vec![self.val_type(&return_)]
        };
        let import = self.host_import(module, name, &import_params, &results);
        let index = if return_.is_bool() || return_.is_nil() || params.iter().any(|p| p.is_string())
        {
            self.code_environment_call(&global_name, import, &params, &return_)
        } else {
            import
        };
        self.add_function_to_globals(global_name, index)
    }

    /// Calls `import` with the arguments, a `String` copied to the arena,
    /// which is released after the call, and makes its result a `Bool` or
    /// `Nil`: a `Bool` is any `i32`, where 0 is `False`, and `Nil` is no
    /// result.
    fn code_environment_call(
        &mut self,
        name: &EcoString,
        import: FunctionId,
        param_types: &[Arc<Type>],
        return_: &Arc<Type>,
    ) -> FunctionId {
        let return_type = self.val_type(return_);
        let params: Vec<_> = param_types
            .iter()
            .map(|type_| {
                let val_type = self.val_type(type_);
                self.wasm_local(val_type)
            })
            .collect();
        let copies = param_types.iter().any(|type_| type_.is_string());
        let (mark, release, lower_string) = if copies {
            let mark = self.rust_function(ARENA_MARK);
            let release = self.rust_function(ARENA_RELEASE);
            (
                Some(mark),
                Some(release),
                Some(self.function_lower_string()),
            )
        } else {
            (None, None, None)
        };
        let arena = self.wasm_local(ValType::I32);
        let result = (!return_.is_nil()).then(|| self.wasm_local(return_type));
        let mut function = Function::new(self, name, &params, &[return_type]);
        let mut instructions = function.extend_instructions(self);
        if let Some(mark) = mark {
            let _ = instructions.call(mark).local_set(arena);
        }
        for (param, type_) in params.iter().zip(param_types) {
            let _ = instructions.local_get(*param);
            if type_.is_string() {
                let _ = instructions.call(lower_string.expect("the copy of a string"));
            }
        }
        let _ = instructions.call(import);
        if let Some(result) = result {
            let _ = instructions.local_set(result);
        }
        if let Some(release) = release {
            let _ = instructions.local_get(arena).call(release);
        }
        let _ = match result {
            None => instructions.nil_const(),
            Some(result) if return_.is_bool() => {
                instructions.local_get(result).i32_const(0).i32_ne()
            }
            Some(result) => instructions.local_get(result),
        };
        let _ = instructions.end();
        function.finish(self)
    }
}
