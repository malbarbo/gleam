// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! The global `Dynamic` of a program, which `_global_get` reads and
//! `_global_set` writes. The standard library keeps its global state in it.

use super::*;
use dynamic::DYNAMIC_MODULE;

impl Generator<'_> {
    /// `_global_get()`, the value of the global, or `Nil` before the first
    /// `_global_set`.
    pub(super) fn code_global_get(&mut self) -> FunctionId {
        let global = self.state_global();
        let dynamic_nil = self.dynamic_nil();
        let dynamic = self.runtime_named_type(DYNAMIC_MODULE, "Dynamic", vec![]);
        let dynamic_vt = self.val_type(&dynamic);
        let mut function = Function::new(self, "_global_get", &[], &[dynamic_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions
            .global_get(global)
            .ref_is_null()
            .if_else(
                dynamic_vt,
                |then_| {
                    let _ = then_.call(dynamic_nil);
                },
                |else_| {
                    let _ = else_.global_as_non_null(global);
                },
            )
            .end();
        function.finish(self)
    }

    /// `_global_set(value)`.
    pub(super) fn code_global_set(&mut self) -> FunctionId {
        let global = self.state_global();
        let dynamic = self.runtime_named_type(DYNAMIC_MODULE, "Dynamic", vec![]);
        let dynamic_vt = self.val_type(&dynamic);
        let value = self.wasm_local(dynamic_vt);
        let nil_vt = self.val_type(&type_::nil());
        let mut function = Function::new(self, "_global_set", &[value], &[nil_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions
            .local_get(value)
            .global_set(global)
            .i32_const(0)
            .end();
        function.finish(self)
    }

    fn state_global(&mut self) -> GlobalId {
        if let Some(global) = self.state_global {
            return global;
        }
        let dynamic = self.runtime_named_type(DYNAMIC_MODULE, "Dynamic", vec![]);
        let type_index = self.type_index(&dynamic);
        let global = self.wasm_module.globals.add_local(
            self.val_type_ref_nullable(type_index),
            true,
            false,
            const_expr_ref_null(type_index),
        );
        self.wasm_module.globals.get_mut(global).name = Some("_global".into());
        self.state_global = Some(global);
        global
    }
}
