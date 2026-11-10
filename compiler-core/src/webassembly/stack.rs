// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `stack_overflow = "fail"`: a call that goes past `stack_limit` calls in
//! depth is a failure, like a `panic`, before the stack of the engine ends
//! in a trap. Each function of the program adds 1 to the depth when it
//! starts, and takes 1 away when it returns and before a tail call, so a
//! tail call does not add to the depth, as it does not add to the stack.
//! The functions of the runtime do not count, so a failure at the limit
//! does not go past it again while `_panic` makes it. `_catch` gives the
//! depth back to what it was when it started, when it catches a failure.
//!
//! The limit is the exported mutable global `_stack_limit`, which the host
//! can change before it runs the program, as the stack of the engine
//! depends on the engine and the host. The limit counts calls and the
//! engine counts bytes, so a function with many locals can still end the
//! stack of the engine before the limit.

use super::*;

/// The global that the host can change, see `stack`.
const STACK_LIMIT: &str = "_stack_limit";

impl Generator<'_> {
    /// Returns `true` if the functions of the module that is being compiled
    /// count in the depth of the calls, `false` otherwise.
    pub(super) fn counts_stack_depth(&self) -> bool {
        self.stack_overflow == WasmStackOverflow::Fail
            && !self
                .runtime
                .iter()
                .any(|module| module.module.name == self.current_module_name)
    }

    /// The global of the depth of the calls, and the exported global of its
    /// limit.
    pub(super) fn stack_depth(&mut self) -> (GlobalId, GlobalId) {
        if let Some(globals) = self.stack_depth {
            return globals;
        }
        let depth = self.wasm_module.globals.add_local(
            ValType::I32,
            true,
            false,
            ConstExpr::Value(Value::I32(0)),
        );
        self.wasm_module.globals.get_mut(depth).name = Some("_stack_depth".into());
        let limit = self.wasm_module.globals.add_local(
            ValType::I32,
            true,
            false,
            // The depth is an `i32`, which a larger limit does not fit.
            ConstExpr::Value(Value::I32(
                i32::try_from(self.stack_limit).unwrap_or(i32::MAX),
            )),
        );
        self.wasm_module.globals.get_mut(limit).name = Some(STACK_LIMIT.into());
        let _ = self.wasm_module.exports.add(STACK_LIMIT, limit);
        self.stack_depth = Some((depth, limit));
        (depth, limit)
    }

    /// Adds 1 to the depth of the calls when a function starts at `start`,
    /// and fails there if the depth goes past the limit.
    pub(super) fn enter_stack_depth(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        locals: &Locals,
        scope: Scope,
        start: u32,
    ) {
        let (depth, limit) = self.stack_depth();
        let _ = instructions
            .global_get(depth)
            .i32_const(1)
            .i32_add()
            .global_set(depth)
            .global_get(depth)
            .global_get(limit)
            .i32_gt_s()
            .if_(InstrSeqType::Simple(None), |then_| {
                self.panic(then_, locals, scope, Failure::StackOverflow, start, None);
            });
    }
}
