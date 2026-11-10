// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `int_overflow = "fail"`: an `Int` operation of the program whose result
//! does not fit in the `Int` is a failure at the operation, like a `panic`.
//! Each operation calls a function that checks its result, and that is
//! given the location of the operation for the failure. `float.truncate`
//! and `float.round` fail at their declaration, as they also are values.
//! The modules of the runtime keep the modular operations, as their hashes
//! wrap.

use super::*;

/// An `Int` operation that can overflow.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(super) enum IntOperation {
    Add,
    Sub,
    Mul,
    /// `MIN / -1`, as `x / 0` is 0.
    Div,
}

impl IntOperation {
    fn name(self) -> &'static str {
        match self {
            IntOperation::Add => "_int_add_checked",
            IntOperation::Sub => "_int_sub_checked",
            IntOperation::Mul => "_int_mul_checked",
            IntOperation::Div => "_int_div_checked",
        }
    }
}

impl Generator<'_> {
    /// Returns `true` if the `Int` operations of the module that is being
    /// compiled fail when they overflow, `false` otherwise.
    pub(super) fn checks_int_overflow(&self) -> bool {
        self.int_overflow == WasmIntOverflow::Fail
            && !self
                .runtime
                .iter()
                .any(|module| module.module.name == self.current_module_name)
    }

    /// `[Int Int] -> [Int]`: `operation` of the two `Int`s, which fails at
    /// `start` when the result does not fit.
    pub(super) fn checked_int_operation(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        operation: IntOperation,
        start: u32,
    ) {
        self.push_failure_location(instructions, start);
        let function = self.function_checked_int(operation);
        let _ = instructions.call(function);
    }

    /// The function of `builtin`, which is `float.truncate` or
    /// `float.round`, that `function` of `module` declares. It fails at the
    /// declaration when the result does not fit in the `Int`. NaN is 0, as
    /// in the conversion without the check.
    pub(super) fn function_checked_float_to_int(
        &mut self,
        builtin: BuiltinFunctionExternal,
        function: &TypedFunction,
        module: &EcoString,
    ) -> Id {
        let name = ecow::eco_format!("{}_checked@{module}", builtin.name());
        if let Some(id) = self.find_global(&name) {
            return id;
        }
        let int_vt = self.int.val_type();
        let float_vt = self.float.val_type();
        let x = self.wasm_local(float_vt);
        let result = self.wasm_local(float_vt);
        let mut code = Function::new(self, &name, &[x], &[int_vt]);
        let mut instructions = code.extend_instructions(self);
        if builtin == BuiltinFunctionExternal::FloatRound {
            self.float_rounded(&mut instructions, x);
        } else {
            let _ = instructions.local_get(x).float_trunc();
        }
        // The limits are powers of 2, which a float has exactly.
        let limit = BigInt::from(1) << (instructions.int_bits() - 1);
        let _ = instructions
            .local_tee(result)
            .float_const(&format!("{limit}.0"))
            .float_ge()
            .local_get(result)
            .float_const(&format!("-{limit}.0"))
            .float_lt()
            .i32_or()
            .if_(InstrSeqType::Simple(None), |then_| {
                self.int_overflow_failure_at(then_, function, module);
            });
        let _ = instructions.local_get(result).float_to_int_sat().end();
        let index = code.finish(self);
        self.add_function_to_globals(name, index)
    }

    /// `name(a, b, file, module, function, line)`.
    fn function_checked_int(&mut self, operation: IntOperation) -> FunctionId {
        if let Some(function) = self.checked_int.get(&operation) {
            return *function;
        }
        let int_vt = self.int.val_type();
        let string_vt = self.val_type(&type_::string());
        let a = self.wasm_local(int_vt);
        let b = self.wasm_local(int_vt);
        let location: Vec<_> = (0..4).map(|_| self.wasm_local(string_vt)).collect();
        let result = self.wasm_local(int_vt);
        let mut params = vec![a, b];
        params.extend(&location);
        let mut function = Function::new(self, operation.name(), &params, &[int_vt]);
        let mut instructions = function.extend_instructions(self);
        let min = -(BigInt::from(1) << (instructions.int_bits() - 1));
        let fail = |generator: &mut Generator<'_>, instructions: &mut Instructions<'_, '_>| {
            let _ = instructions.if_(InstrSeqType::Simple(None), |then_| {
                generator.int_overflow_failure(then_, &location);
            });
        };
        match operation {
            IntOperation::Add | IntOperation::Sub => {
                // The sum overflows when the result has not the sign of
                // either operand, `((a ^ r) & (b ^ r)) < 0`, and the
                // difference when the operands have different signs and the
                // result has not the sign of `a`, `((a ^ b) & (a ^ r)) < 0`.
                let (sign, signs) = if operation == IntOperation::Add {
                    let _ = instructions.local_get(a).local_get(b).int_add();
                    ([a, result], [b, result])
                } else {
                    let _ = instructions.local_get(a).local_get(b).int_sub();
                    ([a, b], [a, result])
                };
                let _ = instructions
                    .local_set(result)
                    .local_get(sign[0])
                    .local_get(sign[1])
                    .int_xor()
                    .local_get(signs[0])
                    .local_get(signs[1])
                    .int_xor()
                    .int_and()
                    .int_const(&0.into())
                    .int_lt();
                fail(self, &mut instructions);
            }
            IntOperation::Mul => match self.int {
                IntType::I32 => {
                    let product = self.wasm_local(ValType::I64);
                    let _ = instructions
                        .local_get(a)
                        .builder()
                        .unop(UnaryOp::I64ExtendSI32)
                        .local_get(b)
                        .unop(UnaryOp::I64ExtendSI32)
                        .binop(BinaryOp::I64Mul)
                        .local_tee(product)
                        .unop(UnaryOp::I32WrapI64)
                        .local_tee(result)
                        .unop(UnaryOp::I64ExtendSI32)
                        .local_get(product)
                        .binop(BinaryOp::I64Ne);
                    fail(self, &mut instructions);
                }
                IntType::I64 => {
                    let _ = instructions
                        .local_get(a)
                        .local_get(b)
                        .int_mul()
                        .local_set(result)
                        // `MIN / -1` traps, so `-1 * MIN` is checked first.
                        .local_get(a)
                        .int_const(&(-1).into())
                        .int_eq()
                        .local_get(b)
                        .int_const(&min)
                        .int_eq()
                        .i32_and();
                    fail(self, &mut instructions);
                    // Otherwise the product overflows when dividing it by
                    // `a` does not give `b` back.
                    let _ = instructions.local_get(a).int_eqz().if_(
                        InstrSeqType::Simple(None),
                        |then_| {
                            let _ = then_.local_get(result).return_();
                        },
                    );
                    let _ = instructions
                        .local_get(result)
                        .local_get(a)
                        .int_div_s()
                        .local_get(b)
                        .int_ne();
                    fail(self, &mut instructions);
                }
            },
            IntOperation::Div => {
                let _ = instructions
                    .local_get(a)
                    .int_const(&min)
                    .int_eq()
                    .local_get(b)
                    .int_const(&(-1).into())
                    .int_eq()
                    .i32_and();
                fail(self, &mut instructions);
                let _ = instructions.local_get(a).local_get(b).int_div(a, b);
                let _ = instructions.local_set(result);
            }
        }
        let _ = instructions.local_get(result).end();
        let function = function.finish(self);
        let _ = self.checked_int.insert(operation, function);
        function
    }
}
