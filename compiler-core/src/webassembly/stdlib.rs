// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Compiler builtins used by the standard library: operations that map to a
//! few wasm instructions or that work directly on GC values. See
//! `builtins-wasm/ABI.md` for their contract.

use super::*;

impl Instructions<'_, '_> {
    /// Emits `builtin` over its arguments on the stack when it is a few
    /// instructions that need no locals, so that a call to it is replaced by
    /// them. Returns false, emitting nothing, for any other builtin.
    pub(super) fn builtin_inline(&mut self, builtin: BuiltinFunctionExternal) -> bool {
        use BuiltinFunctionExternal as B;
        let _ = match builtin {
            B::IntBitwiseAnd => self.int_and(),
            B::IntBitwiseOr => self.int_or(),
            B::IntBitwiseExclusiveOr => self.int_xor(),
            B::IntBitwiseNot => self.int_const(&(-1).into()).int_xor(),
            B::IntToFloat => self.int_to_float(),
            B::FloatFloor => self.float_floor(),
            B::FloatCeiling => self.float_ceil(),
            B::FloatSquareRoot => self.float_sqrt(),
            B::FloatTruncate => self.float_to_int_sat(),
            B::I32ToInt | B::UtfCodepointToInt => self.i32_to_int(),
            B::IntToI32 | B::UnsafeIntToUtfCodepoint => self.int_to_i32(),
            B::I64ToInt => self.i64_to_int(),
            B::IntToI64 => self.int_to_i64(),
            B::F32ToFloat => self.f32_to_float(),
            B::FloatToF32 => self.float_to_f32(),
            B::F64ToFloat => self.f64_to_float(),
            B::FloatToF64 => self.float_to_f64(),
            B::StringNumBytes | B::StringByteSize => self.string_utf8_len().i32_to_int(),
            B::BytesNew => self.bytes_new(),
            B::BytesSize => self.array_len().i32_to_int(),
            B::BytesGet => self.bytes_get().i32_to_int(),
            B::BytesSet => self.bytes_set().i32_const(0),
            B::BytesCopy => self.bytes_copy().i32_const(0),
            B::StringBytes => self.string_utf8_bytes(),
            B::StringOffset => self.string_utf8_off().i32_to_int(),
            B::BytesToString => self.string_from_utf8(),
            B::Float64Byte => self
                .i64_shr_u()
                .i32_wrap_i64()
                .i32_const(0xFF)
                .i32_and()
                .i32_to_int(),
            B::Float32Byte => self.i32_shr_u().i32_const(0xFF).i32_and().i32_to_int(),
            B::Float64FromBits => self.i64_or().f64_reinterpret_i64().f64_to_float(),
            B::Float32FromBits => self.f32_reinterpret_i32().f32_to_float(),
            B::CellNew => self.struct_new(self.repr.cell),
            B::CellGet => self.struct_get(self.repr.cell, 0).i64_to_int(),
            B::CellSet => self.struct_set(self.repr.cell, 0).i32_const(0),
            B::StringConcat
            | B::StringGetByte
            | B::IntToUtfCodepoint
            | B::IntRepr
            | B::FloatRepr
            | B::UtfCodepointRepr
            | B::StringRepr
            | B::StringToMemory
            | B::MemoryToString
            | B::ParseInt
            | B::ParseFloat
            | B::IntBitwiseShiftLeft
            | B::IntBitwiseShiftRight
            | B::FloatRound
            | B::StringByteSlice
            | B::StringCodeunitSlice
            | B::StringPopCodeunit
            | B::StringStartsWith
            | B::StringEndsWith
            | B::StringRemovePrefix
            | B::StringRemoveSuffix
            | B::StringLessThan
            | B::StringConcatList
            | B::StringJoin
            | B::StringPopGrapheme => return false,
        };
        true
    }

    /// Converts the argument at `index` of `builtin`, just pushed, to the
    /// type that `builtin_inline` expects: the `Int` positions and sizes of
    /// `Bytes` are `i32`, a float whose bytes are taken is its bits, and the
    /// bits of a float are `i64` or `i32`, and the value of a cell is `i64`.
    /// Only builtins that `builtin_inline` emits convert their arguments.
    pub(super) fn builtin_argument(
        &mut self,
        builtin: BuiltinFunctionExternal,
        index: usize,
    ) -> &mut Self {
        use BuiltinFunctionExternal as B;
        match (builtin, index) {
            // The bits of the float, shifted by the second argument.
            (B::Float64Byte, 0) => self.float_to_f64().f64_reinterpret(),
            (B::Float64Byte, _) => self.int_to_i64(),
            (B::Float32Byte, 0) => self.float_to_f32().f32_reinterpret(),
            // The value of a cell is an `i64`.
            (B::CellNew, 0) | (B::CellSet, 1) => self.int_to_i64(),
            // The high and the low 32 bits of the float, joined.
            (B::Float64FromBits, 0) => self.int_to_i64().i64_const(32).i64_shl(),
            (B::Float64FromBits, _) => self.int_to_i64().i64_const(0xFFFF_FFFF).i64_and(),
            // Sizes and indices of bytes, and the bits of an `f32`.
            (B::BytesNew | B::Float32FromBits, _)
            | (B::BytesGet | B::BytesSet | B::BytesToString, 1..)
            | (B::BytesCopy, 1 | 3..)
            | (B::Float32Byte, 1) => self.int_to_i32(),
            _ => self,
        }
    }

    /// Shifts the `Int` of `x` by the `Int` of `n`, which it changes, by any
    /// amount: a negative shift goes the other way, and shifting by the
    /// width of `Int` or more gives 0 (or -1 for an arithmetic right shift
    /// of a negative number), unlike the wasm instructions that take the
    /// shift modulo the width.
    pub(super) fn int_shift(&mut self, left: bool, x: LocalId, n: LocalId) -> &mut Self {
        let int_vt = self.repr.int.val_type();
        let bits = self.int_bits();
        // `amount` is non negative or `MIN` (whose negation is itself);
        // compared unsigned, `MIN` is out of range like any big shift.
        let shl = |s: &mut Instructions<'_, '_>, amount: LocalId| {
            let _ = s
                .local_get(amount)
                .int_const(&bits.into())
                .int_ge_u()
                .if_else(
                    int_vt,
                    |then_s| {
                        let _ = then_s.int_const(&0.into());
                    },
                    |else_s| {
                        let _ = else_s.local_get(x).local_get(amount).int_shl();
                    },
                );
        };
        let shr = |s: &mut Instructions<'_, '_>, amount: LocalId| {
            let _ = s
                .local_get(amount)
                .int_const(&bits.into())
                .int_ge_u()
                .if_else(
                    int_vt,
                    |then_s| {
                        let _ = then_s
                            .local_get(x)
                            .int_const(&(bits - 1).into())
                            .int_shr_s();
                    },
                    |else_s| {
                        let _ = else_s.local_get(x).local_get(amount).int_shr_s();
                    },
                );
        };
        #[rustfmt::skip]
        let _ = self
            .local_get(n)
            .int_const(&0.into())
            .int_lt()
            .if_else(
                int_vt,
                |then_s| {
                    let _ = then_s
                        .int_const(&0.into())
                        .local_get(n)
                        .int_sub()
                        .local_set(n);
                    if left { shr(then_s, n) } else { shl(then_s, n) }
                },
                |else_s| {
                    if left { shl(else_s, n) } else { shr(else_s, n) }
                },
            );
        self
    }

    /// Shifts the `Int` on the stack by the constant `amount`, like
    /// `int_shift` does for any amount.
    pub(super) fn int_shift_by(&mut self, left: bool, amount: &BigInt) -> &mut Self {
        let (left, amount) = if *amount < BigInt::from(0) {
            (!left, -amount)
        } else {
            (left, amount.clone())
        };
        let bits = self.int_bits();
        match (left, amount < BigInt::from(bits)) {
            (true, true) => self.int_const(&amount).int_shl(),
            (false, true) => self.int_const(&amount).int_shr_s(),
            (true, false) => self.drop().int_const(&0.into()),
            (false, false) => self.int_const(&(bits - 1).into()).int_shr_s(),
        }
    }
}

impl<'a> Generator<'a> {
    /// A builtin that `builtin_inline` emits, as a function to call it as a
    /// value.
    pub(super) fn code_builtin_inline(&mut self, builtin: BuiltinFunctionExternal) -> FunctionId {
        let (params, result) = builtin.type_();
        let params: Vec<_> = params
            .iter()
            .map(|type_| {
                let val_type = self.builtin_val_type(type_);
                self.wasm_local(val_type)
            })
            .collect();
        let result = self.builtin_val_type(&result);
        let mut function = Function::new(self, builtin.name(), &params, &[result]);
        let mut instructions = function.extend_instructions(self);
        for (index, param) in params.into_iter().enumerate() {
            let _ = instructions
                .local_get(param)
                .builtin_argument(builtin, index);
        }
        assert!(instructions.builtin_inline(builtin));
        function.finish(self)
    }

    /// Shifts by any amount, see `Instructions::int_shift`.
    pub(super) fn code_int_shift(&mut self, left: bool) -> FunctionId {
        let int_vt = self.int.val_type();
        // params
        let x = self.wasm_local(int_vt); // Int
        let n = self.wasm_local(int_vt); // Int
        let name = if left {
            "_int_bitwise_shift_left"
        } else {
            "_int_bitwise_shift_right"
        };
        let mut function = Function::new(self, name, &[x, n], &[int_vt]);
        let _ = function
            .extend_instructions(self)
            .int_shift(left, x, n)
            .end();
        function.finish(self)
    }

    /// Rounds half away from zero. `floor(x + 0.5)` would be wrong for
    /// `0.49999999999999994` (the sum rounds up to 1.0) and `nearest` rounds
    /// half to even, so the fraction is compared instead.
    pub(super) fn code_float_round(&mut self) -> FunctionId {
        let int_vt = self.int.val_type();
        let float_vt = self.float.val_type();
        // params
        let x = self.wasm_local(float_vt); // Float
        // locals
        let abs = self.wasm_local(float_vt); // Float
        let t = self.wasm_local(float_vt); // Float
        let mut function = Function::new(self, "_float_round", &[x], &[int_vt]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            // t = trunc(|x|)
            .local_get(x)
            .float_abs()
            .local_tee(abs)
            .float_trunc()
            .local_tee(t)
            // |x| - t >= 0.5 ? t + 1 : t
            .local_get(abs)
            .local_get(t)
            .float_sub()
            .float_const("0.5")
            .float_ge()
            .if_else(
                float_vt,
                |then_s| {
                    let _ = then_s.float_const("1.0");
                },
                |else_s| {
                    let _ = else_s.float_const("0.0");
                },
            )
            .float_add()
            // restore the sign
            .local_get(x)
            .float_copysign()
            .float_to_int_sat()
            .end();
        function.finish(self)
    }
}
