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
            B::StringNumBytes | B::StringByteSize => self.string_len().i32_to_int(),
            B::BytesNew => self.bytes_new(),
            B::BytesSize => self.array_len().i32_to_int(),
            B::BytesGet => self.bytes_get().i32_to_int(),
            B::BytesSet => self.bytes_set().i32_const(0),
            B::BytesCopy => self
                .array_copy(self.repr.string.bytes_index, self.repr.string.bytes_index)
                .i32_const(0),
            B::StringBytes => self.string_bytes(),
            B::StringOffset => self.string_off().i32_to_int(),
            B::BytesToString => self.struct_new(self.repr.string.type_index),
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

    /// `_string_byte_slice(s, start, length)`: the caller guarantees the
    /// bounds. A view over `s` unless the slice is small.
    pub(super) fn code_string_byte_slice(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        let int_vt = self.int.val_type();
        // params
        let s = self.wasm_local(string_vt); // String
        let start = self.wasm_local(int_vt); // Int
        let len = self.wasm_local(int_vt); // Int
        let mut function =
            Function::new(self, "_string_byte_slice", &[s, start, len], &[string_vt]);
        let _ = function
            .extend_instructions(self)
            .local_get(s)
            .local_get(start)
            .int_to_i32()
            .local_get(len)
            .int_to_i32()
            .string_slice()
            .end();
        function.finish(self)
    }

    pub(super) fn code_string_pop_codeunit(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        let tuple = type_::tuple(vec![type_::int(), type_::string()]);
        let tuple_vt = self.val_type(&tuple);
        let tuple_index = self.type_index(&tuple);
        let slice = self.get_function_builtin_external(BuiltinFunctionExternal::StringByteSlice);
        // params
        let s = self.wasm_local(string_vt); // String
        let mut function = Function::new(self, "_string_pop_codeunit", &[s], &[tuple_vt]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .local_get(s)
            .string_len()
            .if_else(
                tuple_vt,
                |then_s| {
                    let _ = then_s
                        .local_get(s)
                        .i32_const(0)
                        .string_get()
                        .i32_to_int()
                        .local_get(s)
                        .int_const(&1.into())
                        .local_get(s)
                        .string_len()
                        .i32_const(1)
                        .i32_sub()
                        .i32_to_int()
                        .call(slice)
                        .struct_new(tuple_index);
                },
                |else_s| {
                    let _ = else_s
                        .int_const(&0.into())
                        .local_get(s)
                        .struct_new(tuple_index);
                },
            )
            .end();
        function.finish(self)
    }

    pub(super) fn code_string_ends_with(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        // params
        let a = self.wasm_local(string_vt); // String
        let b = self.wasm_local(string_vt); // String
        // locals
        let i = self.wasm_local(ValType::I32); // I32
        let offset = self.wasm_local(ValType::I32); // I32
        let mut function = Function::new(self, "_string_ends_with", &[a, b], &[BOOL_VALTYPE]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            // offset = a.len - b.len
            .local_get(a)
            .string_len()
            .local_get(b)
            .string_len()
            .i32_sub()
            .local_tee(offset)
            .i32_const(0)
            .i32_lt_s()
            .if_(InstrSeqType::Simple(None), |body| {
                let _ = body.bool_const(false).return_();
            })
            .loop_(InstrSeqType::Simple(None), |loop_s| {
                let loop_id = loop_s.id();
                let _ = loop_s
                    .local_get(i)
                    .local_get(b)
                    .string_len()
                    .i32_ge_u()
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body.bool_const(true).return_();
                    })
                    // a[offset + i] != b[i]
                    .local_get(a)
                    .local_get(offset)
                    .local_get(i)
                    .i32_add()
                    .string_get()
                    .local_get(b)
                    .local_get(i)
                    .string_get()
                    .i32_ne()
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body.bool_const(false).return_();
                    })
                    .i32_inc(i)
                    .br(loop_id);
            })
            .unreachable()
            .end();
        function.finish(self)
    }

    /// `_string_remove_prefix` and `_string_remove_suffix`: the string without
    /// the affix, or the string itself when it does not match.
    pub(super) fn code_string_remove_affix(&mut self, prefix: bool) -> FunctionId {
        let string_vt = self.string.val_type();
        let matches = if prefix {
            self.function_string_starts_with()
        } else {
            self.get_function_builtin_external(BuiltinFunctionExternal::StringEndsWith)
        };
        let slice = self.get_function_builtin_external(BuiltinFunctionExternal::StringByteSlice);
        // params
        let s = self.wasm_local(string_vt); // String
        let affix = self.wasm_local(string_vt); // String
        let name = if prefix {
            "_string_remove_prefix"
        } else {
            "_string_remove_suffix"
        };
        let mut function = Function::new(self, name, &[s, affix], &[string_vt]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .local_get(s)
            .local_get(affix)
            .call(matches)
            .if_else(
                string_vt,
                |then_s| {
                    let _ = then_s.local_get(s);
                    let _ = if prefix {
                        then_s.local_get(affix).string_len().i32_to_int()
                    } else {
                        then_s.int_const(&0.into())
                    };
                    let _ = then_s
                        .local_get(s)
                        .string_len()
                        .local_get(affix)
                        .string_len()
                        .i32_sub()
                        .i32_to_int()
                        .call(slice);
                },
                |else_s| {
                    let _ = else_s.local_get(s);
                },
            )
            .end();
        function.finish(self)
    }

    /// Byte-wise comparison, like Erlang binaries.
    pub(super) fn code_string_less_than(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        // params
        let a = self.wasm_local(string_vt); // String
        let b = self.wasm_local(string_vt); // String
        // locals
        let i = self.wasm_local(ValType::I32); // I32
        let x = self.wasm_local(ValType::I32); // I32
        let y = self.wasm_local(ValType::I32); // I32
        let mut function = Function::new(self, "_string_less_than", &[a, b], &[BOOL_VALTYPE]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .loop_(InstrSeqType::Simple(None), |loop_s| {
                let loop_id = loop_s.id();
                let _ = loop_s
                    // the shorter string is less when it is a prefix
                    .local_get(i)
                    .local_get(b)
                    .string_len()
                    .i32_ge_u()
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body.bool_const(false).return_();
                    })
                    .local_get(i)
                    .local_get(a)
                    .string_len()
                    .i32_ge_u()
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body.bool_const(true).return_();
                    })
                    .local_get(a)
                    .local_get(i)
                    .string_get()
                    .local_tee(x)
                    .local_get(b)
                    .local_get(i)
                    .string_get()
                    .local_tee(y)
                    .i32_ne()
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body
                            .local_get(x)
                            .local_get(y)
                            .i32_lt_u()
                            .return_();
                    })
                    .i32_inc(i)
                    .br(loop_id);
            })
            .unreachable()
            .end();
        function.finish(self)
    }

    /// `_string_concat_list(strings)` and `_string_join(strings, separator)`:
    /// one pass to size the result and another to copy, so the cost is linear
    /// in the total size instead of quadratic as with repeated `<>`.
    pub(super) fn code_string_join(&mut self, with_separator: bool) -> FunctionId {
        let string_vt = self.string.val_type();
        let list_type = type_::list(type_::string());
        let list_vt = self.val_type(&list_type);
        let (custom_type, cons, _, args) = self.list_type_cons(&list_type);
        let (_, cons_index, _) =
            self.mono_union_subtype_index(&list_type, &custom_type, &cons, &args);
        let (rest_field, first_field) = self.list_cons_fields(&list_type);
        // params
        let strings = self.wasm_local(list_vt); // List(String)
        let separator = self.wasm_local(string_vt); // String
        // locals
        let l = self.wasm_local(list_vt); // List(String)
        let item = self.wasm_local(string_vt); // String
        let total = self.wasm_local(ValType::I32); // I32
        let pos = self.wasm_local(ValType::I32); // I32
        let seen = self.wasm_local(ValType::I32); // Bool
        let r = self.wasm_local(self.string.bytes_val_type());
        let (name, params) = if with_separator {
            ("_string_join", vec![strings, separator])
        } else {
            ("_string_concat_list", vec![strings])
        };
        let mut function = Function::new(self, name, &params, &[string_vt]);
        let mut instructions = function.extend_instructions(self);
        // Iterates `l` from `strings`, with the current element in `item`.
        let for_each = |s: &mut Instructions<'_, '_>, body: &dyn Fn(&mut Instructions<'_, '_>)| {
            let _ =
                s.local_get(strings)
                    .local_set(l)
                    .block_(InstrSeqType::Simple(None), |block_s| {
                        let block_id = block_s.id();
                        let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                            let loop_id = loop_s.id();
                            let _ = loop_s
                                .local_get(l)
                                .ref_is_null()
                                .br_if(block_id)
                                .local_get(l)
                                .variant_field_get(cons_index, first_field)
                                .local_set(item);
                            body(loop_s);
                            let _ = loop_s
                                .local_get(l)
                                .ref_cast_non_null(HeapType::Concrete(cons_index))
                                .struct_get(cons_index, rest_field)
                                .local_set(l)
                                .br(loop_id);
                        });
                    });
        };
        // The separator goes before every element but the first.
        let separator_before =
            |s: &mut Instructions<'_, '_>, body: &dyn Fn(&mut Instructions<'_, '_>)| {
                if with_separator {
                    let _ = s
                        .local_get(seen)
                        .if_(InstrSeqType::Simple(None), |then_s| body(then_s))
                        .i32_const(1)
                        .local_set(seen);
                }
            };
        for_each(&mut instructions, &|s| {
            separator_before(s, &|s| {
                let _ = s
                    .local_get(total)
                    .local_get(separator)
                    .string_len()
                    .i32_add()
                    .local_set(total);
            });
            let _ = s
                .local_get(total)
                .local_get(item)
                .string_len()
                .i32_add()
                .local_set(total);
        });
        let _ = instructions
            .local_get(total)
            .bytes_new()
            .local_set(r)
            .i32_const(0)
            .local_set(seen);
        for_each(&mut instructions, &|s| {
            separator_before(s, &|s| {
                let _ = s
                    .local_get(r)
                    .local_get(pos)
                    .local_get(separator)
                    .i32_const(0)
                    .local_get(separator)
                    .string_len()
                    .bytes_copy_from_string()
                    .local_get(pos)
                    .local_get(separator)
                    .string_len()
                    .i32_add()
                    .local_set(pos);
            });
            let _ = s
                .local_get(r)
                .local_get(pos)
                .local_get(item)
                .i32_const(0)
                .local_get(item)
                .string_len()
                .bytes_copy_from_string()
                .local_get(pos)
                .local_get(item)
                .string_len()
                .i32_add()
                .local_set(pos);
        });
        let _ = instructions.local_get(r).string_from_bytes().end();
        function.finish(self)
    }

    /// `_string_pop_grapheme(s)`: asks Rust for the length of the first
    /// grapheme of a window of the string, doubling the window while the
    /// grapheme could continue past it, so the cost is O(grapheme) and not
    /// O(string). Both parts are slices of `s`.
    pub(super) fn code_string_pop_grapheme(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        let pair = type_::tuple(vec![type_::string(), type_::string()]);
        let pair_index = self.type_index(&pair);
        let result_vt = self.val_type(&type_::result(pair.clone(), type_::nil()));
        let ok_fn = self.ok_variant_constructor(pair.clone(), type_::nil());
        let error_fn = self.error_variant_constructor(pair, type_::nil());
        let lower_string = self.function_lower_string();
        let prefix_len = self.rust_function(GRAPHEME_PREFIX_LEN);
        let arena_mark = self.rust_function(ARENA_MARK);
        let arena_release = self.rust_function(ARENA_RELEASE);
        // params
        let s = self.wasm_local(string_vt);
        // locals
        let len = self.wasm_local(ValType::I32);
        let window = self.wasm_local(ValType::I32);
        let n = self.wasm_local(ValType::I32);
        let mark = self.wasm_local(ValType::I32);
        let mut function = Function::new(self, "_string_pop_grapheme", &[s], &[result_vt]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .local_get(s)
            .string_len()
            .local_tee(len)
            .i32_eqz()
            .if_(InstrSeqType::Simple(None), |then_s| {
                let _ = then_s.nil_const().call(error_fn).return_();
            })
            .i32_const(GRAPHEME_WINDOW)
            .local_set(window)
            .call(arena_mark)
            .local_set(mark)
            .loop_(InstrSeqType::Simple(None), |loop_s| {
                let loop_id = loop_s.id();
                let _ = loop_s
                    // window = min(window, len)
                    .local_get(window)
                    .local_get(len)
                    .i32_ge_u()
                    .if_(InstrSeqType::Simple(None), |then_s| {
                        let _ = then_s.local_get(len).local_set(window);
                    })
                    .local_get(s)
                    .local_get(window)
                    .call(lower_string)
                    .local_get(window)
                    // complete when the window is the whole string
                    .local_get(window)
                    .local_get(len)
                    .i32_eq()
                    .call(prefix_len)
                    .local_set(n)
                    .local_get(mark)
                    .call(arena_release)
                    .local_get(n)
                    .i32_const(0)
                    .i32_lt_s()
                    .if_(InstrSeqType::Simple(None), |then_s| {
                        let _ = then_s
                            .local_get(window)
                            .i32_const(1)
                            .i32_shl()
                            .local_set(window)
                            .br(loop_id);
                    });
            })
            .local_get(s)
            .i32_const(0)
            .local_get(n)
            .string_slice()
            .local_get(s)
            .local_get(n)
            .local_get(len)
            .local_get(n)
            .i32_sub()
            .string_slice()
            .struct_new(pair_index)
            .call(ok_fn)
            .end();
        function.finish(self)
    }
}

const GRAPHEME_PREFIX_LEN: &str = "_grapheme_prefix_len";

/// The first window of `_string_pop_grapheme`, in bytes.
const GRAPHEME_WINDOW: i32 = 64;
