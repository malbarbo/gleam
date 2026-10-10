// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Compiler builtins used by the standard library: operations that map to a
//! few wasm instructions or that work directly on GC values. See
//! `builtins-wasm/ABI.md` for their contract.

use super::*;

#[derive(Clone, Copy)]
pub(super) enum IntBitwise {
    And,
    Or,
    ExclusiveOr,
}

#[derive(Clone, Copy)]
pub(super) enum FloatUnary {
    Floor,
    Ceiling,
    SquareRoot,
}

impl<'a> Generator<'a> {
    pub(super) fn code_int_bitwise_binop(&mut self, operator: IntBitwise) -> FunctionId {
        let int_vt = self.int.val_type();
        // params
        let a = self.wasm_local(int_vt); // Int
        let b = self.wasm_local(int_vt); // Int
        let name = match operator {
            IntBitwise::And => "_int_bitwise_and",
            IntBitwise::Or => "_int_bitwise_or",
            IntBitwise::ExclusiveOr => "_int_bitwise_exclusive_or",
        };
        let mut function = Function::new(self, name, &[a, b], &[int_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions.local_get(a).local_get(b);
        let _ = match operator {
            IntBitwise::And => instructions.int_and(),
            IntBitwise::Or => instructions.int_or(),
            IntBitwise::ExclusiveOr => instructions.int_xor(),
        };
        function.finish(self)
    }

    pub(super) fn code_int_bitwise_not(&mut self) -> FunctionId {
        let int_vt = self.int.val_type();
        // params
        let a = self.wasm_local(int_vt); // Int
        let mut function = Function::new(self, "_int_bitwise_not", &[a], &[int_vt]);
        let _ = function
            .extend_instructions(self)
            .local_get(a)
            .int_const(&(-1).into())
            .int_xor()
            .end();
        function.finish(self)
    }

    /// Shifts by any amount: a negative shift goes the other way, and shifting
    /// by the width of `Int` or more gives 0 (or -1 for an arithmetic right
    /// shift of a negative number), unlike the wasm instructions that take
    /// the shift modulo the width.
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
        let mut instructions = function.extend_instructions(self);
        let bits = instructions.int_bits();
        // `amount` is on the stack, non negative or `MIN` (whose negation is
        // itself); compared unsigned, `MIN` is out of range like any big shift.
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
        let _ = instructions
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
            )
            .end();
        function.finish(self)
    }

    pub(super) fn code_int_to_float(&mut self) -> FunctionId {
        let int_vt = self.int.val_type();
        let float_vt = self.float.val_type();
        // params
        let a = self.wasm_local(int_vt); // Int
        let mut function = Function::new(self, "_int_to_float", &[a], &[float_vt]);
        let _ = function
            .extend_instructions(self)
            .local_get(a)
            .int_to_float()
            .end();
        function.finish(self)
    }

    pub(super) fn code_float_unop(&mut self, operator: FloatUnary) -> FunctionId {
        let float_vt = self.float.val_type();
        // params
        let a = self.wasm_local(float_vt); // Float
        let name = match operator {
            FloatUnary::Floor => "_float_floor",
            FloatUnary::Ceiling => "_float_ceiling",
            FloatUnary::SquareRoot => "_float_square_root",
        };
        let mut function = Function::new(self, name, &[a], &[float_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions.local_get(a);
        let _ = match operator {
            FloatUnary::Floor => instructions.float_floor(),
            FloatUnary::Ceiling => instructions.float_ceil(),
            FloatUnary::SquareRoot => instructions.float_sqrt(),
        };
        function.finish(self)
    }

    pub(super) fn code_float_truncate(&mut self) -> FunctionId {
        let int_vt = self.int.val_type();
        let float_vt = self.float.val_type();
        // params
        let a = self.wasm_local(float_vt); // Float
        let mut function = Function::new(self, "_float_truncate", &[a], &[int_vt]);
        let _ = function
            .extend_instructions(self)
            .local_get(a)
            .float_to_int_sat()
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

    pub(super) fn code_utf_codepoint_to_int(&mut self) -> FunctionId {
        let int_vt = self.int.val_type();
        // params
        let a = self.wasm_local(ValType::I32); // UtfCodepoint
        let mut function = Function::new(self, "_utf_codepoint_to_int", &[a], &[int_vt]);
        let _ = function
            .extend_instructions(self)
            .local_get(a)
            .i32_to_int()
            .end();
        function.finish(self)
    }

    pub(super) fn code_unsafe_int_to_utf_codepoint(&mut self) -> FunctionId {
        let int_vt = self.int.val_type();
        // params
        let a = self.wasm_local(int_vt); // Int
        let mut function =
            Function::new(self, "_unsafe_int_to_utf_codepoint", &[a], &[ValType::I32]);
        let _ = function
            .extend_instructions(self)
            .local_get(a)
            .int_to_i32()
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
                                .ref_cast_non_null(HeapType::Concrete(cons_index))
                                .struct_get(cons_index, 2) // first
                                .local_set(item);
                            body(loop_s);
                            let _ = loop_s
                                .local_get(l)
                                .ref_cast_non_null(HeapType::Concrete(cons_index))
                                .struct_get(cons_index, 1) // rest
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
}
