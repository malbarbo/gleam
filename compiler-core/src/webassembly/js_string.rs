// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `String` as a JavaScript string, see `JsString`.

use super::*;

/// The module of the builtins of the JS String Builtins proposal.
const BUILTINS_MODULE: &str = "wasm:js-string";

/// The module of the string literals, set by `importedStringConstants` in
/// `wasm_host.mjs`: an import of this module is the string of its name.
const CONSTANTS_MODULE: &str = "'";

pub(super) const EXTERNREF: ValType = ValType::Ref(RefType {
    nullable: true,
    heap_type: HeapType::Abstract(walrus::AbstractHeapType::Extern),
});

const EXTERN: ValType = ValType::Ref(RefType {
    nullable: false,
    heap_type: HeapType::Abstract(walrus::AbstractHeapType::Extern),
});

/// `String` is a JavaScript string in a `(ref extern)`, operated with the
/// builtins of `wasm:js-string`, so it crosses to the host without a copy and
/// `<>` uses the ropes of the engine. The literals are imported, and the
/// UTF-8 bytes of a string are a conversion. The program runs only on a
/// JavaScript host.
#[derive(Clone, Copy)]
pub(super) struct JsString {
    /// `(s) -> i32`: the number of UTF-16 code units of `s`.
    pub(super) length: FunctionId,
    /// `(s, i) -> i32`: the code unit `i`.
    pub(super) char_code_at: FunctionId,
    /// `(s, i) -> i32`: the code point at the unit `i`, or the unit itself
    /// when it is a lone surrogate.
    pub(super) code_point_at: FunctionId,
    /// `(a, b) -> String`.
    pub(super) concat: FunctionId,
    /// `(s, start, end) -> String`: the units from `start` to `end`.
    pub(super) substring: FunctionId,
    /// `(s, start, len) -> String`: the `len` units from `start`, as
    /// `string_slice`.
    pub(super) slice: FunctionId,
    /// `(a, b) -> i32`.
    pub(super) equals: FunctionId,
    /// `(s) -> i32`: the number of UTF-8 bytes of `s`.
    pub(super) utf8_len: FunctionId,
    /// `(s) -> bytes`: the UTF-8 bytes of `s`.
    pub(super) to_utf8: FunctionId,
    /// `(bytes, off, len) -> String`: the string of the UTF-8 bytes.
    pub(super) from_utf8: FunctionId,
    /// `(bytes) -> String`: the string of all the UTF-8 bytes.
    pub(super) from_bytes: FunctionId,
}

impl JsString {
    /// The builtins are imported and the conversions created here, as the
    /// unused ones are removed with the rest of the dead code.
    pub(super) fn new(module: &mut walrus::Module, bytes: BytesType) -> JsString {
        // The builtins take an array of code units that is final and alone
        // in its rec group, so it is created before the types of the program
        // (see `rec_group_from`).
        let units = module.types.add_array(FieldType {
            element_type: StorageType::I16,
            mutable: true,
        });
        module.types.get_mut(units).name = Some("Units".into());
        let units_vt = ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Concrete(units),
        });
        let i32 = ValType::I32;
        let length = import(module, "length", &[EXTERNREF], &[i32]);
        let char_code_at = import(module, "charCodeAt", &[EXTERNREF, i32], &[i32]);
        let code_point_at = import(module, "codePointAt", &[EXTERNREF, i32], &[i32]);
        let concat = import(module, "concat", &[EXTERNREF, EXTERNREF], &[EXTERN]);
        let substring = import(module, "substring", &[EXTERNREF, i32, i32], &[EXTERN]);
        let equals = import(module, "equals", &[EXTERNREF, EXTERNREF], &[i32]);
        let from_char_code_array = import(
            module,
            "fromCharCodeArray",
            &[units_vt, i32, i32],
            &[EXTERN],
        );
        let into_char_code_array = import(
            module,
            "intoCharCodeArray",
            &[EXTERNREF, units_vt, i32],
            &[i32],
        );
        let slice = {
            let s = module.locals.add(EXTERN);
            let start = module.locals.add(i32);
            let len = module.locals.add(i32);
            let mut fb = FunctionBuilder::new(&mut module.types, &[EXTERN, i32, i32], &[EXTERN]);
            let _ = fb.name("_string_slice".into());
            let _ = fb
                .func_body()
                .local_get(s)
                .local_get(start)
                .local_get(start)
                .local_get(len)
                .binop(BinaryOp::I32Add)
                .call(substring);
            fb.finish(vec![s, start, len], &mut module.funcs)
        };
        let utf8_len = code_utf8_len(module, length, char_code_at);
        let to_utf8 = code_to_utf8(module, bytes, units, length, into_char_code_array);
        let from_utf8 = code_from_utf8(module, bytes, units, from_char_code_array);
        let from_bytes = {
            let bytes_vt = bytes.val_type();
            let b = module.locals.add(bytes_vt);
            let mut fb = FunctionBuilder::new(&mut module.types, &[bytes_vt], &[EXTERN]);
            let _ = fb.name("_string_from_bytes".into());
            let _ = fb
                .func_body()
                .local_get(b)
                .i32_const(0)
                .local_get(b)
                .array_len()
                .call(from_utf8);
            fb.finish(vec![b], &mut module.funcs)
        };
        JsString {
            length,
            char_code_at,
            code_point_at,
            concat,
            substring,
            slice,
            equals,
            utf8_len,
            to_utf8,
            from_utf8,
            from_bytes,
        }
    }
}

fn import(
    module: &mut walrus::Module,
    name: &str,
    params: &[ValType],
    results: &[ValType],
) -> FunctionId {
    let type_ = module.types.add(params, results);
    module.add_import_func(BUILTINS_MODULE, name, type_).0
}

/// Pushes the number of UTF-8 bytes of the code point in `c`, 3 for a lone
/// surrogate, which becomes U+FFFD.
fn utf8_width(seq: &mut InstrSeqBuilder<'_>, c: LocalId) {
    let _ = seq.i32_const(1);
    for limit in [0x80, 0x800, 0x10000] {
        let _ = seq
            .local_get(c)
            .i32_const(limit)
            .binop(BinaryOp::I32GeU)
            .binop(BinaryOp::I32Add);
    }
}

/// Advances `i` over the units below 0x80 that `unit` pushes for the index
/// in `i`, before `n`, and calls `each` after setting `c` to each of them.
fn ascii_prefix(
    seq: &mut InstrSeqBuilder<'_>,
    n: LocalId,
    i: LocalId,
    c: LocalId,
    unit: &impl Fn(&mut InstrSeqBuilder<'_>),
    each: impl Fn(&mut InstrSeqBuilder<'_>),
) {
    let _ = seq.block(InstrSeqType::Simple(None), |block_s| {
        let block_id = block_s.id();
        let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
            let loop_id = loop_s.id();
            let _ = loop_s
                .local_get(i)
                .local_get(n)
                .binop(BinaryOp::I32GeU)
                .br_if(block_id);
            unit(loop_s);
            let _ = loop_s
                .local_tee(c)
                .i32_const(0x80)
                .binop(BinaryOp::I32GeU)
                .br_if(block_id);
            each(loop_s);
            let _ = loop_s
                .local_get(i)
                .i32_const(1)
                .binop(BinaryOp::I32Add)
                .local_set(i)
                .br(loop_id);
        });
    });
}

/// Sets `total` to the number of UTF-8 bytes of the `n` units that `unit`
/// pushes for the index in `i`, from 0: 1 to 3 for each unit, but 4 for a
/// high surrogate followed by a low one, as `_string_to_utf8` encodes them.
/// The ASCII prefix is counted first, in a simpler loop.
fn utf8_count(
    seq: &mut InstrSeqBuilder<'_>,
    locals: &mut walrus::ModuleLocals,
    n: LocalId,
    i: LocalId,
    total: LocalId,
    unit: impl Fn(&mut InstrSeqBuilder<'_>),
) {
    let c = locals.add(ValType::I32);
    let prev = locals.add(ValType::I32);
    ascii_prefix(seq, n, i, c, &unit, |_| {});
    let _ = seq.local_get(i).local_set(total);
    let _ = seq.block(InstrSeqType::Simple(None), |block_s| {
        let block_id = block_s.id();
        let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
            let loop_id = loop_s.id();
            let _ = loop_s
                .local_get(i)
                .local_get(n)
                .binop(BinaryOp::I32GeU)
                .br_if(block_id);
            unit(loop_s);
            let _ = loop_s.local_set(c).local_get(total);
            utf8_width(loop_s, c);
            #[rustfmt::skip]
            let _ = loop_s
                .binop(BinaryOp::I32Add)
                // a low surrogate after a high one: 4 bytes for both
                .local_get(c)
                .i32_const(0xFC00)
                .binop(BinaryOp::I32And)
                .i32_const(0xDC00)
                .binop(BinaryOp::I32Eq)
                .local_get(prev)
                .i32_const(0xFC00)
                .binop(BinaryOp::I32And)
                .i32_const(0xD800)
                .binop(BinaryOp::I32Eq)
                .binop(BinaryOp::I32And)
                .i32_const(1)
                .binop(BinaryOp::I32Shl)
                .binop(BinaryOp::I32Sub)
                .local_set(total)
                .local_get(c)
                .local_set(prev)
                .local_get(i)
                .i32_const(1)
                .binop(BinaryOp::I32Add)
                .local_set(i)
                .br(loop_id);
        });
    });
}

/// `_string_utf8_len(s)`, without converting `s`.
fn code_utf8_len(
    module: &mut walrus::Module,
    length: FunctionId,
    char_code_at: FunctionId,
) -> FunctionId {
    let s = module.locals.add(EXTERN);
    let n = module.locals.add(ValType::I32);
    let i = module.locals.add(ValType::I32);
    let total = module.locals.add(ValType::I32);
    let mut fb = FunctionBuilder::new(&mut module.types, &[EXTERN], &[ValType::I32]);
    let _ = fb.name("_string_utf8_len".into());
    let mut body = fb.func_body();
    let _ = body.local_get(s).call(length).local_set(n);
    utf8_count(&mut body, &mut module.locals, n, i, total, |seq| {
        let _ = seq.local_get(s).local_get(i).call(char_code_at);
    });
    let _ = body.local_get(total);
    fb.finish(vec![s], &mut module.funcs)
}

/// Pushes `[] -> [x]`: whether the unit or code point `c` is a surrogate.
fn is_surrogate(seq: &mut InstrSeqBuilder<'_>, c: LocalId) {
    let _ = seq
        .local_get(c)
        .i32_const(0xD800)
        .binop(BinaryOp::I32Sub)
        .i32_const(0x800)
        .binop(BinaryOp::I32LtU);
}

/// `_string_to_utf8(s)`: the units of `s` are copied to an array, counted
/// (with `utf8_count`) and encoded from there. A lone surrogate becomes U+FFFD, as with
/// `TextEncoder`.
fn code_to_utf8(
    module: &mut walrus::Module,
    bytes: BytesType,
    units: TypeId,
    length: FunctionId,
    into_char_code_array: FunctionId,
) -> FunctionId {
    let bytes_vt = bytes.val_type();
    let s = module.locals.add(EXTERN);
    let n = module.locals.add(ValType::I32);
    let a = module.locals.add(ValType::Ref(RefType {
        nullable: false,
        heap_type: HeapType::Concrete(units),
    }));
    let out = module.locals.add(bytes_vt);
    let i = module.locals.add(ValType::I32);
    let j = module.locals.add(ValType::I32);
    let c = module.locals.add(ValType::I32);
    let d = module.locals.add(ValType::I32);
    let total = module.locals.add(ValType::I32);
    let mut fb = FunctionBuilder::new(&mut module.types, &[EXTERN], &[bytes_vt]);
    let _ = fb.name("_string_to_utf8".into());
    let mut body = fb.func_body();
    #[rustfmt::skip]
    let _ = body
        .local_get(s)
        .call(length)
        .local_tee(n)
        .array_new_default(units)
        .local_set(a)
        .local_get(s)
        .local_get(a)
        .i32_const(0)
        .call(into_char_code_array)
        .drop();
    let unit = |seq: &mut InstrSeqBuilder<'_>| {
        let _ = seq.local_get(a).local_get(i).array_get_u(units);
    };
    utf8_count(&mut body, &mut module.locals, n, i, total, unit);
    #[rustfmt::skip]
    let _ = body
        .local_get(total)
        .array_new_default(bytes.index)
        .local_set(out)
        .i32_const(0)
        .local_set(i);
    // the ASCII prefix: the bytes are the units
    ascii_prefix(&mut body, n, i, c, &unit, |seq| {
        let _ = seq
            .local_get(out)
            .local_get(i)
            .local_get(c)
            .array_set(bytes.index);
    });
    #[rustfmt::skip]
    let _ = body
        .local_get(i)
        .local_set(j)
        .block(InstrSeqType::Simple(None), |block_s| {
            let block_id = block_s.id();
            let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                let loop_id = loop_s.id();
                let _ = loop_s
                    .local_get(i)
                    .local_get(n)
                    .binop(BinaryOp::I32GeU)
                    .br_if(block_id)
                    // c = a[i++]
                    .local_get(a)
                    .local_get(i)
                    .array_get_u(units)
                    .local_set(c)
                    .local_get(i)
                    .i32_const(1)
                    .binop(BinaryOp::I32Add)
                    .local_set(i);
                // a surrogate: with the next unit when it is a pair, and
                // otherwise U+FFFD
                is_surrogate(loop_s, c);
                let _ = loop_s.if_else(
                    InstrSeqType::Simple(None),
                    |then_s| {
                        #[rustfmt::skip]
                        let _ = then_s
                            .local_get(c)
                            .i32_const(0xDC00)
                            .binop(BinaryOp::I32LtU)
                            .local_get(i)
                            .local_get(n)
                            .binop(BinaryOp::I32LtU)
                            .binop(BinaryOp::I32And)
                            .if_else(
                                InstrSeqType::Simple(Some(ValType::I32)),
                                |high_s| {
                                    let _ = high_s
                                        .local_get(a)
                                        .local_get(i)
                                        .array_get_u(units)
                                        .local_tee(d)
                                        .i32_const(0xFC00)
                                        .binop(BinaryOp::I32And)
                                        .i32_const(0xDC00)
                                        .binop(BinaryOp::I32Eq);
                                },
                                |else_s| {
                                    let _ = else_s.i32_const(0);
                                },
                            )
                            .if_else(
                                InstrSeqType::Simple(None),
                                |pair_s| {
                                    // c = 0x10000 + (c - 0xD800) << 10 + (d - 0xDC00)
                                    let _ = pair_s
                                        .local_get(c)
                                        .i32_const(0xD800)
                                        .binop(BinaryOp::I32Sub)
                                        .i32_const(10)
                                        .binop(BinaryOp::I32Shl)
                                        .local_get(d)
                                        .i32_const(0xDC00 - 0x10000)
                                        .binop(BinaryOp::I32Sub)
                                        .binop(BinaryOp::I32Add)
                                        .local_set(c)
                                        .local_get(i)
                                        .i32_const(1)
                                        .binop(BinaryOp::I32Add)
                                        .local_set(i);
                                },
                                |lone_s| {
                                    let _ = lone_s.i32_const(0xFFFD).local_set(c);
                                },
                            );
                    },
                    |_| {},
                );
                put_utf8(loop_s, bytes.index, out, j, c, 1);
                let _ = loop_s.br(loop_id);
            });
        })
        .local_get(out);
    fb.finish(vec![s], &mut module.funcs)
}

/// Writes the UTF-8 bytes of the code point `c` to `out` at `j`, advancing
/// `j`, when it takes `width` bytes or more.
fn put_utf8(
    seq: &mut InstrSeqBuilder<'_>,
    bytes: TypeId,
    out: LocalId,
    j: LocalId,
    c: LocalId,
    width: i32,
) {
    let put = |seq: &mut InstrSeqBuilder<'_>| {
        for k in 0..width {
            let shift = 6 * (width - 1 - k);
            let _ = seq
                .local_get(out)
                .local_get(j)
                .i32_const(k)
                .binop(BinaryOp::I32Add)
                .local_get(c);
            if shift > 0 {
                let _ = seq.i32_const(shift).binop(BinaryOp::I32ShrU);
            }
            if k > 0 {
                let _ = seq
                    .i32_const(0x3F)
                    .binop(BinaryOp::I32And)
                    .i32_const(0x80)
                    .binop(BinaryOp::I32Or);
            } else if width > 1 {
                // the marks of the first byte: 110, 1110 or 11110
                let _ = seq.i32_const(0xFF00 >> width & 0xFF).binop(BinaryOp::I32Or);
            }
            let _ = seq.array_set(bytes);
        }
        let _ = seq
            .local_get(j)
            .i32_const(width)
            .binop(BinaryOp::I32Add)
            .local_set(j);
    };
    if width == 4 {
        put(seq);
        return;
    }
    let limit = match width {
        1 => 0x80,
        2 => 0x800,
        _ => 0x10000,
    };
    let _ = seq
        .local_get(c)
        .i32_const(limit)
        .binop(BinaryOp::I32LtU)
        .if_else(InstrSeqType::Simple(None), put, |else_s| {
            put_utf8(else_s, bytes, out, j, c, width + 1)
        });
}

/// `_string_from_utf8(bytes, off, len)`: counts the units of the valid UTF-8
/// bytes and decodes them to an array.
fn code_from_utf8(
    module: &mut walrus::Module,
    bytes: BytesType,
    units: TypeId,
    from_char_code_array: FunctionId,
) -> FunctionId {
    let bytes_vt = bytes.val_type();
    let b = module.locals.add(bytes_vt);
    let off = module.locals.add(ValType::I32);
    let len = module.locals.add(ValType::I32);
    let end = module.locals.add(ValType::I32);
    let i = module.locals.add(ValType::I32);
    let n = module.locals.add(ValType::I32);
    let a = module.locals.add(ValType::Ref(RefType {
        nullable: false,
        heap_type: HeapType::Concrete(units),
    }));
    let j = module.locals.add(ValType::I32);
    let c = module.locals.add(ValType::I32);
    let mut fb = FunctionBuilder::new(
        &mut module.types,
        &[bytes_vt, ValType::I32, ValType::I32],
        &[EXTERN],
    );
    let _ = fb.name("_string_from_utf8".into());
    let mut body = fb.func_body();
    // Loops over the bytes from `off` to `end`, with `i` at the current one.
    let for_each = |seq: &mut InstrSeqBuilder<'_>, step: &dyn Fn(&mut InstrSeqBuilder<'_>)| {
        let _ = seq
            .local_get(off)
            .local_set(i)
            .block(InstrSeqType::Simple(None), |block_s| {
                let block_id = block_s.id();
                let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                    let loop_id = loop_s.id();
                    let _ = loop_s
                        .local_get(i)
                        .local_get(end)
                        .binop(BinaryOp::I32GeU)
                        .br_if(block_id);
                    step(loop_s);
                    let _ = loop_s.br(loop_id);
                });
            });
    };
    let _ = body
        .local_get(off)
        .local_get(len)
        .binop(BinaryOp::I32Add)
        .local_set(end);
    // n = the bytes that start a code point, and the 4-byte ones again, as
    // they become two units
    for_each(&mut body, &|seq| {
        let _ = seq
            .local_get(n)
            .local_get(b)
            .local_get(i)
            .array_get_u(bytes.index)
            .local_tee(c)
            .i32_const(0xC0)
            .binop(BinaryOp::I32And)
            .i32_const(0x80)
            .binop(BinaryOp::I32Ne)
            .binop(BinaryOp::I32Add)
            .local_get(c)
            .i32_const(0xF0)
            .binop(BinaryOp::I32GeU)
            .binop(BinaryOp::I32Add)
            .local_set(n)
            .local_get(i)
            .i32_const(1)
            .binop(BinaryOp::I32Add)
            .local_set(i);
    });
    let _ = body.local_get(n).array_new_default(units).local_set(a);
    for_each(&mut body, &|seq| {
        let _ = seq
            .local_get(b)
            .local_get(i)
            .array_get_u(bytes.index)
            .local_set(c);
        get_utf8(seq, bytes.index, b, i, c, 1);
        // a code point above U+FFFF is a pair of surrogates
        let _ = seq
            .local_get(c)
            .i32_const(0x10000)
            .binop(BinaryOp::I32GeU)
            .if_else(
                InstrSeqType::Simple(None),
                |then_s| {
                    let _ = then_s
                        .local_get(a)
                        .local_get(j)
                        .local_get(c)
                        .i32_const(0x10000 - (0xD800 << 10))
                        .binop(BinaryOp::I32Sub)
                        .i32_const(10)
                        .binop(BinaryOp::I32ShrU)
                        .array_set(units)
                        .local_get(a)
                        .local_get(j)
                        .i32_const(1)
                        .binop(BinaryOp::I32Add)
                        .local_get(c)
                        .i32_const(0x3FF)
                        .binop(BinaryOp::I32And)
                        .i32_const(0xDC00)
                        .binop(BinaryOp::I32Or)
                        .array_set(units)
                        .local_get(j)
                        .i32_const(2)
                        .binop(BinaryOp::I32Add)
                        .local_set(j);
                },
                |else_s| {
                    let _ = else_s
                        .local_get(a)
                        .local_get(j)
                        .local_get(c)
                        .array_set(units)
                        .local_get(j)
                        .i32_const(1)
                        .binop(BinaryOp::I32Add)
                        .local_set(j);
                },
            );
    });
    let _ = body
        .local_get(a)
        .i32_const(0)
        .local_get(n)
        .call(from_char_code_array);
    fb.finish(vec![b, off, len], &mut module.funcs)
}

/// Decodes to `c` the code point of the UTF-8 bytes of `b` at `i`, whose
/// first byte is in `c`, advancing `i`, when it takes `width` bytes or more.
fn get_utf8(
    seq: &mut InstrSeqBuilder<'_>,
    bytes: TypeId,
    b: LocalId,
    i: LocalId,
    c: LocalId,
    width: i32,
) {
    let get = |seq: &mut InstrSeqBuilder<'_>| {
        if width > 1 {
            // the bits of the first byte after its marks
            let _ = seq
                .local_get(c)
                .i32_const(0x7F >> width)
                .binop(BinaryOp::I32And)
                .local_set(c);
        }
        for k in 1..width {
            let _ = seq
                .local_get(c)
                .i32_const(6)
                .binop(BinaryOp::I32Shl)
                .local_get(b)
                .local_get(i)
                .i32_const(k)
                .binop(BinaryOp::I32Add)
                .array_get_u(bytes)
                .i32_const(0x3F)
                .binop(BinaryOp::I32And)
                .binop(BinaryOp::I32Or)
                .local_set(c);
        }
        let _ = seq
            .local_get(i)
            .i32_const(width)
            .binop(BinaryOp::I32Add)
            .local_set(i);
    };
    if width == 4 {
        get(seq);
        return;
    }
    let limit = match width {
        1 => 0x80,
        2 => 0xE0,
        _ => 0xF0,
    };
    let _ = seq
        .local_get(c)
        .i32_const(limit)
        .binop(BinaryOp::I32LtU)
        .if_else(InstrSeqType::Simple(None), get, |else_s| {
            get_utf8(else_s, bytes, b, i, c, width + 1)
        });
}

impl Instructions<'_, '_> {
    /// `[] -> [n]`, see `utf8_width`.
    fn js_utf8_width(&mut self, c: LocalId) -> &mut Self {
        utf8_width(self.builder(), c);
        self
    }
}

impl<'a> Generator<'a> {
    /// A string literal is imported, see `CONSTANTS_MODULE`.
    pub(super) fn js_string_literal(&mut self, string: &str) -> GlobalId {
        self.wasm_module
            .add_import_global(CONSTANTS_MODULE, string, EXTERN, false, false)
            .0
    }

    pub(super) fn code_js_string_concat(&mut self, js: JsString) -> FunctionId {
        self.code_js_string_binary("_string_concat", EXTERN, |s, a, b| {
            let _ = s.local_get(a).local_get(b).call(js.concat);
        })
    }

    pub(super) fn code_js_string_eq(&mut self, js: JsString) -> FunctionId {
        self.code_js_string_binary("_equal_string", BOOL_VALTYPE, |s, a, b| {
            let _ = s.local_get(a).local_get(b).call(js.equals);
        })
    }

    /// A function of two strings `a` and `b`, which `body` pushes the result
    /// of.
    fn code_js_string_binary(
        &mut self,
        name: &str,
        result: ValType,
        body: impl FnOnce(&mut Instructions<'_, '_>, LocalId, LocalId),
    ) -> FunctionId {
        // params
        let a = self.wasm_local(EXTERN);
        let b = self.wasm_local(EXTERN);
        let mut function = Function::new(self, name, &[a, b], &[result]);
        let mut instructions = function.extend_instructions(self);
        body(&mut instructions, a, b);
        let _ = instructions.end();
        function.finish(self)
    }

    /// `_string_byte_slice(s, start, length)`, through the UTF-8 bytes.
    pub(super) fn code_js_string_byte_slice(&mut self, js: JsString) -> FunctionId {
        let int_vt = self.int.val_type();
        // params
        let s = self.wasm_local(EXTERN);
        let start = self.wasm_local(int_vt);
        let len = self.wasm_local(int_vt);
        let mut function = Function::new(self, "_string_byte_slice", &[s, start, len], &[EXTERN]);
        let _ = function
            .extend_instructions(self)
            .local_get(s)
            .call(js.to_utf8)
            .local_get(start)
            .int_to_i32()
            .local_get(len)
            .int_to_i32()
            .call(js.from_utf8)
            .end();
        function.finish(self)
    }

    /// `_string_concat_list(strings)` and `_string_join(strings, separator)`:
    /// a JavaScript string is a rope, so the strings are concatenated one
    /// after the other.
    pub(super) fn code_js_string_join(&mut self, js: JsString, with_separator: bool) -> FunctionId {
        let cons = self.string_list_cons();
        let empty = self.string_index(&"".into());
        // params
        let strings = self.wasm_local(cons.list_vt);
        let separator = self.wasm_local(EXTERN);
        // locals
        let l = self.wasm_local(cons.list_vt);
        let item = self.wasm_local(EXTERN);
        let seen = self.wasm_local(ValType::I32);
        let joined = self.wasm_local(EXTERN);
        let (name, params) = string_join_signature(with_separator, strings, separator);
        let mut function = Function::new(self, name, &params, &[EXTERN]);
        let _ = function
            .extend_instructions(self)
            .global_as_non_null(empty)
            .local_set(joined)
            .string_list_for_each(cons, strings, l, item, &|s| {
                let _ = s
                    .separator_before(with_separator, seen, &|s| {
                        let _ = s
                            .local_get(joined)
                            .local_get(separator)
                            .call(js.concat)
                            .local_set(joined);
                    })
                    .local_get(joined)
                    .local_get(item)
                    .call(js.concat)
                    .local_set(joined);
            })
            .local_get(joined)
            .end();
        function.finish(self)
    }

    /// `_string_pop_grapheme(s)`, like the UTF-8 version, with the window in
    /// units: only the window is converted for Rust, and the length in bytes
    /// of the grapheme is converted back to units.
    pub(super) fn code_js_string_pop_grapheme(&mut self, js: JsString) -> FunctionId {
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
        let s = self.wasm_local(EXTERN);
        // locals
        let len = self.wasm_local(ValType::I32);
        let window = self.wasm_local(ValType::I32);
        let w = self.wasm_local(ValType::I32);
        let n = self.wasm_local(ValType::I32);
        let mark = self.wasm_local(ValType::I32);
        let c = self.wasm_local(ValType::I32);
        let mut function = Function::new(self, "_string_pop_grapheme", &[s], &[result_vt]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(s)
            .call(js.length)
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
                    // w = min(window, len), without splitting a pair
                    .local_get(window)
                    .local_get(len)
                    .local_get(window)
                    .local_get(len)
                    .i32_lt_u()
                    .select(None)
                    .local_tee(w)
                    .local_get(len)
                    .i32_lt_u()
                    .if_(InstrSeqType::Simple(None), |then_s| {
                        let _ = then_s
                            .local_get(s)
                            .local_get(w)
                            .i32_const(1)
                            .i32_sub()
                            .call(js.code_point_at)
                            .i32_const(0x10000)
                            .i32_ge_u()
                            .local_get(w)
                            .i32_add()
                            .local_set(w);
                    })
                    .local_get(s)
                    .i32_const(0)
                    .local_get(w)
                    .call(js.substring)
                    .call(lower_string)
                    // complete when the window is the whole string
                    .local_get(w)
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
            // w = the units of the first n bytes
            .i32_const(0)
            .local_set(w)
            .loop_(InstrSeqType::Simple(None), |loop_s| {
                let loop_id = loop_s.id();
                let _ = loop_s
                    .local_get(s)
                    .local_get(w)
                    .call(js.code_point_at)
                    .local_set(c)
                    .local_get(n)
                    .js_utf8_width(c)
                    .i32_sub()
                    .local_set(n)
                    .local_get(w)
                    .i32_const(1)
                    .i32_add()
                    .local_get(c)
                    .i32_const(0x10000)
                    .i32_ge_u()
                    .i32_add()
                    .local_set(w)
                    .local_get(n)
                    .i32_const(0)
                    .i32_gt_s()
                    .br_if(loop_id);
            })
            .local_get(s)
            .i32_const(0)
            .local_get(w)
            .call(js.substring)
            .local_get(s)
            .local_get(w)
            .local_get(len)
            .call(js.substring)
            .struct_new(pair_index)
            .call(ok_fn)
            .end();
        function.finish(self)
    }
}
