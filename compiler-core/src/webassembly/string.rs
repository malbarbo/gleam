// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! The representation of `String` and the operations that depend on it.

use super::*;

/// Slices of up to this many bytes are copied instead of sharing the bytes of
/// the original string.
pub(super) const STRING_SLICE_COPY_MAX: i32 = 64;

/// How a `String` is represented, see `WasmString`.
#[derive(Clone, Copy)]
pub(super) enum StringType {
    Utf8(Utf8String),
    Js(JsString),
}

impl StringType {
    pub(super) fn new(
        module: &mut walrus::Module,
        bytes: BytesType,
        cell: TypeId,
        string: WasmString,
    ) -> Self {
        match string {
            WasmString::Utf8 => StringType::Utf8(Utf8String::new(module, bytes, cell)),
            WasmString::JsString => StringType::Js(JsString::new(module, bytes)),
        }
    }

    pub(super) fn val_type(&self) -> ValType {
        ValType::Ref(RefType {
            heap_type: self.heap_type(),
            nullable: false,
        })
    }

    pub(super) fn val_type_nullable(&self) -> ValType {
        ValType::Ref(RefType {
            heap_type: self.heap_type(),
            nullable: true,
        })
    }

    pub(super) fn heap_type(&self) -> HeapType {
        match self {
            StringType::Utf8(utf8) => HeapType::Concrete(utf8.type_index),
            StringType::Js(_) => HeapType::Abstract(walrus::AbstractHeapType::Extern),
        }
    }

    /// The initial value of a string global, set by the start function.
    pub(super) fn null_const(&self) -> ConstExpr {
        ConstExpr::RefNull(RefType {
            nullable: true,
            heap_type: self.heap_type(),
        })
    }
}

/// `String` is a view `{ bytes, off, len }` over an immutable byte array, so
/// slicing is O(1). Small slices, and slices smaller than a quarter of the
/// array they come from, are copied: tokens never retain big strings and a
/// view retains at most 4 times its size, while consuming a string from the
/// start stays linear (the copies form a geometric series).
///
/// A string made by `_string_concat` also has the `Cell` of its bytes, with
/// the end of the bytes written so far, as a bit array (see
/// `runtime/bit_array.gleam`): a string that ends there is extended by
/// writing after it, in the same bytes, which makes `acc <> s` in a loop
/// linear. The other strings have no `Cell`, and are copied to bytes of
/// their exact size, so a single `<>` does not take twice its memory.
#[derive(Clone, Copy)]
pub(super) struct Utf8String {
    /// The `String` struct, `{ bytes, off, len, written: (ref null Cell) }`.
    type_index: TypeId,
    /// The `Cell` of the runtime.
    cell: TypeId,
    /// `(s: String, i: i32) -> i32`: the byte at `i`.
    byte_at: FunctionId,
    /// `(bytes) -> String`: a string with all of `bytes`.
    from_bytes: FunctionId,
    /// `(dst: bytes, dst_off: i32, src: String, src_off: i32, len: i32)`.
    copy_to_bytes: FunctionId,
    /// `(s: String, start: i32, len: i32) -> String`, applying the slice
    /// rule.
    slice: FunctionId,
}

impl Utf8String {
    fn new(module: &mut walrus::Module, bytes: BytesType, cell: TypeId) -> Utf8String {
        let bytes_index = bytes.index;
        let bytes_vt = bytes.val_type();
        let immutable = |val_type| FieldType {
            element_type: StorageType::Val(val_type),
            mutable: false,
        };
        let type_index = module.types.add_struct(vec![
            immutable(bytes_vt),
            immutable(ValType::I32),
            immutable(ValType::I32),
            immutable(ValType::Ref(RefType {
                heap_type: HeapType::Concrete(cell),
                nullable: true,
            })),
        ]);
        module.types.get_mut(type_index).name = Some("String".into());
        let string_vt = ValType::Ref(RefType {
            heap_type: HeapType::Concrete(type_index),
            nullable: false,
        });

        let byte_at = {
            let s = module.locals.add(string_vt);
            let i = module.locals.add(ValType::I32);
            let mut fb = FunctionBuilder::new(
                &mut module.types,
                &[string_vt, ValType::I32],
                &[ValType::I32],
            );
            let _ = fb.name("_string_byte_at".into());
            let _ = fb
                .func_body()
                .local_get(s)
                .struct_get(type_index, 0)
                .local_get(s)
                .struct_get(type_index, 1)
                .local_get(i)
                .binop(BinaryOp::I32Add)
                .array_get_u(bytes_index);
            fb.finish(vec![s, i], &mut module.funcs)
        };

        let from_bytes = {
            let bytes = module.locals.add(bytes_vt);
            let mut fb = FunctionBuilder::new(&mut module.types, &[bytes_vt], &[string_vt]);
            let _ = fb.name("_string_from_bytes".into());
            let _ = fb
                .func_body()
                .local_get(bytes)
                .i32_const(0)
                .local_get(bytes)
                .array_len()
                .ref_null(RefType {
                    heap_type: HeapType::Concrete(cell),
                    nullable: true,
                })
                .struct_new(type_index);
            fb.finish(vec![bytes], &mut module.funcs)
        };

        let copy_to_bytes = {
            let dst = module.locals.add(bytes_vt);
            let dst_off = module.locals.add(ValType::I32);
            let src = module.locals.add(string_vt);
            let src_off = module.locals.add(ValType::I32);
            let len = module.locals.add(ValType::I32);
            let mut fb = FunctionBuilder::new(
                &mut module.types,
                &[
                    bytes_vt,
                    ValType::I32,
                    string_vt,
                    ValType::I32,
                    ValType::I32,
                ],
                &[],
            );
            let _ = fb.name("_string_copy_to_bytes".into());
            let _ = fb
                .func_body()
                .local_get(dst)
                .local_get(dst_off)
                .local_get(src)
                .struct_get(type_index, 0)
                .local_get(src)
                .struct_get(type_index, 1)
                .local_get(src_off)
                .binop(BinaryOp::I32Add)
                .local_get(len)
                .array_copy(bytes_index, bytes_index);
            fb.finish(vec![dst, dst_off, src, src_off, len], &mut module.funcs)
        };

        let slice = {
            let s = module.locals.add(string_vt);
            let start = module.locals.add(ValType::I32);
            let len = module.locals.add(ValType::I32);
            let bytes = module.locals.add(bytes_vt);
            let copy = module.locals.add(bytes_vt);
            let mut fb = FunctionBuilder::new(
                &mut module.types,
                &[string_vt, ValType::I32, ValType::I32],
                &[string_vt],
            );
            let _ = fb.name("_string_slice".into());
            let mut body = fb.func_body();
            let _ = body
                .local_get(s)
                .struct_get(type_index, 0)
                .local_set(bytes)
                // len <= 64 || len < bytes.len / 4
                .local_get(len)
                .i32_const(STRING_SLICE_COPY_MAX)
                .binop(BinaryOp::I32LeU)
                .local_get(len)
                .local_get(bytes)
                .array_len()
                .i32_const(2)
                .binop(BinaryOp::I32ShrU)
                .binop(BinaryOp::I32LtU)
                .binop(BinaryOp::I32Or)
                .if_else(
                    string_vt,
                    |then_s| {
                        let _ = then_s
                            .local_get(len)
                            .array_new_default(bytes_index)
                            .local_tee(copy)
                            .i32_const(0)
                            .local_get(bytes)
                            .local_get(s)
                            .struct_get(type_index, 1)
                            .local_get(start)
                            .binop(BinaryOp::I32Add)
                            .local_get(len)
                            .array_copy(bytes_index, bytes_index)
                            .local_get(copy)
                            .i32_const(0)
                            .local_get(len)
                            .ref_null(RefType {
                                heap_type: HeapType::Concrete(cell),
                                nullable: true,
                            })
                            .struct_new(type_index);
                    },
                    |else_s| {
                        let _ = else_s
                            .local_get(bytes)
                            .local_get(s)
                            .struct_get(type_index, 1)
                            .local_get(start)
                            .binop(BinaryOp::I32Add)
                            .local_get(len)
                            .local_get(s)
                            .struct_get(type_index, 3)
                            .struct_new(type_index);
                    },
                );
            fb.finish(vec![s, start, len], &mut module.funcs)
        };

        Utf8String {
            type_index,
            cell,
            byte_at,
            from_bytes,
            copy_to_bytes,
            slice,
        }
    }
}

impl Instructions<'_, '_> {
    /// `[dst: bytes, dst_off, src: String, src_off, len] -> []`.
    pub(super) fn bytes_copy_from_string(&mut self) -> &mut Self {
        match self.repr.string {
            StringType::Utf8(utf8) => self.call(utf8.copy_to_bytes),
            StringType::Js(_) => unreachable!("a JavaScript string has no bytes"),
        }
    }

    /// `[bytes] -> [String]`: the string of all the bytes, which must not be
    /// changed afterwards.
    pub(super) fn string_from_bytes(&mut self) -> &mut Self {
        match self.repr.string {
            StringType::Utf8(utf8) => self.call(utf8.from_bytes),
            StringType::Js(js) => self.call(js.from_bytes),
        }
    }

    /// `[bytes, off, len] -> [String]`: the string of the UTF-8 bytes
    /// `bytes[off..off + len]`, which must not be changed afterwards.
    pub(super) fn string_from_utf8(&mut self) -> &mut Self {
        match self.repr.string {
            StringType::Utf8(utf8) => self
                .ref_null(HeapType::Concrete(utf8.cell))
                .struct_new(utf8.type_index),
            StringType::Js(js) => self.call(js.from_utf8),
        }
    }

    /// `[s, i] -> [unit]`: the code unit `i` of `s`, a byte with UTF8 and a
    /// UTF-16 unit with JSString.
    pub(super) fn string_get(&mut self) -> &mut Self {
        match self.repr.string {
            StringType::Utf8(utf8) => self.call(utf8.byte_at),
            StringType::Js(js) => self.call(js.char_code_at),
        }
    }

    /// `[s] -> [len]`: the number of code units of `s`.
    pub(super) fn string_len(&mut self) -> &mut Self {
        match self.repr.string {
            StringType::Utf8(utf8) => self.struct_get(utf8.type_index, 2),
            StringType::Js(js) => self.call(js.length),
        }
    }

    /// `[s] -> [len]`: the number of UTF-8 bytes of `s`.
    pub(super) fn string_utf8_len(&mut self) -> &mut Self {
        match self.repr.string {
            StringType::Utf8(_) => self.string_len(),
            StringType::Js(js) => self.call(js.utf8_len),
        }
    }

    /// `[] -> []`: sets `len` to the number of UTF-8 bytes of `s`, which are
    /// in `bytes` from `off` and must not be changed, converting a JavaScript
    /// string only once, to a new array.
    pub(super) fn string_utf8_view(
        &mut self,
        s: LocalId,
        bytes: LocalId,
        off: LocalId,
        len: LocalId,
    ) -> &mut Self {
        match self.repr.string {
            StringType::Utf8(utf8) => self
                .local_get(s)
                .struct_get(utf8.type_index, 0)
                .local_set(bytes)
                .local_get(s)
                .struct_get(utf8.type_index, 1)
                .local_set(off)
                .local_get(s)
                .string_len()
                .local_set(len),
            StringType::Js(js) => self
                .local_get(s)
                .call(js.to_utf8)
                .local_tee(bytes)
                .array_len()
                .local_set(len)
                .i32_const(0)
                .local_set(off),
        }
    }

    /// `[s, start, len] -> [String]`: the `len` code units from `start`,
    /// sharing the bytes of `s` when the slice is big with UTF8.
    pub(super) fn string_slice(&mut self) -> &mut Self {
        match self.repr.string {
            StringType::Utf8(utf8) => self.call(utf8.slice),
            StringType::Js(js) => self.call(js.slice),
        }
    }

    /// `[] -> [rest]`: `s` without `prefix`, which it starts with.
    pub(super) fn string_drop_prefix(&mut self, s: LocalId, prefix: &str) -> &mut Self {
        let prefix_len = match self.repr.string {
            StringType::Utf8(_) => prefix.len(),
            StringType::Js(_) => prefix.encode_utf16().count(),
        } as i32;
        self.local_get(s)
            .i32_const(prefix_len)
            .local_get(s)
            .string_len()
            .i32_const(prefix_len)
            .i32_sub()
            .string_slice()
    }

    /// `[] -> []`: runs `body` for each element of the list `strings`, in
    /// `item`, walking it with `l`.
    pub(super) fn string_list_for_each(
        &mut self,
        cons: StringListCons,
        strings: LocalId,
        l: LocalId,
        item: LocalId,
        body: &dyn Fn(&mut Instructions<'_, '_>),
    ) -> &mut Self {
        self.local_get(strings)
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
                        .variant_field_get(cons.index, cons.first_field)
                        .local_set(item);
                    body(loop_s);
                    let _ = loop_s
                        .local_get(l)
                        .ref_cast_non_null(HeapType::Concrete(cons.index))
                        .struct_get(cons.index, cons.rest_field)
                        .local_set(l)
                        .br(loop_id);
                });
            })
    }

    /// `[] -> []`: runs `body` when `with_separator` and an element was
    /// `seen`, so that the separator goes before every element but the
    /// first.
    pub(super) fn separator_before(
        &mut self,
        with_separator: bool,
        seen: LocalId,
        body: &dyn Fn(&mut Instructions<'_, '_>),
    ) -> &mut Self {
        if !with_separator {
            return self;
        }
        self.local_get(seen)
            .if_(InstrSeqType::Simple(None), |then_s| body(then_s))
            .i32_const(1)
            .local_set(seen)
    }
}

/// The cons of `List(String)`, which `_string_join` walks.
#[derive(Clone, Copy)]
pub(super) struct StringListCons {
    pub(super) list_vt: ValType,
    index: TypeId,
    rest_field: u32,
    first_field: Option<u32>,
}

/// The name and the parameters of `_string_join`, or of
/// `_string_concat_list` without a separator.
pub(super) fn string_join_signature(
    with_separator: bool,
    strings: LocalId,
    separator: LocalId,
) -> (&'static str, Vec<LocalId>) {
    if with_separator {
        ("_string_join", vec![strings, separator])
    } else {
        ("_string_concat_list", vec![strings])
    }
}

impl<'a> Generator<'a> {
    pub(super) fn string_list_cons(&mut self) -> StringListCons {
        let list_type = type_::list(type_::string());
        let list_vt = self.val_type(&list_type);
        let (custom_type, cons, _, args) = self.list_type_cons(&list_type);
        let (_, index, _) = self.mono_union_subtype_index(&list_type, &custom_type, &cons, &args);
        let (rest_field, first_field) = self.list_cons_fields(&list_type);
        StringListCons {
            list_vt,
            index,
            rest_field,
            first_field,
        }
    }

    /// Sets the globals of the string literals, see `string_index`.
    pub(super) fn code_string_literals(&mut self, instructions: &mut Instructions<'_, '_>) {
        if let StringType::Js(_) = self.string {
            return;
        }
        for (string, index) in self.strings.clone() {
            self.push_bytes(instructions, string.as_bytes().to_vec());
            let _ = instructions.string_from_bytes().global_set(index);
        }
    }

    pub(super) fn string_index(&mut self, string: &EcoString) -> GlobalId {
        let string = unescape(string);
        if let Some(id) = self.strings.get(string.as_str()) {
            return *id;
        }
        let id = match self.string {
            StringType::Utf8(_) => {
                let val_type = self.string.val_type_nullable();
                let init = self.string.null_const();
                self.wasm_module
                    .globals
                    .add_local(val_type, true, false, init)
            }
            StringType::Js(_) => self.js_string_literal(&string),
        };
        // Named after the text it holds. A module carries one of these per
        // distinct literal and they are otherwise identical, so a disassembly
        // reading `global.set 7` says nothing about which literal that is.
        self.wasm_module.globals.get_mut(id).name = Some(format!(
            "string@{}<{}>",
            self.strings.len(),
            string_tag(&string)
        ));
        let _ = self.strings.insert(string.into(), id);
        id
    }

    /// `a <> b`, written after `a` in its bytes when `a` ends where they are
    /// written and they have room, and otherwise in new bytes, with room for
    /// as many bytes more when `a` has a `Cell` (see `Utf8String`).
    pub(super) fn code_string_concat(&mut self) -> FunctionId {
        let utf8 = match self.string {
            StringType::Utf8(utf8) => utf8,
            StringType::Js(js) => return self.code_js_string_concat(js),
        };
        let string_vt = self.string.val_type();
        let cell_vt = ValType::Ref(RefType {
            heap_type: HeapType::Concrete(utf8.cell),
            nullable: true,
        });
        // params
        let a = self.wasm_local(string_vt); // String
        let b = self.wasm_local(string_vt); // String
        // locals
        let len_a = self.wasm_local(ValType::I32); // I32
        let len_b = self.wasm_local(ValType::I32); // I32
        let end = self.wasm_local(ValType::I32); // I32: where a ends
        let written = self.wasm_local(cell_vt); // the Cell of a
        let bytes = self.wasm_local(self.bytes.val_type()); // the bytes of a
        let len = self.wasm_local(ValType::I32); // I32: len_a + len_b
        // result bytes
        let r = self.wasm_local(self.bytes.val_type());
        let mut function = Function::new(self, "_string_concat", &[a, b], &[string_vt]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            // len_a = a.len
            .local_get(a)
            .string_len()
            .local_tee(len_a)
            // if len_a == 0: return b
            .i32_eqz()
            .if_(InstrSeqType::Simple(None), |then_s| {
                let _ = then_s.local_get(b).return_();
            })
            // len_b = b.len
            .local_get(b)
            .string_len()
            .local_tee(len_b)
            // if len_b == 0: return a
            .i32_eqz()
            .if_(InstrSeqType::Simple(None), |then_s| {
                let _ = then_s.local_get(a).return_();
            })
            .local_get(len_a)
            .local_get(len_b)
            .i32_add()
            .local_set(len)
            // bytes = a.bytes; end = a.off + len_a; written = a.written
            .local_get(a)
            .struct_get(utf8.type_index, 0)
            .local_set(bytes)
            .local_get(a)
            .struct_get(utf8.type_index, 1)
            .local_get(len_a)
            .i32_add()
            .local_set(end)
            .local_get(a)
            .struct_get(utf8.type_index, 3)
            .local_tee(written)
            .ref_is_null()
            .i32_eqz()
            .if_(InstrSeqType::Simple(None), |then_s| {
                let _ = then_s
                    // written.value == end
                    .local_get(written)
                    .struct_get(utf8.cell, 0)
                    .i32_wrap_i64()
                    .local_get(end)
                    .i32_eq()
                    // len_b <= bytes.len - end
                    .local_get(len_b)
                    .local_get(bytes)
                    .array_len()
                    .local_get(end)
                    .i32_sub()
                    .i32_le_u()
                    .i32_and()
                    .if_(InstrSeqType::Simple(None), |then_s| {
                        let _ = then_s
                            // bytes[end..] = b
                            .local_get(bytes)
                            .local_get(end)
                            .local_get(b)
                            .i32_const(0)
                            .local_get(len_b)
                            .bytes_copy_from_string()
                            // written.value = end + len_b
                            .local_get(written)
                            .local_get(end)
                            .local_get(len_b)
                            .i32_add()
                            .i64_extend_i32_u()
                            .struct_set(utf8.cell, 0)
                            // return { bytes, a.off, len, written }
                            .local_get(bytes)
                            .local_get(a)
                            .struct_get(utf8.type_index, 1)
                            .local_get(len)
                            .local_get(written)
                            .struct_new(utf8.type_index)
                            .return_();
                    });
            })
            // r = array.new_default(len), or 2 * len when a has a Cell
            .local_get(len)
            .local_get(len)
            .i32_const(1)
            .i32_shl()
            .local_get(written)
            .ref_is_null()
            .select(None)
            .bytes_new()
            .local_set(r)
            // r[0..] = a
            .local_get(r)
            .i32_const(0)
            .local_get(a)
            .i32_const(0)
            .local_get(len_a)
            .bytes_copy_from_string()
            // r[len_a..] = b
            .local_get(r)
            .local_get(len_a)
            .local_get(b)
            .i32_const(0)
            .local_get(len_b)
            .bytes_copy_from_string()
            // { r, 0, len, Cell(len) }
            .local_get(r)
            .i32_const(0)
            .local_get(len)
            .local_get(len)
            .i64_extend_i32_u()
            .struct_new(utf8.cell)
            .struct_new(utf8.type_index)
            .end();
        function.finish(self)
    }

    pub(super) fn code_string_eq(&mut self) -> FunctionId {
        if let StringType::Js(js) = self.string {
            return self.code_js_string_eq(js);
        }
        let string_vt = self.string.val_type();
        // params
        let a = self.wasm_local(string_vt); // String
        let b = self.wasm_local(string_vt); // String
        // locals
        let i = self.wasm_local(ValType::I32); // I32
        let len = self.wasm_local(ValType::I32); // I32
        // return Bool
        let mut function = Function::new(self, "_equal_string", &[a, b], &[BOOL_VALTYPE]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(a)
            .local_get(b)
            .ref_eq()
            // if ref a == ref b
            .if_(InstrSeqType::Simple(None), |body| {
                let _ = body
                    .bool_const(true)
                    .return_();
            })
            // len = a.len; push len
            .local_get(a)
            .string_len()
            .local_tee(len)
            // b.len
            .local_get(b)
            .string_len()
            .i32_ne()
            // if a.len != b.len
            .if_(InstrSeqType::Simple(None), |body| {
                let _ = body
                    .bool_const(false)
                    .return_();
            })
            // i = 0
            .i32_const(0)
            .local_set(i)
            // loop
            .loop_(InstrSeqType::Simple(None), |loop_s| {
                let loop_id = loop_s.id();
                let _ = loop_s
                    .local_get(i)
                    .local_get(len)
                    .i32_ge_u()
                    // if i >= len
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body
                            .bool_const(true)
                            .return_();
                    })
                    // a[i]
                    .local_get(a)
                    .local_get(i)
                    .string_get()
                    // b[i]
                    .local_get(b)
                    .local_get(i)
                    .string_get()
                    .i32_ne()
                    // if a[i] != b[i]
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body
                            .bool_const(false)
                            .return_();
                    })
                    .i32_inc(i)
                    // loop
                    .br(loop_id);
            })
            .bool_const(true)
            .end();
        function.finish(self)
    }

    pub(super) fn function_string_starts_with(&mut self) -> FunctionId {
        self.get_function_builtin(BuiltinFunction::StringStartsWith, |s| {
            s.code_string_starts_with()
        })
    }

    pub(super) fn code_string_starts_with(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        // params
        let a = self.wasm_local(string_vt); // String
        let b = self.wasm_local(string_vt); // String
        // locals
        let i = self.wasm_local(ValType::I32); // I32
        let prefix_len = self.wasm_local(ValType::I32); // I32
        let mut function = Function::new(self, "_string_starts_with", &[a, b], &[BOOL_VALTYPE]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(b)
            .string_len()
            .local_set(prefix_len)
            // if a.len < prefix_len
            .local_get(a)
            .string_len()
            .local_get(prefix_len)
            .i32_lt_u()
            .if_(InstrSeqType::Simple(None), |body| {
                let _ = body
                    .bool_const(false)
                    .return_();
            })
            .i32_const(0)
            .local_set(i)
            // loop
            .loop_(InstrSeqType::Simple(None), |loop_s| {
                let loop_id = loop_s.id();
                let _ = loop_s
                    .local_get(i)
                    .local_get(prefix_len)
                    .i32_ge_u()
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body
                            .bool_const(true)
                            .return_();
                    })
                    // a[i]
                    .local_get(a)
                    .local_get(i)
                    .string_get()
                    // b[i]
                    .local_get(b)
                    .local_get(i)
                    .string_get()
                    .i32_ne()
                    // if a[i] != b[i]
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body
                            .bool_const(false)
                            .return_();
                    })
                    .i32_inc(i)
                    // loop
                    .br(loop_id);
            })
            .bool_const(true)
            .end();
        function.finish(self)
    }

    /// `_string_byte_slice(s, start, length)`: the caller guarantees the
    /// bounds. A view over `s` unless the slice is small.
    pub(super) fn code_string_byte_slice(&mut self) -> FunctionId {
        match self.string {
            StringType::Utf8(_) => self.code_string_slice("_string_byte_slice"),
            StringType::Js(js) => self.code_js_string_byte_slice(js),
        }
    }

    /// `name(s, start, length)`: `string_slice` with `Int` arguments.
    fn code_string_slice(&mut self, name: &str) -> FunctionId {
        let string_vt = self.string.val_type();
        let int_vt = self.int.val_type();
        // params
        let s = self.wasm_local(string_vt); // String
        let start = self.wasm_local(int_vt); // Int
        let len = self.wasm_local(int_vt); // Int
        let mut function = Function::new(self, name, &[s, start, len], &[string_vt]);
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

    /// `_string_codeunit_slice(s, start, length)`, in the code units of the
    /// representation: the bytes with UTF8, so it is `_string_byte_slice`,
    /// and the UTF-16 units with JSString. The caller guarantees the bounds.
    pub(super) fn code_string_codeunit_slice(&mut self) -> FunctionId {
        match self.string {
            StringType::Utf8(_) => {
                self.get_function_builtin_external(BuiltinFunctionExternal::StringByteSlice)
            }
            StringType::Js(_) => self.code_string_slice("_string_codeunit_slice"),
        }
    }

    /// `_string_pop_codeunit(s)`: the first code unit of `s`, as
    /// `_string_codeunit_slice` counts them, and the rest.
    pub(super) fn code_string_pop_codeunit(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        let tuple = type_::tuple(vec![type_::int(), type_::string()]);
        let tuple_vt = self.val_type(&tuple);
        let tuple_index = self.type_index(&tuple);
        // params
        let s = self.wasm_local(string_vt); // String
        // locals
        let len = self.wasm_local(ValType::I32);
        let mut function = Function::new(self, "_string_pop_codeunit", &[s], &[tuple_vt]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .local_get(s)
            .string_len()
            .local_tee(len)
            .if_else(
                tuple_vt,
                |then_s| {
                    let _ = then_s
                        .local_get(s)
                        .i32_const(0)
                        .string_get()
                        .i32_to_int()
                        .local_get(s)
                        .i32_const(1)
                        .local_get(len)
                        .i32_const(1)
                        .i32_sub()
                        .string_slice()
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

    /// `_string_find(haystack, needle, from)`: the index of the first match
    /// of `needle` in `haystack` from `from`, or -1, in the code units of
    /// `_string_codeunit_slice`. A match of valid UTF-8 or UTF-16 starts at
    /// the start of a code point, so comparing code units is enough. The
    /// caller guarantees that `from` is in the bounds.
    pub(super) fn code_string_find(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        let int_vt = self.int.val_type();
        // params
        let haystack = self.wasm_local(string_vt); // String
        let needle = self.wasm_local(string_vt); // String
        let from = self.wasm_local(int_vt); // Int
        // locals
        let i = self.wasm_local(ValType::I32);
        let j = self.wasm_local(ValType::I32);
        let needle_len = self.wasm_local(ValType::I32);
        let last = self.wasm_local(ValType::I32); // the last start of a match
        let first = self.wasm_local(ValType::I32); // needle[0]
        let mut function =
            Function::new(self, "_string_find", &[haystack, needle, from], &[int_vt]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .local_get(from)
            .int_to_i32()
            .local_set(i)
            .local_get(needle)
            .string_len()
            .local_tee(needle_len)
            .i32_eqz()
            .if_(InstrSeqType::Simple(None), |body| {
                let _ = body
                    .local_get(from)
                    .return_();
            })
            .local_get(haystack)
            .string_len()
            .local_get(needle_len)
            .i32_sub()
            .local_set(last)
            .local_get(needle)
            .i32_const(0)
            .string_get()
            .local_set(first)
            .loop_(InstrSeqType::Simple(None), |outer| {
                let outer_id = outer.id();
                let _ = outer
                    // if i > last
                    .local_get(i)
                    .local_get(last)
                    .i32_gt_s()
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body
                            .int_const(&(-1).into())
                            .return_();
                    })
                    // if haystack[i] == first
                    .local_get(haystack)
                    .local_get(i)
                    .string_get()
                    .local_get(first)
                    .i32_eq()
                    .if_(InstrSeqType::Simple(None), |then_s| {
                        let _ = then_s
                            .i32_const(1)
                            .local_set(j)
                            .block_(InstrSeqType::Simple(None), |block_s| {
                                let block_id = block_s.id();
                                let _ = block_s.loop_(InstrSeqType::Simple(None), |inner| {
                                    let inner_id = inner.id();
                                    let _ = inner
                                        // if j >= needle_len
                                        .local_get(j)
                                        .local_get(needle_len)
                                        .i32_ge_u()
                                        .if_(InstrSeqType::Simple(None), |body| {
                                            let _ = body
                                                .local_get(i)
                                                .i32_to_int()
                                                .return_();
                                        })
                                        // haystack[i + j] != needle[j]
                                        .local_get(haystack)
                                        .local_get(i)
                                        .local_get(j)
                                        .i32_add()
                                        .string_get()
                                        .local_get(needle)
                                        .local_get(j)
                                        .string_get()
                                        .i32_ne()
                                        .br_if(block_id)
                                        .i32_inc(j)
                                        .br(inner_id);
                                });
                            });
                    })
                    .i32_inc(i)
                    .br(outer_id);
            })
            .unreachable()
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
        let b_len = self.wasm_local(ValType::I32); // I32
        let mut function = Function::new(self, "_string_ends_with", &[a, b], &[BOOL_VALTYPE]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            // offset = a.len - b.len
            .local_get(a)
            .string_len()
            .local_get(b)
            .string_len()
            .local_tee(b_len)
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
                    .local_get(b_len)
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
                        then_s.local_get(affix).string_len()
                    } else {
                        then_s.i32_const(0)
                    };
                    let _ = then_s
                        .local_get(s)
                        .string_len()
                        .local_get(affix)
                        .string_len()
                        .i32_sub()
                        .string_slice();
                },
                |else_s| {
                    let _ = else_s.local_get(s);
                },
            )
            .end();
        function.finish(self)
    }

    /// In the order of the code points, as on Erlang: byte-wise with UTF8.
    /// With JSString, the order of the UTF-16 units differs from it when the
    /// first different units are a surrogate and a unit from 0xE000, which
    /// comes first as a code point.
    pub(super) fn code_string_less_than(&mut self) -> FunctionId {
        let utf16 = matches!(self.string, StringType::Js(_));
        let string_vt = self.string.val_type();
        // params
        let a = self.wasm_local(string_vt); // String
        let b = self.wasm_local(string_vt); // String
        // locals
        let i = self.wasm_local(ValType::I32); // I32
        let a_len = self.wasm_local(ValType::I32); // I32
        let b_len = self.wasm_local(ValType::I32); // I32
        let x = self.wasm_local(ValType::I32); // I32
        let y = self.wasm_local(ValType::I32); // I32
        // Pushes `unit`, with the surrogates moved after the units from
        // 0xE000 with UTF-16, which keeps the order of the code points of
        // different units.
        let code_point_order = |s: &mut Instructions<'_, '_>, unit: LocalId| {
            if !utf16 {
                let _ = s.local_get(unit);
                return;
            }
            #[rustfmt::skip]
            let _ = s
                .local_get(unit)
                .i32_const(0xE000)
                .i32_ge_u()
                .if_else(
                    ValType::I32,
                    |then_s| {
                        let _ = then_s.local_get(unit).i32_const(0x800).i32_sub();
                    },
                    |else_s| {
                        let _ = else_s
                            .local_get(unit)
                            .i32_const(0xD800)
                            .i32_ge_u()
                            .if_else(
                                ValType::I32,
                                |then_s| {
                                    let _ = then_s.local_get(unit).i32_const(0x2000).i32_add();
                                },
                                |else_s| {
                                    let _ = else_s.local_get(unit);
                                },
                            );
                    },
                );
        };
        let mut function = Function::new(self, "_string_less_than", &[a, b], &[BOOL_VALTYPE]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .local_get(a)
            .string_len()
            .local_set(a_len)
            .local_get(b)
            .string_len()
            .local_set(b_len)
            .loop_(InstrSeqType::Simple(None), |loop_s| {
                let loop_id = loop_s.id();
                let _ = loop_s
                    // the shorter string is less when it is a prefix
                    .local_get(i)
                    .local_get(b_len)
                    .i32_ge_u()
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body.bool_const(false).return_();
                    })
                    .local_get(i)
                    .local_get(a_len)
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
                        code_point_order(body, x);
                        code_point_order(body, y);
                        let _ = body.i32_lt_u().return_();
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
        if let StringType::Js(js) = self.string {
            return self.code_js_string_join(js, with_separator);
        }
        let string_vt = self.string.val_type();
        let cons = self.string_list_cons();
        // params
        let strings = self.wasm_local(cons.list_vt); // List(String)
        let separator = self.wasm_local(string_vt); // String
        // locals
        let l = self.wasm_local(cons.list_vt); // List(String)
        let item = self.wasm_local(string_vt); // String
        let total = self.wasm_local(ValType::I32); // I32
        let pos = self.wasm_local(ValType::I32); // I32
        let seen = self.wasm_local(ValType::I32); // Bool
        let r = self.wasm_local(self.bytes.val_type());
        let (name, params) = string_join_signature(with_separator, strings, separator);
        let mut function = Function::new(self, name, &params, &[string_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions.string_list_for_each(cons, strings, l, item, &|s| {
            let _ = s.separator_before(with_separator, seen, &|s| {
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
        let _ = instructions.string_list_for_each(cons, strings, l, item, &|s| {
            let _ = s.separator_before(with_separator, seen, &|s| {
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
        if let StringType::Js(js) = self.string {
            return self.code_js_string_pop_grapheme(js);
        }
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
                    .i32_const(0)
                    .local_get(window)
                    .string_slice()
                    .call(lower_string)
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

pub(super) const GRAPHEME_PREFIX_LEN: &str = "_grapheme_prefix_len";

/// The first window of `_string_pop_grapheme`, in bytes.
pub(super) const GRAPHEME_WINDOW: i32 = 64;
