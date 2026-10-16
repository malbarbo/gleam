// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `_hash(T)`: a structural hash generated for each type, consistent with
//! `_equal(T)` (equal values have equal hashes). It mirrors the code of the
//! equality, with the same dispatch on tags and the same field mapping. The
//! standard library declares it as
//! `@external(webassembly, "builtins", "_hash") fn hash(x: a) -> Int` and uses
//! it for its hash tries.

use super::*;

pub(super) const HASH: &str = "_hash";

/// How to hash a value of a type: inline for the types that are numbers in
/// wasm, by calling a generated function otherwise.
#[derive(Clone, Copy)]
pub(super) enum Hash {
    I32,
    Int,
    Float,
    /// Functions can't be compared, so they all hash the same.
    Constant,
    /// The bits of the external numbers `I64`, `F32` and `F64`.
    I64,
    F32,
    F64,
    Call(FunctionId),
    /// A hook of the runtime, which returns an `Int`.
    Hook(FunctionId),
}

pub(super) const SEED: i32 = 0x2545_F491;
pub(super) const MULTIPLIER: i32 = 0x0100_0193;

impl<'a> Generator<'a> {
    /// The `_hash` external with the type `fn(T) -> Int`.
    pub(super) fn function_hash_external(&mut self, fn_type: &Arc<Type>) -> Id {
        let (params, _) = fn_type.fn_types().expect("hash must be a function type");
        let arg_type = params.into_iter().next().expect("hash takes one argument");
        let builtin = BuiltinFunction::HashExternal(
            self.val_type(&arg_type),
            self.type_pretty_name(&arg_type),
        );
        let name = builtin.name();
        let index = self.get_function_builtin(builtin, |s| s.code_hash_external(&arg_type));
        let _ = self.add_function_to_globals(name.clone(), index);
        Id::func(name, index)
    }

    fn code_hash_external(&mut self, arg_type: &Arc<Type>) -> FunctionId {
        let hash = self.function_hash(arg_type);
        let arg_vt = self.val_type(arg_type);
        let int_vt = self.int.val_type();
        // params
        let value = self.wasm_local(arg_vt);
        let mut function = Function::new(self, "_hash", &[value], &[int_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions.local_get(value);
        self.hash_instructions(&mut instructions, hash);
        let _ = instructions.i32_to_int().end();
        function.finish(self)
    }

    /// `[value] -> [i32]`.
    pub(super) fn hash_instructions(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        hash: Hash,
    ) {
        let mix = self.function_hash_mix();
        let fold_int = match self.int {
            IntType::I32 => None,
            IntType::I64 => Some(self.function_fold_i64()),
        };
        let fold_float = match self.float {
            FloatType::F32 => None,
            FloatType::F64 => Some(self.function_fold_i64()),
        };
        let fold = |instructions: &mut Instructions<'_, '_>, fold: Option<FunctionId>| {
            if let Some(fold) = fold {
                let _ = instructions.call(fold);
            }
        };
        let _ = match hash {
            Hash::I32 => instructions.call(mix),
            Hash::Int => {
                fold(instructions, fold_int);
                instructions.call(mix)
            }
            Hash::Float => {
                // `-0.0 + 0.0` is `0.0`, as `0.0 == -0.0`
                let _ = instructions
                    .float_const("0.0")
                    .float_add()
                    .float_reinterpret();
                fold(instructions, fold_float);
                instructions.call(mix)
            }
            Hash::Constant => instructions.drop().i32_const(SEED),
            Hash::I64 => {
                let fold = self.function_fold_i64();
                instructions.call(fold).call(mix)
            }
            // `-0.0 + 0.0` is `0.0`, as `0.0 == -0.0`
            Hash::F32 => instructions
                .f32_const(0.0)
                .f32_add()
                .f32_reinterpret()
                .call(mix),
            Hash::F64 => {
                let fold = self.function_fold_i64();
                instructions
                    .f64_const(0.0)
                    .f64_add()
                    .f64_reinterpret()
                    .call(fold)
                    .call(mix)
            }
            Hash::Call(function) => instructions.call(function),
            Hash::Hook(function) => {
                let _ = instructions.call(function);
                fold(instructions, fold_int);
                instructions
            }
        };
    }

    /// `(i64) -> i32`: the two halves xored.
    fn function_fold_i64(&mut self) -> FunctionId {
        self.get_function_builtin(BuiltinFunction::FoldI64, |s| {
            // params
            let value = s.wasm_local(ValType::I64);
            let mut function = Function::new(s, "_fold_i64", &[value], &[ValType::I32]);
            let _ = function.extend_instructions(s).fold_i64(value).end();
            function.finish(s)
        })
    }

    pub(super) fn function_hash(&mut self, type_: &Arc<Type>) -> Hash {
        if let Some(representation) = self.representation(type_) {
            return self.function_hash(&representation);
        } else if let Some((module, hooks)) = self.type_hooks(type_) {
            let fn_type = type_::fn_(vec![type_.clone()], type_::int());
            return Hash::Hook(self.function_hook(&module, hooks.hash, fn_type));
        } else if type_.fn_types().is_some() {
            return Hash::Constant;
        } else if type_.is_int() {
            return Hash::Int;
        } else if type_.is_float() {
            return Hash::Float;
        } else if type_.is_utf_codepoint() {
            return Hash::I32;
        } else if let Some((CustomType::Enum { .. }, _)) = self.custom_type(type_) {
            return Hash::I32;
        } else if let Some((CustomType::External { val_type, .. }, _)) = self.custom_type(type_) {
            return match val_type {
                ValType::I32 => Hash::I32,
                ValType::I64 => Hash::I64,
                ValType::F32 => Hash::F32,
                ValType::F64 => Hash::F64,
                ValType::V128 | ValType::Ref(_) => {
                    panic!("values of type {val_type:?} can't be hashed")
                }
            };
        }
        let builtin = BuiltinFunction::Hash(self.val_type(type_), self.type_pretty_name(type_));
        let index = if self.is_struct(type_) {
            let struct_type = type_.clone();
            self.get_function_constructor(builtin, type_, 0, move |s| s.code_hash(&struct_type))
        } else {
            let type_ = type_.clone();
            self.get_function_builtin(builtin, |s| s.code_hash(&type_))
        };
        Hash::Call(index)
    }

    /// The finalizer of MurmurHash3, `(i32) -> i32`.
    pub(super) fn function_hash_mix(&mut self) -> FunctionId {
        self.get_function_builtin(BuiltinFunction::HashMix, |s| s.code_hash_mix())
    }

    fn code_hash_mix(&mut self) -> FunctionId {
        // params
        let h = self.wasm_local(ValType::I32);
        let mut function = Function::new(self, "_hash_mix", &[h], &[ValType::I32]);
        let mut instructions = function.extend_instructions(self);
        for (shift, multiplier) in [(16, 0x85eb_ca6b_u32 as i32), (13, 0xc2b2_ae35_u32 as i32)] {
            let _ = instructions
                .local_get(h)
                .local_get(h)
                .i32_const(shift)
                .i32_shr_u()
                .i32_xor()
                .i32_const(multiplier)
                .i32_mul()
                .local_set(h);
        }
        let _ = instructions
            .local_get(h)
            .local_get(h)
            .i32_const(16)
            .i32_shr_u()
            .i32_xor()
            .end();
        function.finish(self)
    }

    fn code_hash(&mut self, type_: &Arc<Type>) -> FunctionId {
        if type_.is_string() {
            self.code_string_hash()
        } else if let Some(types) = type_.tuple_types() {
            let type_index = self.tuple_type_index(types.clone());
            self.code_composite_hash(type_index, types)
        } else if is_never_type(type_) {
            let val_type = self.val_type(type_);
            self.code_never(&[val_type], &[ValType::I32])
        } else if let Some((custom_type, args)) = self.custom_type(type_) {
            match custom_type {
                CustomType::Struct {
                    custom_type,
                    constructor,
                } => {
                    let (type_index, types) =
                        self.mono_struct_type_index(type_, &custom_type, &constructor, &args);
                    self.code_composite_hash(type_index, types)
                }
                CustomType::Union { .. } => self.code_union_hash(type_),
                CustomType::Array => {
                    let element = args.first().expect("array element type");
                    self.code_array_hash(element)
                }
                CustomType::External { .. }
                | CustomType::Runtime { .. }
                | CustomType::Enum { .. } => {
                    panic!("external/enum types should not reach hash code generation")
                }
            }
        } else {
            panic!("unexpected type should not reach hash code generation")
        }
    }

    /// FNV-1a over the code units, then mixed: over the bytes of the view
    /// with UTF8, and over the UTF-16 units with JSString, which are not
    /// converted.
    fn code_string_hash(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        let bytes_vt = self.bytes.val_type();
        let mix = self.function_hash_mix();
        // params
        let s = self.wasm_local(string_vt);
        // locals
        let bytes = self.wasm_local(bytes_vt);
        let i = self.wasm_local(ValType::I32);
        let end = self.wasm_local(ValType::I32);
        let h = self.wasm_local(ValType::I32);
        let utf8 = matches!(self.string, StringType::Utf8(_));
        let mut function = Function::new(self, "_hash_string", &[s], &[ValType::I32]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions.i32_const(0x811c_9dc5_u32 as i32).local_set(h);
        let _ = if utf8 {
            instructions
                .string_utf8_view(s, bytes, i, end)
                .local_get(i)
                .local_get(end)
                .i32_add()
                .local_set(end)
        } else {
            instructions.local_get(s).string_len().local_set(end)
        };
        #[rustfmt::skip]
        let _ = instructions
            .block_(InstrSeqType::Simple(None), |block_s| {
                let block_id = block_s.id();
                let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                    let loop_id = loop_s.id();
                    let _ = loop_s
                        .local_get(i)
                        .local_get(end)
                        .i32_ge_u()
                        .br_if(block_id)
                        .local_get(h);
                    let _ = if utf8 {
                        loop_s.local_get(bytes).local_get(i).bytes_get()
                    } else {
                        loop_s.local_get(s).local_get(i).string_get()
                    };
                    let _ = loop_s
                        .i32_xor()
                        .i32_const(MULTIPLIER)
                        .i32_mul()
                        .local_set(h)
                        .i32_inc(i)
                        .br(loop_id);
                });
            })
            .local_get(h)
            .call(mix)
            .end();
        function.finish(self)
    }

    /// Combines the hashes of the fields in order.
    fn code_composite_hash(
        &mut self,
        type_index: TypeId,
        types: impl IntoIterator<Item = Arc<Type>>,
    ) -> FunctionId {
        let composite_vt = self.val_type_ref(type_index);
        let mix = self.function_hash_mix();
        // params
        let value = self.wasm_local(composite_vt);
        // locals
        let h = self.wasm_local(ValType::I32);
        let types = types.into_iter().collect_vec();
        let field_indices = self.field_indices(&types);
        let hashes = types
            .iter()
            .map(|type_| self.function_hash(type_))
            .collect_vec();
        let mut function = Function::new(self, "_hash_composite", &[value], &[ValType::I32]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions.i32_const(SEED).local_set(h);
        for (field_index, hash) in field_indices.into_iter().zip(hashes) {
            // h = (h ^ hash(field)) * MULTIPLIER
            let _ = instructions
                .local_get(h)
                .local_get(value)
                .field_get(type_index, field_index);
            self.hash_instructions(&mut instructions, hash);
            let _ = instructions
                .i32_xor()
                .i32_const(MULTIPLIER)
                .i32_mul()
                .local_set(h);
        }
        let _ = instructions.local_get(h).call(mix).end();
        function.finish(self)
    }

    /// `mix(_hash_into(value, SEED))`.
    fn code_union_hash(&mut self, type_: &Arc<Type>) -> FunctionId {
        let hash_into = self.function_union_hash_into(type_);
        let union_vt = self.val_type(type_);
        let mix = self.function_hash_mix();
        // params
        let value = self.wasm_local(union_vt);
        let mut function = Function::new(self, "_hash_union", &[value], &[ValType::I32]);
        let _ = function
            .extend_instructions(self)
            .local_get(value)
            .i32_const(SEED)
            .call(hash_into)
            .call(mix)
            .end();
        function.finish(self)
    }

    /// `_hash_into(T)` of a union: `fn(T, h) -> h`, combining the hash of the
    /// value into `h`.
    fn function_union_hash_into(&mut self, type_: &Arc<Type>) -> FunctionId {
        let builtin = BuiltinFunction::HashInto(self.val_type(type_), self.type_pretty_name(type_));
        let type_ = type_.clone();
        self.get_function_builtin(builtin, |s| s.code_union_hash_into(&type_))
    }

    /// Dispatches on the tag to the `_hash_into` of the variant, with a tail
    /// call.
    fn code_union_hash_into(&mut self, type_: &Arc<Type>) -> FunctionId {
        let Some((CustomType::Union { custom_type, .. }, args)) = self.custom_type(type_) else {
            panic!("union type expected")
        };
        let layout = self.union_layout(type_);
        let null_tag = layout.null_tag;
        let supertype_index = self.mono_union_supertype_index(type_, &custom_type);
        let union_vt = self.val_type(type_);
        // params
        let value = self.wasm_local(union_vt);
        let h = self.wasm_local(ValType::I32);
        // locals
        let tag = self.wasm_local(ValType::I32);
        let mut variant_hashes: Vec<Option<(TypeId, FunctionId)>> =
            Vec::with_capacity(custom_type.constructors.len());
        for (i, constructor) in custom_type.constructors.iter().enumerate() {
            if null_tag == Some(i) {
                variant_hashes.push(None);
            } else {
                let (_, type_index, types) =
                    self.union_subtype_index(type_, &custom_type, constructor, &args);
                let mapping = layout.fields(i).to_vec();
                let name = self.type_pretty_name(type_) + "." + constructor.name.clone();
                let builtin = BuiltinFunction::HashInto(self.val_type_ref(type_index), name);
                let union_type = type_.clone();
                let hash_index = self.get_function_constructor(builtin, type_, i, move |s| {
                    s.code_variant_hash_into(type_index, &mapping, i as i32, types, &union_type)
                });
                variant_hashes.push(Some((type_index, hash_index)));
            }
        }
        let mut function = Function::new(self, "_hash_into_union", &[value, h], &[ValType::I32]);
        let mut instructions = function.extend_instructions(self);
        if let Some(null_tag) = null_tag {
            // The fields of the null variant, of unit types, have the value 0.
            let constructor = custom_type
                .constructors
                .get(null_tag)
                .expect("null variant for union type");
            let hashes = Monomorphizer::variant_constructor(&custom_type, constructor, &args)
                .iter()
                .map(|type_| self.function_hash(type_))
                .collect_vec();
            let _ = instructions.local_get(value).ref_is_null().if_(
                InstrSeqType::Simple(None),
                |body| {
                    // h = (h ^ (SEED ^ tag)) * MULTIPLIER
                    let _ = body
                        .local_get(h)
                        .i32_const(SEED ^ null_tag as i32)
                        .i32_xor()
                        .i32_const(MULTIPLIER)
                        .i32_mul()
                        .local_set(h);
                    for hash in hashes {
                        // h = (h ^ hash(0)) * MULTIPLIER
                        let _ = body.local_get(h).i32_const(0);
                        self.hash_instructions(body, hash);
                        let _ = body.i32_xor().i32_const(MULTIPLIER).i32_mul().local_set(h);
                    }
                    let _ = body.local_get(h).return_();
                },
            );
        }
        if !layout.tagged {
            // The value is of the only variant that is not null.
            let (type_index, hash_index) = variant_hashes
                .into_iter()
                .flatten()
                .exactly_one()
                .expect("one variant that is not null");
            let _ = instructions
                .local_get(value)
                .ref_cast_non_null(HeapType::Concrete(type_index))
                .local_get(h)
                .return_call(hash_index)
                .end();
            return function.finish(self);
        }
        let _ = instructions
            .local_get(value)
            .ref_as_non_null()
            .struct_get(supertype_index, 0)
            .local_set(tag);
        for (i, info) in variant_hashes.iter().enumerate() {
            if let Some((type_index, hash_index)) = *info {
                let _ = instructions
                    .local_get(tag)
                    .i32_const(i as i32)
                    .i32_eq()
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body
                            .local_get(value)
                            .ref_cast_non_null(HeapType::Concrete(type_index))
                            .local_get(h)
                            .return_call(hash_index);
                    });
            }
        }
        let _ = instructions.unreachable().end();
        function.finish(self)
    }

    /// `fn(variant, h) -> h`: combines the tag and the hashes of the fields
    /// into `h`. A field of the type of the union (the rest of a list) is
    /// combined last, with a tail call, so that long lists don't overflow the
    /// stack.
    fn code_variant_hash_into(
        &mut self,
        type_index: TypeId,
        field_mapping: &[Option<u32>],
        tag: i32,
        types: Vec<Arc<Type>>,
        parent: &Arc<Type>,
    ) -> FunctionId {
        let variant_vt = self.val_type_ref(type_index);
        // params
        let value = self.wasm_local(variant_vt);
        let h = self.wasm_local(ValType::I32);
        let last = types.iter().rposition(|type_| type_.same_as(parent));
        let fields = types
            .iter()
            .enumerate()
            .filter(|(index, _)| Some(*index) != last)
            .map(|(index, type_)| (index, self.function_hash(type_)))
            .collect_vec();
        let mut function = Function::new(self, "_hash_into_variant", &[value, h], &[ValType::I32]);
        let mut instructions = function.extend_instructions(self);
        // h = (h ^ (SEED ^ tag)) * MULTIPLIER
        let _ = instructions
            .local_get(h)
            .i32_const(SEED ^ tag)
            .i32_xor()
            .i32_const(MULTIPLIER)
            .i32_mul()
            .local_set(h);
        let field_index = |gleam_index: usize| {
            field_mapping
                .get(gleam_index)
                .copied()
                .expect("field index")
        };
        for (gleam_index, hash) in fields {
            // h = (h ^ hash(field)) * MULTIPLIER
            let _ = instructions
                .local_get(h)
                .local_get(value)
                .field_get(type_index, field_index(gleam_index));
            self.hash_instructions(&mut instructions, hash);
            let _ = instructions
                .i32_xor()
                .i32_const(MULTIPLIER)
                .i32_mul()
                .local_set(h);
        }
        if let Some(last) = last {
            let hash_into = self.function_union_hash_into(parent);
            let _ = instructions
                .local_get(value)
                .field_get(type_index, field_index(last))
                .local_get(h)
                .return_call(hash_into)
                .end();
        } else {
            let _ = instructions.local_get(h).end();
        }
        function.finish(self)
    }
}
