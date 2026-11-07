// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use super::*;
use strum::{EnumIter, EnumString, IntoEnumIterator, IntoStaticStr};

/// A type in the keys of the generated functions: its name qualified by its
/// module, as two types can have the same printed name.
#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub(super) struct TypeKey(EcoString);

impl TypeKey {
    pub(super) fn new(type_: &Arc<Type>) -> Self {
        Self(qualified_type_name(type_))
    }

    /// The key of the variant `constructor` of `type_`.
    pub(super) fn variant(type_: &Arc<Type>, constructor: &str) -> Self {
        Self(ecow::eco_format!(
            "{}.{constructor}",
            qualified_type_name(type_)
        ))
    }
}

impl std::fmt::Display for TypeKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub(super) enum BuiltinFunction {
    Equal(ValType, TypeKey),
    ListRepr(ValType, TypeKey),
    TupleRepr(ValType, TypeKey),
    FunctionRepr(ValType, TypeKey),
    CustomTypeRepr(ValType, TypeKey),
    VariantConstructor(Vec<ValType>, ValType, TypeKey),
    Inspect(ValType, TypeKey),
    InspectHook(ValType, TypeKey),
    Hash(ValType, TypeKey),
    HashInto(ValType, TypeKey),
    HashExternal(ValType, TypeKey),
    HashMix,
    FoldI64,
    /// `_dynamic_cast(T)`, with the representations of `T` and `Dynamic`.
    DynamicCast(ValType, ValType, TypeKey),
    /// The functions of the table of a boxed `Dynamic` of type `T`: equal,
    /// hash (with the representation of `Int`) and inspect (with the
    /// representation of `String`).
    DynamicEqual(TypeKey),
    DynamicHash(ValType, TypeKey),
    DynamicInspect(ValType, TypeKey),
    /// The `view` of the table of a boxed `Dynamic` of type `T`, with the
    /// representation of `Reflection`, and the `view` of one constructor of
    /// `T`, with the representation of the constructor.
    DynamicView(ValType, TypeKey),
    ConstructorView(ValType, ValType, TypeKey),
    /// The conversion of a value of the host type `T` to JavaScript and
    /// back, with the representation of `T` (see `host.rs`).
    ToJs(ValType, TypeKey),
    FromJs(ValType, TypeKey),
    /// The hook `fields` of the table of a boxed value of the host type `T`,
    /// with the representation of its result.
    JsFields(ValType, TypeKey),
    /// The function that JavaScript calls to call a closure of the function
    /// type `T`, and the code of the closures of `T` that call a JavaScript
    /// function, with their params and result (see `host.rs`).
    JsTrampoline(Vec<ValType>, ValType, TypeKey),
    JsCall(Vec<ValType>, ValType, TypeKey),
    /// `_catch` for the function type `T`, with its params and result (see
    /// `panic`).
    Catch(Vec<ValType>, ValType, TypeKey),
    /// A function of `Array`, with its params, result and element type.
    Array {
        function: ArrayFunction,
        params: Vec<ValType>,
        result: ValType,
        element: TypeKey,
    },
}

impl BuiltinFunction {
    pub(super) fn name(&self) -> EcoString {
        match self {
            BuiltinFunction::Equal(_, name) => format!("_equal({name})").into(),
            BuiltinFunction::ListRepr(_, name) => format!("_repr({name})").into(),
            BuiltinFunction::TupleRepr(_, name) => format!("_repr({name})").into(),
            BuiltinFunction::FunctionRepr(_, name) => format!("_repr({name})").into(),
            BuiltinFunction::CustomTypeRepr(_, name) => format!("_repr({name})").into(),
            BuiltinFunction::VariantConstructor(_, _, name) => format!("_create({name})").into(),
            BuiltinFunction::Inspect(_, name) => format!("_inspect({name})").into(),
            BuiltinFunction::InspectHook(_, name) => format!("_inspect_hook({name})").into(),
            BuiltinFunction::Hash(_, name) => format!("_hash({name})").into(),
            BuiltinFunction::HashInto(_, name) => format!("_hash_into({name})").into(),
            BuiltinFunction::HashExternal(_, name) => format!("_hash_int({name})").into(),
            BuiltinFunction::HashMix => "_hash_mix".into(),
            BuiltinFunction::FoldI64 => "_fold_i64".into(),
            BuiltinFunction::DynamicCast(_, _, name) => format!("_dynamic_cast({name})").into(),
            BuiltinFunction::DynamicEqual(name) => format!("_dynamic_equal({name})").into(),
            BuiltinFunction::DynamicHash(_, name) => format!("_dynamic_hash({name})").into(),
            BuiltinFunction::DynamicInspect(_, name) => format!("_dynamic_inspect({name})").into(),
            BuiltinFunction::DynamicView(_, name) => format!("_dynamic_view({name})").into(),
            BuiltinFunction::ConstructorView(_, _, name) => format!("_view({name})").into(),
            BuiltinFunction::ToJs(_, name) => format!("_to_js({name})").into(),
            BuiltinFunction::FromJs(_, name) => format!("_from_js({name})").into(),
            BuiltinFunction::JsFields(_, name) => format!("_js_fields({name})").into(),
            BuiltinFunction::JsTrampoline(_, _, name) => format!("_js_trampoline({name})").into(),
            BuiltinFunction::JsCall(_, _, name) => format!("_js_call({name})").into(),
            BuiltinFunction::Catch(_, _, name) => format!("_catch({name})").into(),
            BuiltinFunction::Array {
                function, element, ..
            } => format!("{}({element})", function.name()).into(),
        }
    }
}

/// A builtin that a Gleam program declares as an `@external`, named as the
/// external function.
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug, EnumIter, EnumString, IntoStaticStr)]
pub(super) enum BuiltinFunctionExternal {
    #[strum(serialize = "_string_concat")]
    StringConcat,
    #[strum(serialize = "_i32_to_int")]
    I32ToInt,
    #[strum(serialize = "_int_to_i32")]
    IntToI32,
    #[strum(serialize = "_i64_to_int")]
    I64ToInt,
    #[strum(serialize = "_int_to_i64")]
    IntToI64,
    #[strum(serialize = "_f32_to_float")]
    F32ToFloat,
    #[strum(serialize = "_float_to_f32")]
    FloatToF32,
    #[strum(serialize = "_f64_to_float")]
    F64ToFloat,
    #[strum(serialize = "_float_to_f64")]
    FloatToF64,
    #[strum(serialize = "_i32_add")]
    I32Add,
    #[strum(serialize = "_i32_sub")]
    I32Sub,
    #[strum(serialize = "_i32_mul")]
    I32Mul,
    #[strum(serialize = "_i32_and")]
    I32And,
    #[strum(serialize = "_i32_or")]
    I32Or,
    #[strum(serialize = "_i32_xor")]
    I32Xor,
    #[strum(serialize = "_i32_shift_left")]
    I32ShiftLeft,
    #[strum(serialize = "_i32_shift_right")]
    I32ShiftRight,
    #[strum(serialize = "_i32_shift_right_unsigned")]
    I32ShiftRightUnsigned,
    #[strum(serialize = "_i64_add")]
    I64Add,
    #[strum(serialize = "_i64_sub")]
    I64Sub,
    #[strum(serialize = "_i64_mul")]
    I64Mul,
    #[strum(serialize = "_i64_and")]
    I64And,
    #[strum(serialize = "_i64_or")]
    I64Or,
    #[strum(serialize = "_i64_xor")]
    I64Xor,
    #[strum(serialize = "_i64_shift_left")]
    I64ShiftLeft,
    #[strum(serialize = "_i64_shift_right")]
    I64ShiftRight,
    #[strum(serialize = "_i64_shift_right_unsigned")]
    I64ShiftRightUnsigned,
    #[strum(serialize = "_repr_int")]
    IntRepr,
    #[strum(serialize = "_repr_float")]
    FloatRepr,
    #[strum(serialize = "_repr_utf_code_point")]
    UtfCodepointRepr,
    #[strum(serialize = "_repr_string")]
    StringRepr,
    #[strum(serialize = "_string_to_memory")]
    StringToMemory,
    #[strum(serialize = "_memory_to_string")]
    MemoryToString,
    #[strum(serialize = "_int_bitwise_and")]
    IntBitwiseAnd,
    #[strum(serialize = "_int_bitwise_or")]
    IntBitwiseOr,
    #[strum(serialize = "_int_bitwise_exclusive_or")]
    IntBitwiseExclusiveOr,
    #[strum(serialize = "_int_bitwise_not")]
    IntBitwiseNot,
    #[strum(serialize = "_int_bitwise_shift_left")]
    IntBitwiseShiftLeft,
    #[strum(serialize = "_int_bitwise_shift_right")]
    IntBitwiseShiftRight,
    #[strum(serialize = "_int_to_float")]
    IntToFloat,
    #[strum(serialize = "_float_floor")]
    FloatFloor,
    #[strum(serialize = "_float_ceiling")]
    FloatCeiling,
    #[strum(serialize = "_float_truncate")]
    FloatTruncate,
    #[strum(serialize = "_float_round")]
    FloatRound,
    #[strum(serialize = "_float_square_root")]
    FloatSquareRoot,
    #[strum(serialize = "_string_byte_size")]
    StringByteSize,
    #[strum(serialize = "_string_byte_slice")]
    StringByteSlice,
    #[strum(serialize = "_string_codeunit_slice")]
    StringCodeunitSlice,
    #[strum(serialize = "_string_pop_codeunit")]
    StringPopCodeunit,
    #[strum(serialize = "_string_codeunit_size")]
    StringCodeunitSize,
    #[strum(serialize = "_string_find")]
    StringFind,
    #[strum(serialize = "_string_starts_with")]
    StringStartsWith,
    #[strum(serialize = "_string_ends_with")]
    StringEndsWith,
    #[strum(serialize = "_string_remove_prefix")]
    StringRemovePrefix,
    #[strum(serialize = "_string_remove_suffix")]
    StringRemoveSuffix,
    #[strum(serialize = "_string_less_than")]
    StringLessThan,
    #[strum(serialize = "_string_concat_list")]
    StringConcatList,
    #[strum(serialize = "_string_join")]
    StringJoin,
    #[strum(serialize = "_utf_codepoint_to_int")]
    UtfCodepointToInt,
    #[strum(serialize = "_unsafe_int_to_utf_codepoint")]
    UnsafeIntToUtfCodepoint,
    #[strum(serialize = "_string_pop_grapheme")]
    StringPopGrapheme,
    #[strum(serialize = "_bytes_new")]
    BytesNew,
    #[strum(serialize = "_bytes_size")]
    BytesSize,
    #[strum(serialize = "_bytes_get")]
    BytesGet,
    #[strum(serialize = "_bytes_set")]
    BytesSet,
    #[strum(serialize = "_bytes_is_utf8")]
    BytesIsUtf8,
    #[strum(serialize = "_bits_copy")]
    BitsCopy,
    #[strum(serialize = "_bytes_write_string")]
    BytesWriteString,
    #[strum(serialize = "_bytes_to_memory")]
    BytesToMemory,
    #[strum(serialize = "_bytes_from_memory")]
    BytesFromMemory,
    #[strum(serialize = "_string_bits")]
    StringBits,
    #[strum(serialize = "_bytes_to_string")]
    BytesToString,
    #[strum(serialize = "_float64_byte")]
    Float64Byte,
    #[strum(serialize = "_float32_byte")]
    Float32Byte,
    #[strum(serialize = "_float64_from_bits")]
    Float64FromBits,
    #[strum(serialize = "_float32_from_bits")]
    Float32FromBits,
    #[strum(serialize = "_cell_new")]
    CellNew,
    #[strum(serialize = "_cell_get")]
    CellGet,
    #[strum(serialize = "_cell_set")]
    CellSet,
    #[strum(serialize = "_global_get")]
    GlobalGet,
    #[strum(serialize = "_global_set")]
    GlobalSet,
}

pub fn builtin_function_names() -> Vec<EcoString> {
    BuiltinFunctionExternal::iter()
        .map(|b| b.name().into())
        .chain([
            INSPECT.into(),
            HASH.into(),
            DYNAMIC_CAST.into(),
            CATCH.into(),
        ])
        .chain(ArrayFunction::all().iter().map(|f| f.name().into()))
        .collect()
}

impl BuiltinFunctionExternal {
    pub(super) fn by_name(name: &str) -> Option<BuiltinFunctionExternal> {
        name.parse().ok()
    }

    pub(super) fn name(self) -> &'static str {
        self.into()
    }

    /// The type of the params and of the result.
    pub(super) fn type_(&self) -> (Vec<BuiltinType>, BuiltinType) {
        use BuiltinFunctionExternal as B;
        use type_::{bit_array, bool, float, int, list, nil, result, string, tuple, utf_codepoint};
        let ext = BuiltinType::External;
        let dynamic = || {
            Arc::new(Type::Named {
                publicity: Publicity::Public,
                package: RUNTIME_PACKAGE.into(),
                module: DYNAMIC_MODULE.into(),
                name: "Dynamic".into(),
                arguments: vec![],
                inferred_variant: None,
            })
        };
        let (params, result) = match self {
            B::StringConcat => (vec![string(), string()], string()),
            B::I32ToInt => return (vec![ext("I32")], int().into()),
            B::IntToI32 => return (vec![int().into()], ext("I32")),
            B::I64ToInt => return (vec![ext("I64")], int().into()),
            B::IntToI64 => return (vec![int().into()], ext("I64")),
            B::F32ToFloat => return (vec![ext("F32")], float().into()),
            B::FloatToF32 => return (vec![float().into()], ext("F32")),
            B::F64ToFloat => return (vec![ext("F64")], float().into()),
            B::FloatToF64 => return (vec![float().into()], ext("F64")),
            B::I32Add | B::I32Sub | B::I32Mul | B::I32And | B::I32Or | B::I32Xor => {
                return (vec![ext("I32"), ext("I32")], ext("I32"));
            }
            B::I32ShiftLeft | B::I32ShiftRight | B::I32ShiftRightUnsigned => {
                return (vec![ext("I32"), int().into()], ext("I32"));
            }
            B::I64Add | B::I64Sub | B::I64Mul | B::I64And | B::I64Or | B::I64Xor => {
                return (vec![ext("I64"), ext("I64")], ext("I64"));
            }
            B::I64ShiftLeft | B::I64ShiftRight | B::I64ShiftRightUnsigned => {
                return (vec![ext("I64"), int().into()], ext("I64"));
            }
            B::IntRepr => return (vec![int().into(), ext("I32")], ext("I32")),
            B::FloatRepr => return (vec![float().into(), ext("I32")], ext("I32")),
            B::UtfCodepointRepr => return (vec![utf_codepoint().into(), ext("I32")], ext("I32")),
            B::StringRepr | B::StringToMemory => {
                return (vec![string().into(), ext("I32")], ext("I32"));
            }
            B::MemoryToString => return (vec![ext("I32"), ext("I32")], string().into()),
            B::IntBitwiseAnd
            | B::IntBitwiseOr
            | B::IntBitwiseExclusiveOr
            | B::IntBitwiseShiftLeft
            | B::IntBitwiseShiftRight => (vec![int(), int()], int()),
            B::IntBitwiseNot => (vec![int()], int()),
            B::IntToFloat => (vec![int()], float()),
            B::FloatFloor | B::FloatCeiling | B::FloatSquareRoot => (vec![float()], float()),
            B::FloatTruncate | B::FloatRound => (vec![float()], int()),
            B::StringByteSize => (vec![string()], int()),
            B::StringByteSlice | B::StringCodeunitSlice => (vec![string(), int(), int()], string()),
            B::StringPopCodeunit => (vec![string()], tuple(vec![int(), string()])),
            B::StringCodeunitSize => (vec![string()], int()),
            B::StringFind => (vec![string(), string(), int()], int()),
            B::StringStartsWith | B::StringEndsWith | B::StringLessThan => {
                (vec![string(), string()], bool())
            }
            B::StringRemovePrefix | B::StringRemoveSuffix => (vec![string(), string()], string()),
            B::StringConcatList => (vec![list(string())], string()),
            B::StringJoin => (vec![list(string()), string()], string()),
            B::UtfCodepointToInt => (vec![utf_codepoint()], int()),
            B::UnsafeIntToUtfCodepoint => (vec![int()], utf_codepoint()),
            B::StringPopGrapheme => (
                vec![string()],
                result(tuple(vec![string(), string()]), nil()),
            ),
            B::BytesNew => return (vec![int().into()], ext("Words")),
            B::BytesSize => return (vec![ext("Words")], int().into()),
            B::BytesGet => return (vec![ext("Words"), int().into()], int().into()),
            B::BytesSet => {
                return (vec![ext("Words"), int().into(), int().into()], nil().into());
            }
            B::BytesIsUtf8 => {
                return (
                    vec![ext("Words"), int().into(), int().into()],
                    bool().into(),
                );
            }
            B::BitsCopy => {
                let params = vec![
                    ext("Words"),
                    int().into(),
                    ext("Words"),
                    int().into(),
                    int().into(),
                ];
                return (params, nil().into());
            }
            B::BytesWriteString => {
                return (
                    vec![ext("Words"), int().into(), string().into()],
                    nil().into(),
                );
            }
            B::BytesToMemory => {
                let params = vec![ext("Words"), int().into(), int().into(), ext("I32")];
                return (params, nil().into());
            }
            B::BytesFromMemory => return (vec![ext("I32"), int().into()], ext("Words")),
            B::StringBits => (vec![string()], bit_array()),
            B::BytesToString => {
                return (
                    vec![ext("Words"), int().into(), int().into()],
                    string().into(),
                );
            }
            B::Float64Byte | B::Float32Byte => (vec![float(), int()], int()),
            B::Float64FromBits => (vec![int(), int()], float()),
            B::Float32FromBits => (vec![int()], float()),
            B::CellNew => return (vec![int().into()], ext("Cell")),
            B::CellGet => return (vec![ext("Cell")], int().into()),
            B::CellSet => return (vec![ext("Cell"), int().into()], nil().into()),
            B::GlobalGet => (vec![], dynamic()),
            B::GlobalSet => (vec![dynamic()], nil()),
        };
        (params.into_iter().map(Into::into).collect(), result.into())
    }
}

/// A param or the result of a `BuiltinFunctionExternal`: a Gleam type, or
/// an external type of the builtins module, by its name, whose values are
/// wasm values (see `Generator::wasm_external`).
#[derive(Debug)]
pub(super) enum BuiltinType {
    Gleam(Arc<Type>),
    External(&'static str),
}

impl From<Arc<Type>> for BuiltinType {
    fn from(type_: Arc<Type>) -> Self {
        BuiltinType::Gleam(type_)
    }
}

impl<'a> Generator<'a> {
    pub(super) fn function_start(&mut self, constants: FunctionId) -> FunctionId {
        self.code_command_start();
        self.code_start(constants)
    }

    /// Exports `_start`, the entry point of a WASI command, which calls
    /// `main`. `main` is not called by the start function, so the hosts that
    /// call `_start` or `main` after instantiating the module do not run it
    /// twice. A failure that `main` does not catch is printed by
    /// `_panic_exit`, see `panic`.
    fn code_command_start(&mut self) {
        let Some(main) = self.main else {
            return;
        };
        let uncaught = match (self.panic_exit, self.failure_tag) {
            (Some(panic_exit), Some(tag)) => {
                let params = self.panic_params();
                let failure = InstrSeqType::new(&mut self.wasm_module.types, &[], &params);
                Some((panic_exit, tag, failure))
            }
            _ => None,
        };
        let mut function = Function::new(self, "_start", &[], &[]);
        let mut instructions = function.extend_instructions(self);
        match uncaught {
            Some((panic_exit, tag, failure)) => {
                let _ = instructions
                    .block_(failure, |caught| {
                        let label = caught.id();
                        let catches = vec![walrus::ir::TryTableCatch::Catch { tag, label }];
                        let _ = caught
                            .try_table(None, catches, |body| {
                                let _ = body.call(main).drop();
                            })
                            .return_();
                    })
                    .call(panic_exit);
            }
            None => {
                let _ = instructions.call(main).drop();
            }
        }
        let _ = instructions.end();
        let start = function.finish(self);
        let _ = self.wasm_module.exports.add("_start", start);
    }

    /// The start function: configures the rust builtins, creates the strings
    /// and calls `constants`.
    fn code_start(&mut self, constants: FunctionId) -> FunctionId {
        let mut function = Function::new(self, "$start", &[], &[]);
        let mut instructions = function.extend_instructions(self);
        // the configuration of the rust builtins
        if let Some(config) = self.find_global(CONFIG) {
            let int_bits = match self.int {
                IntType::I32 => 32,
                IntType::I64 => 64,
            };
            let float_bits = match self.float {
                FloatType::F32 => 32,
                FloatType::F64 => 64,
            };
            let _ = instructions
                .i32_const(int_bits)
                .i32_const(float_bits)
                .call(config.func_id());
        }
        self.code_string_literals(&mut instructions);
        self.code_bit_arrays(&mut instructions);
        let _ = instructions.call(constants).end();
        function.finish(self)
    }

    /// Pushes a new array with `bytes`, which are put in a data segment.
    pub(super) fn push_bytes(&mut self, instructions: &mut Instructions<'_, '_>, bytes: Vec<u8>) {
        let length = bytes.len() as i32;
        let data = self.wasm_module.data.add(walrus::DataKind::Passive, bytes);
        let _ = instructions
            .i32_const(0)
            .i32_const(length)
            .array_new_data(self.bytes.index, data);
    }

    /// Pushes new words with `bytes` (see `WordsType`), which are put in a
    /// data segment: the bytes of each word in the reverse order, as an
    /// array of `i64` is read from it in little endian.
    pub(super) fn push_words(&mut self, instructions: &mut Instructions<'_, '_>, bytes: Vec<u8>) {
        let data: Vec<u8> = bytes
            .chunks(8)
            .flat_map(|chunk| {
                let word = chunk
                    .iter()
                    .enumerate()
                    .fold(0, |word, (i, byte)| word | u64::from(*byte) << (56 - 8 * i));
                word.to_le_bytes()
            })
            .collect();
        let length = (data.len() / 8) as i32;
        let data = self.wasm_module.data.add(walrus::DataKind::Passive, data);
        let _ = instructions
            .i32_const(0)
            .i32_const(length)
            .array_new_data(self.words.index, data);
    }

    /// Creates the constants of the module. It is generated before the start
    /// function, which creates the strings and the literal bit arrays and then
    /// calls it, because the
    /// values it constructs may generate functions that use new strings (see
    /// `get_function_constructor`).
    pub(super) fn function_constants(&mut self) -> FunctionId {
        let mut function = Function::new(self, "$constants", &[], &[]);
        let mut instructions = function.extend_instructions(self);
        let scope = Scope::Global(self.globals.clone());
        for const_ in self.consts.clone() {
            match const_ {
                WasmConst::Global { dest, src } => {
                    let _ = instructions.global_get(src).global_set(dest);
                }
                WasmConst::Constant {
                    global_index,
                    value,
                } => {
                    let _ = instructions
                        .constant(self, &scope, &value)
                        .global_set(global_index);
                }
                WasmConst::Struct {
                    global_index,
                    type_index,
                    tag,
                    elements,
                } => {
                    if let Some(tag) = tag {
                        let _ = instructions.i32_const(tag);
                    }
                    let _ = instructions
                        .constants(self, &scope, &elements)
                        .struct_new(type_index)
                        .global_set(global_index);
                }
                WasmConst::Var { global_index, name } => {
                    let _ = instructions
                        .global_get(self.find_global_expect(&name).global_id())
                        .global_set(global_index);
                }
            }
        }
        let _ = instructions.end();
        function.finish(self)
    }

    pub(super) fn variant_constructor(
        &mut self,
        params: Vec<Arc<Type>>,
        return_: Arc<Type>,
        variant: Variant,
    ) -> FunctionId {
        self.constructed(&return_, variant.tag.unwrap_or(0) as usize);
        let key = match &variant.custom_type {
            CustomType::Union { .. } => TypeKey::variant(&return_, &variant.constructor.name),
            CustomType::External { .. }
            | CustomType::Enum { .. }
            | CustomType::Struct { .. }
            | CustomType::Runtime { .. }
            | CustomType::Array => TypeKey::new(&return_),
        };
        let builtin = BuiltinFunction::VariantConstructor(
            self.val_types(params.iter().cloned()),
            self.val_type(&return_),
            key,
        );

        self.get_function_builtin(builtin, |s| {
            s.code_variant_constructor_for(return_, variant)
        })
    }

    pub(super) fn ok_variant_constructor(&mut self, ok: Arc<Type>, error: Arc<Type>) -> FunctionId {
        self.variant_constructor(
            vec![ok.clone()],
            type_::result(ok, error),
            self.variant_expect(PRELUDE_MODULE_NAME.into(), "Ok".into()),
        )
    }

    pub(super) fn error_variant_constructor(
        &mut self,
        ok: Arc<Type>,
        error: Arc<Type>,
    ) -> FunctionId {
        self.variant_constructor(
            vec![error.clone()],
            type_::result(ok, error),
            self.variant_expect(PRELUDE_MODULE_NAME.into(), "Error".into()),
        )
    }

    pub(super) fn function_eq(&mut self, type_: &Arc<Type>) -> Eq {
        if let Some(representation) = self.representation(type_) {
            return self.function_eq(&representation);
        } else if let Some((module, hooks)) = self.type_hooks(type_) {
            let fn_type = type_::fn_(vec![type_.clone(), type_.clone()], type_::bool());
            return Eq::Call(self.function_hook(&module, hooks.equal, fn_type));
        } else if type_.fn_types().is_some() {
            // Functions are equal when they are the same closure
            return Eq::Ref;
        } else if type_.is_int() {
            return Eq::Int;
        } else if type_.is_float() {
            return Eq::Float;
        } else if type_.is_utf_codepoint() {
            return Eq::I32;
        } else if let Some((CustomType::Enum { .. }, _)) = self.custom_type(type_) {
            return Eq::I32;
        } else if let Some((CustomType::External { val_type, .. }, _)) = self.custom_type(type_) {
            return match val_type {
                ValType::I32 => Eq::I32,
                ValType::I64 => Eq::I64,
                ValType::F32 => Eq::F32,
                ValType::F64 => Eq::F64,
                ValType::V128 | ValType::Ref(_) => {
                    panic!("values of type {val_type:?} can't be compared")
                }
            };
        }

        let builtin = BuiltinFunction::Equal(self.val_type(type_), TypeKey::new(type_));
        let index = if self.is_struct(type_) {
            let struct_type = type_.clone();
            self.get_function_constructor(builtin, type_, 0, move |s| s.code_eq(&struct_type))
        } else {
            let type_ = type_.clone();
            self.get_function_builtin(builtin, |s| s.code_eq(&type_))
        };
        Eq::Call(index)
    }

    fn function_variant_eq(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &TypedCustomType,
        constructor: &TypedRecordConstructor,
        args: &[Arc<Type>],
    ) -> (TypeId, FunctionId) {
        let (_, type_index, types) =
            self.union_subtype_index(type_, custom_type, constructor, args);
        let layout = self.union_layout(type_);
        let variant_index = custom_type
            .constructors
            .iter()
            .position(|c| c.name == constructor.name)
            .expect("constructor in union");
        let mapping = layout.fields(variant_index).to_vec();
        let key = TypeKey::variant(type_, &constructor.name);
        let builtin = BuiltinFunction::Equal(self.val_type_ref(type_index), key);
        let union_type = type_.clone();
        let eq_index = self.get_function_constructor(builtin, type_, variant_index, move |s| {
            s.code_composite_or_union_eq(type_index, Some(&mapping), types, Some(&union_type))
        });
        (type_index, eq_index)
    }

    fn code_eq(&mut self, type_: &Arc<Type>) -> FunctionId {
        if type_.is_string() {
            self.code_string_eq()
        } else if let Some(types) = type_.tuple_types() {
            let type_index = self.tuple_type_index(types.clone());
            self.code_composite_or_union_eq(type_index, None, types, None)
        } else if is_never_type(type_) {
            let val_type = self.val_type(type_);
            self.code_never(&[val_type, val_type], &[BOOL_VALTYPE])
        } else if let Some((custom_type, args)) = self.custom_type(type_) {
            match custom_type {
                CustomType::Struct {
                    custom_type,
                    constructor,
                } => {
                    let (type_index, types) =
                        self.mono_struct_type_index(type_, &custom_type, &constructor, &args);
                    self.code_composite_or_union_eq(type_index, None, types, Some(type_))
                }
                CustomType::Union { custom_type, .. } => {
                    let supertype_index = self.mono_union_supertype_index(type_, &custom_type);
                    self.code_union_eq(type_, supertype_index, &custom_type, &args)
                }
                CustomType::Array => {
                    let element = args.first().expect("array element type");
                    self.code_array_eq(element)
                }
                CustomType::External { .. }
                | CustomType::Runtime { .. }
                | CustomType::Enum { .. } => {
                    panic!("external/enum types should not reach code generation")
                }
            }
        } else {
            panic!("unexpected type should not reach equality code generation")
        }
    }

    /// A function on values of `never_type`, which do not exist.
    pub(super) fn code_never(&mut self, params: &[ValType], results: &[ValType]) -> FunctionId {
        let params = params
            .iter()
            .map(|val_type| self.wasm_local(*val_type))
            .collect_vec();
        let mut function = Function::new(self, "_never", &params, results);
        let _ = function.extend_instructions(self).unreachable().end();
        function.finish(self)
    }

    /// The equality of the fields `types` of a struct, a tuple or a variant
    /// of the union `parent`. The last field compared is a field of type
    /// `parent` when there is one (the rest of a list), otherwise the last
    /// field, and it is compared with a tail call, so comparing a long list
    /// doesn't grow the stack.
    fn code_composite_or_union_eq(
        &mut self,
        type_index: TypeId,
        field_mapping: Option<&[Option<u32>]>,
        types: Vec<Arc<Type>>,
        parent: Option<&Arc<Type>>,
    ) -> FunctionId {
        // The fields of unit types are not stored, and are always equal.
        let field_indices = field_mapping.map_or_else(|| self.field_indices(&types), <[_]>::to_vec);
        let fields = types
            .iter()
            .zip(field_indices)
            .filter_map(|(type_, field)| Some((type_, field?)))
            .collect_vec();
        let composite_vt = self.val_type_ref(type_index);
        // params
        let a = self.wasm_local(composite_vt); // composite
        let b = self.wasm_local(composite_vt); // composite
        // return Bool
        let mut function = Function::new(self, "_equal_composite", &[a, b], &[BOOL_VALTYPE]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(a)
            .local_get(b)
            .ref_eq()
            .if_(InstrSeqType::Simple(None), |body| {
                let _ = body
                    .bool_const(true)
                    .return_();
            });
        let last = parent
            .and_then(|parent| fields.iter().rposition(|(type_, _)| type_.same_as(parent)))
            .or(fields.len().checked_sub(1));
        let order = fields
            .iter()
            .enumerate()
            .filter(|(index, _)| Some(*index) != last)
            .chain(last.and_then(|last| fields.get(last).map(|field| (last, field))));
        for (index, (type_, field_index)) in order {
            let field_index = *field_index;
            let eq = self.function_eq(type_);
            let _ = instructions
                .local_get(a)
                .struct_get(type_index, field_index)
                .local_get(b)
                .struct_get(type_index, field_index);
            if Some(index) == last {
                let _ = instructions.eq_tail(eq).end();
                return function.finish(self);
            }
            #[rustfmt::skip]
            let _ = instructions
                .eq(eq)
                .bool_not()
                .if_(InstrSeqType::Simple(None), |body| {
                    let _ = body
                        .bool_const(false)
                        .return_();
                });
        }
        let _ = instructions.bool_const(true).end();
        function.finish(self)
    }

    fn code_union_eq(
        &mut self,
        type_: &Arc<Type>,
        supertype_index: TypeId,
        custom_type: &TypedCustomType,
        args: &[Arc<Type>],
    ) -> FunctionId {
        let layout = self.union_layout(type_);
        let null_tag = layout.null_tag;
        let tagged = layout.tagged;
        let composite_vt = if null_tag.is_some() {
            self.val_type_ref_nullable(supertype_index)
        } else {
            self.val_type_ref(supertype_index)
        };
        // params
        let a = self.wasm_local(composite_vt); // union
        let b = self.wasm_local(composite_vt); // union
        // locals
        let tag = self.wasm_local(ValType::I32);
        // return Bool

        // Pre-compute variant eq pairs for non-null constructors
        let mut variant_eqs: Vec<Option<(TypeId, FunctionId)>> =
            Vec::with_capacity(custom_type.constructors.len());
        for (i, constructor) in custom_type.constructors.iter().enumerate() {
            if null_tag == Some(i) {
                variant_eqs.push(None);
            } else {
                let (type_index, eq_index) =
                    self.function_variant_eq(type_, custom_type, constructor, args);
                variant_eqs.push(Some((type_index, eq_index)));
            }
        }

        let mut function = Function::new(self, "_equal_union", &[a, b], &[BOOL_VALTYPE]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions.local_get(a).local_get(b).ref_eq().if_(
            InstrSeqType::Simple(None),
            |body| {
                let _ = body.bool_const(true).return_();
            },
        );

        if null_tag.is_some() {
            // If either is null but not both (ref_eq already handled both-null)
            let _ = instructions
                .local_get(a)
                .ref_is_null()
                .local_get(b)
                .ref_is_null()
                .i32_or()
                .if_(InstrSeqType::Simple(None), |body| {
                    let _ = body.bool_const(false).return_();
                });
        }

        if !tagged {
            // The values are of the only variant that is not null.
            let (type_index, eq_index) = variant_eqs
                .into_iter()
                .flatten()
                .exactly_one()
                .expect("one variant that is not null");
            let _ = instructions
                .local_get(a)
                .ref_cast_non_null(HeapType::Concrete(type_index))
                .local_get(b)
                .ref_cast_non_null(HeapType::Concrete(type_index))
                .return_call(eq_index)
                .end();
            return function.finish(self);
        }

        let _ = instructions
            .local_get(a)
            .ref_as_non_null()
            .struct_get(supertype_index, 0)
            .local_tee(tag)
            .local_get(b)
            .ref_as_non_null()
            .struct_get(supertype_index, 0)
            .i32_ne()
            .if_(InstrSeqType::Simple(None), |body| {
                let _ = body.bool_const(false).return_();
            });

        // Dispatch on tag: for each non-null variant, check tag == i and call its eq.
        for (i, info) in variant_eqs.iter().enumerate() {
            if let Some((type_index, eq_index)) = info {
                let type_index = *type_index;
                let eq_index = *eq_index;
                let _ = instructions
                    .local_get(tag)
                    .i32_const(i as i32)
                    .i32_eq()
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body
                            .local_get(a)
                            .ref_cast_non_null(HeapType::Concrete(type_index))
                            .local_get(b)
                            .ref_cast_non_null(HeapType::Concrete(type_index))
                            .return_call(eq_index);
                    });
            }
        }
        let _ = instructions.bool_const(false).end();
        function.finish(self)
    }

    fn code_variant_constructor_for(&mut self, return_: Arc<Type>, variant: Variant) -> FunctionId {
        let (_, _, args) = return_
            .named_type_information()
            .expect("named type information");
        let (type_index, tag, types) = match &variant.custom_type {
            CustomType::External { .. }
            | CustomType::Runtime { .. }
            | CustomType::Array
            | CustomType::Enum { .. } => {
                panic!("external/enum types should not reach code generation")
            }
            CustomType::Struct {
                custom_type,
                constructor,
                ..
            } => {
                let (type_index, types) =
                    self.mono_struct_type_index(&return_, custom_type, constructor, &args);
                (type_index, None, types)
            }
            CustomType::Union { custom_type, .. } => {
                let (_, type_index, types) = self.mono_union_subtype_index(
                    &return_,
                    custom_type,
                    &variant.constructor,
                    &args,
                );
                let tag = variant.tag.filter(|_| self.union_layout(&return_).tagged);
                (type_index, tag, types)
            }
        };
        let (null_supertype, field_mapping) =
            if let CustomType::Union { custom_type, .. } = &variant.custom_type {
                let layout = self.union_layout(&return_);
                let variant_index = custom_type
                    .constructors
                    .iter()
                    .position(|c| c.name == variant.constructor.name)
                    .expect("constructor in union");
                let null_supertype = (layout.null_tag == Some(variant_index))
                    .then(|| self.mono_union_supertype_index(&return_, custom_type));
                (null_supertype, layout.fields(variant_index).to_vec())
            } else {
                (None, self.field_indices(&types))
            };
        let result_type = self.val_type(&return_);
        // Params in Gleam (declaration) order — this is the function signature.
        let param_vts = self.val_types(types);
        self.code_variant_constructor(
            type_index,
            tag,
            null_supertype,
            &field_mapping,
            result_type,
            &param_vts,
        )
    }

    fn code_variant_constructor(
        &mut self,
        struct_index: TypeId,
        tag: Option<i32>,
        null_supertype: Option<TypeId>,
        field_mapping: &[Option<u32>],
        result_type: ValType,
        param_vts: &[ValType],
    ) -> FunctionId {
        let param_ids: Vec<LocalId> = param_vts.iter().map(|vt| self.wasm_local(*vt)).collect();

        let mut function = Function::new(self, "_create_variant", &param_ids, &[result_type]);
        let mut instructions = function.extend_instructions(self);
        if let Some(supertype_index) = null_supertype {
            let _ = instructions
                .ref_null(HeapType::Concrete(supertype_index))
                .end();
            return function.finish(self);
        }
        if let Some(tag) = tag {
            let _ = instructions.i32_const(tag);
        }
        // The params in the order of the struct fields (shared first, then
        // specific), without the ones of unit types, which are not stored.
        let params = field_mapping
            .iter()
            .zip(&param_ids)
            .filter_map(|(field_index, param)| Some((field_index.as_ref()?, param)))
            .sorted_by_key(|(field_index, _)| **field_index);
        for (_, param) in params {
            let _ = instructions.local_get(*param);
        }
        let _ = instructions.struct_new(struct_index).end();
        function.finish(self)
    }

    pub(super) fn function_inspect(&mut self, fn_type: &Arc<Type>) -> Id {
        let (params, _) = fn_type.fn_types().expect("inspect must be a function type");
        let arg_type = params
            .into_iter()
            .next()
            .expect("inspect takes one argument");

        let builtin = BuiltinFunction::Inspect(self.val_type(&arg_type), TypeKey::new(&arg_type));
        let name = builtin.name();
        let index = self.get_function_builtin(builtin, |s| s.code_inspect(&arg_type));
        let _ = self.add_function_to_globals(name.clone(), index);
        Id::func(name, index)
    }

    /// `string.inspect` with `inspect = { module, function }`, the call of
    /// the function with the value as `Dynamic`.
    pub(super) fn function_inspect_hook(&mut self, fn_type: &Arc<Type>, hook: HookFunction) -> Id {
        let (params, _) = fn_type.fn_types().expect("inspect must be a function type");
        let arg_type = params
            .into_iter()
            .next()
            .expect("inspect takes one argument");
        let builtin =
            BuiltinFunction::InspectHook(self.val_type(&arg_type), TypeKey::new(&arg_type));
        let name = builtin.name();
        let index = self.get_function_builtin(builtin, |s| {
            let cast = s.function_dynamic_cast(&arg_type);
            let function = s
                .function_imported(&hook.module, &hook.name, &hook.type_)
                .func_id();
            let arg_vt = s.val_type(&arg_type);
            let value = s.wasm_local(arg_vt);
            let string_vt = s.string.val_type();
            let mut code = Function::new(s, "_inspect", &[value], &[string_vt]);
            let _ = code
                .extend_instructions(s)
                .local_get(value)
                .call(cast)
                .call(function)
                .end();
            code.finish(s)
        });
        let _ = self.add_function_to_globals(name.clone(), index);
        Id::func(name, index)
    }

    fn code_inspect(&mut self, arg_type: &Arc<Type>) -> FunctionId {
        let repr_index = self.function_repr(arg_type);
        let heap_base = self.find_global_expect(HEAP_BASE);
        let memory_to_string =
            self.get_function_builtin_external(BuiltinFunctionExternal::MemoryToString);

        let arg_vt = self.val_type(arg_type);
        let string_vt = self.string.val_type();
        // params
        let value = self.wasm_local(arg_vt);
        // locals
        let ptr = self.wasm_local(ValType::I32);
        let mut function = Function::new(self, "_inspect", &[value], &[string_vt]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .call(heap_base.func_id())
            .local_set(ptr)
            .local_get(ptr)
            .local_get(value)
            .local_get(ptr)
            .call(repr_index)
            .call(memory_to_string)
            .end();
        function.finish(self)
    }

    pub(super) fn function_repr(&mut self, type_: &Arc<Type>) -> FunctionId {
        if let Some(representation) = self.representation(type_) {
            return self.function_repr(&representation);
        }
        if let Some((module, hooks)) = self.type_hooks(type_) {
            let builtin =
                BuiltinFunction::CustomTypeRepr(self.val_type(type_), TypeKey::new(type_));
            let fn_type = type_::fn_(vec![type_.clone()], type_::string());
            let type_ = type_.clone();
            return self.get_function_builtin(builtin, |s| {
                let inspect = s.function_hook(&module, hooks.inspect, fn_type);
                s.code_repr_hook(&type_, inspect)
            });
        }
        if type_.is_int() {
            return self.get_function_builtin_external(BuiltinFunctionExternal::IntRepr);
        }
        if type_.is_float() {
            return self.get_function_builtin_external(BuiltinFunctionExternal::FloatRepr);
        }
        if type_.is_string() {
            return self.get_function_builtin_external(BuiltinFunctionExternal::StringRepr);
        }
        if type_.is_utf_codepoint() {
            return self.get_function_builtin_external(BuiltinFunctionExternal::UtfCodepointRepr);
        }

        let val_type = self.val_type(type_);
        let key = TypeKey::new(type_);
        let repr = if type_.is_list() {
            BuiltinFunction::ListRepr(val_type, key)
        } else if type_.is_tuple() {
            BuiltinFunction::TupleRepr(val_type, key)
        } else if type_.fn_types().is_some() {
            BuiltinFunction::FunctionRepr(val_type, key)
        } else if is_never_type(type_) || self.custom_type(type_).is_some() {
            BuiltinFunction::CustomTypeRepr(val_type, key)
        } else {
            panic!("unexpected type should not reach repr: {type_:#?}");
        };

        if self.is_struct(type_) {
            let struct_type = type_.clone();
            self.get_function_constructor(repr, type_, 0, move |s| s.code_repr(&struct_type))
        } else {
            let type_ = type_.clone();
            self.get_function_builtin(repr, |s| s.code_repr(&type_))
        }
    }

    pub(super) fn code_int_repr(&mut self) -> FunctionId {
        let int_vt = self.int.val_type();
        let value = self.wasm_local(int_vt);
        let ptr = self.wasm_local(ValType::I32);
        let id = match self.int {
            IntType::I32 => self.find_global_expect(I32_TO_STR),
            IntType::I64 => self.find_global_expect(I64_TO_STR),
        };
        let needed = self.wasm_local(ValType::I32);
        let mut function = Function::new(self, "_repr_int", &[value, ptr], &[ValType::I32]);
        let _ = function
            .extend_instructions(self)
            // the longest text of a number is shorter than 64 bytes
            .local_get(ptr)
            .i32_const(64)
            .i32_add()
            .ensure_memory(needed)
            .local_get(value)
            .local_get(ptr)
            .call(id.func_id())
            .end();
        function.finish(self)
    }

    fn function_variant_repr(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &TypedCustomType,
        constructor: &TypedRecordConstructor,
        args: &[Arc<Type>],
    ) -> (TypeId, FunctionId) {
        let (_, type_index, types) =
            self.union_subtype_index(type_, custom_type, constructor, args);
        let constructor_name = constructor.name.clone();
        let builtin = BuiltinFunction::CustomTypeRepr(
            self.val_type_ref(type_index),
            TypeKey::variant(type_, &constructor_name),
        );
        let layout = self.union_layout(type_);
        let variant_index = custom_type
            .constructors
            .iter()
            .position(|c| c.name == constructor.name)
            .expect("constructor in union");
        let mapping = layout.fields(variant_index).to_vec();
        let labels = labels(constructor);
        let repr_index = self.get_function_constructor(builtin, type_, variant_index, move |s| {
            s.code_composite_repr(
                &constructor_name,
                &labels,
                Some(&mapping),
                type_index,
                &types,
            )
        });
        (type_index, repr_index)
    }

    pub(super) fn code_float_repr(&mut self) -> FunctionId {
        let float_vt = self.float.val_type();
        let value = self.wasm_local(float_vt);
        let ptr = self.wasm_local(ValType::I32);
        let id = match self.float {
            FloatType::F32 => self.find_global_expect(F32_TO_STR),
            FloatType::F64 => self.find_global_expect(F64_TO_STR),
        };
        let needed = self.wasm_local(ValType::I32);
        let mut function = Function::new(self, "_repr_float", &[value, ptr], &[ValType::I32]);
        let _ = function
            .extend_instructions(self)
            // the longest text of a number is shorter than 64 bytes
            .local_get(ptr)
            .i32_const(64)
            .i32_add()
            .ensure_memory(needed)
            .local_get(value)
            .local_get(ptr)
            .call(id.func_id())
            .end();
        function.finish(self)
    }

    pub(super) fn code_string_repr(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        // params
        let s = self.wasm_local(string_vt); // String
        let ptr = self.wasm_local(ValType::I32); // I32
        // locals
        let bytes = self.wasm_local(self.bytes.val_type());
        let off = self.wasm_local(ValType::I32); // I32
        let len = self.wasm_local(ValType::I32); // I32
        let i = self.wasm_local(ValType::I32); // I32
        let s_i = self.wasm_local(ValType::I32); // I32
        let dest = self.wasm_local(ValType::I32); // I32
        let needed = self.wasm_local(ValType::I32); // I32
        let next = self.wasm_local(ValType::I32); // I32
        let digit = self.wasm_local(ValType::I32); // I32
        // return I32 - number of written bytes

        let mut function = Function::new(self, "_repr_string", &[s, ptr], &[ValType::I32]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .string_utf8_view(s, bytes, off, len)
            // max output = ptr + 2 + len * 8, as a byte can become `\u{00XX}`
            .local_get(ptr)
            .local_get(len)
            .i32_const(8)
            .i32_mul()
            .i32_add()
            .i32_const(2)
            .i32_add()
            .ensure_memory(needed)
            .local_get(ptr)
            .local_tee(dest)
            .byte_store(b'"')
            .i32_inc(dest)
            .i32_const(0)
            .local_set(i)
            // while i <= len
            .loop_(InstrSeqType::Simple(None), |loop_s| {
                let loop_id = loop_s.id();
                let _ = loop_s
                    .local_get(i)
                    .local_get(len)
                    .i32_lt_u()
                    .if_(InstrSeqType::Simple(None), |body| {
                        // value = s[i]
                        let _ = body
                            .local_get(bytes)
                            .local_get(off)
                            .local_get(i)
                            .i32_add()
                            .bytes_get()
                            .local_set(s_i)
                            .block_(InstrSeqType::Simple(None), |b| {
                                let block_id = b.id();
                                let _ = b
                                    .local_get(s_i)
                                    .try_escape(dest, b'"', b'"')
                                    .br_if(block_id)
                                    .local_get(s_i)
                                    .try_escape(dest, b'\\', b'\\')
                                    .br_if(block_id)
                                    .local_get(s_i)
                                    .try_escape(dest, b'\x0C', b'f')
                                    .br_if(block_id)
                                    .local_get(s_i)
                                    .try_escape(dest, b'\n', b'n')
                                    .br_if(block_id)
                                    .local_get(s_i)
                                    .try_escape(dest, b'\r', b'r')
                                    .br_if(block_id)
                                    .local_get(s_i)
                                    .try_escape(dest, b'\t', b't')
                                    .br_if(block_id)
                                    // the other control characters, as on Erlang:
                                    // below 32, 127, and from 128 to 159, which
                                    // are 0xC2 followed by 0x80 to 0x9F
                                    .local_get(s_i)
                                    .i32_const(32)
                                    .i32_lt_u()
                                    .local_get(s_i)
                                    .i32_const(127)
                                    .i32_eq()
                                    .i32_or()
                                    .if_else(
                                        ValType::I32,
                                        |then_s| {
                                            let _ = then_s
                                                .escape_unicode(dest, s_i, digit)
                                                .i32_const(1);
                                        },
                                        |else_s| {
                                            let _ = else_s.i32_const(0);
                                        },
                                    )
                                    .br_if(block_id)
                                    .local_get(s_i)
                                    .i32_const(0xC2)
                                    .i32_eq()
                                    .local_get(i)
                                    .i32_const(1)
                                    .i32_add()
                                    .local_get(len)
                                    .i32_lt_u()
                                    .i32_and()
                                    .if_else(
                                        ValType::I32,
                                        |then_s| {
                                            let _ = then_s
                                                .local_get(bytes)
                                                .local_get(off)
                                                .local_get(i)
                                                .i32_add()
                                                .i32_const(1)
                                                .i32_add()
                                                .bytes_get()
                                                .local_tee(next)
                                                .i32_const(0x80)
                                                .i32_sub()
                                                .i32_const(0x20)
                                                .i32_lt_u()
                                                .if_else(
                                                    ValType::I32,
                                                    |then_s| {
                                                        let _ = then_s
                                                            .escape_unicode(dest, next, digit)
                                                            .i32_inc(i)
                                                            .i32_const(1);
                                                    },
                                                    |else_s| {
                                                        let _ = else_s.i32_const(0);
                                                    },
                                                );
                                        },
                                        |else_s| {
                                            let _ = else_s.i32_const(0);
                                        },
                                    )
                                    .br_if(block_id)
                                    // default, do not escape
                                    .local_get(dest)
                                    .local_get(s_i)
                                    .i32_store8(MemArg { offset: 0, align: 0 })
                                    .i32_inc(dest);
                            })
                            .i32_inc(i)
                            .br(loop_id);
                    });
            })
            .write_byte(dest, b'"')
            .local_get(dest)
            .local_get(ptr)
            .i32_sub()
            .end();

        trait Escape {
            fn try_escape(&mut self, dest: LocalId, byte: u8, escape: u8) -> &mut Self;
            fn escape_unicode(&mut self, dest: LocalId, code: LocalId, digit: LocalId)
            -> &mut Self;
            fn hex_digit(&mut self, dest: LocalId, digit: LocalId) -> &mut Self;
        }

        impl<'a, 'b> Escape for Instructions<'a, 'b> {
            #[rustfmt::skip]
            fn try_escape(&mut self, dest: LocalId, byte: u8, escape: u8) -> &mut Self {
                self.i32_const(byte as i32)
                    .i32_eq()
                    .if_else(
                        ValType::I32,
                        |then_s| {
                            let _ = then_s
                                .write_byte(dest, b'\\')
                                .write_byte(dest, escape)
                                .i32_const(1);
                        },
                        |else_s| {
                            let _ = else_s.i32_const(0);
                        },
                    )
            }

            /// Writes `\u{00XX}`, with `XX` the hex digits of `code`, a byte.
            #[rustfmt::skip]
            fn escape_unicode(&mut self, dest: LocalId, code: LocalId, digit: LocalId)
            -> &mut Self {
                for byte in *b"\\u{00" {
                    let _ = self.write_byte(dest, byte);
                }
                self.local_get(code)
                    .i32_const(4)
                    .i32_shr_u()
                    .local_set(digit)
                    .hex_digit(dest, digit)
                    .local_get(code)
                    .i32_const(15)
                    .i32_and()
                    .local_set(digit)
                    .hex_digit(dest, digit)
                    .write_byte(dest, b'}')
            }

            /// Writes the uppercase hex digit of `digit`.
            #[rustfmt::skip]
            fn hex_digit(&mut self, dest: LocalId, digit: LocalId) -> &mut Self {
                self.local_get(dest)
                    .local_get(digit)
                    .local_get(digit)
                    .i32_const(10)
                    .i32_lt_u()
                    .if_else(
                        ValType::I32,
                        |then_s| {
                            let _ = then_s.i32_const(b'0' as i32);
                        },
                        |else_s| {
                            let _ = else_s.i32_const(b'A' as i32 - 10);
                        },
                    )
                    .i32_add()
                    .i32_store8(MemArg { offset: 0, align: 0 })
                    .i32_inc(dest)
            }
        }

        function.finish(self)
    }

    /// The repr of a type whose text is given by its `inspect` hook, a Gleam
    /// function of the runtime. The hook can inspect values itself, which
    /// writes after the top of the arena, so the top is first moved past
    /// `ptr`, where this repr writes.
    fn code_repr_hook(&mut self, type_: &Arc<Type>, inspect: FunctionId) -> FunctionId {
        let arena_mark = self.find_global_expect(ARENA_MARK).func_id();
        let arena_release = self.find_global_expect(ARENA_RELEASE).func_id();
        let heap_base = self.find_global_expect(HEAP_BASE).func_id();
        let cabi_realloc = self.rust_function(CABI_REALLOC);
        let string_to_memory =
            self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
        let value_vt = self.val_type(type_);
        let string_vt = self.string.val_type();
        // params
        let value = self.wasm_local(value_vt);
        let ptr = self.wasm_local(ValType::I32);
        // locals
        let mark = self.wasm_local(ValType::I32);
        let text = self.wasm_local(string_vt);
        let mut function = Function::new(self, "_repr_hook", &[value, ptr], &[ValType::I32]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .call(arena_mark)
            .local_set(mark)
            // cabi_realloc(0, 0, 1, ptr - heap_base + 16)
            .i32_const(0)
            .i32_const(0)
            .i32_const(1)
            .local_get(ptr)
            .call(heap_base)
            .i32_sub()
            .i32_const(16)
            .i32_add()
            .call(cabi_realloc)
            .drop()
            .local_get(value)
            .call(inspect)
            .local_set(text)
            .local_get(mark)
            .call(arena_release)
            .local_get(text)
            .local_get(ptr)
            .call(string_to_memory)
            .end();
        function.finish(self)
    }

    fn code_repr(&mut self, type_: &Arc<Type>) -> FunctionId {
        if let Some(item_type) = type_.list_type()
            && item_type.is_int()
        {
            self.code_charlist_repr()
        } else if let Some(item_type) = type_.list_type() {
            self.code_list_repr(&item_type)
        } else if let Some(types) = type_.tuple_types() {
            self.code_tuple_repr(&types)
        } else if type_.fn_types().is_some() {
            self.code_function_repr(type_)
        } else if is_never_type(type_) {
            let val_type = self.val_type(type_);
            self.code_never(&[val_type, ValType::I32], &[ValType::I32])
        } else if let Some((custom_type, args)) = self.custom_type(type_) {
            self.code_custom_type_repr(type_, &custom_type, &args)
        } else {
            panic!("unexpected type should not reach repr: {type_:#?}");
        }
    }

    /// The repr of a `List(Int)`. As on Erlang and JavaScript, a list of
    /// printable ASCII codes is shown as `charlist.from_string("...")`.
    fn code_charlist_repr(&mut self) -> FunctionId {
        let list_type = type_::list(type_::int());
        let (custom_type, cons, _, args) = self.list_type_cons(&list_type);
        let (_, struct_index, _) = self.union_subtype_index(&list_type, &custom_type, &cons, &args);
        let (rest_field, first_field) = self.list_cons_fields(&list_type);
        let list_repr = self.code_list_repr(&type_::int());
        let list_vt = self.val_type(&list_type);
        let int_vt = self.int.val_type();
        let prefix = b"charlist.from_string(\"";
        // params
        let list = self.wasm_local(list_vt); // List(Int)
        let ptr = self.wasm_local(ValType::I32); // I32
        // locals
        let current = self.wasm_local(list_vt); // List(Int)
        let item = self.wasm_local(int_vt); // Int
        let count = self.wasm_local(ValType::I32); // I32
        let dest = self.wasm_local(ValType::I32); // I32
        let needed = self.wasm_local(ValType::I32); // I32
        // return I32 - number of written bytes
        let mut function = Function::new(self, "_repr_charlist", &[list, ptr], &[ValType::I32]);
        let mut instructions = function.extend_instructions(self);
        let first = |s: &mut Instructions<'_, '_>, list: LocalId| {
            let _ = s
                .local_get(list)
                .variant_field_get(struct_index, first_field);
        };
        let rest = |s: &mut Instructions<'_, '_>, list: LocalId| {
            let _ = s
                .local_get(list)
                .ref_cast_non_null(HeapType::Concrete(struct_index))
                .struct_get(struct_index, rest_field);
        };
        #[rustfmt::skip]
        let _ = instructions
            // an empty list is `[]`
            .local_get(list)
            .ref_is_null()
            .if_(InstrSeqType::Simple(None), |then_s| {
                let _ = then_s
                    .local_get(list)
                    .local_get(ptr)
                    .return_call(list_repr);
            })
            // count the items, falling back to the list repr on the first
            // that is not printable
            .local_get(list)
            .local_set(current)
            .block_(InstrSeqType::Simple(None), |block_s| {
                let block_id = block_s.id();
                let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                    let loop_id = loop_s.id();
                    let _ = loop_s.local_get(current).ref_is_null().br_if(block_id);
                    first(loop_s, current);
                    let _ = loop_s
                        .local_tee(item)
                        .int_const(&32.into())
                        .int_lt()
                        .local_get(item)
                        .int_const(&126.into())
                        .int_gt()
                        .i32_or()
                        .if_(InstrSeqType::Simple(None), |then_s| {
                            let _ = then_s
                                .local_get(list)
                                .local_get(ptr)
                                .return_call(list_repr);
                        })
                        .i32_inc(count);
                    rest(loop_s, current);
                    let _ = loop_s.local_set(current).br(loop_id);
                });
            })
            // ptr + prefix + count + `")`
            .local_get(ptr)
            .local_get(count)
            .i32_add()
            .i32_const(prefix.len() as i32 + 2)
            .i32_add()
            .ensure_memory(needed)
            .local_get(ptr)
            .local_set(dest);
        for byte in prefix {
            let _ = instructions.write_byte(dest, *byte);
        }
        #[rustfmt::skip]
        let _ = instructions
            .local_get(list)
            .local_set(current)
            .block_(InstrSeqType::Simple(None), |block_s| {
                let block_id = block_s.id();
                let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                    let loop_id = loop_s.id();
                    let _ = loop_s
                        .local_get(current)
                        .ref_is_null()
                        .br_if(block_id)
                        .local_get(dest);
                    first(loop_s, current);
                    let _ = loop_s
                        .int_to_i32()
                        .i32_store8(MemArg { offset: 0, align: 0 })
                        .i32_inc(dest);
                    rest(loop_s, current);
                    let _ = loop_s.local_set(current).br(loop_id);
                });
            })
            .write_byte(dest, b'"')
            .write_byte(dest, b')')
            .local_get(dest)
            .local_get(ptr)
            .i32_sub()
            .end();
        function.finish(self)
    }

    fn code_list_repr(&mut self, item_type: &Arc<Type>) -> FunctionId {
        let list_type = type_::list(item_type.clone());
        let (custom_type, cons, _, args) = self.list_type_cons(&list_type);
        let (_, struct_index, _) = self.union_subtype_index(&list_type, &custom_type, &cons, &args);
        let (rest_field, first_field) = self.list_cons_fields(&list_type);
        let item_repr = self.function_repr(item_type);
        let list_vt = self.val_type(&list_type);
        // params
        let lst = self.wasm_local(list_vt); // List(a)
        let ptr = self.wasm_local(ValType::I32); // I32
        // locals
        let dest = self.wasm_local(ValType::I32); // I32
        // return I32 - number of written bytes
        let mut function = Function::new(self, "_repr_list", &[lst, ptr], &[ValType::I32]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(ptr)
            .local_set(dest)
            .write_byte(dest, b'[')
            .local_get(lst)
            .ref_is_null()
            .if_else(
                InstrSeqType::Simple(None),
                |then_s| {
                    let _ = then_s.write_byte(dest, b']').i32_const(2).return_();
                },
                |else_s| {
                    let _ = else_s
                        .local_get(lst)
                        .variant_field_get(struct_index, first_field)
                        .local_get(dest)
                        .call(item_repr)
                        .local_get(dest)
                        .i32_add()
                        .local_set(dest);
                },
            )
            .loop_(InstrSeqType::Simple(None), |loop_s| {
                let loop_id = loop_s.id();
                let _ = loop_s
                    .local_get(lst)
                    .ref_cast_non_null(HeapType::Concrete(struct_index))
                    .struct_get(struct_index, rest_field)
                    .local_tee(lst)
                    .ref_is_null()
                    .if_else(
                        InstrSeqType::Simple(None),
                        |then_s| {
                            let _ = then_s
                                .write_byte(dest, b']');
                        },
                        |else_s| {
                            let _ = else_s
                                .write_byte(dest, b',')
                                .write_byte(dest, b' ')
                                .local_get(lst)
                                .variant_field_get(struct_index, first_field)
                                .local_get(dest)
                                .call(item_repr)
                                .local_get(dest)
                                .i32_add()
                                .local_set(dest)
                                .br(loop_id);
                        },
                    );
            })
            .local_get(dest)
            .local_get(ptr)
            .i32_sub()
            .end();
        function.finish(self)
    }

    fn code_tuple_repr(&mut self, types: &[Arc<Type>]) -> FunctionId {
        let type_index = self.tuple_type_index(types.iter().cloned());
        if types.is_empty() {
            let value_vt = self.val_type_ref(type_index);
            return self.code_constant_repr(value_vt, "#()");
        }
        self.code_composite_repr(&"#".into(), &[], None, type_index, types)
    }

    /// A repr that writes `text` for any value.
    fn code_constant_repr(&mut self, value_vt: ValType, text: &str) -> FunctionId {
        // params
        let value = self.wasm_local(value_vt);
        let ptr = self.wasm_local(ValType::I32); // I32
        // return I32 - number of written bytes
        let string_index = self.string_index(&text.into());
        let string_to_memory =
            self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
        let mut function = Function::new(self, "_repr_constant", &[value, ptr], &[ValType::I32]);
        let _ = function
            .extend_instructions(self)
            .global_as_non_null(string_index)
            .local_get(ptr)
            .call(string_to_memory)
            .end();
        function.finish(self)
    }

    /// The repr of a record or a tuple, `name(field, ...)`. A field with a
    /// label in `labels` is written `label: value`, as on JavaScript.
    fn code_composite_repr(
        &mut self,
        name: &EcoString,
        labels: &[Option<EcoString>],
        field_mapping: Option<&[Option<u32>]>,
        type_index: TypeId,
        types: &[Arc<Type>],
    ) -> FunctionId {
        let wasm_indices = field_mapping.map_or_else(|| self.field_indices(types), <[_]>::to_vec);
        let value_vt = self.val_type_ref(type_index);
        // params
        let value = self.wasm_local(value_vt);
        let ptr = self.wasm_local(ValType::I32); // I32
        // local
        let dest = self.wasm_local(ValType::I32); // I32
        // return I32 - number of written bytes
        let string_to_memory =
            self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
        let string_index = self.string_index(name);
        let mut function = Function::new(self, "_repr_composite", &[value, ptr], &[ValType::I32]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions
            .global_as_non_null(string_index)
            .local_get(ptr)
            .local_tee(dest)
            .call(string_to_memory)
            .local_get(dest)
            .i32_add()
            .local_set(dest);

        for (position, (type_, &field_index)) in types.iter().zip(wasm_indices.iter()).enumerate() {
            let repr = self.function_repr(type_);
            if position == 0 {
                let _ = instructions.write_byte(dest, b'(');
            } else {
                let _ = instructions.write_byte(dest, b',').write_byte(dest, b' ');
            }
            if let Some(Some(label)) = labels.get(position) {
                let label_index = self.string_index(&ecow::eco_format!("{label}: "));
                let _ = instructions
                    .global_as_non_null(label_index)
                    .local_get(dest)
                    .call(string_to_memory)
                    .local_get(dest)
                    .i32_add()
                    .local_set(dest);
            }
            let _ = instructions
                .local_get(value)
                .field_get(type_index, field_index)
                .local_get(dest)
                .call(repr)
                .local_get(dest)
                .i32_add()
                .local_set(dest);
        }
        if !types.is_empty() {
            let _ = instructions.write_byte(dest, b')');
        }

        let _ = instructions.local_get(dest).local_get(ptr).i32_sub().end();
        function.finish(self)
    }

    fn code_function_repr(&mut self, type_: &Arc<Type>) -> FunctionId {
        // As on Erlang and JavaScript, only the arity is shown, with the
        // parameters named by the characters from `a` on.
        let (arguments, _) = type_.fn_types().expect("function type");
        let names = (0..arguments.len() as u32)
            .map(|index| char::from_u32(u32::from('a') + index).expect("a character"))
            .join(", ");
        let fn_vt = self.val_type(type_);
        self.code_constant_repr(fn_vt, &format!("//fn({names}) {{ ... }}"))
    }

    pub(super) fn code_utf_codepoint_repr(&mut self) -> FunctionId {
        // params
        let codepoint = self.wasm_local(ValType::I32);
        let ptr = self.wasm_local(ValType::I32);
        // locals
        let needed = self.wasm_local(ValType::I32);
        let repr = self.find_global_expect(UTF_CODEPOINT_REPR);
        let mut function = Function::new(
            self,
            "_repr_utf_code_point",
            &[codepoint, ptr],
            &[ValType::I32],
        );
        let _ = function
            .extend_instructions(self)
            // `//utfcodepoint(c)` has at most 20 bytes
            .local_get(ptr)
            .i32_const(20)
            .i32_add()
            .ensure_memory(needed)
            .local_get(codepoint)
            .local_get(ptr)
            .call(repr.func_id())
            .end();
        function.finish(self)
    }

    fn code_custom_type_repr(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &CustomType,
        args: &[Arc<Type>],
    ) -> FunctionId {
        let value_vt = self.val_type(type_);
        // params
        let value = self.wasm_local(value_vt); // value
        let ptr = self.wasm_local(ValType::I32); // I32
        // return I32 - number of written bytes
        let string_to_memory =
            self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
        match custom_type {
            CustomType::Runtime { .. } => {
                panic!("runtime types should be replaced by their representation")
            }
            CustomType::Array => {
                let element = args.first().expect("array element type");
                self.code_array_repr(element)
            }
            CustomType::External { to_str, .. } => {
                let to_str = to_str.expect("external type with a text");
                let to_str_fn = self.find_global_expect(to_str);
                let needed = self.wasm_local(ValType::I32);
                let mut function =
                    Function::new(self, "_repr_external", &[value, ptr], &[ValType::I32]);
                let _ = function
                    .extend_instructions(self)
                    // the longest text of a number is shorter than 64 bytes
                    .local_get(ptr)
                    .i32_const(64)
                    .i32_add()
                    .ensure_memory(needed)
                    .local_get(value)
                    .local_get(ptr)
                    .call(to_str_fn.func_id())
                    .end();
                function.finish(self)
            }
            CustomType::Enum { values } => {
                let string_vt = self.string.val_type();
                let string_indices: Vec<GlobalId> =
                    values.iter().map(|name| self.string_index(name)).collect();
                let mut function =
                    Function::new(self, "_repr_enum", &[value, ptr], &[ValType::I32]);
                let mut instructions = function.extend_instructions(self);
                #[rustfmt::skip]
                let _ = instructions
                    .block_(InstrSeqType::Simple(Some(string_vt)), |block_b| {
                        let block_id = block_b.id();
                        for (enum_value, string_index) in string_indices.iter().enumerate() {
                            let _ = block_b
                                .local_get(value)
                                .i32_const(enum_value as i32)
                                .i32_eq()
                                .if_(InstrSeqType::Simple(None), |body| {
                                    let _ = body
                                        .global_as_non_null(*string_index)
                                        .br(block_id);
                                });
                        }
                        let _ = block_b.unreachable();
                    })
                    .local_get(ptr)
                    .call(string_to_memory)
                    .end();
                function.finish(self)
            }
            CustomType::Struct {
                custom_type,
                constructor,
            } => {
                let custom_type = custom_type.clone();
                let constructor = constructor.clone();
                let (type_index, types) =
                    self.mono_struct_type_index(type_, &custom_type, &constructor, args);
                self.code_composite_repr(
                    &constructor.name,
                    &labels(&constructor),
                    None,
                    type_index,
                    &types,
                )
            }
            CustomType::Union { custom_type, .. } => {
                let custom_type = custom_type.clone();
                let supertype_index = self.mono_union_supertype_index(type_, &custom_type);
                let layout = self.union_layout(type_);
                let null_tag = layout.null_tag;
                let tagged = layout.tagged;

                // The text of the null variant, whose fields have unit types,
                // is a constant.
                let null_name_and_index = null_tag.map(|null_tag| {
                    let constructor = custom_type
                        .constructors
                        .get(null_tag)
                        .expect("null variant for union type");
                    let mut text = constructor.name.clone();
                    let types = Monomorphizer::variant_constructor(&custom_type, constructor, args);
                    if !types.is_empty() {
                        let mut values =
                            types.iter().zip(labels(constructor)).map(|(type_, label)| {
                                let Some((CustomType::Enum { values }, _)) =
                                    self.custom_type(type_)
                                else {
                                    panic!("unit type expected")
                                };
                                let value = values.first().expect("unit value");
                                match label {
                                    Some(label) => ecow::eco_format!("{label}: {value}"),
                                    None => value.clone(),
                                }
                            });
                        text += "(";
                        text += values.join(", ").as_str();
                        text += ")";
                    }
                    self.string_index(&text)
                });

                let mut variant_reprs: Vec<Option<(TypeId, FunctionId)>> =
                    Vec::with_capacity(custom_type.constructors.len());
                for (i, constructor) in custom_type.constructors.iter().enumerate() {
                    if null_tag == Some(i) {
                        variant_reprs.push(None);
                    } else {
                        let (type_index, repr_index) =
                            self.function_variant_repr(type_, &custom_type, constructor, args);
                        variant_reprs.push(Some((type_index, repr_index)));
                    }
                }

                let tag_local = self.wasm_local(ValType::I32);
                let mut function =
                    Function::new(self, "_repr_union", &[value, ptr], &[ValType::I32]);
                let mut instructions = function.extend_instructions(self);
                if let Some(string_index) = null_name_and_index {
                    let _ = instructions.local_get(value).ref_is_null().if_(
                        InstrSeqType::Simple(None),
                        |body| {
                            let _ = body
                                .global_as_non_null(string_index)
                                .local_get(ptr)
                                .call(string_to_memory)
                                .return_();
                        },
                    );
                }
                if !tagged {
                    // The value is of the only variant that is not null.
                    let (type_index, repr_index) = variant_reprs
                        .into_iter()
                        .flatten()
                        .exactly_one()
                        .expect("one variant that is not null");
                    let _ = instructions
                        .local_get(value)
                        .ref_cast_non_null(HeapType::Concrete(type_index))
                        .local_get(ptr)
                        .return_call(repr_index)
                        .end();
                    return function.finish(self);
                }
                let _ = instructions
                    .local_get(value)
                    .ref_as_non_null()
                    .struct_get(supertype_index, 0)
                    .local_set(tag_local);
                for (i, info) in variant_reprs.iter().enumerate() {
                    if let Some((type_index, repr_index)) = info {
                        let type_index = *type_index;
                        let repr_index = *repr_index;
                        let _ = instructions
                            .local_get(tag_local)
                            .i32_const(i as i32)
                            .i32_eq()
                            .if_(InstrSeqType::Simple(None), |body| {
                                let _ = body
                                    .local_get(value)
                                    .ref_cast_non_null(HeapType::Concrete(type_index))
                                    .local_get(ptr)
                                    .call(repr_index)
                                    .return_();
                            });
                    }
                }
                let _ = instructions.unreachable().end();
                function.finish(self)
            }
        }
    }

    pub(super) fn code_string_to_memory(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        // params
        let s = self.wasm_local(string_vt); // String
        let dest = self.wasm_local(ValType::I32); // I32
        // locals
        let bytes = self.wasm_local(self.bytes.val_type());
        let off = self.wasm_local(ValType::I32); // I32
        let len = self.wasm_local(ValType::I32); // I32
        let i = self.wasm_local(ValType::I32); // I32
        let needed = self.wasm_local(ValType::I32); // I32
        // result len - I32
        let mut function = Function::new(self, "_string_to_memory", &[s, dest], &[ValType::I32]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .string_utf8_view(s, bytes, off, len)
            .local_get(dest)
            .local_get(len)
            .i32_add()
            .ensure_memory(needed)
            .i32_const(0)
            .local_set(i)
            // while i <= len
            .loop_(InstrSeqType::Simple(None), |loop_s| {
                let loop_id = loop_s.id();
                let _ = loop_s
                    .local_get(i)
                    .local_get(len)
                    .i32_lt_u()
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body
                            .local_get(dest)
                            // value
                            .local_get(bytes)
                            .local_get(off)
                            .local_get(i)
                            .i32_add()
                            .bytes_get()
                            // store
                            .i32_store8(MemArg { offset: 0, align: 0 })
                            .i32_inc(dest)
                            .i32_inc(i)
                            .br(loop_id);
                    });
            })
            .local_get(len)
            .end();
        function.finish(self)
    }

    pub(super) fn code_memory_to_string(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        // params
        let src = self.wasm_local(ValType::I32); // I32
        let len = self.wasm_local(ValType::I32); // I32
        // locals
        let i = self.wasm_local(ValType::I32); // I32
        // result bytes
        let s = self.wasm_local(self.bytes.val_type());
        let mut function = Function::new(self, "_memory_to_string", &[src, len], &[string_vt]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(len)
            .bytes_new()
            .local_set(s)
            .i32_const(0)
            .local_set(i)
            .loop_(InstrSeqType::Simple(None), |loop_s| {
                let loop_id = loop_s.id();
                let _ = loop_s
                    .local_get(i)
                    .local_get(len)
                    .i32_lt_u()
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body
                            .local_get(s)
                            .local_get(i)
                            .local_get(src)
                            .i32_load8_u(MemArg { offset: 0, align: 0 })
                            .bytes_set()
                            .i32_inc(src)
                            .i32_inc(i)
                            .br(loop_id);
                    });
            })
            .local_get(s)
            .string_from_bytes()
            .end();
        function.finish(self)
    }

    pub(super) fn get_function_builtin(
        &mut self,
        builtin: BuiltinFunction,
        code_fn: impl FnOnce(&mut Self) -> FunctionId,
    ) -> FunctionId {
        if let Some(index) = self.builtins.get(&builtin) {
            return *index;
        }
        // Register a placeholder FunctionId so recursive references can resolve;
        // later we swap its body with the real one.
        let (params, results) = self.builtin_type(&builtin);
        let placeholder_id = self.allocate_placeholder_function(&builtin.name(), &params, &results);
        let _ = self.builtins.insert(builtin, placeholder_id);
        if self.error.is_some() {
            // The code is not used, see `val_type`.
            return placeholder_id;
        }
        let real_id = code_fn(self);
        replace_function_body(&mut self.wasm_module, placeholder_id, real_id);
        placeholder_id
    }

    /// Like `get_function_builtin`, for a function that reads the fields of
    /// the constructor at `index` of `type_`, like its equality, hash and
    /// inspect. Its body is generated by `code_fn` when the program
    /// constructs values of the constructor (see `constructed`), until then
    /// it is unreachable.
    ///
    /// The equality of `T(Int)` in `type T(a) { T(a, List(T(#(a, a)))) }`
    /// reads its list, so it needs the equality of `List(T(#(Int, Int)))`,
    /// which reads `T(#(Int, Int))` only when the program constructs a
    /// `List(T(#(Int, Int)))` that is not empty; otherwise it would need the
    /// equality of every `T(#(…))`. This terminates because the program
    /// constructs values of a finite number of types.
    pub(super) fn get_function_constructor(
        &mut self,
        builtin: BuiltinFunction,
        type_: &Arc<Type>,
        index: usize,
        code_fn: impl FnOnce(&mut Self) -> FunctionId + 'a,
    ) -> FunctionId {
        if self.is_constructed(type_, index) {
            return self.get_function_builtin(builtin, code_fn);
        }
        if let Some(function) = self.builtins.get(&builtin) {
            return *function;
        }
        let (params, results) = self.builtin_type(&builtin);
        let function = self.allocate_placeholder_function(&builtin.name(), &params, &results);
        let _ = self.builtins.insert(builtin, function);
        let key = (qualified_type_name(&self.represent(type_)), index);
        self.pending
            .entry(key)
            .or_default()
            .push((function, Box::new(code_fn)));
        function
    }

    /// Records that the program constructs values of the constructor at
    /// `index` of `type_`, and generates the bodies of its functions, see
    /// `get_function_constructor`.
    pub(super) fn constructed(&mut self, type_: &Arc<Type>, index: usize) {
        let name = qualified_type_name(&self.represent(type_));
        if !self
            .constructed
            .entry(name.clone())
            .or_default()
            .insert(index)
        {
            return;
        }
        for (function, code_fn) in self.pending.remove(&(name, index)).unwrap_or_default() {
            let id = code_fn(self);
            replace_function_body(&mut self.wasm_module, function, id);
        }
    }

    /// Whether the program constructs values of the constructor at `index`
    /// of `type_`.
    fn is_constructed(&self, type_: &Arc<Type>, index: usize) -> bool {
        self.constructed
            .get(&qualified_type_name(&self.represent(type_)))
            .is_some_and(|indexes| indexes.contains(&index))
    }

    pub(super) fn is_struct(&self, type_: &Arc<Type>) -> bool {
        matches!(
            self.custom_type(type_),
            Some((CustomType::Struct { .. }, _))
        )
    }
}

/// A function of `get_function_constructor` and the code of its body.
pub(super) type PendingFunction<'a> = (
    FunctionId,
    Box<dyn FnOnce(&mut Generator<'a>) -> FunctionId + 'a>,
);

/// The label of each field of `constructor`, `None` for a field without one.
pub(super) fn labels(constructor: &TypedRecordConstructor) -> Vec<Option<EcoString>> {
    constructor
        .arguments
        .iter()
        .map(|argument| argument.label.as_ref().map(|(_, label)| label.clone()))
        .collect()
}
