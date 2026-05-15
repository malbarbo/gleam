use super::*;

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub(super) enum BuiltinFunction {
    Equal(ValType, EcoString),
    ListRepr(ValType, EcoString),
    TupleRepr(ValType, EcoString),
    FunctionRepr(ValType, EcoString),
    CustomTypeRepr(ValType, EcoString, Option<EcoString>),
    VariantConstructor(Vec<ValType>, ValType, EcoString),
    Inspect(ValType, EcoString),
    StringStartsWith,
}

impl BuiltinFunction {
    pub(super) fn name(&self) -> EcoString {
        match self {
            BuiltinFunction::Equal(_, name) => format!("_equal({name})").into(),
            BuiltinFunction::ListRepr(_, name) => format!("_repr({name})").into(),
            BuiltinFunction::TupleRepr(_, name) => format!("_repr({name})").into(),
            BuiltinFunction::FunctionRepr(_, name) => format!("_repr({name})").into(),
            BuiltinFunction::CustomTypeRepr(_, name, variant) => {
                if let Some(variant) = variant {
                    format!("_repr({name}.{variant})").into()
                } else {
                    format!("_repr({name})").into()
                }
            }
            BuiltinFunction::VariantConstructor(_, _, name) => format!("_create({name})").into(),
            BuiltinFunction::Inspect(_, name) => format!("_inspect({name})").into(),
            BuiltinFunction::StringStartsWith => "_string_starts_with".into(),
        }
    }
}

#[derive(Hash, PartialEq, Eq, Ord, PartialOrd, Clone, Debug)]
pub(super) enum BuiltinFunctionExternal {
    StringConcat,
    StringNumBytes,
    StringGetByte,
    I32ToInt,
    IntToI32,
    I64ToInt,
    IntToI64,
    F32ToFloat,
    FloatToF32,
    F64ToFloat,
    FloatToF64,
    IntToUtfCodepoint,
    IntRepr,
    FloatRepr,
    UtfCodepointRepr,
    StringRepr,
    StringToMemory,
    MemoryToString,
    ParseInt,
    ParseFloat,
}

pub fn builtin_function_names() -> Vec<EcoString> {
    BuiltinFunctionExternal::all()
        .iter()
        .map(|b| b.name().into())
        .chain(iter::once(INSPECT.into()))
        .collect()
}

impl BuiltinFunctionExternal {
    pub(super) fn all() -> &'static [BuiltinFunctionExternal] {
        &[
            BuiltinFunctionExternal::StringConcat,
            BuiltinFunctionExternal::StringNumBytes,
            BuiltinFunctionExternal::StringGetByte,
            BuiltinFunctionExternal::I32ToInt,
            BuiltinFunctionExternal::IntToI32,
            BuiltinFunctionExternal::I64ToInt,
            BuiltinFunctionExternal::IntToI64,
            BuiltinFunctionExternal::F32ToFloat,
            BuiltinFunctionExternal::FloatToF32,
            BuiltinFunctionExternal::F64ToFloat,
            BuiltinFunctionExternal::FloatToF64,
            BuiltinFunctionExternal::IntToUtfCodepoint,
            BuiltinFunctionExternal::IntRepr,
            BuiltinFunctionExternal::FloatRepr,
            BuiltinFunctionExternal::UtfCodepointRepr,
            BuiltinFunctionExternal::StringRepr,
            BuiltinFunctionExternal::StringToMemory,
            BuiltinFunctionExternal::MemoryToString,
            BuiltinFunctionExternal::ParseInt,
            BuiltinFunctionExternal::ParseFloat,
        ]
    }

    pub(super) fn by_name(name: &str) -> Option<BuiltinFunctionExternal> {
        Self::all().iter().find(|f| f.name() == name).cloned()
    }

    pub(super) fn name(&self) -> &'static str {
        match self {
            BuiltinFunctionExternal::StringConcat => "_string_concat",
            BuiltinFunctionExternal::StringNumBytes => "_string_num_bytes",
            BuiltinFunctionExternal::StringGetByte => "_string_get_byte",
            BuiltinFunctionExternal::I32ToInt => "_i32_to_int",
            BuiltinFunctionExternal::IntToI32 => "_int_to_i32",
            BuiltinFunctionExternal::I64ToInt => "_i64_to_int",
            BuiltinFunctionExternal::IntToI64 => "_int_to_i64",
            BuiltinFunctionExternal::F32ToFloat => "_f32_to_float",
            BuiltinFunctionExternal::FloatToF32 => "_float_to_f32",
            BuiltinFunctionExternal::F64ToFloat => "_f64_to_float",
            BuiltinFunctionExternal::FloatToF64 => "_float_to_f64",
            BuiltinFunctionExternal::IntToUtfCodepoint => "_int_to_utf_codepoint",
            BuiltinFunctionExternal::IntRepr => "_repr_int",
            BuiltinFunctionExternal::FloatRepr => "_repr_float",
            BuiltinFunctionExternal::UtfCodepointRepr => "_repr_utf_code_point",
            BuiltinFunctionExternal::StringRepr => "_repr_string",
            BuiltinFunctionExternal::StringToMemory => "_string_to_memory",
            BuiltinFunctionExternal::MemoryToString => "_memory_to_string",
            BuiltinFunctionExternal::ParseInt => "_parse_int",
            BuiltinFunctionExternal::ParseFloat => "_parse_float",
        }
    }

    pub(super) fn type_(&self, i32: Arc<Type>) -> (Vec<Arc<Type>>, Arc<Type>) {
        use type_::{float, int, nil, result, string, utf_codepoint};
        match self {
            BuiltinFunctionExternal::StringConcat => (vec![string(), string()], string()),
            BuiltinFunctionExternal::StringNumBytes => (vec![string()], int()),
            BuiltinFunctionExternal::StringGetByte => (vec![string(), int()], result(int(), nil())),
            BuiltinFunctionExternal::I32ToInt | BuiltinFunctionExternal::I64ToInt => {
                (vec![i32], int())
            }
            BuiltinFunctionExternal::IntToI32 | BuiltinFunctionExternal::IntToI64 => {
                (vec![int()], i32)
            }
            BuiltinFunctionExternal::F32ToFloat | BuiltinFunctionExternal::F64ToFloat => {
                (vec![i32], float())
            }
            BuiltinFunctionExternal::FloatToF32 | BuiltinFunctionExternal::FloatToF64 => {
                (vec![float()], i32)
            }
            BuiltinFunctionExternal::IntToUtfCodepoint => {
                (vec![int()], result(utf_codepoint(), nil()))
            }
            BuiltinFunctionExternal::IntRepr => (vec![int(), i32.clone()], i32),
            BuiltinFunctionExternal::FloatRepr => (vec![float(), i32.clone()], i32),
            BuiltinFunctionExternal::UtfCodepointRepr => (vec![utf_codepoint(), i32.clone()], i32),
            BuiltinFunctionExternal::StringRepr => (vec![string(), i32.clone()], i32),
            BuiltinFunctionExternal::StringToMemory => (vec![string(), i32.clone()], i32),
            BuiltinFunctionExternal::MemoryToString => (vec![i32.clone(), i32], string()),
            BuiltinFunctionExternal::ParseInt => (vec![string()], result(int(), nil())),
            BuiltinFunctionExternal::ParseFloat => (vec![string()], result(float(), nil())),
        }
    }
}

impl<'a> Generator<'a> {
    pub(super) fn function_start(&mut self) -> FunctionId {
        self.code_start()
    }

    fn code_start(&mut self) -> FunctionId {
        let mut function = Function::new(self, "$start", &[], &[]);
        let mut instructions = function.extend_instructions(self);
        // string data
        for (string, index) in self.strings.clone() {
            let bytes = string.as_bytes().to_vec();
            let data = self.wasm_module.data.add(walrus::DataKind::Passive, bytes);
            let _ = instructions
                .i32_const(0)
                .i32_const(string.len() as i32)
                .array_new_data(self.string.type_index, data)
                .ref_as_non_null()
                .global_set(index);
        }
        // constants
        for const_ in self.consts.clone() {
            match const_ {
                WasmConst::String { dest, src } => {
                    let _ = instructions.global_get(src).global_set(dest);
                }
                WasmConst::Constant {
                    global_index,
                    value,
                } => {
                    let _ = instructions.constant(self, &value).global_set(global_index);
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
                        .constants(self, &elements)
                        .struct_new(type_index)
                        .global_set(global_index);
                }
                WasmConst::Function { dest, src } => {
                    let _ = instructions.ref_func(src).global_set(dest);
                }
                WasmConst::Var { global_index, name } => {
                    let _ = instructions
                        .global_get(self.find_global_expect(&name).global_id())
                        .global_set(global_index);
                }
            }
        }
        // main
        if let Some(main) = self.main {
            let _ = instructions.call(main).drop();
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
        let mut constructor_name = self.type_pretty_name(&return_);
        if let CustomType::Union { .. } = &variant.custom_type {
            constructor_name += ".";
            constructor_name += variant.constructor.name.clone();
        }

        let builtin = BuiltinFunction::VariantConstructor(
            self.val_types(params.iter().cloned()),
            self.val_type(&return_),
            constructor_name.clone(),
        );

        let num_fields = params.len() as u32;
        self.get_function_builtin(builtin, |s| {
            s.code_variant_constructor_for(return_, variant, num_fields)
        })
    }

    fn ok_variant_constructor(&mut self, ok: Arc<Type>, error: Arc<Type>) -> FunctionId {
        self.variant_constructor(
            vec![ok.clone()],
            type_::result(ok, error),
            self.variant_expect(PRELUDE_MODULE_NAME.into(), "Ok".into()),
        )
    }

    fn error_variant_constructor(&mut self, ok: Arc<Type>, error: Arc<Type>) -> FunctionId {
        self.variant_constructor(
            vec![error.clone()],
            type_::result(ok, error),
            self.variant_expect(PRELUDE_MODULE_NAME.into(), "Error".into()),
        )
    }

    pub(super) fn string_index(&mut self, string: &EcoString) -> GlobalId {
        let string = unescape(string);
        if let Some(id) = self.strings.get(string.as_str()) {
            return *id;
        }
        let val_type = self.string.val_type_nullable();
        let init = ConstExpr::RefNull(RefType {
            nullable: true,
            heap_type: self.string.heap_type(),
        });
        let id = self
            .wasm_module
            .globals
            .add_local(val_type, true, false, init);
        let _ = self.strings.insert(string.into(), id);
        id
    }

    pub(super) fn code_string_concat(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        // params
        let a = self.wasm_local(string_vt); // String
        let b = self.wasm_local(string_vt); // String
        // locals
        let len_a = self.wasm_local(ValType::I32); // I32
        let len_b = self.wasm_local(ValType::I32); // I32
        // return
        let r = self.wasm_local(string_vt); // String
        let mut function = Function::new(self, "_string_concat", &[a, b], &[string_vt]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            // len_a = a.len
            .local_get(a)
            .string_len()
            .local_set(len_a)
            // len_b = b.len
            .local_get(b)
            .string_len()
            .local_set(len_b)
            // r = array.new_default(len_a + len_b)
            .local_get(len_a)
            .local_get(len_b)
            .i32_add()
            .string_new()
            .local_set(r)
            // array.copy r[0..] from a[0..len_a]
            .local_get(r)
            .i32_const(0)
            .local_get(a)
            .i32_const(0)
            .local_get(len_a)
            .string_copy()
            // array.copy r[len_a..] from b[0..len_b]
            .local_get(r)
            .local_get(len_a)
            .local_get(b)
            .i32_const(0)
            .local_get(len_b)
            .string_copy()
            // return r
            .local_get(r)
            .end();
        function.finish(self)
    }

    pub(super) fn code_string_num_bytes(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        let int_vt = self.int.val_type();
        // params
        let s = self.wasm_local(string_vt); // String
        // return Int
        let mut function = Function::new(self, "_string_num_bytes", &[s], &[int_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions.local_get(s).string_len().i32_to_int().end();
        function.finish(self)
    }

    pub(super) fn code_string_get_byte(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        let int_vt = self.int.val_type();
        // params
        let s = self.wasm_local(string_vt); // String
        let i = self.wasm_local(int_vt); // Index
        // return Int
        let result = type_::result(type_::int(), type_::nil());
        let result_vt = self.val_type(&result);
        let ok_index = self.ok_variant_constructor(type_::int(), type_::nil());
        let error_index = self.error_variant_constructor(type_::int(), type_::nil());
        let mut function = Function::new(self, "_string_get_byte", &[s, i], &[result_vt]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(i)
            .local_get(s)
            .string_len()
            .i32_to_int()
            .int_lt()
            .local_get(i)
            .int_const(&0.into())
            .int_ge()
            .i32_and()
            .if_else(
                result_vt,
                |then_s| {
                    let _ = then_s
                        .local_get(s)
                        .local_get(i)
                        .int_to_i32()
                        .string_get()
                        .i32_to_int()
                        .call(ok_index);
                },
                |else_s| {
                    let _ = else_s
                        .nil_const()
                        .call(error_index);
                },
            )
            .end();
        function.finish(self)
    }

    pub(super) fn function_eq(&mut self, type_: &Arc<Type>) -> Eq {
        if type_.fn_types().is_some() {
            todo!("function equality is not yet supported");
        } else if type_.is_int() {
            return Eq::Int;
        } else if type_.is_float() {
            return Eq::Float;
        } else if type_.is_utf_codepoint() {
            return Eq::I32;
        } else if let Some((CustomType::External { .. } | CustomType::Enum { .. }, _)) =
            self.custom_type(type_)
        {
            // External types and Enums are all i32 at the WASM level
            return Eq::I32;
        }

        let builtin = BuiltinFunction::Equal(self.val_type(type_), self.type_pretty_name(type_));
        let type_ = type_.clone();
        let index = self.get_function_builtin(builtin, |s| s.code_eq(&type_));
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
            self.mono_union_subtype_index(type_, custom_type, constructor, args);
        let layout = self.union_layout(type_).clone();
        let variant_index = custom_type
            .constructors
            .iter()
            .position(|c| c.name == constructor.name)
            .expect("constructor in union");
        let mapping = layout.fields(variant_index).to_vec();
        let name = self.type_pretty_name(type_) + "." + constructor.name.clone();
        let builtin = BuiltinFunction::Equal(self.val_type_ref(type_index), name);
        let eq_index = self.get_function_builtin(builtin, |s| {
            s.code_composite_or_union_eq(type_index, Some(&mapping), types)
        });
        (type_index, eq_index)
    }

    fn code_eq(&mut self, type_: &Arc<Type>) -> FunctionId {
        if type_.is_string() {
            self.code_string_eq()
        } else if let Some(types) = type_.tuple_types() {
            let type_index = self.tuple_type_index(types.clone());
            self.code_composite_eq(type_index, types)
        } else if let Some((custom_type, args)) = self.custom_type(type_) {
            match custom_type {
                CustomType::Struct {
                    custom_type,
                    constructor,
                } => {
                    let (type_index, types) =
                        self.mono_struct_type_index(type_, &custom_type, &constructor, &args);
                    self.code_composite_eq(type_index, types)
                }
                CustomType::Union { custom_type, .. } => {
                    let supertype_index = self.mono_union_supertype_index(type_, &custom_type);
                    self.code_union_eq(type_, supertype_index, &custom_type, &args)
                }
                CustomType::External { .. } | CustomType::Enum { .. } => {
                    panic!("external/enum types should not reach code generation")
                }
            }
        } else {
            panic!("unexpected type should not reach equality code generation")
        }
    }

    fn code_string_eq(&mut self) -> FunctionId {
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

    fn code_string_starts_with(&mut self) -> FunctionId {
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

    fn code_composite_eq(
        &mut self,
        type_index: TypeId,
        types: impl IntoIterator<Item = Arc<Type>>,
    ) -> FunctionId {
        self.code_composite_or_union_eq(type_index, None, types)
    }

    fn code_composite_or_union_eq(
        &mut self,
        type_index: TypeId,
        field_mapping: Option<&[u32]>,
        types: impl IntoIterator<Item = Arc<Type>>,
    ) -> FunctionId {
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
        for (gleam_index, type_) in types.into_iter().enumerate() {
            let field_index = field_mapping
                .and_then(|m| m.get(gleam_index).copied())
                .unwrap_or(gleam_index as u32);
            let eq = self.function_eq(&type_);
            #[rustfmt::skip]
            let _ = instructions
                .local_get(a)
                .struct_get(type_index, field_index)
                .local_get(b)
                .struct_get(type_index, field_index)
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
        let composite_vt = if Self::null_variant_tag(custom_type).is_some() {
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
        let null_tag = Self::null_variant_tag(custom_type);

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
                            .call(eq_index)
                            .return_();
                    });
            }
        }
        let _ = instructions.bool_const(false).end();
        function.finish(self)
    }

    fn code_variant_constructor_for(
        &mut self,
        return_: Arc<Type>,
        variant: Variant,
        num_fields: u32,
    ) -> FunctionId {
        let (_, _, args) = return_
            .named_type_information()
            .expect("named type information");
        let (type_index, tag, types) = match &variant.custom_type {
            CustomType::External { .. } | CustomType::Enum { .. } => {
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
                (type_index, variant.tag, types)
            }
        };
        let null_supertype = if num_fields == 0 {
            if let CustomType::Union { custom_type, .. } = &variant.custom_type {
                if Self::null_variant_tag(custom_type).is_some() {
                    Some(self.mono_union_supertype_index(&return_, custom_type))
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };
        let field_mapping = if let CustomType::Union { custom_type, .. } = &variant.custom_type {
            let layout = self.union_layout(&return_).clone();
            let variant_index = custom_type
                .constructors
                .iter()
                .position(|c| c.name == variant.constructor.name)
                .expect("constructor in union");
            Some(layout.fields(variant_index).to_vec())
        } else {
            None
        };
        let result_type = self.val_type(&return_);
        // Params in Gleam (declaration) order — this is the function signature.
        let param_vts = self.val_types(types);
        self.code_variant_constructor(
            type_index,
            num_fields,
            tag,
            null_supertype,
            field_mapping.as_deref(),
            result_type,
            &param_vts,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn code_variant_constructor(
        &mut self,
        struct_index: TypeId,
        num_fields: u32,
        tag: Option<i32>,
        null_supertype: Option<TypeId>,
        field_mapping: Option<&[u32]>,
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
        if let Some(mapping) = field_mapping {
            // Emit local_get in wasm struct field order (shared first, then specific)
            let mut wasm_to_gleam: Vec<(u32, u32)> = mapping
                .iter()
                .enumerate()
                .map(|(gleam_pos, &field_index)| (field_index, gleam_pos as u32))
                .collect();
            wasm_to_gleam.sort_by_key(|(field_index, _)| *field_index);
            for (_, gleam_pos) in wasm_to_gleam {
                let _ = instructions.local_get(*param_ids.get(gleam_pos as usize).expect("param"));
            }
        } else {
            for index in 0..num_fields {
                let _ = instructions.local_get(*param_ids.get(index as usize).expect("param"));
            }
        }
        let _ = instructions.struct_new(struct_index).end();
        function.finish(self)
    }

    pub(super) fn code_i32_to_int(&mut self) -> FunctionId {
        let int_vt = self.int.val_type();
        let p = self.wasm_local(ValType::I32);
        let mut function = Function::new(self, "_i32_to_int", &[p], &[int_vt]);
        let _ = function
            .extend_instructions(self)
            .local_get(p)
            .i32_to_int()
            .end();
        function.finish(self)
    }

    pub(super) fn code_int_to_i32(&mut self) -> FunctionId {
        let int_vt = self.int.val_type();
        let p = self.wasm_local(int_vt);
        let mut function = Function::new(self, "_int_to_i32", &[p], &[ValType::I32]);
        let _ = function
            .extend_instructions(self)
            .local_get(p)
            .int_to_i32()
            .end();
        function.finish(self)
    }

    pub(super) fn code_i64_to_int(&mut self) -> FunctionId {
        let int_vt = self.int.val_type();
        let p = self.wasm_local(ValType::I64);
        let mut function = Function::new(self, "_i64_to_int", &[p], &[int_vt]);
        let _ = function
            .extend_instructions(self)
            .local_get(p)
            .i64_to_int()
            .end();
        function.finish(self)
    }

    pub(super) fn code_int_to_i64(&mut self) -> FunctionId {
        let int_vt = self.int.val_type();
        let p = self.wasm_local(int_vt);
        let mut function = Function::new(self, "_int_to_i64", &[p], &[ValType::I64]);
        let _ = function
            .extend_instructions(self)
            .local_get(p)
            .int_to_i64()
            .end();
        function.finish(self)
    }

    pub(super) fn code_f32_to_float(&mut self) -> FunctionId {
        let float_vt = self.float.val_type();
        let p = self.wasm_local(ValType::F32);
        let mut function = Function::new(self, "_f32_to_float", &[p], &[float_vt]);
        let _ = function
            .extend_instructions(self)
            .local_get(p)
            .f32_to_float()
            .end();
        function.finish(self)
    }

    pub(super) fn code_float_to_f32(&mut self) -> FunctionId {
        let float_vt = self.float.val_type();
        let p = self.wasm_local(float_vt);
        let mut function = Function::new(self, "_float_to_f32", &[p], &[ValType::F32]);
        let _ = function
            .extend_instructions(self)
            .local_get(p)
            .float_to_f32()
            .end();
        function.finish(self)
    }

    pub(super) fn code_f64_to_float(&mut self) -> FunctionId {
        let float_vt = self.float.val_type();
        let p = self.wasm_local(ValType::F64);
        let mut function = Function::new(self, "_f64_to_float", &[p], &[float_vt]);
        let _ = function
            .extend_instructions(self)
            .local_get(p)
            .f64_to_float()
            .end();
        function.finish(self)
    }

    pub(super) fn code_float_to_f64(&mut self) -> FunctionId {
        let float_vt = self.float.val_type();
        let p = self.wasm_local(float_vt);
        let mut function = Function::new(self, "_float_to_f64", &[p], &[ValType::F64]);
        let _ = function
            .extend_instructions(self)
            .local_get(p)
            .float_to_f64()
            .end();
        function.finish(self)
    }

    pub(super) fn code_int_to_utf_codepoint(&mut self) -> FunctionId {
        let int_vt = self.int.val_type();
        // params
        let i = self.wasm_local(int_vt); // Int
        // return Result(UtfCodepoint, Nil)
        let result = type_::result(type_::utf_codepoint(), type_::nil());
        let result_vt = self.val_type(&result);
        let is_codepoint = match self.int {
            IntType::I32 => self.find_global_expect(I32_IS_CODEPOINT),
            IntType::I64 => self.find_global_expect(I64_IS_CODEPOINT),
        };
        let ok_index = self.ok_variant_constructor(type_::utf_codepoint(), type_::nil());
        let error_index = self.error_variant_constructor(type_::utf_codepoint(), type_::nil());
        let mut function = Function::new(self, "_int_to_utf_codepoint", &[i], &[result_vt]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .local_get(i)
            .call(is_codepoint.func_id())
            .if_else(
                result_vt,
                |then_s| {
                    let _ = then_s
                        .local_get(i)
                        .int_to_i32()
                        .call(ok_index);
                },
                |else_s| {
                    let _ = else_s
                        .nil_const()
                        .call(error_index);
                },
            )
            .end();
        function.finish(self)
    }

    pub(super) fn function_inspect(&mut self, fn_type: &Arc<Type>) -> Id {
        let (params, _) = fn_type.fn_types().expect("inspect must be a function type");
        let arg_type = params
            .into_iter()
            .next()
            .expect("inspect takes one argument");

        let builtin =
            BuiltinFunction::Inspect(self.val_type(&arg_type), self.type_pretty_name(&arg_type));
        let name = builtin.name();
        let index = self.get_function_builtin(builtin, |s| s.code_inspect(&arg_type));
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

        let repr = if type_.is_list() {
            BuiltinFunction::ListRepr(self.val_type(type_), self.type_pretty_name(type_))
        } else if type_.is_tuple() {
            BuiltinFunction::TupleRepr(
                self.val_type(type_),
                self.type_pretty_name(type_).replace("#", "Tuple"),
            )
        } else if type_.fn_types().is_some() {
            BuiltinFunction::FunctionRepr(self.val_type(type_), self.type_pretty_name(type_))
        } else if self.custom_type(type_).is_some() {
            BuiltinFunction::CustomTypeRepr(
                self.val_type(type_),
                self.type_pretty_name(type_),
                None,
            )
        } else if type_.is_bit_array() {
            todo!("BitArray repr is not yet supported");
        } else {
            panic!("unexpected type should not reach repr: {type_:#?}");
        };

        let type_ = type_.clone();
        self.get_function_builtin(repr, |s| s.code_repr(&type_))
    }

    pub(super) fn code_int_repr(&mut self) -> FunctionId {
        let int_vt = self.int.val_type();
        let value = self.wasm_local(int_vt);
        let ptr = self.wasm_local(ValType::I32);
        let id = match self.int {
            IntType::I32 => self.find_global_expect(I32_TO_STR),
            IntType::I64 => self.find_global_expect(I64_TO_STR),
        };
        let mut function = Function::new(self, "_repr_int", &[value, ptr], &[ValType::I32]);
        let _ = function
            .extend_instructions(self)
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
        let name = self.type_pretty_name(type_);
        let (_, type_index, types) =
            self.mono_union_subtype_index(type_, custom_type, constructor, args);
        let constructor_name = constructor.name.clone();
        let builtin = BuiltinFunction::CustomTypeRepr(
            self.val_type_ref(type_index),
            name + "." + constructor_name.clone(),
            Some(constructor_name.clone()),
        );
        let layout = self.union_layout(type_).clone();
        let variant_index = custom_type
            .constructors
            .iter()
            .position(|c| c.name == constructor.name)
            .expect("constructor in union");
        let mapping = layout.fields(variant_index).to_vec();
        let repr_index = self.get_function_builtin(builtin, |s| {
            s.code_composite_repr(&constructor_name, Some(&mapping), type_index, &types)
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
        let mut function = Function::new(self, "_repr_float", &[value, ptr], &[ValType::I32]);
        let _ = function
            .extend_instructions(self)
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
        let len = self.wasm_local(ValType::I32); // I32
        let i = self.wasm_local(ValType::I32); // I32
        let s_i = self.wasm_local(ValType::I32); // I32
        let dest = self.wasm_local(ValType::I32); // I32
        let needed = self.wasm_local(ValType::I32); // I32
        // return I32 - number of written bytes

        let mut function = Function::new(self, "_repr_string", &[s, ptr], &[ValType::I32]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            // max output = ptr + 2 + len * 2
            .local_get(ptr)
            .local_get(s)
            .string_len()
            .i32_const(2)
            .i32_mul()
            .i32_add()
            .i32_const(2)
            .i32_add()
            .ensure_memory(needed)
            .local_get(ptr)
            .local_tee(dest)
            .byte_store(b'"')
            .i32_inc(dest)
            .local_get(s)
            .string_len()
            .local_set(len)
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
                            .local_get(s)
                            .local_get(i)
                            .string_get()
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
            .local_get(dest)
            .byte_store(b'"')
            .i32_inc(dest)
            .local_get(dest)
            .local_get(ptr)
            .i32_sub()
            .end();

        trait Escape {
            fn try_escape(&mut self, dest: LocalId, byte: u8, escape: u8) -> &mut Self;
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
                                .local_get(dest)
                                .byte_store(b'\\')
                                .i32_inc(dest)
                                .local_get(dest)
                                .byte_store(escape)
                                .i32_inc(dest)
                                .i32_const(1);
                        },
                        |else_s| {
                            let _ = else_s.i32_const(0);
                        },
                    )
            }
        }

        function.finish(self)
    }

    fn code_repr(&mut self, type_: &Arc<Type>) -> FunctionId {
        if let Some(item_type) = type_.list_type() {
            self.code_list_repr(&item_type)
        } else if let Some(types) = type_.tuple_types() {
            self.code_tuple_repr(&types)
        } else if type_.fn_types().is_some() {
            self.code_function_repr(type_)
        } else if let Some((custom_type, args)) = self.custom_type(type_) {
            self.code_custom_type_repr(type_, &custom_type, &args)
        } else if type_.is_bit_array() {
            todo!("BitArray repr is not yet supported");
        } else {
            panic!("unexpected type should not reach repr: {type_:#?}");
        }
    }

    fn code_list_repr(&mut self, item_type: &Arc<Type>) -> FunctionId {
        let list_type = type_::list(item_type.clone());
        let (custom_type, cons, _, args) = self.list_type_cons(&list_type);
        let (_, struct_index, _) =
            self.mono_union_subtype_index(&list_type, &custom_type, &cons, &args);
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
            .local_tee(dest)
            .byte_store(b'[')
            .i32_inc(dest)
            .local_get(lst)
            .ref_is_null()
            .if_else(
                InstrSeqType::Simple(None),
                |then_s| {
                    let _ = then_s
                        .local_get(dest)
                        .byte_store(b']')
                        .i32_const(2)
                        .return_();
                },
                |else_s| {
                    let _ = else_s
                        .local_get(lst)
                        .ref_cast_non_null(HeapType::Concrete(struct_index))
                        .struct_get(struct_index, 2) // first
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
                    .struct_get(struct_index, 1) // rest
                    .local_tee(lst)
                    .ref_is_null()
                    .if_else(
                        InstrSeqType::Simple(None),
                        |then_s| {
                            let _ = then_s
                                .local_get(dest)
                                .byte_store(b']')
                                .i32_inc(dest);
                        },
                        |else_s| {
                            let _ = else_s
                                .local_get(dest)
                                .byte_store(b',')
                                .i32_inc(dest)
                                .local_get(dest)
                                .byte_store(b' ')
                                .i32_inc(dest)
                                .local_get(lst)
                                .ref_cast_non_null(HeapType::Concrete(struct_index))
                                .struct_get(struct_index, 2) // first
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
        self.code_composite_repr(&"#".into(), None, type_index, types)
    }

    fn code_composite_repr(
        &mut self,
        name: &EcoString,
        field_mapping: Option<&[u32]>,
        type_index: TypeId,
        types: &[Arc<Type>],
    ) -> FunctionId {
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

        let wasm_indices: Vec<u32> = (0..types.len())
            .map(|i| {
                field_mapping
                    .and_then(|m| m.get(i).copied())
                    .unwrap_or(i as u32)
            })
            .collect();
        let mut fields_iter = types.iter().zip(wasm_indices.iter());
        if let Some((first_type, &first_idx)) = fields_iter.next() {
            let first_repr = self.function_repr(first_type);
            let _ = instructions
                .local_get(dest)
                .byte_store(b'(')
                .i32_inc(dest)
                .local_get(value)
                .struct_get(type_index, first_idx)
                .local_get(dest)
                .call(first_repr)
                .local_get(dest)
                .i32_add()
                .local_set(dest);

            for (type_, &field_index) in fields_iter {
                let repr = self.function_repr(type_);
                let _ = instructions
                    .local_get(dest)
                    .byte_store(b',')
                    .i32_inc(dest)
                    .local_get(dest)
                    .byte_store(b' ')
                    .i32_inc(dest)
                    .local_get(value)
                    .struct_get(type_index, field_index)
                    .local_get(dest)
                    .call(repr)
                    .local_get(dest)
                    .i32_add()
                    .local_set(dest);
            }

            let _ = instructions.local_get(dest).byte_store(b')').i32_inc(dest);
        }

        let _ = instructions.local_get(dest).local_get(ptr).i32_sub().end();
        function.finish(self)
    }

    fn code_function_repr(&mut self, type_: &Arc<Type>) -> FunctionId {
        let fn_vt = self.val_type(type_);
        // params
        let _func = self.wasm_local(fn_vt); // Function
        let ptr = self.wasm_local(ValType::I32); // I32
        // return I32 - number of written bytes
        let repr = format!("//{} {{ ... }}", self.type_pretty_name(type_));
        let string_index = self.string_index(&repr.into());
        let string_to_memory =
            self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
        let mut function = Function::new(self, "_repr_function", &[_func, ptr], &[ValType::I32]);
        let _ = function
            .extend_instructions(self)
            .global_as_non_null(string_index)
            .local_get(ptr)
            .call(string_to_memory)
            .end();
        function.finish(self)
    }

    pub(super) fn code_utf_codepoint_repr(&mut self) -> FunctionId {
        // params
        let _func = self.wasm_local(ValType::I32); // Function
        let ptr = self.wasm_local(ValType::I32); // I32
        // return I32 - number of written bytes
        let repr = "//utfcodepoint()";
        let string_index = self.string_index(&repr.into());
        let string_to_memory =
            self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
        let mut function =
            Function::new(self, "_repr_utf_code_point", &[_func, ptr], &[ValType::I32]);
        let _ = function
            .extend_instructions(self)
            .global_as_non_null(string_index)
            .local_get(ptr)
            .call(string_to_memory)
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
            CustomType::External { to_str, .. } => {
                let to_str_fn = self.find_global_expect(to_str);
                let mut function =
                    Function::new(self, "_repr_external", &[value, ptr], &[ValType::I32]);
                let _ = function
                    .extend_instructions(self)
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
                self.code_composite_repr(&constructor.name, None, type_index, &types)
            }
            CustomType::Union { custom_type, .. } => {
                let custom_type = custom_type.clone();
                let supertype_index = self.mono_union_supertype_index(type_, &custom_type);
                let null_tag = Self::null_variant_tag(&custom_type);

                let null_name_and_index = if let Some(null_idx) = null_tag {
                    let null_name = custom_type
                        .constructors
                        .get(null_idx)
                        .expect("null variant for union type")
                        .name
                        .clone();
                    let _ = null_idx;
                    Some(self.string_index(&null_name))
                } else {
                    None
                };

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
        let len = self.wasm_local(ValType::I32); // I32
        let i = self.wasm_local(ValType::I32); // I32
        let needed = self.wasm_local(ValType::I32); // I32
        // result len - I32
        let mut function = Function::new(self, "_string_to_memory", &[s, dest], &[ValType::I32]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(s)
            .array_len()
            .local_set(len)
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
                            .local_get(s)
                            .local_get(i)
                            .string_get()
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
        // return String
        let s = self.wasm_local(string_vt);
        let string_type_index = self.string.type_index;
        let mut function = Function::new(self, "_memory_to_string", &[src, len], &[string_vt]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(len)
            .string_new()
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
                            .array_set(string_type_index)
                            .i32_inc(src)
                            .i32_inc(i)
                            .br(loop_id);
                    });
            })
            .local_get(s)
            .end();
        function.finish(self)
    }

    pub(super) fn code_parse_int(&mut self) -> FunctionId {
        let parse = match self.int {
            IntType::I32 => self.find_global_expect(I32_PARSE),
            IntType::I64 => self.find_global_expect(I64_PARSE),
        };
        self.code_parse(type_::int(), parse.func_id())
    }

    pub(super) fn code_parse_float(&mut self) -> FunctionId {
        let parse = match self.float {
            FloatType::F32 => self.find_global_expect(F32_PARSE),
            FloatType::F64 => self.find_global_expect(F64_PARSE),
        };
        self.code_parse(type_::float(), parse.func_id())
    }

    fn code_parse(&mut self, type_: Arc<Type>, parse: FunctionId) -> FunctionId {
        let string_vt = self.string.val_type();
        let value_vt = self.val_type(&type_);
        // params
        let s = self.wasm_local(string_vt); // String
        // locals
        let ptr = self.wasm_local(ValType::I32); // I32
        let dest = self.wasm_local(ValType::I32); // I32
        let len = self.wasm_local(ValType::I32); // I32
        let r = self.wasm_local(value_vt); // type_
        // return Result(type, Nil)
        let string_to_memory =
            self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
        let heap_base = self.find_global_expect(HEAP_BASE);
        let result = type_::result(type_.clone(), type_::nil());
        let result_vt = self.val_type(&result);
        let ok_index = self.ok_variant_constructor(type_.clone(), type_::nil());
        let error_index = self.error_variant_constructor(type_.clone(), type_::nil());
        let mut function = Function::new(self, "_parse", &[s], &[result_vt]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .call(heap_base.func_id())
            .local_tee(ptr)
            .i32_const(0)
            .i32_store(MemArg { offset: 0, align: 2 })
            .local_get(ptr)
            .i32_const(4)
            .i32_add()
            .local_set(dest)
            .local_get(s)
            .local_get(dest)
            .call(string_to_memory)
            .local_set(len)
            .local_get(ptr) // result
            .local_get(dest) // ptr
            .local_get(len)
            .call(parse)
            .local_set(r)
            .local_get(ptr)
            .i32_load(MemArg { offset: 0, align: 2 })
            .if_else(
                result_vt,
                |then_s| {
                    let _ = then_s
                        .local_get(r)
                        .call(ok_index);
                },
                |else_s| {
                    let _ = else_s
                        .nil_const()
                        .call(error_index);
                },
            )
            .end();
        function.finish(self)
    }

    fn get_function_builtin(
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
        let _ = self.builtins.insert(builtin.clone(), placeholder_id);
        let real_id = code_fn(self);
        replace_function_body(&mut self.wasm_module, placeholder_id, real_id);
        placeholder_id
    }
}
