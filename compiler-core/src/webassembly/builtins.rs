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

    /// Get the WASM-level parameter and result types for conversion builtins.
    /// Returns None for builtins that use complex types (strings, results, etc.).
    pub(super) fn wasm_type(
        &self,
        int: IntType,
        float: FloatType,
    ) -> Option<(Vec<ValType>, Vec<ValType>)> {
        let i32 = ValType::I32;
        let i64 = ValType::I64;
        let f32 = ValType::F32;
        let f64 = ValType::F64;
        let int_vt = int.val_type();
        let float_vt = float.val_type();
        Some(match self {
            BuiltinFunctionExternal::I32ToInt => (vec![i32], vec![int_vt]),
            BuiltinFunctionExternal::IntToI32 => (vec![int_vt], vec![i32]),
            BuiltinFunctionExternal::I64ToInt => (vec![i64], vec![int_vt]),
            BuiltinFunctionExternal::IntToI64 => (vec![int_vt], vec![i64]),
            BuiltinFunctionExternal::F32ToFloat => (vec![f32], vec![float_vt]),
            BuiltinFunctionExternal::FloatToF32 => (vec![float_vt], vec![f32]),
            BuiltinFunctionExternal::F64ToFloat => (vec![f64], vec![float_vt]),
            BuiltinFunctionExternal::FloatToF64 => (vec![float_vt], vec![f64]),
            _ => return None,
        })
    }
}

impl<'a> Generator<'a> {
    pub(super) fn function_start(&mut self) -> u32 {
        let function = self.code_start();
        self.add_function(
            "$start".into(),
            true,
            vec![],
            vec![],
            function.into_raw_body(),
        )
        .index
    }

    fn code_start(&mut self) -> Function {
        let mut function = Function::new(vec![]);
        let mut instructions = function.extend_instructions(self);
        // string data
        for (string, index) in &self.strings {
            let data_segment = self.data_section.len();
            let _ = self.data_section.passive(string.as_bytes().iter().cloned());
            let _ = instructions
                .i32_const(0)
                .i32_const(string.len() as i32)
                .array_new_data(self.string.type_index, data_segment)
                .ref_as_non_null()
                .global_set(*index);
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
                        .global_get(self.find_global_expect(&name).index)
                        .global_set(global_index);
                }
            }
        }
        // main
        if let Some(main) = &self.main {
            let _ = function.instructions().call(*main).drop();
        }
        let _ = function.instructions().end();
        function
    }

    pub(super) fn variant_constructor(
        &mut self,
        params: Vec<Arc<Type>>,
        return_: Arc<Type>,
        variant: Variant,
    ) -> u32 {
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

    fn ok_variant_constructor(&mut self, ok: Arc<Type>, error: Arc<Type>) -> u32 {
        self.variant_constructor(
            vec![ok.clone()],
            type_::result(ok, error),
            self.variant_expect(PRELUDE_MODULE_NAME.into(), "Ok".into()),
        )
    }

    fn error_variant_constructor(&mut self, ok: Arc<Type>, error: Arc<Type>) -> u32 {
        self.variant_constructor(
            vec![error.clone()],
            type_::result(ok, error),
            self.variant_expect(PRELUDE_MODULE_NAME.into(), "Error".into()),
        )
    }

    pub(super) fn string_index(&mut self, string: &EcoString) -> u32 {
        let string = unescape(string);
        let index = self.global_section.len();
        *self.strings.entry(string.into()).or_insert_with(|| {
            let _ = self.global_section.global(
                GlobalType {
                    val_type: self.string.val_type_nullable(),
                    mutable: true,
                    shared: false,
                },
                &ConstExpr::ref_null(self.string.heap_type()),
            );
            index
        })
    }

    pub(super) fn code_string_concat(&self) -> Function {
        let mut function = Function::new(vec![(2, ValType::I32), (1, self.string.val_type())]);
        let mut instructions = function.extend_instructions(self);
        // params
        let a = 0; // String
        let b = 1; // String
        // locals
        let len_a = 2; // I32
        let len_b = 3; // I32
        // return
        let r = 4; // String
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
        function
    }

    pub(super) fn code_string_num_bytes(&self) -> Function {
        let mut function = Function::new(vec![]);
        let mut instructions = function.extend_instructions(self);
        // params
        let s = 0; // String
        // return Int
        let _ = instructions.local_get(s).string_len().i32_to_int().end();
        function
    }

    pub(super) fn code_string_get_byte(&mut self) -> Function {
        let mut function = Function::new(vec![]);
        let mut instructions = function.extend_instructions(self);
        // params
        let s = 0; // String
        let i = 1; // Index
        // return Int
        let result = type_::result(type_::int(), type_::nil());
        let ok_index = self.ok_variant_constructor(type_::int(), type_::nil());
        let error_index = self.error_variant_constructor(type_::int(), type_::nil());
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
            .if_(BlockType::Result(self.val_type(&result)))
              .local_get(s)
              .local_get(i)
              .int_to_i32()
              .string_get()
              .i32_to_int()
              .call(ok_index)
            .else_()
              .nil_const()
              .call(error_index)
            .end()
            .end();
        function
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
    ) -> (u32, u32) {
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

    fn code_eq(&mut self, type_: &Arc<Type>) -> Function {
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

    fn code_string_eq(&self) -> Function {
        let mut function = Function::new(vec![(2, ValType::I32)]);
        let mut instructions = function.extend_instructions(self);
        // params
        let a = 0; // String
        let b = 1; // String
        // locals
        let i = 2; // I32
        let len = 3; // I32
        // return Bool
        #[rustfmt::skip]
        let _ = instructions
            .local_get(a)
            .local_get(b)
            .ref_eq()
            // if ref a == ref b
            .if_(BlockType::Empty)
              .bool_const(true)
              .return_()
            .end()
            // len = a.len; push len
            .local_get(a)
            .string_len()
            .local_tee(len)
            // b.len
            .local_get(b)
            .string_len()
            .i32_ne()
            // if a.len != b.len
            .if_(BlockType::Empty)
              .bool_const(false)
              .return_()
            .end()
            // i = 0
            .i32_const(0)
            .local_set(i)
            // loop
            .loop_(BlockType::Empty)
              .local_get(i)
              .local_get(len)
              .i32_ge_u()
              // if i >= len
              .if_(BlockType::Empty)
                .bool_const(true)
                .return_()
              .end()
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
              .if_(BlockType::Empty)
                .bool_const(false)
                .return_()
              .end()
              .i32_inc(i)
              // loop
              .br(0)
            // end loop
            .end()
            .bool_const(true)
            // end function
            .end();
        function
    }

    pub(super) fn function_string_starts_with(&mut self) -> u32 {
        self.get_function_builtin(BuiltinFunction::StringStartsWith, |s| {
            s.code_string_starts_with()
        })
    }

    fn code_string_starts_with(&self) -> Function {
        let mut function = Function::new(vec![(2, ValType::I32)]);
        let mut instructions = function.extend_instructions(self);
        // params
        let a = 0; // String
        let b = 1; // String
        // locals
        let i = 2; // I32
        let prefix_len = 3; // I32
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
            .if_(BlockType::Empty)
              .bool_const(false)
              .return_()
            .end()
            .i32_const(0)
            .local_set(i)
            // loop
            .loop_(BlockType::Empty)
              .local_get(i)
              .local_get(prefix_len)
              .i32_ge_u()
              .if_(BlockType::Empty)
                .bool_const(true)
                .return_()
              .end()
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
              .if_(BlockType::Empty)
                .bool_const(false)
                .return_()
              .end()
              .i32_inc(i)
              // loop
              .br(0)
            // end loop
            .end()
            .bool_const(true)
            .end();
        function
    }

    fn code_composite_eq(
        &mut self,
        type_index: u32,
        types: impl IntoIterator<Item = Arc<Type>>,
    ) -> Function {
        self.code_composite_or_union_eq(type_index, None, types)
    }

    fn code_composite_or_union_eq(
        &mut self,
        type_index: u32,
        field_mapping: Option<&[u32]>,
        types: impl IntoIterator<Item = Arc<Type>>,
    ) -> Function {
        let mut function = Function::new(vec![]);
        let mut instructions = function.extend_instructions(self);
        // params
        let a = 0; // composite
        let b = 1; // composite
        // return Bool
        #[rustfmt::skip]
        let _ = instructions
            .local_get(a)
            .local_get(b)
            .ref_eq()
            .if_(BlockType::Empty)
              .bool_const(true)
              .return_()
            .end();
        for (gleam_index, type_) in types.into_iter().enumerate() {
            let field_index = field_mapping
                .and_then(|m| m.get(gleam_index).copied())
                .unwrap_or(gleam_index as u32);
            #[rustfmt::skip]
            let _ = instructions
                .local_get(a)
                .struct_get(type_index, field_index)
                .local_get(b)
                .struct_get(type_index, field_index)
                .eq(self.function_eq(&type_))
                .bool_not()
                .if_(BlockType::Empty)
                  .bool_const(false)
                  .return_()
                .end();
        }
        let _ = instructions.bool_const(true).end();
        function
    }

    fn code_union_eq(
        &mut self,
        type_: &Arc<Type>,
        supertype_index: u32,
        custom_type: &TypedCustomType,
        args: &[Arc<Type>],
    ) -> Function {
        let mut function = Function::new(vec![(1, ValType::I32)]);
        let mut instructions = function.extend_instructions(self);
        // params
        let a = 0; // union
        let b = 1; // union
        // locals
        let tag = 2;
        // return Bool
        let null_tag = Self::null_variant_tag(custom_type);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(a)
            .local_get(b)
            .ref_eq()
            .if_(BlockType::Empty)
              .bool_const(true)
              .return_()
            .end();

        if null_tag.is_some() {
            // If either is null but not both (ref_eq already handled both-null)
            #[rustfmt::skip]
            let _ = instructions
                .local_get(a)
                .ref_is_null()
                .local_get(b)
                .ref_is_null()
                .i32_or()
                .if_(BlockType::Empty)
                  .bool_const(false)
                  .return_()
                .end();
        }

        #[rustfmt::skip]
        let _ = instructions
            .local_get(a)
            .ref_as_non_null()
            .struct_get(supertype_index, 0)
            .local_tee(tag)
            .local_get(b)
            .ref_as_non_null()
            .struct_get(supertype_index, 0)
            .i32_ne()
            .if_(BlockType::Empty)
              .bool_const(false)
              .return_()
            .end()
            ;

        let n = custom_type.constructors.len() as u32;
        for _ in 0..n {
            let _ = instructions.block(BlockType::Empty);
        }
        let _ = instructions.local_get(tag).br_table(0..n - 1, n - 1);
        for (i, constructor) in custom_type.constructors.iter().enumerate() {
            let _ = instructions.end();
            if null_tag == Some(i) {
                let _ = instructions.unreachable();
            } else {
                let (type_index, eq_index) =
                    self.function_variant_eq(type_, custom_type, constructor, args);
                #[rustfmt::skip]
                let _ = instructions
                    .local_get(a)
                    .ref_cast_non_null(HeapType::Concrete(type_index))
                    .local_get(b)
                    .ref_cast_non_null(HeapType::Concrete(type_index))
                    .call(eq_index)
                    .return_();
            }
        }
        let _ = instructions.end();
        function
    }

    fn code_variant_constructor_for(
        &mut self,
        return_: Arc<Type>,
        variant: Variant,
        num_fields: u32,
    ) -> Function {
        let (_, _, args) = return_
            .named_type_information()
            .expect("named type information");
        let (type_index, tag) = match &variant.custom_type {
            CustomType::External { .. } | CustomType::Enum { .. } => {
                panic!("external/enum types should not reach code generation")
            }
            CustomType::Struct {
                custom_type,
                constructor,
                ..
            } => (
                self.mono_struct_type_index(&return_, custom_type, constructor, &args)
                    .0,
                None,
            ),
            CustomType::Union { custom_type, .. } => {
                let (_, type_index, _) = self.mono_union_subtype_index(
                    &return_,
                    custom_type,
                    &variant.constructor,
                    &args,
                );
                (type_index, variant.tag)
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
        self.code_variant_constructor(
            type_index,
            num_fields,
            tag,
            null_supertype,
            field_mapping.as_deref(),
        )
    }

    fn code_variant_constructor(
        &mut self,
        struct_index: u32,
        num_fields: u32,
        tag: Option<i32>,
        null_supertype: Option<u32>,
        field_mapping: Option<&[u32]>,
    ) -> Function {
        let mut function = Function::new(vec![]);
        let mut instructions = function.extend_instructions(self);
        if let Some(supertype_index) = null_supertype {
            let _ = instructions
                .ref_null(HeapType::Concrete(supertype_index))
                .end();
            return function;
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
                let _ = instructions.local_get(gleam_pos);
            }
        } else {
            for index in 0..num_fields {
                let _ = instructions.local_get(index);
            }
        }
        let _ = instructions.struct_new(struct_index).end();
        function
    }

    pub(super) fn code_i32_to_int(&self) -> Function {
        let mut function = Function::new(vec![]);
        let _ = function
            .extend_instructions(self)
            .local_get(0)
            .i32_to_int()
            .end();
        function
    }

    pub(super) fn code_int_to_i32(&self) -> Function {
        let mut function = Function::new(vec![]);
        let _ = function
            .extend_instructions(self)
            .local_get(0)
            .int_to_i32()
            .end();
        function
    }

    pub(super) fn code_i64_to_int(&self) -> Function {
        let mut function = Function::new(vec![]);
        let _ = function
            .extend_instructions(self)
            .local_get(0)
            .i64_to_int()
            .end();
        function
    }

    pub(super) fn code_int_to_i64(&self) -> Function {
        let mut function = Function::new(vec![]);
        let _ = function
            .extend_instructions(self)
            .local_get(0)
            .int_to_i64()
            .end();
        function
    }

    pub(super) fn code_f32_to_float(&self) -> Function {
        let mut function = Function::new(vec![]);
        let _ = function
            .extend_instructions(self)
            .local_get(0)
            .f32_to_float()
            .end();
        function
    }

    pub(super) fn code_float_to_f32(&self) -> Function {
        let mut function = Function::new(vec![]);
        let _ = function
            .extend_instructions(self)
            .local_get(0)
            .float_to_f32()
            .end();
        function
    }

    pub(super) fn code_f64_to_float(&self) -> Function {
        let mut function = Function::new(vec![]);
        let _ = function
            .extend_instructions(self)
            .local_get(0)
            .f64_to_float()
            .end();
        function
    }

    pub(super) fn code_float_to_f64(&self) -> Function {
        let mut function = Function::new(vec![]);
        let _ = function
            .extend_instructions(self)
            .local_get(0)
            .float_to_f64()
            .end();
        function
    }

    pub(super) fn code_int_to_utf_codepoint(&mut self) -> Function {
        let mut function = Function::new(vec![]);
        // params
        let i = 0; // Int
        // return Result(UtfCodepoint, Nil)
        let is_codepoint = match self.int {
            IntType::I32 => self.find_global_expect(I32_IS_CODEPOINT),
            IntType::I64 => self.find_global_expect(I64_IS_CODEPOINT),
        };
        let ok_index = self.ok_variant_constructor(type_::utf_codepoint(), type_::nil());
        let error_index = self.error_variant_constructor(type_::utf_codepoint(), type_::nil());
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .local_get(i)
            .call(is_codepoint.index)
            .if_(BlockType::Result(self.val_type(&type_::result(type_::utf_codepoint(), type_::nil()))))
              .local_get(i)
              .int_to_i32()
              .call(ok_index)
            .else_()
              .nil_const()
              .call(error_index)
            .end()
            .end();
        function
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

    fn code_inspect(&mut self, arg_type: &Arc<Type>) -> Function {
        let repr_index = self.function_repr(arg_type);
        let heap_base = self.find_global_expect(HEAP_BASE);
        let memory_to_string =
            self.get_function_builtin_external(BuiltinFunctionExternal::MemoryToString);

        let mut function = Function::new(vec![(1, ValType::I32)]);
        let mut instructions = function.extend_instructions(self);
        // params
        let value = 0;
        // locals
        let ptr = 1;
        #[rustfmt::skip]
        let _ = instructions
            .call(heap_base.index)
            .local_set(ptr)
            .local_get(ptr)
            .local_get(value)
            .local_get(ptr)
            .call(repr_index)
            .call(memory_to_string)
            .end();
        function
    }

    pub(super) fn function_repr(&mut self, type_: &Arc<Type>) -> u32 {
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

    pub(super) fn code_int_repr(&self) -> Function {
        let mut function = Function::new(vec![]);
        let id = match self.int {
            IntType::I32 => self.find_global_expect(I32_TO_STR),
            IntType::I64 => self.find_global_expect(I64_TO_STR),
        };
        let _ = function
            .instructions()
            .local_get(0)
            .local_get(1)
            .call(id.index)
            .end();
        function
    }

    fn function_variant_repr(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &TypedCustomType,
        constructor: &TypedRecordConstructor,
        args: &[Arc<Type>],
    ) -> (u32, u32) {
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

    pub(super) fn code_float_repr(&self) -> Function {
        let mut function = Function::new(vec![]);
        let id = match self.float {
            FloatType::F32 => self.find_global_expect(F32_TO_STR),
            FloatType::F64 => self.find_global_expect(F64_TO_STR),
        };
        let _ = function
            .instructions()
            .local_get(0)
            .local_get(1)
            .call(id.index)
            .end();
        function
    }

    pub(super) fn code_string_repr(&self) -> Function {
        let mut function = Function::new(vec![(5, ValType::I32)]);
        // params
        let s = 0; // String
        let ptr = 1; // I32
        // locals
        let len = 2; // I32
        let i = 3; // I32
        let s_i = 4; // I32
        let dest = 5; // I32
        let needed = 6; // I32
        // return I32 - number of written bytes
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
            .loop_(BlockType::Empty)
              .local_get(i)
              .local_get(len)
              .i32_lt_u()
              .if_(BlockType::Empty)
                // value = s[i]
                .local_get(s)
                .local_get(i)
                .string_get()
                .local_set(s_i)
                .block(BlockType::Empty)
                  .local_get(s_i)
                  .try_escape(dest, b'"', b'"')
                  .br_if(0)
                  .local_get(s_i)
                  .try_escape(dest, b'\\', b'\\')
                  .br_if(0)
                  .local_get(s_i)
                  .try_escape(dest, b'\x0C', b'f')
                  .br_if(0)
                  .local_get(s_i)
                  .try_escape(dest, b'\n', b'n')
                  .br_if(0)
                  .local_get(s_i)
                  .try_escape(dest, b'\r', b'r')
                  .br_if(0)
                  .local_get(s_i)
                  .try_escape(dest, b'\t', b't')
                  .br_if(0)
                  // default, do not escape
                  .local_get(dest)
                  .local_get(s_i)
                  .i32_store8(MemArg {
                      offset: 0,
                      align: 0,
                      memory_index: 0,
                  })
                  .i32_inc(dest)
                // block
                .end()
                .i32_inc(i)
                .br(1)
              // if
              .end()
            // loop
            .end()
            .local_get(dest)
            .byte_store(b'"')
            .i32_inc(dest)
            .local_get(dest)
            .local_get(ptr)
            .i32_sub()
            .end();

        trait Escape {
            fn try_escape(&mut self, dest: u32, byte: u8, escape: u8) -> &mut Self;
        }

        impl<'a> Escape for ExtendedInstructionSink<'a> {
            #[rustfmt::skip]
            fn try_escape(&mut self, dest: u32, byte: u8, escape: u8) -> &mut Self {
                self.i32_const(byte as i32)
                    .i32_eq()
                    .if_(BlockType::Result(ValType::I32))
                      .local_get(dest)
                      .byte_store(b'\\')
                      .i32_inc(dest)
                      .local_get(dest)
                      .byte_store(escape)
                      .i32_inc(dest)
                      .i32_const(1)
                    .else_()
                      .i32_const(0)
                    .end()
            }
        }

        function
    }

    fn code_repr(&mut self, type_: &Arc<Type>) -> Function {
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

    fn code_list_repr(&mut self, item_type: &Arc<Type>) -> Function {
        let list_type = type_::list(item_type.clone());
        let (custom_type, cons, _, args) = self.list_type_cons(&list_type);
        let (_, struct_index, _) =
            self.mono_union_subtype_index(&list_type, &custom_type, &cons, &args);
        let mut function = Function::new(vec![(1, ValType::I32)]);
        // params
        let lst = 0; // List(a)
        let ptr = 1; // I32
        let dest = 2; // I32
        // return I32 - number of written bytes
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(ptr)
            .local_tee(dest)
            .byte_store(b'[')
            .i32_inc(dest)
            .local_get(lst)
            .ref_is_null()
            .if_(BlockType::Empty)
              .local_get(dest)
              .byte_store(b']')
              .i32_const(2)
              .return_()
            .else_()
              .local_get(lst)
              .ref_cast_non_null(HeapType::Concrete(struct_index))
              .struct_get(struct_index, 2) // first
              .local_get(dest)
              .call(self.function_repr(item_type))
              .local_get(dest)
              .i32_add()
              .local_set(dest)
            // if
            .end()
            .loop_(BlockType::Empty)
              .local_get(lst)
              .ref_cast_non_null(HeapType::Concrete(struct_index))
              .struct_get(struct_index, 1) // rest
              .local_tee(lst)
              .ref_is_null()
              .if_(BlockType::Empty)
                .local_get(dest)
                .byte_store(b']')
                .i32_inc(dest)
              .else_()
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
                .call(self.function_repr(item_type))
                .local_get(dest)
                .i32_add()
                .local_set(dest)
                .br(1)
              .end()
            // loop
            .end()
            .local_get(dest)
            .local_get(ptr)
            .i32_sub()
            // function
            .end();
        function
    }

    fn code_tuple_repr(&mut self, types: &[Arc<Type>]) -> Function {
        let type_index = self.tuple_type_index(types.iter().cloned());
        self.code_composite_repr(&"#".into(), None, type_index, types)
    }

    fn code_composite_repr(
        &mut self,
        name: &EcoString,
        field_mapping: Option<&[u32]>,
        type_index: u32,
        types: &[Arc<Type>],
    ) -> Function {
        let mut function = Function::new(vec![(1, ValType::I32)]);
        // params
        let value = 0;
        let ptr = 1; // I32
        // local
        let dest = 2; // I32
        // return I32 - number of written bytes
        let string_to_memory =
            self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
        let string_index = self.string_index(name);
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
            let _ = instructions
                .local_get(dest)
                .byte_store(b'(')
                .i32_inc(dest)
                .local_get(value)
                .struct_get(type_index, first_idx)
                .local_get(dest)
                .call(self.function_repr(first_type))
                .local_get(dest)
                .i32_add()
                .local_set(dest);

            for (type_, &field_index) in fields_iter {
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
                    .call(self.function_repr(type_))
                    .local_get(dest)
                    .i32_add()
                    .local_set(dest);
            }

            let _ = instructions.local_get(dest).byte_store(b')').i32_inc(dest);
        }

        let _ = instructions.local_get(dest).local_get(ptr).i32_sub().end();

        function
    }

    fn code_function_repr(&mut self, type_: &Arc<Type>) -> Function {
        let mut function = Function::new(vec![]);
        // params
        let _func = 0; // Function
        let ptr = 1; // I32
        // return I32 - number of written bytes
        let repr = format!("//{} {{ ... }}", self.type_pretty_name(type_));
        let string_index = self.string_index(&repr.into());
        let _ = function
            .extend_instructions(self)
            .global_as_non_null(string_index)
            .local_get(ptr)
            .call(self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory))
            .end();
        function
    }

    pub(super) fn code_utf_codepoint_repr(&mut self) -> Function {
        let mut function = Function::new(vec![]);
        // params
        let _func = 0; // Function
        let ptr = 1; // I32
        // return I32 - number of written bytes
        let repr = "//utfcodepoint()";
        let string_index = self.string_index(&repr.into());
        let _ = function
            .extend_instructions(self)
            .global_as_non_null(string_index)
            .local_get(ptr)
            .call(self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory))
            .end();
        function
    }

    fn code_custom_type_repr(
        &mut self,
        type_: &Arc<Type>,
        custom_type: &CustomType,
        args: &[Arc<Type>],
    ) -> Function {
        let mut function = Function::new(vec![]);
        // params
        let value = 0; // value
        let ptr = 1; // I32
        // return I32 - number of written bytes
        let string_to_memory =
            self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
        let mut instructions = function.extend_instructions(self);
        match custom_type {
            CustomType::External { to_str, .. } => {
                let to_str_fn = self.find_global_expect(to_str);
                let _ = instructions
                    .local_get(value)
                    .local_get(ptr)
                    .call(to_str_fn.index)
                    .end();
            }
            CustomType::Enum { values } => {
                let _ = instructions.block(BlockType::Result(self.string.val_type()));
                for (enum_value, name) in values.iter().enumerate() {
                    let string_index = self.string_index(name);
                    #[rustfmt::skip]
                    let _ = instructions
                        .local_get(value)
                        .i32_const(enum_value as i32)
                        .i32_eq()
                        .if_(BlockType::Empty)
                          .global_as_non_null(string_index)
                          .br(1)
                        // if
                        .end();
                }
                let _ = instructions
                    .unreachable()
                    .end()
                    .local_get(ptr)
                    .call(string_to_memory)
                    .end();
            }
            CustomType::Struct {
                custom_type,
                constructor,
            } => {
                let (type_index, types) =
                    self.mono_struct_type_index(type_, custom_type, constructor, args);
                return self.code_composite_repr(&constructor.name, None, type_index, &types);
            }
            CustomType::Union { custom_type, .. } => {
                let _ = instructions.block(BlockType::Result(ValType::I32));
                let supertype_index = self.mono_union_supertype_index(type_, custom_type);
                let null_tag = Self::null_variant_tag(custom_type);

                // Handle null variant before struct_get
                if let Some(null_idx) = null_tag {
                    let null_name = custom_type
                        .constructors
                        .get(null_idx)
                        .expect("null variant for union type")
                        .name
                        .clone();
                    let string_index = self.string_index(&null_name);
                    #[rustfmt::skip]
                    let _ = instructions
                        .local_get(value)
                        .ref_is_null()
                        .if_(BlockType::Empty)
                          .global_as_non_null(string_index)
                          .local_get(ptr)
                          .call(string_to_memory)
                          .br(1)
                        .end();
                }

                let n = custom_type.constructors.len() as u32;
                for _ in 0..n {
                    let _ = instructions.block(BlockType::Empty);
                }
                let _ = instructions
                    .local_get(value)
                    .ref_as_non_null()
                    .struct_get(supertype_index, 0)
                    .br_table(0..n - 1, n - 1);
                for (i, constructor) in custom_type.constructors.iter().enumerate() {
                    let _ = instructions.end();
                    if null_tag == Some(i) {
                        let _ = instructions.unreachable();
                    } else {
                        let (type_index, repr_index) =
                            self.function_variant_repr(type_, custom_type, constructor, args);
                        #[rustfmt::skip]
                        let _ = instructions
                            .local_get(value)
                            .ref_cast_non_null(HeapType::Concrete(type_index))
                            .local_get(ptr)
                            .call(repr_index)
                            .br(n - 1 - i as u32);
                    }
                }
                let _ = instructions.end().end();
            }
        };

        function
    }

    pub(super) fn code_string_to_memory(&self) -> Function {
        let mut function = Function::new(vec![(3, ValType::I32)]);
        // params
        let s = 0; // String
        let dest = 1; // I32
        // locals
        let len = 2; // I32
        let i = 3; // I32
        let needed = 4; // I32
        // result len - I32
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
            .loop_(BlockType::Empty)
              .local_get(i)
              .local_get(len)
              .i32_lt_u()
              .if_(BlockType::Empty)
                .local_get(dest)
                // value
                .local_get(s)
                .local_get(i)
                .string_get()
                // store
                .i32_store8(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                })
                .i32_inc(dest)
                .i32_inc(i)
                .br(1)
              // if
              .end()
            // loop
            .end()
            .local_get(len)
            .end();
        function
    }

    pub(super) fn code_memory_to_string(&self) -> Function {
        let mut function = Function::new(vec![(1, ValType::I32), (1, self.string.val_type())]);
        // params
        let src = 0; // I32
        let len = 1; // I32
        // locals
        let i = 2; // I32
        // return String
        let s = 3;
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(len)
            .string_new()
            .local_set(s)
            .i32_const(0)
            .local_set(i)
            .loop_(BlockType::Empty)
              .local_get(i)
              .local_get(len)
              .i32_lt_u()
              .if_(BlockType::Empty)
                .local_get(s)
                .local_get(i)
                .local_get(src)
                .i32_load8_u(MemArg { offset: 0, align: 0, memory_index: 0 })
                .array_set(self.string.type_index)
                .i32_inc(src)
                .i32_inc(i)
                .br(1)
              // if
              .end()
            // loop
            .end()
            .local_get(s)
            .end();
        function
    }

    pub(super) fn code_parse_int(&mut self) -> Function {
        let parse = match self.int {
            IntType::I32 => self.find_global_expect(I32_PARSE),
            IntType::I64 => self.find_global_expect(I64_PARSE),
        };
        self.code_parse(type_::int(), parse.index)
    }

    pub(super) fn code_parse_float(&mut self) -> Function {
        let parse = match self.float {
            FloatType::F32 => self.find_global_expect(F32_PARSE),
            FloatType::F64 => self.find_global_expect(F64_PARSE),
        };
        self.code_parse(type_::float(), parse.index)
    }

    fn code_parse(&mut self, type_: Arc<Type>, parse: u32) -> Function {
        let mut function = Function::new(vec![(3, ValType::I32), (1, self.val_type(&type_))]);
        // params
        let s = 0; // String
        // locals
        let ptr = 1; // I32
        let dest = 2; // I32
        let len = 3; // I32
        let r = 4; // type_
        // return Result(type, Nil)
        let string_to_memory =
            self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
        let heap_base = self.find_global_expect(HEAP_BASE);
        let result = type_::result(type_.clone(), type_::nil());
        let ok_index = self.ok_variant_constructor(type_.clone(), type_::nil());
        let error_index = self.error_variant_constructor(type_.clone(), type_::nil());
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .call(heap_base.index)
            .local_tee(ptr)
            .i32_const(0)
            .i32_store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            })
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
            .i32_load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            })
            .if_(BlockType::Result(self.val_type(&result)))
              .local_get(r)
              .call(ok_index)
            .else_()
              .nil_const()
              .call(error_index)
            .end()
            .end();
        function
    }

    fn get_function_builtin(
        &mut self,
        builtin: BuiltinFunction,
        code_fn: impl FnOnce(&mut Self) -> Function,
    ) -> u32 {
        if let Some(index) = self.builtins.get(&builtin) {
            return *index;
        }
        // Register empty function first to handle recursive types
        let mut function = self.add_function_builtin(builtin, Function::new(vec![]));
        let code = code_fn(self);
        function.code = code.into_raw_body().into();
        assert!(self.functions.replace(function.clone()).is_some());
        function.index
    }

    fn add_function_builtin(
        &mut self,
        builtin: BuiltinFunction,
        function: Function,
    ) -> WasmFunction {
        let (params, results) = self.builtin_type(&builtin);
        let function = self.add_function(
            builtin.name(),
            false,
            params,
            results,
            function.into_raw_body(),
        );
        let _ = self.builtins.insert(builtin, function.index);
        function
    }
}
