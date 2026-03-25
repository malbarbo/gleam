use super::*;

#[allow(unused)]
pub(super) struct ExtendedInstructionSink<'a> {
    pub(super) int: IntType,
    pub(super) float: FloatType,
    pub(super) string: StringType,
    pub(super) instructions: InstructionSink<'a>,
}

#[derive(Clone, Copy)]
pub(super) enum Eq {
    I32,
    Int,
    Float,
    Call(u32),
}

pub(super) trait NewExtendedInstructionSink {
    fn extend_instructions<'a>(
        &'a mut self,
        generator: &Generator<'_>,
    ) -> ExtendedInstructionSink<'a>;
}

impl NewExtendedInstructionSink for Function {
    fn extend_instructions<'a>(
        &'a mut self,
        generator: &Generator<'_>,
    ) -> ExtendedInstructionSink<'a> {
        ExtendedInstructionSink {
            int: generator.int,
            float: generator.float,
            string: generator.string,
            instructions: self.instructions(),
        }
    }
}

macro_rules! delegate {
    ($($name:ident ( $( $arg:ident : $typ:ty ),* ) ),+ $(,)? ) => {
        $(
            pub(super) fn $name(&mut self $(, $arg: $typ )* ) -> &mut Self {
                let _ = self.instructions.$name($( $arg, )*);
                self
            }
        )+
    };
}

// We do not implement Deref and DerefMut so we do not call "native" int and float instructions directly.
impl<'a> ExtendedInstructionSink<'a> {
    pub(super) fn i32_inc(&mut self, local: u32) -> &mut Self {
        self.local_get(local)
            .i32_const(1)
            .i32_add()
            .local_set(local)
    }

    pub(super) fn br_table<I: IntoIterator<Item = u32>>(
        &mut self,
        labels: I,
        default: u32,
    ) -> &mut Self
    where
        I::IntoIter: ExactSizeIterator,
    {
        let _ = self.instructions.br_table(labels, default);
        self
    }

    pub(super) fn string_new(&mut self) -> &mut Self {
        self.array_new_default(self.string.type_index)
    }

    pub(super) fn string_get(&mut self) -> &mut Self {
        self.array_get_u(self.string.type_index)
    }

    pub(super) fn string_len(&mut self) -> &mut Self {
        self.array_len()
    }

    pub(super) fn string_copy(&mut self) -> &mut Self {
        self.array_copy(self.string.type_index, self.string.type_index)
    }

    /// Ensure linear memory is large enough to access the given address.
    /// Takes the required end address on the stack, leaves nothing.
    pub(super) fn ensure_memory(&mut self, local: u32) -> &mut Self {
        #[rustfmt::skip]
        let _ = self
            .memory_size(0)
            .i32_const(16)
            .i32_shl()
            .i32_sub()
            .local_tee(local)
            .i32_const(0)
            .i32_gt_s()
            .if_(BlockType::Empty)
              .local_get(local)
              .i32_const(65535)
              .i32_add()
              .i32_const(16)
              .i32_shr_u()
              .memory_grow(0)
              .drop()
            .end();
        self
    }

    pub(super) fn byte_store(&mut self, byte: u8) -> &mut Self {
        self.i32_const(byte as i32).i32_store8(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        })
    }

    pub(super) fn global_as_non_null(&mut self, index: u32) -> &mut Self {
        self.global_get(index).ref_as_non_null()
    }

    pub(super) fn eq(&mut self, eq: Eq) -> &mut Self {
        match eq {
            Eq::I32 => {
                let _ = self.instructions.i32_eq();
                self
            }
            Eq::Int => self.int_eq(),
            Eq::Float => self.float_eq(),
            Eq::Call(index) => self.call(index),
        }
    }

    pub(super) fn constant(
        &mut self,
        generator: &mut Generator<'_>,
        const_: &TypedConstant,
    ) -> &mut Self {
        generator._constant(self, const_);
        self
    }

    pub(super) fn constants<'b, 'c>(
        &mut self,
        generator: &mut Generator<'b>,
        consts: impl IntoIterator<Item = &'c TypedConstant>,
    ) -> &mut Self {
        for const_ in consts {
            generator._constant(self, const_);
        }
        self
    }

    pub(super) fn expression(
        &mut self,
        generator: &mut Generator<'_>,
        locals: &Locals,
        scope: Scope,
        expression: &TypedExpr,
    ) -> &mut Self {
        generator._expression(locals, scope, self, expression);
        self
    }

    pub(super) fn expressions<'b, 'c>(
        &mut self,
        generator: &mut Generator<'b>,
        locals: &Locals,
        scope: Scope,
        expressions: impl IntoIterator<Item = &'c TypedExpr>,
    ) -> &mut Self {
        for expression in expressions {
            generator._expression(locals, scope.clone(), self, expression);
        }
        self
    }

    pub(super) fn pattern(
        &mut self,
        generator: &mut Generator<'_>,
        locals: &Locals,
        scope: &mut Scope,
        pattern: &TypedPattern,
        fail_depth: u32,
    ) -> &mut Self {
        *scope = generator._pattern(locals, scope.clone(), self, pattern, fail_depth);
        self
    }

    pub(super) fn patterns<'b>(
        &mut self,
        generator: &mut Generator<'_>,
        locals: &Locals,
        scope: &mut Scope,
        (type_index, subtype_index): (u32, Option<u32>),
        pattern: &Pattern<Arc<Type>>,
        elements: impl IntoIterator<Item = &'b Pattern<Arc<Type>>> + Clone,
        fail_depth: u32,
    ) -> &mut Self {
        generator._patterns(
            locals,
            scope,
            self,
            (type_index, subtype_index),
            pattern,
            elements,
            fail_depth,
        );
        self
    }

    pub(super) fn clause_guard(
        &mut self,
        generator: &mut Generator<'_>,
        locals: &Locals,
        scope: &Scope,
        guard: &TypedClauseGuard,
    ) -> &mut Self {
        generator._clause_guard(locals, scope, self, guard);
        self
    }

    pub(super) fn clause_guards<'b, 'c>(
        &mut self,
        generator: &mut Generator<'b>,
        locals: &Locals,
        scope: &Scope,
        guards: impl IntoIterator<Item = &'c TypedClauseGuard>,
    ) -> &mut Self {
        for guard in guards {
            generator._clause_guard(locals, scope, self, guard);
        }
        self
    }

    pub(super) fn show_error_message(
        &mut self,
        prefix: u32,
        location: u32,
        string_to_memory: u32,
        heap_base: u32,
        print: u32,
    ) -> &mut Self {
        for string_index in [prefix, location] {
            let _ = self
                .i32_const(STDERR)
                .call(heap_base)
                .global_as_non_null(string_index)
                .call(heap_base)
                .call(string_to_memory)
                .call(print)
                .drop();
        }
        self
    }

    delegate! {
        if_(bt: BlockType),
        else_(),
        end(),
        loop_(bt: BlockType),
        block(bt: BlockType),
        br(l: u32),
        br_if(l: u32),
        unreachable(),
        drop(),
        local_set(index: u32),
        local_get(index: u32),
        local_tee(index: u32),
        global_set(index: u32),
        global_get(index: u32),
        struct_new(struct_type_index: u32),
        struct_get(struct_type_index: u32, field_index: u32),
        ref_func(index: u32),
        ref_null(ht: HeapType),
        ref_is_null(),
        ref_as_non_null(),
        ref_eq(),
        ref_cast_non_null(ht: HeapType),
        call_ref(index: u32),
        call(index: u32),
        array_new_default(type_index: u32),
        array_new_data(type_index: u32, data_segment: u32),
        array_len(),
        array_get_u(type_index: u32),
        array_set(type_index: u32),
        array_copy(array_type_index_dst: u32, array_type_index_src: u32),
        return_(),
        i32_const(x: i32),
        i32_eq(),
        i32_ne(),
        i32_ge_u(),
        i32_lt_u(),
        i32_add(),
        i32_and(),
        i32_or(),
        i32_sub(),
        i32_store8(m: MemArg),
        i32_store(m: MemArg),
        i32_load8_u(m: MemArg),
        i32_load(m: MemArg),
        i32_mul(),
        i32_shl(),
        i32_shr_u(),
        i32_gt_s(),
        memory_size(mem: u32),
        memory_grow(mem: u32),
    }
}

impl<'a> ExtendedInstructionSink<'a> {
    pub(super) fn bool_const(&mut self, value: bool) -> &mut Self {
        let _ = self.instructions.i32_const(value as _);
        self
    }

    pub(super) fn nil_const(&mut self) -> &mut Self {
        self.i32_const(0)
    }

    pub(super) fn bool_not(&mut self) -> &mut Self {
        let _ = self.instructions.i32_eqz();
        self
    }
}

#[allow(unused)]
#[derive(Debug, Copy, Clone)]
pub(super) enum IntType {
    I32,
    I64,
}

impl IntType {
    pub(super) fn val_type(&self) -> ValType {
        match self {
            IntType::I32 => ValType::I32,
            IntType::I64 => ValType::I64,
        }
    }

    pub(super) fn int_const(&self, value: &BigInt) -> ConstExpr {
        match self {
            IntType::I32 => ConstExpr::i32_const(self.to_i32(value)),
            IntType::I64 => ConstExpr::i64_const(self.to_i64(value)),
        }
    }

    fn to_i32(&self, value: &BigInt) -> i32 {
        value.try_into().expect("int literal to fit in i32")
    }

    fn to_i64(&self, value: &BigInt) -> i64 {
        value.try_into().expect("int literal to fit in i64")
    }
}

macro_rules! int_op {
    ($name:ident, $i32:ident, $i64:ident) => {
        pub(super) fn $name(&mut self) -> &mut Self {
            let _ = match self.int {
                IntType::I32 => self.instructions.$i32(),
                IntType::I64 => self.instructions.$i64(),
            };
            self
        }
    };
}

impl<'a> ExtendedInstructionSink<'a> {
    pub(super) fn int_const(&mut self, value: &BigInt) -> &mut Self {
        let _ = match self.int {
            IntType::I32 => self.instructions.i32_const(self.int.to_i32(value)),
            IntType::I64 => self.instructions.i64_const(self.int.to_i64(value)),
        };
        self
    }

    pub(super) fn int_div(&mut self, dividend: u32, divisor: u32) -> &mut Self {
        #[rustfmt::skip]
        let _ = match self.int {
            IntType::I32 => self
                .instructions
                .local_set(divisor)
                .local_set(dividend)
                .local_get(divisor)
                .if_(BlockType::Result(ValType::I32))
                  .local_get(dividend)
                  .local_get(divisor)
                  .i32_div_s()
                .else_()
                  .i32_const(0)
                .end(),
            IntType::I64 => self
                .instructions
                .local_set(divisor)
                .local_set(dividend)
                .local_get(divisor)
                .i64_const(0)
                .i64_ne()
                .if_(BlockType::Result(ValType::I64))
                  .local_get(dividend)
                  .local_get(divisor)
                  .i64_div_s()
                .else_()
                  .i64_const(0)
                .end(),
        };
        self
    }

    pub(super) fn i32_to_int(&mut self) -> &mut Self {
        if let IntType::I64 = self.int {
            let _ = self.instructions.i64_extend_i32_s();
        }
        self
    }

    pub(super) fn int_to_i32(&mut self) -> &mut Self {
        if let IntType::I64 = self.int {
            let _ = self.instructions.i32_wrap_i64();
        }
        self
    }

    pub(super) fn i64_to_int(&mut self) -> &mut Self {
        if let IntType::I32 = self.int {
            let _ = self.instructions.i32_wrap_i64();
        }
        self
    }

    pub(super) fn int_to_i64(&mut self) -> &mut Self {
        if let IntType::I32 = self.int {
            let _ = self.instructions.i64_extend_i32_s();
        }
        self
    }

    pub(super) fn f32_to_float(&mut self) -> &mut Self {
        if let FloatType::F64 = self.float {
            let _ = self.instructions.f64_promote_f32();
        }
        self
    }

    pub(super) fn float_to_f32(&mut self) -> &mut Self {
        if let FloatType::F64 = self.float {
            let _ = self.instructions.f32_demote_f64();
        }
        self
    }

    pub(super) fn f64_to_float(&mut self) -> &mut Self {
        if let FloatType::F32 = self.float {
            let _ = self.instructions.f32_demote_f64();
        }
        self
    }

    pub(super) fn float_to_f64(&mut self) -> &mut Self {
        if let FloatType::F32 = self.float {
            let _ = self.instructions.f64_promote_f32();
        }
        self
    }

    int_op!(int_add, i32_add, i64_add);
    int_op!(int_sub, i32_sub, i64_sub);
    int_op!(int_mul, i32_mul, i64_mul);
    int_op!(int_rem, i32_rem_s, i64_rem_s);
    int_op!(int_eq, i32_eq, i64_eq);
    int_op!(int_ne, i32_ne, i64_ne);
    int_op!(int_lt, i32_lt_s, i64_lt_s);
    int_op!(int_le, i32_le_s, i64_le_s);
    int_op!(int_gt, i32_gt_s, i64_gt_s);
    int_op!(int_ge, i32_ge_s, i64_ge_s);
}

#[allow(unused)]
#[derive(Debug, Copy, Clone)]
pub(super) enum FloatType {
    F32,
    F64,
}

macro_rules! float_op {
    ($name:ident, $f32:ident, $f64:ident) => {
        pub(super) fn $name(&mut self) -> &mut Self {
            let _ = match self.float {
                FloatType::F32 => self.instructions.$f32(),
                FloatType::F64 => self.instructions.$f64(),
            };
            self
        }
    };
}

impl FloatType {
    pub(super) fn val_type(&self) -> ValType {
        match self {
            FloatType::F32 => ValType::F32,
            FloatType::F64 => ValType::F64,
        }
    }

    pub(super) fn float_const(&self, value: &EcoString) -> ConstExpr {
        let value = value.replace("_", "");
        match self {
            FloatType::F32 => ConstExpr::f32_const(self.to_f32(&value).into()),
            FloatType::F64 => ConstExpr::f64_const(self.to_f64(&value).into()),
        }
    }

    fn to_f32(&self, value: &str) -> f32 {
        value.parse().expect("float literal to fit in f32")
    }

    fn to_f64(&self, value: &str) -> f64 {
        value.parse().expect("float literal to fit in f64")
    }
}

impl<'a> ExtendedInstructionSink<'a> {
    pub(super) fn float_const(&mut self, value: &str) -> &mut Self {
        let value = value.replace("_", "");
        let _ = match self.float {
            FloatType::F32 => self
                .instructions
                .f32_const(self.float.to_f32(&value).into()),
            FloatType::F64 => self
                .instructions
                .f64_const(self.float.to_f64(&value).into()),
        };
        self
    }

    pub(super) fn float_div(&mut self, dividend: u32, divisor: u32) -> &mut Self {
        match self.float {
            FloatType::F32 => {
                #[rustfmt::skip]
                let _ = self
                    .instructions
                    .local_set(divisor)
                    .local_set(dividend)
                    .local_get(divisor)
                    .f32_const(0.0f32.into())
                    .f32_ne()
                    .if_(BlockType::Result(ValType::F32))
                      .local_get(dividend)
                      .local_get(divisor)
                      .f32_div()
                    .else_()
                      .f32_const(0.0f32.into())
                    .end();
            }
            FloatType::F64 => {
                #[rustfmt::skip]
                let _ = self
                    .instructions
                    .local_set(divisor)
                    .local_set(dividend)
                    .local_get(divisor)
                    .f64_const(0.0f64.into())
                    .f64_ne()
                    .if_(BlockType::Result(ValType::F64))
                      .local_get(dividend)
                      .local_get(divisor)
                      .f64_div()
                    .else_()
                      .f64_const(0.0f64.into())
                    .end();
            }
        }
        self
    }

    float_op!(float_add, f32_add, f64_add);
    float_op!(float_sub, f32_sub, f64_sub);
    float_op!(float_mul, f32_mul, f64_mul);
    float_op!(float_eq, f32_eq, f64_eq);
    float_op!(float_ne, f32_ne, f64_ne);
    float_op!(float_lt, f32_lt, f64_lt);
    float_op!(float_le, f32_le, f64_le);
    float_op!(float_gt, f32_gt, f64_gt);
    float_op!(float_ge, f32_ge, f64_ge);
}

#[derive(Clone, Copy)]
pub(super) struct StringType {
    pub(super) type_index: u32,
}

impl StringType {
    pub(super) fn wasm_type() -> WasmType {
        WasmType::array(StorageType::I8)
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
        HeapType::Concrete(self.type_index)
    }
}
