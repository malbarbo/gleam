#![allow(dead_code)]

use super::*;

pub(super) struct Function {
    fb: FunctionBuilder,
    params: Vec<LocalId>,
}

impl Function {
    pub(super) fn new(
        generator: &mut Generator<'_>,
        name: &str,
        params: &[LocalId],
        results: &[ValType],
    ) -> Function {
        let param_tys: Vec<ValType> = params
            .iter()
            .map(|&id| generator.wasm_module.locals.get(id).ty())
            .collect();
        let mut fb = FunctionBuilder::new(&mut generator.wasm_module.types, &param_tys, results);
        let _ = fb.name(name.to_string());
        Function {
            fb,
            params: params.to_vec(),
        }
    }

    pub(super) fn extend_instructions<'a>(
        &'a mut self,
        generator: &Generator<'_>,
    ) -> Instructions<'a, 'a> {
        Instructions {
            int: generator.int,
            float: generator.float,
            string: generator.string,
            memory: generator.memory,
            seq: SeqRef::Owned(self.fb.func_body()),
        }
    }

    pub(super) fn finish(self, generator: &mut Generator<'_>) -> FunctionId {
        self.fb
            .finish(self.params, &mut generator.wasm_module.funcs)
    }
}

pub(super) struct Instructions<'a, 'b> {
    pub(super) int: IntType,
    pub(super) float: FloatType,
    pub(super) string: StringType,
    pub(super) memory: Option<MemoryId>,
    seq: SeqRef<'a, 'b>,
}

enum SeqRef<'a, 'b> {
    Owned(InstrSeqBuilder<'b>),
    Borrowed(&'a mut InstrSeqBuilder<'b>),
}

impl<'a, 'b> SeqRef<'a, 'b> {
    fn as_mut(&mut self) -> &mut InstrSeqBuilder<'b> {
        match self {
            SeqRef::Owned(s) => s,
            SeqRef::Borrowed(s) => s,
        }
    }
}

#[derive(Clone, Copy)]
pub(super) enum Eq {
    I32,
    Int,
    Float,
    Call(FunctionId),
}

macro_rules! delegate {
    ($($name:ident ( $( $arg:ident : $typ:ty ),* ) ),+ $(,)? ) => {
        $(
            pub(super) fn $name(&mut self $(, $arg: $typ )* ) -> &mut Self {
                let _ = self.seq.as_mut().$name($( $arg, )*);
                self
            }
        )+
    };
}

macro_rules! binop {
    ($name:ident, $op:ident) => {
        pub(super) fn $name(&mut self) -> &mut Self {
            let _ = self.seq.as_mut().binop(BinaryOp::$op);
            self
        }
    };
}

macro_rules! unop {
    ($name:ident, $op:ident) => {
        pub(super) fn $name(&mut self) -> &mut Self {
            let _ = self.seq.as_mut().unop(UnaryOp::$op);
            self
        }
    };
}

impl<'a, 'b> Instructions<'a, 'b> {
    pub(super) fn id(&self) -> InstrSeqId {
        match &self.seq {
            SeqRef::Owned(s) => s.id(),
            SeqRef::Borrowed(s) => s.id(),
        }
    }

    pub(super) fn br_table<I: IntoIterator<Item = InstrSeqId>>(
        &mut self,
        labels: I,
        default: InstrSeqId,
    ) -> &mut Self {
        let blocks: Box<[InstrSeqId]> = labels.into_iter().collect();
        let _ = self.seq.as_mut().br_table(blocks, default);
        self
    }

    pub(super) fn ref_cast_non_null(&mut self, ht: HeapType) -> &mut Self {
        let _ = self.seq.as_mut().ref_cast(false, ht);
        self
    }

    pub(super) fn memory_size(&mut self) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self.seq.as_mut().memory_size(memory);
        self
    }

    pub(super) fn memory_grow(&mut self) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self.seq.as_mut().memory_grow(memory);
        self
    }

    pub(super) fn i32_store(&mut self, arg: MemArg) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self
            .seq
            .as_mut()
            .store(memory, StoreKind::I32 { atomic: false }, arg);
        self
    }

    pub(super) fn i32_store8(&mut self, arg: MemArg) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self
            .seq
            .as_mut()
            .store(memory, StoreKind::I32_8 { atomic: false }, arg);
        self
    }

    pub(super) fn i32_load(&mut self, arg: MemArg) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self
            .seq
            .as_mut()
            .load(memory, LoadKind::I32 { atomic: false }, arg);
        self
    }

    pub(super) fn i32_load8_u(&mut self, arg: MemArg) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self.seq.as_mut().load(
            memory,
            LoadKind::I32_8 {
                kind: ExtendedLoad::ZeroExtend,
            },
            arg,
        );
        self
    }

    pub(super) fn if_(
        &mut self,
        ty: impl Into<InstrSeqType>,
        then: impl FnOnce(&mut Instructions<'_, '_>),
    ) -> &mut Self {
        self.if_else(ty, then, |_| {})
    }

    pub(super) fn if_else(
        &mut self,
        ty: impl Into<InstrSeqType>,
        then: impl FnOnce(&mut Instructions<'_, '_>),
        else_: impl FnOnce(&mut Instructions<'_, '_>),
    ) -> &mut Self {
        let cfg = self.cfg();
        let _ = self.seq.as_mut().if_else(
            ty,
            |seq| sub_seq(cfg, seq, then),
            |seq| sub_seq(cfg, seq, else_),
        );
        self
    }

    pub(super) fn block_(
        &mut self,
        ty: impl Into<InstrSeqType>,
        make_block: impl FnOnce(&mut Instructions<'_, '_>),
    ) -> &mut Self {
        let cfg = self.cfg();
        let _ = self
            .seq
            .as_mut()
            .block(ty, |seq| sub_seq(cfg, seq, make_block));
        self
    }

    pub(super) fn loop_(
        &mut self,
        ty: impl Into<InstrSeqType>,
        make_loop: impl FnOnce(&mut Instructions<'_, '_>),
    ) -> &mut Self {
        let cfg = self.cfg();
        let _ = self
            .seq
            .as_mut()
            .loop_(ty, |seq| sub_seq(cfg, seq, make_loop));
        self
    }

    fn cfg(&self) -> (IntType, FloatType, StringType, Option<MemoryId>) {
        (self.int, self.float, self.string, self.memory)
    }
}

fn sub_seq(
    cfg: (IntType, FloatType, StringType, Option<MemoryId>),
    seq: &mut InstrSeqBuilder<'_>,
    f: impl FnOnce(&mut Instructions<'_, '_>),
) {
    let (int, float, string, memory) = cfg;
    let mut s = Instructions {
        seq: SeqRef::Borrowed(seq),
        int,
        float,
        string,
        memory,
    };
    f(&mut s);
}

impl<'a, 'b> Instructions<'a, 'b> {
    pub(super) fn i32_inc(&mut self, local: LocalId) -> &mut Self {
        self.local_get(local)
            .i32_const(1)
            .i32_add()
            .local_set(local)
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
    pub(super) fn ensure_memory(&mut self, local: LocalId) -> &mut Self {
        #[rustfmt::skip]
        let _ = self
            .memory_size()
            .i32_const(16)
            .i32_shl()
            .i32_sub()
            .local_tee(local)
            .i32_const(0)
            .i32_gt_s()
            .if_(InstrSeqType::Simple(None), |body| {
                let _ = body
                    .local_get(local)
                    .i32_const(65535)
                    .i32_add()
                    .i32_const(16)
                    .i32_shr_u()
                    .memory_grow()
                    .drop();
            });
        self
    }

    pub(super) fn byte_store(&mut self, byte: u8) -> &mut Self {
        self.i32_const(byte as i32).i32_store8(MemArg {
            offset: 0,
            align: 0,
        })
    }

    pub(super) fn global_as_non_null(&mut self, index: GlobalId) -> &mut Self {
        self.global_get(index).ref_as_non_null()
    }

    pub(super) fn eq(&mut self, eq: Eq) -> &mut Self {
        match eq {
            Eq::I32 => self.i32_eq(),
            Eq::Int => self.int_eq(),
            Eq::Float => self.float_eq(),
            Eq::Call(func) => self.call(func),
        }
    }

    pub(super) fn bool_const(&mut self, value: bool) -> &mut Self {
        self.i32_const(value as _)
    }

    pub(super) fn nil_const(&mut self) -> &mut Self {
        self.i32_const(0)
    }

    pub(super) fn bool_not(&mut self) -> &mut Self {
        self.i32_eqz()
    }

    pub(super) fn end(&mut self) -> &mut Self {
        self
    }
}

impl<'a, 'b> Instructions<'a, 'b> {
    pub(super) fn constant(
        &mut self,
        generator: &mut Generator<'_>,
        const_: &TypedConstant,
    ) -> &mut Self {
        generator._constant(self, const_);
        self
    }

    pub(super) fn constants<'c>(
        &mut self,
        generator: &mut Generator<'_>,
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

    pub(super) fn expressions<'c>(
        &mut self,
        generator: &mut Generator<'_>,
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
        fail_target: InstrSeqId,
    ) -> &mut Self {
        *scope = generator._pattern(locals, scope.clone(), self, pattern, fail_target);
        self
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn patterns<'c>(
        &mut self,
        generator: &mut Generator<'_>,
        locals: &Locals,
        scope: &mut Scope,
        (type_index, subtype_index): (TypeId, Option<TypeId>),
        field_mapping: Option<&[u32]>,
        pattern: &Pattern<Arc<Type>>,
        elements: impl IntoIterator<Item = &'c Pattern<Arc<Type>>> + Clone,
        fail_target: InstrSeqId,
    ) -> &mut Self {
        generator._patterns(
            locals,
            scope,
            self,
            (type_index, subtype_index),
            field_mapping,
            pattern,
            elements,
            fail_target,
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

    pub(super) fn clause_guards<'c>(
        &mut self,
        generator: &mut Generator<'_>,
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
        prefix: GlobalId,
        location: GlobalId,
        string_to_memory: FunctionId,
        heap_base: FunctionId,
        print: FunctionId,
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

    pub(super) fn ref_null(&mut self, ty: HeapType) -> &mut Self {
        let _ = self.seq.as_mut().ref_null(RefType {
            nullable: true,
            heap_type: ty,
        });
        self
    }

    delegate! {
        local_set(index: LocalId),
        local_get(index: LocalId),
        local_tee(index: LocalId),
        global_set(index: GlobalId),
        global_get(index: GlobalId),
        struct_new(struct_type_index: TypeId),
        struct_get(struct_type_index: TypeId, field_index: u32),
        ref_func(index: FunctionId),
        ref_is_null(),
        ref_as_non_null(),
        ref_eq(),
        call_ref(index: TypeId),
        call(index: FunctionId),
        array_new_default(type_index: TypeId),
        array_new_data(type_index: TypeId, data_segment: DataId),
        array_len(),
        array_get_u(type_index: TypeId),
        array_set(type_index: TypeId),
        array_copy(array_type_index_dst: TypeId, array_type_index_src: TypeId),
        return_(),
        drop(),
        unreachable(),
        br(l: InstrSeqId),
        br_if(l: InstrSeqId),
        i32_const(x: i32),
        i64_const(x: i64),
        f32_const(x: f32),
        f64_const(x: f64),
    }

    binop!(i32_eq, I32Eq);
    binop!(i32_ne, I32Ne);
    binop!(i32_lt_u, I32LtU);
    binop!(i32_gt_s, I32GtS);
    binop!(i32_ge_u, I32GeU);
    binop!(i32_add, I32Add);
    binop!(i32_sub, I32Sub);
    binop!(i32_mul, I32Mul);
    binop!(i32_and, I32And);
    binop!(i32_or, I32Or);
    binop!(i32_shl, I32Shl);
    binop!(i32_shr_u, I32ShrU);
    binop!(i32_div_s, I32DivS);
    binop!(i64_ne, I64Ne);
    binop!(i64_div_s, I64DivS);
    binop!(f32_div, F32Div);
    binop!(f64_div, F64Div);

    unop!(i32_eqz, I32Eqz);
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

    fn to_i32(self, value: &BigInt) -> i32 {
        value.try_into().expect("int literal to fit in i32")
    }

    fn to_i64(self, value: &BigInt) -> i64 {
        value.try_into().expect("int literal to fit in i64")
    }

    pub(super) fn int_const(&self, value: &BigInt) -> ConstExpr {
        match self {
            IntType::I32 => ConstExpr::Value(Value::I32(self.to_i32(value))),
            IntType::I64 => ConstExpr::Value(Value::I64(self.to_i64(value))),
        }
    }
}

macro_rules! int_op {
    ($name:ident, $i32:ident, $i64:ident) => {
        pub(super) fn $name(&mut self) -> &mut Self {
            let op = match self.int {
                IntType::I32 => BinaryOp::$i32,
                IntType::I64 => BinaryOp::$i64,
            };
            let _ = self.seq.as_mut().binop(op);
            self
        }
    };
}

impl<'a, 'b> Instructions<'a, 'b> {
    pub(super) fn int_const(&mut self, value: &BigInt) -> &mut Self {
        let int = self.int;
        let _ = match int {
            IntType::I32 => self.seq.as_mut().i32_const(int.to_i32(value)).id(),
            IntType::I64 => self.seq.as_mut().i64_const(int.to_i64(value)).id(),
        };
        self
    }

    pub(super) fn int_div(&mut self, dividend: LocalId, divisor: LocalId) -> &mut Self {
        match self.int {
            IntType::I32 => {
                #[rustfmt::skip]
                let _ = self
                    .local_set(divisor)
                    .local_set(dividend)
                    .local_get(divisor)
                    .if_else(
                        ValType::I32,
                        |then_s| {
                            let _ = then_s
                                .local_get(dividend)
                                .local_get(divisor)
                                .i32_div_s();
                        },
                        |else_s| {
                            let _ = else_s.i32_const(0);
                        },
                    );
            }
            IntType::I64 => {
                #[rustfmt::skip]
                let _ = self
                    .local_set(divisor)
                    .local_set(dividend)
                    .local_get(divisor)
                    .i64_const(0)
                    .i64_ne()
                    .if_else(
                        ValType::I64,
                        |then_s| {
                            let _ = then_s
                                .local_get(dividend)
                                .local_get(divisor)
                                .i64_div_s();
                        },
                        |else_s| {
                            let _ = else_s.i64_const(0);
                        },
                    );
            }
        }
        self
    }

    pub(super) fn i32_to_int(&mut self) -> &mut Self {
        if let IntType::I64 = self.int {
            let _ = self.seq.as_mut().unop(UnaryOp::I64ExtendSI32);
        }
        self
    }

    pub(super) fn int_to_i32(&mut self) -> &mut Self {
        if let IntType::I64 = self.int {
            let _ = self.seq.as_mut().unop(UnaryOp::I32WrapI64);
        }
        self
    }

    pub(super) fn i64_to_int(&mut self) -> &mut Self {
        if let IntType::I32 = self.int {
            let _ = self.seq.as_mut().unop(UnaryOp::I32WrapI64);
        }
        self
    }

    pub(super) fn int_to_i64(&mut self) -> &mut Self {
        if let IntType::I32 = self.int {
            let _ = self.seq.as_mut().unop(UnaryOp::I64ExtendSI32);
        }
        self
    }

    pub(super) fn f32_to_float(&mut self) -> &mut Self {
        if let FloatType::F64 = self.float {
            let _ = self.seq.as_mut().unop(UnaryOp::F64PromoteF32);
        }
        self
    }

    pub(super) fn float_to_f32(&mut self) -> &mut Self {
        if let FloatType::F64 = self.float {
            let _ = self.seq.as_mut().unop(UnaryOp::F32DemoteF64);
        }
        self
    }

    pub(super) fn f64_to_float(&mut self) -> &mut Self {
        if let FloatType::F32 = self.float {
            let _ = self.seq.as_mut().unop(UnaryOp::F32DemoteF64);
        }
        self
    }

    pub(super) fn float_to_f64(&mut self) -> &mut Self {
        if let FloatType::F32 = self.float {
            let _ = self.seq.as_mut().unop(UnaryOp::F64PromoteF32);
        }
        self
    }

    int_op!(int_add, I32Add, I64Add);
    int_op!(int_sub, I32Sub, I64Sub);
    int_op!(int_mul, I32Mul, I64Mul);
    int_op!(int_rem, I32RemS, I64RemS);
    int_op!(int_eq, I32Eq, I64Eq);
    int_op!(int_ne, I32Ne, I64Ne);
    int_op!(int_lt, I32LtS, I64LtS);
    int_op!(int_le, I32LeS, I64LeS);
    int_op!(int_gt, I32GtS, I64GtS);
    int_op!(int_ge, I32GeS, I64GeS);
}

#[allow(unused)]
#[derive(Debug, Copy, Clone)]
pub(super) enum FloatType {
    F32,
    F64,
}

impl FloatType {
    pub(super) fn val_type(&self) -> ValType {
        match self {
            FloatType::F32 => ValType::F32,
            FloatType::F64 => ValType::F64,
        }
    }

    fn to_f32(self, value: &str) -> f32 {
        value.parse().expect("float literal to fit in f32")
    }

    fn to_f64(self, value: &str) -> f64 {
        value.parse().expect("float literal to fit in f64")
    }

    pub(super) fn float_const(&self, value: &str) -> ConstExpr {
        let value = value.replace("_", "");
        match self {
            FloatType::F32 => ConstExpr::Value(Value::F32(self.to_f32(&value))),
            FloatType::F64 => ConstExpr::Value(Value::F64(self.to_f64(&value))),
        }
    }
}

macro_rules! float_op {
    ($name:ident, $f32:ident, $f64:ident) => {
        pub(super) fn $name(&mut self) -> &mut Self {
            let op = match self.float {
                FloatType::F32 => BinaryOp::$f32,
                FloatType::F64 => BinaryOp::$f64,
            };
            let _ = self.seq.as_mut().binop(op);
            self
        }
    };
}

impl<'a, 'b> Instructions<'a, 'b> {
    pub(super) fn float_const(&mut self, value: &str) -> &mut Self {
        let value = value.replace("_", "");
        let float = self.float;
        let _ = match float {
            FloatType::F32 => self.seq.as_mut().f32_const(float.to_f32(&value)).id(),
            FloatType::F64 => self.seq.as_mut().f64_const(float.to_f64(&value)).id(),
        };
        self
    }

    pub(super) fn float_div(&mut self, dividend: LocalId, divisor: LocalId) -> &mut Self {
        match self.float {
            FloatType::F32 => {
                #[rustfmt::skip]
                let _ = self
                    .local_set(divisor)
                    .local_set(dividend)
                    .local_get(divisor)
                    .f32_const(0.0)
                    .float_ne()
                    .if_else(
                        ValType::F32,
                        |then_s| {
                            let _ = then_s
                                .local_get(dividend)
                                .local_get(divisor)
                                .f32_div();
                        },
                        |else_s| {
                            let _ = else_s.f32_const(0.0);
                        },
                    );
            }
            FloatType::F64 => {
                #[rustfmt::skip]
                let _ = self
                    .local_set(divisor)
                    .local_set(dividend)
                    .local_get(divisor)
                    .f64_const(0.0)
                    .float_ne()
                    .if_else(
                        ValType::F64,
                        |then_s| {
                            let _ = then_s
                                .local_get(dividend)
                                .local_get(divisor)
                                .f64_div();
                        },
                        |else_s| {
                            let _ = else_s.f64_const(0.0);
                        },
                    );
            }
        }
        self
    }

    float_op!(float_add, F32Add, F64Add);
    float_op!(float_sub, F32Sub, F64Sub);
    float_op!(float_mul, F32Mul, F64Mul);
    float_op!(float_eq, F32Eq, F64Eq);
    float_op!(float_ne, F32Ne, F64Ne);
    float_op!(float_lt, F32Lt, F64Lt);
    float_op!(float_le, F32Le, F64Le);
    float_op!(float_gt, F32Gt, F64Gt);
    float_op!(float_ge, F32Ge, F64Ge);
}

#[derive(Clone, Copy)]
pub(super) struct StringType {
    pub(super) type_index: TypeId,
}

impl StringType {
    pub(super) fn field_type() -> FieldType {
        FieldType {
            element_type: StorageType::I8,
            mutable: true,
        }
    }

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
