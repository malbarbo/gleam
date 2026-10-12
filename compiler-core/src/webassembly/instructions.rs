// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

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

    /// Calls `function`, as a tail call when `tail`.
    pub(super) fn call_maybe_tail(&mut self, function: FunctionId, tail: bool) -> &mut Self {
        if tail {
            self.return_call(function)
        } else {
            self.call(function)
        }
    }

    /// Calls the function reference with type `type_index`, as a tail call
    /// when `tail`.
    pub(super) fn call_ref_maybe_tail(&mut self, type_index: TypeId, tail: bool) -> &mut Self {
        if tail {
            self.return_call_ref(type_index)
        } else {
            self.call_ref(type_index)
        }
    }

    pub(super) fn ref_cast_non_null(&mut self, ht: HeapType) -> &mut Self {
        let _ = self.seq.as_mut().ref_cast(false, ht);
        self
    }

    pub(super) fn ref_cast(&mut self, nullable: bool, ht: HeapType) -> &mut Self {
        let _ = self.seq.as_mut().ref_cast(nullable, ht);
        self
    }

    pub(super) fn ref_test_non_null(&mut self, ht: HeapType) -> &mut Self {
        let _ = self.seq.as_mut().ref_test(false, ht);
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

    pub(super) fn i64_load(&mut self, arg: MemArg) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self
            .seq
            .as_mut()
            .load(memory, LoadKind::I64 { atomic: false }, arg);
        self
    }

    pub(super) fn f64_load(&mut self, arg: MemArg) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self.seq.as_mut().load(memory, LoadKind::F64, arg);
        self
    }

    pub(super) fn i64_store(&mut self, arg: MemArg) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self
            .seq
            .as_mut()
            .store(memory, StoreKind::I64 { atomic: false }, arg);
        self
    }

    pub(super) fn f64_store(&mut self, arg: MemArg) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self.seq.as_mut().store(memory, StoreKind::F64, arg);
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

    /// `[len] -> [bytes]`: a zeroed byte array to build a string.
    pub(super) fn bytes_new(&mut self) -> &mut Self {
        self.array_new_default(self.string.bytes_index)
    }

    /// `[bytes, i, byte] -> []`.
    pub(super) fn bytes_set(&mut self) -> &mut Self {
        self.array_set(self.string.bytes_index)
    }

    /// `[bytes, i] -> [byte]`.
    pub(super) fn bytes_get(&mut self) -> &mut Self {
        self.array_get_u(self.string.bytes_index)
    }

    /// `[dst: bytes, dst_off, src: String, src_off, len] -> []`.
    pub(super) fn bytes_copy_from_string(&mut self) -> &mut Self {
        self.call(self.string.copy_to_bytes)
    }

    /// `[bytes] -> [String]`: the string of all the bytes, which must not be
    /// changed afterwards.
    pub(super) fn string_from_bytes(&mut self) -> &mut Self {
        self.call(self.string.from_bytes)
    }

    /// `[s, i] -> [byte]`.
    pub(super) fn string_get(&mut self) -> &mut Self {
        self.call(self.string.byte_at)
    }

    /// `[s] -> [len]`.
    pub(super) fn string_len(&mut self) -> &mut Self {
        self.struct_get(self.string.type_index, 2)
    }

    /// `[s] -> [bytes]`: the underlying array, indexed from `string_off`.
    pub(super) fn string_bytes(&mut self) -> &mut Self {
        self.struct_get(self.string.type_index, 0)
    }

    /// `[s] -> [off]`.
    pub(super) fn string_off(&mut self) -> &mut Self {
        self.struct_get(self.string.type_index, 1)
    }

    /// `[s, start, len] -> [String]`, sharing the bytes of `s` when the slice
    /// is big.
    pub(super) fn string_slice(&mut self) -> &mut Self {
        self.call(self.string.slice)
    }

    /// `[] -> [String]`: a new empty string.
    pub(super) fn string_empty(&mut self) -> &mut Self {
        self.i32_const(0).bytes_new().string_from_bytes()
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
        generator._expression(locals, scope, self, expression, false);
        self
    }

    /// Like [`Self::expression`], but `expression` keeps the position of the
    /// expression that contains it, which is a tail position when `tail` is
    /// true.
    pub(super) fn tail_expression(
        &mut self,
        generator: &mut Generator<'_>,
        locals: &Locals,
        scope: Scope,
        expression: &TypedExpr,
        tail: bool,
    ) -> &mut Self {
        generator._expression(locals, scope, self, expression, tail);
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
            generator._expression(locals, scope.clone(), self, expression, false);
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

    pub(super) fn patterns<'c>(
        &mut self,
        generator: &mut Generator<'_>,
        locals: &Locals,
        scope: &mut Scope,
        fields: Fields<'_>,
        elements: impl IntoIterator<Item = &'c Pattern<Arc<Type>>>,
        fail_target: InstrSeqId,
    ) -> &mut Self {
        generator._patterns(locals, scope, self, fields, elements, fail_target);
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
        ref_i31(),
        i31_get_s(),
        call_ref(index: TypeId),
        call(index: FunctionId),
        return_call_ref(index: TypeId),
        return_call(index: FunctionId),
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
    binop!(i32_le_u, I32LeU);
    binop!(i32_ge_s, I32GeS);
    binop!(i32_lt_s, I32LtS);
    binop!(i32_gt_s, I32GtS);
    binop!(i32_ge_u, I32GeU);
    binop!(i32_add, I32Add);
    binop!(i32_sub, I32Sub);
    binop!(i32_mul, I32Mul);
    binop!(i32_and, I32And);
    binop!(i32_or, I32Or);
    binop!(i32_shl, I32Shl);
    binop!(i32_shr_u, I32ShrU);
    binop!(i32_xor, I32Xor);
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

    /// Gleam division: `x / 0` is 0 and `MIN / -1` wraps to `MIN` instead of
    /// trapping like `div_s`.
    pub(super) fn int_div(&mut self, dividend: LocalId, divisor: LocalId) -> &mut Self {
        let int_vt = self.int.val_type();
        #[rustfmt::skip]
        let _ = self
            .local_set(divisor)
            .local_set(dividend)
            .local_get(divisor)
            .int_eqz()
            .if_else(
                int_vt,
                |then_s| {
                    let _ = then_s.int_const(&0.into());
                },
                |else_s| {
                    let _ = else_s
                        .local_get(divisor)
                        .int_const(&(-1).into())
                        .int_eq()
                        .if_else(
                            int_vt,
                            |then_s| {
                                let _ = then_s
                                    .int_const(&0.into())
                                    .local_get(dividend)
                                    .int_sub();
                            },
                            |else_s| {
                                let _ = else_s
                                    .local_get(dividend)
                                    .local_get(divisor)
                                    .int_div_s();
                            },
                        );
                },
            );
        self
    }

    /// Gleam remainder: `x % 0` is 0 instead of trapping like `rem_s`.
    pub(super) fn int_rem(&mut self, dividend: LocalId, divisor: LocalId) -> &mut Self {
        let int_vt = self.int.val_type();
        #[rustfmt::skip]
        let _ = self
            .local_set(divisor)
            .local_set(dividend)
            .local_get(divisor)
            .int_eqz()
            .if_else(
                int_vt,
                |then_s| {
                    let _ = then_s.int_const(&0.into());
                },
                |else_s| {
                    let _ = else_s
                        .local_get(dividend)
                        .local_get(divisor)
                        .int_rem_s();
                },
            );
        self
    }

    pub(super) fn int_eqz(&mut self) -> &mut Self {
        let op = match self.int {
            IntType::I32 => UnaryOp::I32Eqz,
            IntType::I64 => UnaryOp::I64Eqz,
        };
        let _ = self.seq.as_mut().unop(op);
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

    pub(super) fn int_to_float(&mut self) -> &mut Self {
        let op = match (self.int, self.float) {
            (IntType::I32, FloatType::F32) => UnaryOp::F32ConvertSI32,
            (IntType::I32, FloatType::F64) => UnaryOp::F64ConvertSI32,
            (IntType::I64, FloatType::F32) => UnaryOp::F32ConvertSI64,
            (IntType::I64, FloatType::F64) => UnaryOp::F64ConvertSI64,
        };
        let _ = self.seq.as_mut().unop(op);
        self
    }

    /// Saturating conversion: NaN gives 0 and out of range values give the
    /// minimum or maximum `Int`.
    pub(super) fn float_to_int_sat(&mut self) -> &mut Self {
        let op = match (self.float, self.int) {
            (FloatType::F32, IntType::I32) => UnaryOp::I32TruncSSatF32,
            (FloatType::F64, IntType::I32) => UnaryOp::I32TruncSSatF64,
            (FloatType::F32, IntType::I64) => UnaryOp::I64TruncSSatF32,
            (FloatType::F64, IntType::I64) => UnaryOp::I64TruncSSatF64,
        };
        let _ = self.seq.as_mut().unop(op);
        self
    }

    /// `[Int] -> [i32]`: the two halves of a 64 bit `Int` xored.
    pub(super) fn int_fold_to_i32(&mut self) -> &mut Self {
        if let IntType::I64 = self.int {
            let _ = self.seq.as_mut().unop(UnaryOp::I64Extend32S);
            return self.fold_i64();
        }
        self
    }

    /// `[Float] -> [i32]`: the bits of a `Float`, folded to 32 bits.
    pub(super) fn float_fold_to_i32(&mut self) -> &mut Self {
        match self.float {
            FloatType::F32 => {
                let _ = self.seq.as_mut().unop(UnaryOp::I32ReinterpretF32);
                self
            }
            FloatType::F64 => {
                let _ = self.seq.as_mut().unop(UnaryOp::I64ReinterpretF64);
                self.fold_i64()
            }
        }
    }

    /// `[i64] -> [i32]`: `low ^ high`.
    fn fold_i64(&mut self) -> &mut Self {
        let seq = self.seq.as_mut();
        let _ = seq
            .i64_const(32)
            .binop(BinaryOp::I64Rotl)
            .unop(UnaryOp::I32WrapI64);
        self
    }

    /// The number of bits of `Int`.
    pub(super) fn int_bits(&self) -> i32 {
        match self.int {
            IntType::I32 => 32,
            IntType::I64 => 64,
        }
    }

    int_op!(int_add, I32Add, I64Add);
    int_op!(int_sub, I32Sub, I64Sub);
    int_op!(int_mul, I32Mul, I64Mul);
    int_op!(int_div_s, I32DivS, I64DivS);
    int_op!(int_rem_s, I32RemS, I64RemS);
    int_op!(int_eq, I32Eq, I64Eq);
    int_op!(int_ne, I32Ne, I64Ne);
    int_op!(int_lt, I32LtS, I64LtS);
    int_op!(int_le, I32LeS, I64LeS);
    int_op!(int_gt, I32GtS, I64GtS);
    int_op!(int_ge, I32GeS, I64GeS);
    int_op!(int_ge_u, I32GeU, I64GeU);
    int_op!(int_and, I32And, I64And);
    int_op!(int_or, I32Or, I64Or);
    int_op!(int_xor, I32Xor, I64Xor);
    int_op!(int_shl, I32Shl, I64Shl);
    int_op!(int_shr_s, I32ShrS, I64ShrS);
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

macro_rules! float_unop {
    ($name:ident, $f32:ident, $f64:ident) => {
        pub(super) fn $name(&mut self) -> &mut Self {
            let op = match self.float {
                FloatType::F32 => UnaryOp::$f32,
                FloatType::F64 => UnaryOp::$f64,
            };
            let _ = self.seq.as_mut().unop(op);
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
    float_op!(float_copysign, F32Copysign, F64Copysign);

    float_unop!(float_floor, F32Floor, F64Floor);
    float_unop!(float_ceil, F32Ceil, F64Ceil);
    float_unop!(float_trunc, F32Trunc, F64Trunc);
    float_unop!(float_sqrt, F32Sqrt, F64Sqrt);
    float_unop!(float_abs, F32Abs, F64Abs);
}

/// Slices of up to this many bytes are copied instead of sharing the bytes of
/// the original string.
pub(super) const STRING_SLICE_COPY_MAX: i32 = 64;

/// `String` is a view `{ bytes, off, len }` over an immutable byte array, so
/// slicing is O(1). Small slices, and slices smaller than a quarter of the
/// array they come from, are copied: tokens never retain big strings and a
/// view retains at most 4 times its size, while consuming a string from the
/// start stays linear (the copies form a geometric series).
#[derive(Clone, Copy)]
pub(super) struct StringType {
    /// The `String` struct.
    pub(super) type_index: TypeId,
    /// The byte array of strings.
    pub(super) bytes_index: TypeId,
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

impl StringType {
    pub(super) fn new(module: &mut walrus::Module) -> StringType {
        let bytes_index = module.types.add_array(FieldType {
            element_type: StorageType::I8,
            mutable: true,
        });
        module.types.get_mut(bytes_index).name = Some("Bytes".into());
        let bytes_vt = ValType::Ref(RefType {
            heap_type: HeapType::Concrete(bytes_index),
            nullable: false,
        });
        let immutable = |val_type| FieldType {
            element_type: StorageType::Val(val_type),
            mutable: false,
        };
        let type_index = module.types.add_struct(vec![
            immutable(bytes_vt),
            immutable(ValType::I32),
            immutable(ValType::I32),
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
                            .struct_new(type_index);
                    },
                );
            fb.finish(vec![s, start, len], &mut module.funcs)
        };

        StringType {
            type_index,
            bytes_index,
            byte_at,
            from_bytes,
            copy_to_bytes,
            slice,
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

    pub(super) fn bytes_val_type(&self) -> ValType {
        ValType::Ref(RefType {
            heap_type: HeapType::Concrete(self.bytes_index),
            nullable: false,
        })
    }
}
