use super::*;

// Top-level wrapper around walrus FunctionBuilder. Methods consume Self and
// return Self so callers can chain `function_builder(...).local_get(a).finish(gen)`.
// Sub-blocks inside if_/if_else/block_/loop_ closures receive `&mut Seq` and
// chain on borrowed references (Seq's methods are &mut self -> &mut Self).
#[allow(unused)]
pub(super) struct FnInstructions {
    pub(super) fb: FunctionBuilder,
    pub(super) params: Vec<LocalId>,
    pub(super) int: IntType,
    pub(super) float: FloatType,
    pub(super) string: StringType,
    pub(super) memory: Option<MemoryId>,
}

#[allow(unused)]
pub(super) struct Seq<'a, 'b> {
    pub(super) int: IntType,
    pub(super) float: FloatType,
    pub(super) string: StringType,
    pub(super) memory: Option<MemoryId>,
    pub(super) seq: &'a mut InstrSeqBuilder<'b>,
}

#[derive(Clone, Copy)]
pub(super) enum Eq {
    I32,
    Int,
    Float,
    Call(FunctionId),
}

impl FnInstructions {
    pub(super) fn finish(self, gen: &mut Generator<'_>) -> FunctionId {
        self.fb.finish(self.params, &mut gen.wasm_module.funcs)
    }

    /// Run `f` with mutable access to both the Generator and self, for cases
    /// where instruction emission must be interleaved with reading/writing
    /// generator state (eg `function_start` iterating over `self.consts`).
    pub(super) fn rust<F>(self, g: &mut Generator<'_>, f: F) -> Self
    where
        F: FnOnce(&mut Generator<'_>, Self) -> Self,
    {
        f(gen, self)
    }
}

// Simple instruction methods generated for both FnInstructions (owned) and
// Seq (&mut). Each method delegates to the underlying walrus InstrSeqBuilder.
macro_rules! delegate_both {
    ($($name:ident ( $( $arg:ident : $typ:ty ),* $(,)? ) ;)+) => {
        impl FnInstructions {
            $(
                pub(super) fn $name(mut self, $( $arg: $typ ),*) -> Self {
                    let _ = self.fb.func_body().$name($( $arg ),*);
                    self
                }
            )+
        }
        impl<'a, 'b> Seq<'a, 'b> {
            $(
                pub(super) fn $name(&mut self, $( $arg: $typ ),*) -> &mut Self {
                    let _ = self.seq.$name($( $arg ),*);
                    self
                }
            )+
        }
    };
}

delegate_both! {
    local_get(local: LocalId);
    local_set(local: LocalId);
    local_tee(local: LocalId);
    global_get(global: GlobalId);
    global_set(global: GlobalId);
    ref_null(ty: RefType);
    ref_is_null();
    ref_as_non_null();
    ref_eq();
    call(func: FunctionId);
    return_();
    drop();
    unreachable();
    array_len();
    i32_const(val: i32);
    i64_const(val: i64);
    f32_const(val: f32);
    f64_const(val: f64);
}

macro_rules! binop_both {
    ($($name:ident => $op:ident ;)+) => {
        impl FnInstructions {
            $(
                pub(super) fn $name(mut self) -> Self {
                    let _ = self.fb.func_body().binop(BinaryOp::$op);
                    self
                }
            )+
        }
        impl<'a, 'b> Seq<'a, 'b> {
            $(
                pub(super) fn $name(&mut self) -> &mut Self {
                    let _ = self.seq.binop(BinaryOp::$op);
                    self
                }
            )+
        }
    };
}

binop_both! {
    i32_eq => I32Eq;
    i32_ne => I32Ne;
    i32_lt_u => I32LtU;
    i32_gt_s => I32GtS;
    i32_ge_u => I32GeU;
    i32_add => I32Add;
    i32_sub => I32Sub;
    i32_mul => I32Mul;
    i32_and => I32And;
    i32_or => I32Or;
    i32_shl => I32Shl;
    i32_shr_u => I32ShrU;
    i32_div_s => I32DivS;
    i64_ne => I64Ne;
    i64_div_s => I64DivS;
    f32_div => F32Div;
    f64_div => F64Div;
}

macro_rules! unop_both {
    ($($name:ident => $op:ident ;)+) => {
        impl FnInstructions {
            $(
                pub(super) fn $name(mut self) -> Self {
                    let _ = self.fb.func_body().unop(UnaryOp::$op);
                    self
                }
            )+
        }
        impl<'a, 'b> Seq<'a, 'b> {
            $(
                pub(super) fn $name(&mut self) -> &mut Self {
                    let _ = self.seq.unop(UnaryOp::$op);
                    self
                }
            )+
        }
    };
}

unop_both! {
    i32_eqz => I32Eqz;
}

// Methods with non-trivial arguments. These need TypeId, HeapType,
// or MemArg, so are written out for both.
impl FnInstructions {
    pub(super) fn br(mut self, target: InstrSeqId) -> Self {
        let _ = self.fb.func_body().br(target);
        self
    }

    pub(super) fn br_if(mut self, target: InstrSeqId) -> Self {
        let _ = self.fb.func_body().br_if(target);
        self
    }

    pub(super) fn br_table<I: IntoIterator<Item = InstrSeqId>>(
        mut self,
        labels: I,
        default: InstrSeqId,
    ) -> Self {
        let blocks: Box<[InstrSeqId]> = labels.into_iter().collect();
        let _ = self.fb.func_body().br_table(blocks, default);
        self
    }

    pub(super) fn struct_new(mut self, type_id: TypeId) -> Self {
        let _ = self.fb.func_body().struct_new(type_id);
        self
    }

    pub(super) fn struct_get(mut self, type_id: TypeId, field: u32) -> Self {
        let _ = self.fb.func_body().struct_get(type_id, field);
        self
    }

    pub(super) fn array_new_default(mut self, type_id: TypeId) -> Self {
        let _ = self.fb.func_body().array_new_default(type_id);
        self
    }

    pub(super) fn array_new_data(mut self, type_id: TypeId, data: DataId) -> Self {
        let _ = self.fb.func_body().array_new_data(type_id, data);
        self
    }

    pub(super) fn array_get_u(mut self, type_id: TypeId) -> Self {
        let _ = self.fb.func_body().array_get_u(type_id);
        self
    }

    pub(super) fn array_set(mut self, type_id: TypeId) -> Self {
        let _ = self.fb.func_body().array_set(type_id);
        self
    }

    pub(super) fn array_copy(mut self, dst: TypeId, src: TypeId) -> Self {
        let _ = self.fb.func_body().array_copy(dst, src);
        self
    }

    pub(super) fn ref_cast_non_null(mut self, ht: HeapType) -> Self {
        let _ = self.fb.func_body().ref_cast(RefType {
            nullable: false,
            heap_type: ht,
        });
        self
    }

    pub(super) fn ref_func(mut self, func: FunctionId) -> Self {
        let _ = self.fb.func_body().ref_func(func);
        self
    }

    pub(super) fn call_ref(mut self, type_id: TypeId) -> Self {
        let _ = self.fb.func_body().call_ref(type_id);
        self
    }

    pub(super) fn memory_size(mut self) -> Self {
        let memory = self.memory.expect("memory available");
        let _ = self.fb.func_body().memory_size(memory);
        self
    }

    pub(super) fn memory_grow(mut self) -> Self {
        let memory = self.memory.expect("memory available");
        let _ = self.fb.func_body().memory_grow(memory);
        self
    }

    pub(super) fn i32_store(mut self, arg: MemArg) -> Self {
        let memory = self.memory.expect("memory available");
        let _ = self
            .fb
            .func_body()
            .store(memory, StoreKind::I32 { atomic: false }, arg);
        self
    }

    pub(super) fn i32_store8(mut self, arg: MemArg) -> Self {
        let memory = self.memory.expect("memory available");
        let _ = self
            .fb
            .func_body()
            .store(memory, StoreKind::I32_8 { atomic: false }, arg);
        self
    }

    pub(super) fn i32_load(mut self, arg: MemArg) -> Self {
        let memory = self.memory.expect("memory available");
        let _ = self
            .fb
            .func_body()
            .load(memory, LoadKind::I32 { atomic: false }, arg);
        self
    }

    pub(super) fn i32_load8_u(mut self, arg: MemArg) -> Self {
        let memory = self.memory.expect("memory available");
        let _ = self.fb.func_body().load(
            memory,
            LoadKind::I32_8 {
                kind: ExtendedLoad::ZeroExtend,
            },
            arg,
        );
        self
    }

    pub(super) fn if_(
        self,
        ty: impl Into<InstrSeqType>,
        then: impl FnOnce(&mut Seq<'_, '_>),
    ) -> Self {
        self.if_else(ty, then, |_| {})
    }

    pub(super) fn if_else(
        mut self,
        ty: impl Into<InstrSeqType>,
        then: impl FnOnce(&mut Seq<'_, '_>),
        else_: impl FnOnce(&mut Seq<'_, '_>),
    ) -> Self {
        let int = self.int;
        let float = self.float;
        let string = self.string;
        let memory = self.memory;
        let _ = self.fb.func_body().if_else(
            ty,
            |then_seq| {
                let mut s = Seq {
                    int,
                    float,
                    string,
                    memory,
                    seq: then_seq,
                };
                then(&mut s);
            },
            |else_seq| {
                let mut s = Seq {
                    int,
                    float,
                    string,
                    memory,
                    seq: else_seq,
                };
                else_(&mut s);
            },
        );
        self
    }

    pub(super) fn block_(
        mut self,
        ty: impl Into<InstrSeqType>,
        make_block: impl FnOnce(&mut Seq<'_, '_>),
    ) -> Self {
        let int = self.int;
        let float = self.float;
        let string = self.string;
        let memory = self.memory;
        let _ = self.fb.func_body().block(ty, |inner| {
            let mut s = Seq {
                int,
                float,
                string,
                memory,
                seq: inner,
            };
            make_block(&mut s);
        });
        self
    }

    pub(super) fn loop_(
        mut self,
        ty: impl Into<InstrSeqType>,
        make_loop: impl FnOnce(&mut Seq<'_, '_>),
    ) -> Self {
        let int = self.int;
        let float = self.float;
        let string = self.string;
        let memory = self.memory;
        let _ = self.fb.func_body().loop_(ty, |inner| {
            let mut s = Seq {
                int,
                float,
                string,
                memory,
                seq: inner,
            };
            make_loop(&mut s);
        });
        self
    }
}

impl<'a, 'b> Seq<'a, 'b> {
    pub(super) fn id(&self) -> InstrSeqId {
        self.seq.id()
    }

    pub(super) fn br(&mut self, target: InstrSeqId) -> &mut Self {
        let _ = self.seq.br(target);
        self
    }

    pub(super) fn br_if(&mut self, target: InstrSeqId) -> &mut Self {
        let _ = self.seq.br_if(target);
        self
    }

    pub(super) fn br_table<I: IntoIterator<Item = InstrSeqId>>(
        &mut self,
        labels: I,
        default: InstrSeqId,
    ) -> &mut Self {
        let blocks: Box<[InstrSeqId]> = labels.into_iter().collect();
        let _ = self.seq.br_table(blocks, default);
        self
    }

    pub(super) fn struct_new(&mut self, type_id: TypeId) -> &mut Self {
        let _ = self.seq.struct_new(type_id);
        self
    }

    pub(super) fn struct_get(&mut self, type_id: TypeId, field: u32) -> &mut Self {
        let _ = self.seq.struct_get(type_id, field);
        self
    }

    pub(super) fn array_new_default(&mut self, type_id: TypeId) -> &mut Self {
        let _ = self.seq.array_new_default(type_id);
        self
    }

    pub(super) fn array_new_data(&mut self, type_id: TypeId, data: DataId) -> &mut Self {
        let _ = self.seq.array_new_data(type_id, data);
        self
    }

    pub(super) fn array_get_u(&mut self, type_id: TypeId) -> &mut Self {
        let _ = self.seq.array_get_u(type_id);
        self
    }

    pub(super) fn array_set(&mut self, type_id: TypeId) -> &mut Self {
        let _ = self.seq.array_set(type_id);
        self
    }

    pub(super) fn array_copy(&mut self, dst: TypeId, src: TypeId) -> &mut Self {
        let _ = self.seq.array_copy(dst, src);
        self
    }

    pub(super) fn ref_cast_non_null(&mut self, ht: HeapType) -> &mut Self {
        let _ = self.seq.ref_cast(RefType {
            nullable: false,
            heap_type: ht,
        });
        self
    }

    pub(super) fn ref_func(&mut self, func: FunctionId) -> &mut Self {
        let _ = self.seq.ref_func(func);
        self
    }

    pub(super) fn call_ref(&mut self, type_id: TypeId) -> &mut Self {
        let _ = self.seq.call_ref(type_id);
        self
    }

    pub(super) fn memory_size(&mut self) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self.seq.memory_size(memory);
        self
    }

    pub(super) fn memory_grow(&mut self) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self.seq.memory_grow(memory);
        self
    }

    pub(super) fn i32_store(&mut self, arg: MemArg) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self
            .seq
            .store(memory, StoreKind::I32 { atomic: false }, arg);
        self
    }

    pub(super) fn i32_store8(&mut self, arg: MemArg) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self
            .seq
            .store(memory, StoreKind::I32_8 { atomic: false }, arg);
        self
    }

    pub(super) fn i32_load(&mut self, arg: MemArg) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self
            .seq
            .load(memory, LoadKind::I32 { atomic: false }, arg);
        self
    }

    pub(super) fn i32_load8_u(&mut self, arg: MemArg) -> &mut Self {
        let memory = self.memory.expect("memory available");
        let _ = self.seq.load(
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
        then: impl FnOnce(&mut Seq<'_, '_>),
    ) -> &mut Self {
        self.if_else(ty, then, |_| {})
    }

    pub(super) fn if_else(
        &mut self,
        ty: impl Into<InstrSeqType>,
        then: impl FnOnce(&mut Seq<'_, '_>),
        else_: impl FnOnce(&mut Seq<'_, '_>),
    ) -> &mut Self {
        let int = self.int;
        let float = self.float;
        let string = self.string;
        let memory = self.memory;
        let _ = self.seq.if_else(
            ty,
            |then_seq| {
                let mut s = Seq {
                    int,
                    float,
                    string,
                    memory,
                    seq: then_seq,
                };
                then(&mut s);
            },
            |else_seq| {
                let mut s = Seq {
                    int,
                    float,
                    string,
                    memory,
                    seq: else_seq,
                };
                else_(&mut s);
            },
        );
        self
    }

    pub(super) fn block_(
        &mut self,
        ty: impl Into<InstrSeqType>,
        make_block: impl FnOnce(&mut Seq<'_, '_>),
    ) -> &mut Self {
        let int = self.int;
        let float = self.float;
        let string = self.string;
        let memory = self.memory;
        let _ = self.seq.block(ty, |inner| {
            let mut s = Seq {
                int,
                float,
                string,
                memory,
                seq: inner,
            };
            make_block(&mut s);
        });
        self
    }

    pub(super) fn loop_(
        &mut self,
        ty: impl Into<InstrSeqType>,
        make_loop: impl FnOnce(&mut Seq<'_, '_>),
    ) -> &mut Self {
        let int = self.int;
        let float = self.float;
        let string = self.string;
        let memory = self.memory;
        let _ = self.seq.loop_(ty, |inner| {
            let mut s = Seq {
                int,
                float,
                string,
                memory,
                seq: inner,
            };
            make_loop(&mut s);
        });
        self
    }
}

// Helper methods built on top of the basics.
impl FnInstructions {
    pub(super) fn i32_inc(self, local: LocalId) -> Self {
        self.local_get(local).i32_const(1).i32_add().local_set(local)
    }

    pub(super) fn string_new(self) -> Self {
        let type_id = self.string.type_id;
        self.array_new_default(type_id)
    }

    pub(super) fn string_get(self) -> Self {
        let type_id = self.string.type_id;
        self.array_get_u(type_id)
    }

    pub(super) fn string_len(self) -> Self {
        self.array_len()
    }

    pub(super) fn string_copy(self) -> Self {
        let type_id = self.string.type_id;
        self.array_copy(type_id, type_id)
    }

    /// Ensure linear memory is large enough to access the given address.
    /// Takes the required end address on the stack, leaves nothing.
    pub(super) fn ensure_memory(self, local: LocalId) -> Self {
        #[rustfmt::skip]
        let res = self
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
        res
    }

    pub(super) fn byte_store(self, byte: u8) -> Self {
        self.i32_const(byte as i32).i32_store8(MemArg {
            offset: 0,
            align: 0,
        })
    }

    pub(super) fn global_as_non_null(self, index: GlobalId) -> Self {
        self.global_get(index).ref_as_non_null()
    }

    pub(super) fn eq(self, eq: Eq) -> Self {
        match eq {
            Eq::I32 => self.i32_eq(),
            Eq::Int => self.int_eq(),
            Eq::Float => self.float_eq(),
            Eq::Call(func) => self.call(func),
        }
    }

    pub(super) fn bool_const(self, value: bool) -> Self {
        self.i32_const(value as _)
    }

    pub(super) fn nil_const(self) -> Self {
        self.i32_const(0)
    }

    pub(super) fn bool_not(self) -> Self {
        self.i32_eqz()
    }
}

impl<'a, 'b> Seq<'a, 'b> {
    pub(super) fn i32_inc(&mut self, local: LocalId) -> &mut Self {
        self.local_get(local).i32_const(1).i32_add().local_set(local)
    }

    pub(super) fn string_new(&mut self) -> &mut Self {
        self.array_new_default(self.string.type_id)
    }

    pub(super) fn string_get(&mut self) -> &mut Self {
        self.array_get_u(self.string.type_id)
    }

    pub(super) fn string_len(&mut self) -> &mut Self {
        self.array_len()
    }

    pub(super) fn string_copy(&mut self) -> &mut Self {
        self.array_copy(self.string.type_id, self.string.type_id)
    }

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
}

// Generator-aware helpers on Seq: they take `gen: &mut Generator` explicitly
// and forward to Generator's private `_constant`/`_expression`/`_pattern`
// /`_clause_guard` methods. These run inside if_else/block_/loop_ closures
// where Seq is the active builder.
impl<'a, 'b> Seq<'a, 'b> {
    pub(super) fn constant(
        &mut self,
        g: &mut Generator<'_>,
        const_: &TypedConstant,
    ) -> &mut Self {
        g._constant(self, const_);
        self
    }

    pub(super) fn constants<'c>(
        &mut self,
        g: &mut Generator<'_>,
        consts: impl IntoIterator<Item = &'c TypedConstant>,
    ) -> &mut Self {
        for const_ in consts {
            g._constant(self, const_);
        }
        self
    }

    pub(super) fn expression(
        &mut self,
        g: &mut Generator<'_>,
        locals: &Locals,
        scope: Scope,
        expression: &TypedExpr,
    ) -> &mut Self {
        g._expression(locals, scope, self, expression);
        self
    }

    pub(super) fn expressions<'c>(
        &mut self,
        g: &mut Generator<'_>,
        locals: &Locals,
        scope: Scope,
        expressions: impl IntoIterator<Item = &'c TypedExpr>,
    ) -> &mut Self {
        for expression in expressions {
            g._expression(locals, scope.clone(), self, expression);
        }
        self
    }

    pub(super) fn pattern(
        &mut self,
        g: &mut Generator<'_>,
        locals: &Locals,
        scope: &mut Scope,
        pattern: &TypedPattern,
        fail_target: InstrSeqId,
    ) -> &mut Self {
        *scope = g._pattern(locals, scope.clone(), self, pattern, fail_target);
        self
    }

    pub(super) fn patterns<'c>(
        &mut self,
        g: &mut Generator<'_>,
        locals: &Locals,
        scope: &mut Scope,
        (type_id, subtype_id): (TypeId, Option<TypeId>),
        field_mapping: Option<&[u32]>,
        pattern: &Pattern<Arc<Type>>,
        elements: impl IntoIterator<Item = &'c Pattern<Arc<Type>>> + Clone,
        fail_target: InstrSeqId,
    ) -> &mut Self {
        g._patterns(
            locals,
            scope,
            self,
            (type_id, subtype_id),
            field_mapping,
            pattern,
            elements,
            fail_target,
        );
        self
    }

    pub(super) fn clause_guard(
        &mut self,
        g: &mut Generator<'_>,
        locals: &Locals,
        scope: &Scope,
        guard: &TypedClauseGuard,
    ) -> &mut Self {
        g._clause_guard(locals, scope, self, guard);
        self
    }

    pub(super) fn clause_guards<'c>(
        &mut self,
        g: &mut Generator<'_>,
        locals: &Locals,
        scope: &Scope,
        guards: impl IntoIterator<Item = &'c TypedClauseGuard>,
    ) -> &mut Self {
        for guard in guards {
            g._clause_guard(locals, scope, self, guard);
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

    fn to_i32(&self, value: &BigInt) -> i32 {
        value.try_into().expect("int literal to fit in i32")
    }

    fn to_i64(&self, value: &BigInt) -> i64 {
        value.try_into().expect("int literal to fit in i64")
    }
}

// Integer ops that dispatch on IntType.
macro_rules! int_op_both {
    ($($name:ident => ($i32_op:ident, $i64_op:ident);)+) => {
        impl FnInstructions {
            $(
                pub(super) fn $name(mut self) -> Self {
                    let op = match self.int {
                        IntType::I32 => BinaryOp::$i32_op,
                        IntType::I64 => BinaryOp::$i64_op,
                    };
                    let _ = self.fb.func_body().binop(op);
                    self
                }
            )+
        }
        impl<'a, 'b> Seq<'a, 'b> {
            $(
                pub(super) fn $name(&mut self) -> &mut Self {
                    let op = match self.int {
                        IntType::I32 => BinaryOp::$i32_op,
                        IntType::I64 => BinaryOp::$i64_op,
                    };
                    let _ = self.seq.binop(op);
                    self
                }
            )+
        }
    };
}

int_op_both! {
    int_add => (I32Add, I64Add);
    int_sub => (I32Sub, I64Sub);
    int_mul => (I32Mul, I64Mul);
    int_rem => (I32RemS, I64RemS);
    int_eq  => (I32Eq, I64Eq);
    int_ne  => (I32Ne, I64Ne);
    int_lt  => (I32LtS, I64LtS);
    int_le  => (I32LeS, I64LeS);
    int_gt  => (I32GtS, I64GtS);
    int_ge  => (I32GeS, I64GeS);
}

impl FnInstructions {
    pub(super) fn int_const(mut self, value: &BigInt) -> Self {
        let body = self.fb.func_body();
        let _ = match self.int {
            IntType::I32 => body.i32_const(self.int.to_i32(value)).id(),
            IntType::I64 => body.i64_const(self.int.to_i64(value)).id(),
        };
        self
    }

    pub(super) fn int_div(self, dividend: LocalId, divisor: LocalId) -> Self {
        match self.int {
            IntType::I32 => {
                #[rustfmt::skip]
                let res = self
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
                res
            }
            IntType::I64 => {
                #[rustfmt::skip]
                let res = self
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
                res
            }
        }
    }

    pub(super) fn i32_to_int(mut self) -> Self {
        if let IntType::I64 = self.int {
            let _ = self.fb.func_body().unop(UnaryOp::I64ExtendSI32);
        }
        self
    }

    pub(super) fn int_to_i32(mut self) -> Self {
        if let IntType::I64 = self.int {
            let _ = self.fb.func_body().unop(UnaryOp::I32WrapI64);
        }
        self
    }

    pub(super) fn i64_to_int(mut self) -> Self {
        if let IntType::I32 = self.int {
            let _ = self.fb.func_body().unop(UnaryOp::I32WrapI64);
        }
        self
    }

    pub(super) fn int_to_i64(mut self) -> Self {
        if let IntType::I32 = self.int {
            let _ = self.fb.func_body().unop(UnaryOp::I64ExtendSI32);
        }
        self
    }

    pub(super) fn f32_to_float(mut self) -> Self {
        if let FloatType::F64 = self.float {
            let _ = self.fb.func_body().unop(UnaryOp::F64PromoteF32);
        }
        self
    }

    pub(super) fn float_to_f32(mut self) -> Self {
        if let FloatType::F64 = self.float {
            let _ = self.fb.func_body().unop(UnaryOp::F32DemoteF64);
        }
        self
    }

    pub(super) fn f64_to_float(mut self) -> Self {
        if let FloatType::F32 = self.float {
            let _ = self.fb.func_body().unop(UnaryOp::F32DemoteF64);
        }
        self
    }

    pub(super) fn float_to_f64(mut self) -> Self {
        if let FloatType::F32 = self.float {
            let _ = self.fb.func_body().unop(UnaryOp::F64PromoteF32);
        }
        self
    }
}

impl<'a, 'b> Seq<'a, 'b> {
    pub(super) fn int_const(&mut self, value: &BigInt) -> &mut Self {
        let _ = match self.int {
            IntType::I32 => self.seq.i32_const(self.int.to_i32(value)).id(),
            IntType::I64 => self.seq.i64_const(self.int.to_i64(value)).id(),
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
            let _ = self.seq.unop(UnaryOp::I64ExtendSI32);
        }
        self
    }

    pub(super) fn int_to_i32(&mut self) -> &mut Self {
        if let IntType::I64 = self.int {
            let _ = self.seq.unop(UnaryOp::I32WrapI64);
        }
        self
    }

    pub(super) fn i64_to_int(&mut self) -> &mut Self {
        if let IntType::I32 = self.int {
            let _ = self.seq.unop(UnaryOp::I32WrapI64);
        }
        self
    }

    pub(super) fn int_to_i64(&mut self) -> &mut Self {
        if let IntType::I32 = self.int {
            let _ = self.seq.unop(UnaryOp::I64ExtendSI32);
        }
        self
    }

    pub(super) fn f32_to_float(&mut self) -> &mut Self {
        if let FloatType::F64 = self.float {
            let _ = self.seq.unop(UnaryOp::F64PromoteF32);
        }
        self
    }

    pub(super) fn float_to_f32(&mut self) -> &mut Self {
        if let FloatType::F64 = self.float {
            let _ = self.seq.unop(UnaryOp::F32DemoteF64);
        }
        self
    }

    pub(super) fn f64_to_float(&mut self) -> &mut Self {
        if let FloatType::F32 = self.float {
            let _ = self.seq.unop(UnaryOp::F32DemoteF64);
        }
        self
    }

    pub(super) fn float_to_f64(&mut self) -> &mut Self {
        if let FloatType::F32 = self.float {
            let _ = self.seq.unop(UnaryOp::F64PromoteF32);
        }
        self
    }
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

    fn to_f32(&self, value: &str) -> f32 {
        value.parse().expect("float literal to fit in f32")
    }

    fn to_f64(&self, value: &str) -> f64 {
        value.parse().expect("float literal to fit in f64")
    }
}

macro_rules! float_op_both {
    ($($name:ident => ($f32_op:ident, $f64_op:ident);)+) => {
        impl FnInstructions {
            $(
                pub(super) fn $name(mut self) -> Self {
                    let op = match self.float {
                        FloatType::F32 => BinaryOp::$f32_op,
                        FloatType::F64 => BinaryOp::$f64_op,
                    };
                    let _ = self.fb.func_body().binop(op);
                    self
                }
            )+
        }
        impl<'a, 'b> Seq<'a, 'b> {
            $(
                pub(super) fn $name(&mut self) -> &mut Self {
                    let op = match self.float {
                        FloatType::F32 => BinaryOp::$f32_op,
                        FloatType::F64 => BinaryOp::$f64_op,
                    };
                    let _ = self.seq.binop(op);
                    self
                }
            )+
        }
    };
}

float_op_both! {
    float_add => (F32Add, F64Add);
    float_sub => (F32Sub, F64Sub);
    float_mul => (F32Mul, F64Mul);
    float_eq  => (F32Eq, F64Eq);
    float_ne  => (F32Ne, F64Ne);
    float_lt  => (F32Lt, F64Lt);
    float_le  => (F32Le, F64Le);
    float_gt  => (F32Gt, F64Gt);
    float_ge  => (F32Ge, F64Ge);
}

impl FnInstructions {
    pub(super) fn float_const(mut self, value: &str) -> Self {
        let value = value.replace("_", "");
        let body = self.fb.func_body();
        let _ = match self.float {
            FloatType::F32 => body.f32_const(self.float.to_f32(&value)).id(),
            FloatType::F64 => body.f64_const(self.float.to_f64(&value)).id(),
        };
        self
    }

    pub(super) fn float_div(self, dividend: LocalId, divisor: LocalId) -> Self {
        match self.float {
            FloatType::F32 => {
                #[rustfmt::skip]
                let res = self
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
                res
            }
            FloatType::F64 => {
                #[rustfmt::skip]
                let res = self
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
                res
            }
        }
    }
}

impl<'a, 'b> Seq<'a, 'b> {
    pub(super) fn float_const(&mut self, value: &str) -> &mut Self {
        let value = value.replace("_", "");
        let _ = match self.float {
            FloatType::F32 => self.seq.f32_const(self.float.to_f32(&value)).id(),
            FloatType::F64 => self.seq.f64_const(self.float.to_f64(&value)).id(),
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
}

#[derive(Clone, Copy)]
pub(super) struct StringType {
    pub(super) type_id: TypeId,
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
        HeapType::Concrete(self.type_id)
    }
}
