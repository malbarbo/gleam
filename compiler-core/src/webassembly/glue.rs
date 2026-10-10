// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Glue for `@external(webassembly, "rust", name)`: the Rust functions of
//! `builtins-wasm` work on linear memory, so each call copies the Gleam
//! arguments into the Rust arena and the result back into GC values. The glue
//! is generated from the Gleam type of the external function, following
//! `builtins-wasm/ABI.md`.

use super::*;

pub(super) const RUST_MODULE: &str = "rust";

pub(super) const CABI_REALLOC: &str = "cabi_realloc";
pub(super) const ARENA_MARK: &str = "_arena_mark";
pub(super) const ARENA_RELEASE: &str = "_arena_release";
pub(super) const CONFIG: &str = "_config";

/// How a Gleam type crosses the boundary.
#[derive(Debug, Clone)]
enum Abi {
    Int,
    Float,
    /// `Bool` and `UtfCodepoint`: an `i32`.
    I32,
    Nil,
    String,
    List(Arc<Type>),
    Tuple(Vec<Arc<Type>>),
    Result(Arc<Type>, Arc<Type>),
}

fn abi(type_: &Arc<Type>) -> Option<Abi> {
    if type_.is_int() {
        Some(Abi::Int)
    } else if type_.is_float() {
        Some(Abi::Float)
    } else if type_.is_bool() || type_.is_utf_codepoint() {
        Some(Abi::I32)
    } else if type_.is_nil() {
        Some(Abi::Nil)
    } else if type_.is_string() {
        Some(Abi::String)
    } else if let Some(item) = type_.list_type() {
        abi(&item).map(|_| Abi::List(item))
    } else if let Some(types) = type_.tuple_types() {
        types
            .iter()
            .all(|t| abi(t).is_some())
            .then_some(Abi::Tuple(types))
    } else if let Some((ok, error)) = type_.result_types() {
        (abi(&ok).is_some() && abi(&error).is_some()).then_some(Abi::Result(ok, error))
    } else {
        None
    }
}

/// Size in linear memory; every value is 8-aligned and sized in multiples
/// of 8.
fn size(abi_: &Abi) -> u32 {
    match abi_ {
        Abi::Int | Abi::Float | Abi::I32 | Abi::String | Abi::List(_) => 8,
        Abi::Nil => 0,
        Abi::Tuple(types) => types.iter().map(|t| size(&abi_expect(t))).sum(),
        Abi::Result(ok, error) => 8 + size(&abi_expect(ok)).max(size(&abi_expect(error))),
    }
}

fn abi_expect(type_: &Arc<Type>) -> Abi {
    abi(type_).expect("type to be supported by the rust ABI")
}

/// The wasm params of a Rust function for a Gleam param, `None` when the
/// type can't be a param.
fn param_val_types(abi_: &Abi) -> Option<Vec<ValType>> {
    match abi_ {
        Abi::Int => Some(vec![ValType::I64]),
        Abi::Float => Some(vec![ValType::F64]),
        Abi::I32 => Some(vec![ValType::I32]),
        Abi::Nil => Some(vec![]),
        Abi::String | Abi::List(_) => Some(vec![ValType::I32, ValType::I32]),
        Abi::Tuple(_) | Abi::Result(_, _) => None,
    }
}

/// The signature the Rust function must have for a Gleam function type.
pub(super) fn rust_signature(
    params: &[Arc<Type>],
    return_: &Arc<Type>,
) -> Option<(Vec<ValType>, Vec<ValType>)> {
    let mut wasm_params = vec![];
    for param in params {
        wasm_params.extend(param_val_types(&abi(param)?)?);
    }
    let results = match abi(return_)? {
        Abi::Int => vec![ValType::I64],
        Abi::Float => vec![ValType::F64],
        Abi::I32 => vec![ValType::I32],
        Abi::Nil => vec![],
        Abi::String | Abi::List(_) | Abi::Tuple(_) | Abi::Result(_, _) => {
            wasm_params.push(ValType::I32);
            vec![]
        }
    };
    Some((wasm_params, results))
}

impl<'a> Generator<'a> {
    pub(super) fn rust_function(&self, name: &str) -> FunctionId {
        *self
            .rust_functions
            .get(name)
            .unwrap_or_else(|| panic!("rust builtin \"{name}\" to be available"))
    }

    /// The glue of the Rust function `name` with the Gleam type `fn_type`,
    /// created on first use.
    pub(super) fn function_rust(&mut self, name: &EcoString, fn_type: &Arc<Type>) -> Id {
        let glue_name: EcoString = format!("{name}@glue").into();
        if let Some(id) = self.find_global(&glue_name) {
            return id;
        }
        let (params, return_) = fn_type.fn_types().expect("rust external to be a function");
        let index = self.code_rust_glue(name, &params, &return_);
        self.add_function_to_globals(glue_name, index)
    }

    fn code_rust_glue(
        &mut self,
        name: &EcoString,
        params: &[Arc<Type>],
        return_: &Arc<Type>,
    ) -> FunctionId {
        let target = self.rust_function(name);
        let cabi_realloc = self.rust_function(CABI_REALLOC);
        let arena_mark = self.rust_function(ARENA_MARK);
        let arena_release = self.rust_function(ARENA_RELEASE);
        let lower_string = self.function_lower_string();
        let return_abi = abi_expect(return_);
        let return_vt = self.val_type(return_);

        let param_locals = params
            .iter()
            .map(|t| {
                let vt = self.val_type(t);
                self.wasm_local(vt)
            })
            .collect_vec();
        let lower_lists = params
            .iter()
            .map(|t| match abi_expect(t) {
                Abi::List(item) => Some(self.function_lower_list(&item)),
                Abi::Int
                | Abi::Float
                | Abi::I32
                | Abi::Nil
                | Abi::String
                | Abi::Tuple(_)
                | Abi::Result(_, _) => None,
            })
            .collect_vec();
        let mark = self.wasm_local(ValType::I32);
        let ret = self.wasm_local(ValType::I32);
        let result = self.wasm_local(return_vt);
        let lift = self.lift_function(&return_abi);

        let mut function =
            Function::new(self, &format!("{name}@glue"), &param_locals, &[return_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions.call(arena_mark).local_set(mark);
        for ((type_, local), lower_list) in params.iter().zip(&param_locals).zip(&lower_lists) {
            let _ = match abi_expect(type_) {
                Abi::Int => instructions.local_get(*local).int_to_i64(),
                Abi::Float => instructions.local_get(*local).float_to_f64(),
                Abi::I32 => instructions.local_get(*local),
                Abi::Nil => &mut instructions,
                Abi::String => instructions
                    .local_get(*local)
                    .local_get(*local)
                    .string_len()
                    .call(lower_string)
                    .local_get(*local)
                    .string_len(),
                Abi::List(_) => instructions
                    .local_get(*local)
                    .call(lower_list.expect("list lowering")),
                Abi::Tuple(_) | Abi::Result(_, _) => {
                    panic!("unsupported rust param type should have been rejected")
                }
            };
        }
        let _ = match &return_abi {
            Abi::Int => instructions.call(target).i64_to_int(),
            Abi::Float => instructions.call(target).f64_to_float(),
            Abi::I32 => instructions.call(target),
            Abi::Nil => instructions.call(target).nil_const(),
            Abi::String | Abi::List(_) | Abi::Tuple(_) | Abi::Result(_, _) => instructions
                // the return area
                .i32_const(0)
                .i32_const(0)
                .i32_const(8)
                .i32_const(size(&return_abi) as i32)
                .call(cabi_realloc)
                .local_tee(ret)
                .call(target)
                .local_get(ret)
                .call(lift.expect("lift function")),
        };
        let _ = instructions
            .local_set(result)
            .local_get(mark)
            .call(arena_release)
            .local_get(result)
            .end();
        function.finish(self)
    }

    /// A function `(addr: i32) -> T` that reads a value laid out at `addr`,
    /// for the types returned through memory.
    fn lift_function(&mut self, abi_: &Abi) -> Option<FunctionId> {
        match abi_ {
            Abi::Int | Abi::Float | Abi::I32 | Abi::Nil => None,
            Abi::String => Some(self.function_lift_string_at()),
            Abi::List(item) => Some(self.function_lift_list_at(item)),
            Abi::Tuple(types) => Some(self.function_lift_at(&type_::tuple(types.clone()))),
            Abi::Result(ok, error) => {
                Some(self.function_lift_at(&type_::result(ok.clone(), error.clone())))
            }
        }
    }

    fn glue_function(
        &mut self,
        name: EcoString,
        code: impl FnOnce(&mut Self) -> FunctionId,
    ) -> FunctionId {
        if let Some(id) = self.find_global(&name) {
            return id.func_id();
        }
        let index = code(self);
        let _ = self.add_function_to_globals(name, index);
        index
    }

    /// `_lower_string(s) -> ptr`: copies the bytes of `s` to the arena.
    pub(super) fn function_lower_string(&mut self) -> FunctionId {
        self.glue_function("_lower_string".into(), |s| s.code_lower_string())
    }

    /// `_lower_string(s, n) -> ptr`: copies the first `n` bytes of `s` to the
    /// arena.
    fn code_lower_string(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        let bytes_vt = self.string.bytes_val_type();
        let cabi_realloc = self.rust_function(CABI_REALLOC);
        // params
        let s = self.wasm_local(string_vt); // String
        let n = self.wasm_local(ValType::I32);
        // locals
        let bytes = self.wasm_local(bytes_vt);
        let off = self.wasm_local(ValType::I32);
        let ptr = self.wasm_local(ValType::I32);
        let i = self.wasm_local(ValType::I32);
        let mut function = Function::new(self, "_lower_string", &[s, n], &[ValType::I32]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .local_get(s)
            .string_bytes()
            .local_set(bytes)
            .local_get(s)
            .string_off()
            .local_set(off)
            .i32_const(0)
            .i32_const(0)
            .i32_const(1)
            .local_get(n)
            .call(cabi_realloc)
            .local_set(ptr)
            .block_(InstrSeqType::Simple(None), |block_s| {
                let block_id = block_s.id();
                let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                    let loop_id = loop_s.id();
                    let _ = loop_s
                        .local_get(i)
                        .local_get(n)
                        .i32_ge_u()
                        .br_if(block_id)
                        // ptr[i] = bytes[off + i]
                        .local_get(ptr)
                        .local_get(i)
                        .i32_add()
                        .local_get(bytes)
                        .local_get(off)
                        .local_get(i)
                        .i32_add()
                        .bytes_get()
                        .i32_store8(MemArg { align: 1, offset: 0 })
                        .i32_inc(i)
                        .br(loop_id);
                });
            })
            .local_get(ptr)
            .end();
        function.finish(self)
    }

    /// `_lower_list(T)(l) -> (ptr, count)`: lays out the elements of `l` in
    /// the arena.
    fn function_lower_list(&mut self, item: &Arc<Type>) -> FunctionId {
        let list_type = type_::list(item.clone());
        let name = format!("_lower_list({})", self.type_pretty_name(&list_type)).into();
        let item = item.clone();
        self.glue_function(name, |s| s.code_lower_list(&item))
    }

    fn code_lower_list(&mut self, item: &Arc<Type>) -> FunctionId {
        let list_type = type_::list(item.clone());
        let list_vt = self.val_type(&list_type);
        let item_vt = self.val_type(item);
        let item_abi = abi_expect(item);
        let item_size = size(&item_abi) as i32;
        let (custom_type, cons, _, args) = self.list_type_cons(&list_type);
        let (_, cons_index, _) =
            self.mono_union_subtype_index(&list_type, &custom_type, &cons, &args);
        let cabi_realloc = self.rust_function(CABI_REALLOC);
        let store = self.store_function(item);
        // params
        let l = self.wasm_local(list_vt); // List(T)
        // locals
        let rest = self.wasm_local(list_vt);
        let count = self.wasm_local(ValType::I32);
        let ptr = self.wasm_local(ValType::I32);
        let addr = self.wasm_local(ValType::I32);
        let value = self.wasm_local(item_vt);
        let name = format!("_lower_list({})", self.type_pretty_name(&list_type));
        let mut function = Function::new(self, &name, &[l], &[ValType::I32, ValType::I32]);
        let mut instructions = function.extend_instructions(self);
        let walk = |s: &mut Instructions<'_, '_>, body: &dyn Fn(&mut Instructions<'_, '_>)| {
            let _ = s
                .local_get(l)
                .local_set(rest)
                .block_(InstrSeqType::Simple(None), |block_s| {
                    let block_id = block_s.id();
                    let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                        let loop_id = loop_s.id();
                        let _ = loop_s
                            .local_get(rest)
                            .ref_is_null()
                            .br_if(block_id)
                            .local_get(rest)
                            .ref_cast_non_null(HeapType::Concrete(cons_index))
                            .struct_get(cons_index, 2) // first
                            .local_set(value);
                        body(loop_s);
                        let _ = loop_s
                            .local_get(rest)
                            .ref_cast_non_null(HeapType::Concrete(cons_index))
                            .struct_get(cons_index, 1) // rest
                            .local_set(rest)
                            .br(loop_id);
                    });
                });
        };
        walk(&mut instructions, &|s| {
            let _ = s.i32_inc(count);
        });
        let _ = instructions
            .i32_const(0)
            .i32_const(0)
            .i32_const(8)
            .local_get(count)
            .i32_const(item_size)
            .i32_mul()
            .call(cabi_realloc)
            .local_tee(ptr)
            .local_set(addr);
        walk(&mut instructions, &|s| {
            let _ = s
                .local_get(addr)
                .local_get(value)
                .call(store)
                .local_get(addr)
                .i32_const(item_size)
                .i32_add()
                .local_set(addr);
        });
        let _ = instructions.local_get(ptr).local_get(count).end();
        function.finish(self)
    }

    /// `_store(T)(addr, value)`: writes a list element in the layout of the
    /// ABI. Only the types allowed in param lists are supported.
    fn store_function(&mut self, type_: &Arc<Type>) -> FunctionId {
        let name = format!("_store({})", self.type_pretty_name(type_)).into();
        let type_ = type_.clone();
        self.glue_function(name, |s| s.code_store(&type_))
    }

    fn code_store(&mut self, type_: &Arc<Type>) -> FunctionId {
        let value_vt = self.val_type(type_);
        let lower_string = self.function_lower_string();
        // params
        let addr = self.wasm_local(ValType::I32);
        let value = self.wasm_local(value_vt);
        let name = format!("_store({})", self.type_pretty_name(type_));
        let mut function = Function::new(self, &name, &[addr, value], &[]);
        let mut instructions = function.extend_instructions(self);
        let _ = match abi_expect(type_) {
            Abi::Int => instructions
                .local_get(addr)
                .local_get(value)
                .int_to_i64()
                .i64_store(mem_arg(0)),
            Abi::Float => instructions
                .local_get(addr)
                .local_get(value)
                .float_to_f64()
                .f64_store(mem_arg(0)),
            Abi::I32 => instructions
                .local_get(addr)
                .local_get(value)
                .i32_store(mem_arg(0)),
            Abi::Nil => &mut instructions,
            Abi::String => instructions
                .local_get(addr)
                .local_get(value)
                .local_get(value)
                .string_len()
                .call(lower_string)
                .i32_store(mem_arg(0))
                .local_get(addr)
                .local_get(value)
                .string_len()
                .i32_store(mem_arg(4)),
            Abi::List(_) | Abi::Tuple(_) | Abi::Result(_, _) => {
                panic!("unsupported list element type for a rust param: {type_:?}")
            }
        };
        function.finish(self)
    }

    /// `_lift_string(addr) -> String`: reads the `(ptr, len)` pair at `addr`.
    fn function_lift_string_at(&mut self) -> FunctionId {
        self.glue_function("_lift_string".into(), |s| s.code_lift_string_at())
    }

    fn code_lift_string_at(&mut self) -> FunctionId {
        let string_vt = self.string.val_type();
        // params
        let addr = self.wasm_local(ValType::I32);
        // locals
        let ptr = self.wasm_local(ValType::I32);
        let len = self.wasm_local(ValType::I32);
        let i = self.wasm_local(ValType::I32);
        let r = self.wasm_local(self.string.bytes_val_type());
        let mut function = Function::new(self, "_lift_string", &[addr], &[string_vt]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .local_get(addr)
            .i32_load(mem_arg(0))
            .local_set(ptr)
            .local_get(addr)
            .i32_load(mem_arg(4))
            .local_tee(len)
            .bytes_new()
            .local_set(r)
            .block_(InstrSeqType::Simple(None), |block_s| {
                let block_id = block_s.id();
                let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                    let loop_id = loop_s.id();
                    let _ = loop_s
                        .local_get(i)
                        .local_get(len)
                        .i32_ge_u()
                        .br_if(block_id)
                        // r[i] = ptr[i]
                        .local_get(r)
                        .local_get(i)
                        .local_get(ptr)
                        .local_get(i)
                        .i32_add()
                        .i32_load8_u(MemArg { align: 1, offset: 0 })
                        .bytes_set()
                        .i32_inc(i)
                        .br(loop_id);
                });
            })
            .local_get(r)
            .string_from_bytes()
            .end();
        function.finish(self)
    }

    /// `_lift_list(T)(addr) -> List(T)`: reads the `(ptr, count)` pair at
    /// `addr` and builds the list from its last element.
    fn function_lift_list_at(&mut self, item: &Arc<Type>) -> FunctionId {
        let list_type = type_::list(item.clone());
        let name = format!("_lift({})", self.type_pretty_name(&list_type)).into();
        let item = item.clone();
        self.glue_function(name, |s| s.code_lift_list_at(&item))
    }

    fn code_lift_list_at(&mut self, item: &Arc<Type>) -> FunctionId {
        let list_type = type_::list(item.clone());
        let list_vt = self.val_type(&list_type);
        let item_size = size(&abi_expect(item)) as i32;
        let (custom_type, cons, item_type, args) = self.list_type_cons(&list_type);
        let supertype_index = self.mono_union_supertype_index(&list_type, &custom_type);
        let _ = self.mono_union_subtype_index(&list_type, &custom_type, &cons, &args);
        let cons_variant = self.variant_expect(PRELUDE_MODULE_NAME.into(), cons.name.clone());
        let cons_fn = self.variant_constructor(
            vec![list_type.clone(), item_type],
            list_type.clone(),
            cons_variant,
        );
        let lift_item = self.lift_item(item);
        // params
        let addr = self.wasm_local(ValType::I32);
        // locals
        let element = self.wasm_local(ValType::I32);
        let count = self.wasm_local(ValType::I32);
        let r = self.wasm_local(list_vt);
        let name = format!("_lift({})", self.type_pretty_name(&list_type));
        let mut function = Function::new(self, &name, &[addr], &[list_vt]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .ref_null(HeapType::Concrete(supertype_index))
            .local_set(r)
            .local_get(addr)
            .i32_load(mem_arg(4))
            .local_set(count)
            // element = ptr + count * size
            .local_get(addr)
            .i32_load(mem_arg(0))
            .local_get(count)
            .i32_const(item_size)
            .i32_mul()
            .i32_add()
            .local_set(element)
            .block_(InstrSeqType::Simple(None), |block_s| {
                let block_id = block_s.id();
                let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                    let loop_id = loop_s.id();
                    let _ = loop_s
                        .local_get(count)
                        .i32_eqz()
                        .br_if(block_id)
                        .local_get(count)
                        .i32_const(1)
                        .i32_sub()
                        .local_set(count)
                        .local_get(element)
                        .i32_const(item_size)
                        .i32_sub()
                        .local_set(element)
                        // r = Cons(r, element)
                        .local_get(r);
                    lift_item(loop_s, element);
                    let _ = loop_s
                        .call(cons_fn)
                        .local_set(r)
                        .br(loop_id);
                });
            })
            .local_get(r)
            .end();
        function.finish(self)
    }

    /// `_lift(T)(addr) -> T` for tuples and results.
    fn function_lift_at(&mut self, type_: &Arc<Type>) -> FunctionId {
        let name = format!("_lift({})", self.type_pretty_name(type_)).into();
        let type_ = type_.clone();
        self.glue_function(name, |s| s.code_lift_at(&type_))
    }

    fn code_lift_at(&mut self, type_: &Arc<Type>) -> FunctionId {
        let value_vt = self.val_type(type_);
        // params
        let addr = self.wasm_local(ValType::I32);
        // locals
        let field = self.wasm_local(ValType::I32);
        let name = format!("_lift({})", self.type_pretty_name(type_));
        match abi_expect(type_) {
            Abi::Tuple(types) => {
                let type_index = self.tuple_type_index(types.iter().cloned());
                let lifts = types
                    .iter()
                    .map(|t| (self.lift_item(t), size(&abi_expect(t))))
                    .collect_vec();
                let mut function = Function::new(self, &name, &[addr], &[value_vt]);
                let mut instructions = function.extend_instructions(self);
                let mut offset = 0;
                for (lift, field_size) in lifts {
                    let _ = instructions
                        .local_get(addr)
                        .i32_const(offset as i32)
                        .i32_add()
                        .local_set(field);
                    lift(&mut instructions, field);
                    offset += field_size;
                }
                let _ = instructions.struct_new(type_index).end();
                function.finish(self)
            }
            Abi::Result(ok, error) => {
                let ok_fn = self.ok_variant_constructor(ok.clone(), error.clone());
                let error_fn = self.error_variant_constructor(ok.clone(), error.clone());
                let lift_ok = self.lift_item(&ok);
                let lift_error = self.lift_item(&error);
                let mut function = Function::new(self, &name, &[addr], &[value_vt]);
                #[rustfmt::skip]
                let _ = function
                    .extend_instructions(self)
                    .local_get(addr)
                    .i32_const(8)
                    .i32_add()
                    .local_set(field)
                    .local_get(addr)
                    .i32_load(mem_arg(0))
                    .i32_eqz()
                    .if_else(
                        value_vt,
                        |then_s| {
                            lift_ok(then_s, field);
                            let _ = then_s.call(ok_fn);
                        },
                        |else_s| {
                            lift_error(else_s, field);
                            let _ = else_s.call(error_fn);
                        },
                    )
                    .end();
                function.finish(self)
            }
            abi_ @ (Abi::Int | Abi::Float | Abi::I32 | Abi::Nil | Abi::String | Abi::List(_)) => {
                panic!("not lifted through a function: {abi_:?}")
            }
        }
    }

    /// Code that pushes the value of type `type_` laid out at the address in
    /// a local.
    fn lift_item(
        &mut self,
        type_: &Arc<Type>,
    ) -> impl Fn(&mut Instructions<'_, '_>, LocalId) + use<> {
        let abi_ = abi_expect(type_);
        let lift = self.lift_function(&abi_);
        move |s: &mut Instructions<'_, '_>, addr: LocalId| {
            let _ = match &abi_ {
                Abi::Int => s.local_get(addr).i64_load(mem_arg(0)).i64_to_int(),
                Abi::Float => s.local_get(addr).f64_load(mem_arg(0)).f64_to_float(),
                Abi::I32 => s.local_get(addr).i32_load(mem_arg(0)),
                Abi::Nil => s.nil_const(),
                Abi::String | Abi::List(_) | Abi::Tuple(_) | Abi::Result(_, _) => {
                    s.local_get(addr).call(lift.expect("lift function"))
                }
            };
        }
    }
}

fn mem_arg(offset: u64) -> MemArg {
    MemArg { align: 1, offset }
}
