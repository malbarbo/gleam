// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `Array(a)`: an immutable array, represented by a wasm GC array of the
//! representation of `a`. Only the runtime declares it as an external type
//! and reaches its functions as builtins, as the nodes of its `Dict`:
//!
//! ```gleam
//! @external(webassembly, "builtins", "Array")
//! pub type Array(a)
//!
//! @external(webassembly, "builtins", "_array_get")
//! fn get(array: Array(a), index: Int) -> a
//! ```
//!
//! `_array_get` traps when the index is out of range, as the runtime only
//! reads the positions it knows. The functions that change an array return a
//! copy, or the same array when the index is out of range. Arrays are
//! compared, hashed and inspected like tuples, as they are on Erlang (tuples)
//! and JavaScript (arrays).

use super::*;

/// The functions of `Array`.
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub(super) enum ArrayFunction {
    /// `fn(Array(a)) -> Int`.
    Size,
    /// `fn(Array(a), Int) -> a`: the element at the index, which traps
    /// when the index is out of range.
    Get,
    /// `fn(Array(a), Int, a) -> Array(a)`: a copy with the element at the
    /// index replaced.
    Set,
    /// `fn(Array(a), Int, a) -> Array(a)`: a copy with the element inserted
    /// at the index, which may be the size.
    Insert,
    /// `fn(Array(a), Int) -> Array(a)`: a copy without the element at the
    /// index.
    Remove,
    /// `fn(List(a)) -> Array(a)`.
    FromList,
}

/// The type of an argument or of the result of a function of `Array`.
#[derive(Clone, Copy)]
enum Shape {
    Array,
    Element,
    Int,
    List,
}

impl ArrayFunction {
    pub(super) fn all() -> &'static [ArrayFunction] {
        &[
            ArrayFunction::Size,
            ArrayFunction::Get,
            ArrayFunction::Set,
            ArrayFunction::Insert,
            ArrayFunction::Remove,
            ArrayFunction::FromList,
        ]
    }

    pub(super) fn name(self) -> &'static str {
        match self {
            ArrayFunction::Size => "_array_size",
            ArrayFunction::Get => "_array_get",
            ArrayFunction::Set => "_array_set",
            ArrayFunction::Insert => "_array_insert",
            ArrayFunction::Remove => "_array_remove",
            ArrayFunction::FromList => "_array_from_list",
        }
    }

    pub(super) fn by_name(name: &str) -> Option<ArrayFunction> {
        Self::all().iter().copied().find(|f| f.name() == name)
    }

    fn shape(self) -> (&'static [Shape], Shape) {
        match self {
            ArrayFunction::Size => (&[Shape::Array], Shape::Int),
            ArrayFunction::Get => (&[Shape::Array, Shape::Int], Shape::Element),
            ArrayFunction::Set | ArrayFunction::Insert => {
                (&[Shape::Array, Shape::Int, Shape::Element], Shape::Array)
            }
            ArrayFunction::Remove => (&[Shape::Array, Shape::Int], Shape::Array),
            ArrayFunction::FromList => (&[Shape::List], Shape::Array),
        }
    }

    fn signature(self) -> &'static str {
        match self {
            ArrayFunction::Size => "fn(Array(a)) -> Int",
            ArrayFunction::Get => "fn(Array(a), Int) -> a",
            ArrayFunction::Set | ArrayFunction::Insert => "fn(Array(a), Int, a) -> Array(a)",
            ArrayFunction::Remove => "fn(Array(a), Int) -> Array(a)",
            ArrayFunction::FromList => "fn(List(a)) -> Array(a)",
        }
    }
}

impl<'a> Generator<'a> {
    /// The element type of `type_` when it is an `Array`.
    pub(super) fn array_element(&self, type_: &Arc<Type>) -> Option<Arc<Type>> {
        match self.custom_type(type_)? {
            (CustomType::Array, arguments) => arguments.into_iter().next(),
            _ => None,
        }
    }

    /// The element type of the external `function` of `Array`, when its type
    /// has the shape of `array_function`.
    fn array_function_element(
        &self,
        array_function: ArrayFunction,
        arguments: &[Arc<Type>],
        return_: &Arc<Type>,
    ) -> Option<Arc<Type>> {
        let (shapes, return_shape) = array_function.shape();
        if shapes.len() != arguments.len() {
            return None;
        }
        let mut element: Option<Arc<Type>> = None;
        // The arrays of a function are of the same type, even when two types
        // are declared as `Array`.
        let mut array: Option<&Arc<Type>> = None;
        for (shape, type_) in shapes
            .iter()
            .zip(arguments)
            .chain([(&return_shape, return_)])
        {
            let found = match shape {
                Shape::Int => {
                    if type_.is_int() {
                        continue;
                    }
                    return None;
                }
                Shape::Element => type_.clone(),
                Shape::Array => {
                    match array {
                        Some(array) if !array.same_as(type_) => return None,
                        Some(_) => {}
                        None => array = Some(type_),
                    }
                    self.array_element(type_)?
                }
                Shape::List => type_.list_type()?,
            };
            match &element {
                Some(element) if !element.same_as(&found) => return None,
                Some(_) => {}
                None => element = Some(found),
            }
        }
        element
    }

    /// Checks the type of an external function of `Array`.
    pub(super) fn validate_array_external(
        &self,
        function: &TypedFunction,
        name: &EcoString,
        array_function: ArrayFunction,
    ) -> Result<(), Error> {
        let arguments = function
            .arguments
            .iter()
            .map(|argument| argument.type_.clone())
            .collect_vec();
        match self.array_function_element(array_function, &arguments, &function.return_type) {
            Some(_) => Ok(()),
            None => Err(Error::WrongBuiltinFunctionSignature {
                location: function.location,
                name: name.clone(),
                expected: array_function.signature().into(),
                got: self.type_pretty_name(&function_type(function)),
            }),
        }
    }

    /// The function of `Array` with the type `fn_type`.
    pub(super) fn function_array_external(
        &mut self,
        array_function: ArrayFunction,
        fn_type: &Arc<Type>,
    ) -> Id {
        let (arguments, return_) = fn_type
            .fn_types()
            .expect("array function must be a function type");
        let element = self
            .array_function_element(array_function, &arguments, &return_)
            .expect("checked array function type");
        let index = self.function_array(array_function, &element);
        Id::func(array_function.name().into(), index)
    }

    /// The function of `Array` for arrays of `element`.
    pub(super) fn function_array(
        &mut self,
        array_function: ArrayFunction,
        element: &Arc<Type>,
    ) -> FunctionId {
        let int = self.int.val_type();
        let element_vt = self.val_type(element);
        let array_index = self.array_type_index(element);
        let array_vt = self.val_type_ref(array_index);
        let list_vt = self.val_type(&type_::list(element.clone()));
        let (shapes, return_shape) = array_function.shape();
        let val_type = |shape: &Shape| match shape {
            Shape::Array => array_vt,
            Shape::Element => element_vt,
            Shape::Int => int,
            Shape::List => list_vt,
        };
        let builtin = BuiltinFunction::Array {
            function: array_function,
            params: shapes.iter().map(val_type).collect(),
            result: val_type(&return_shape),
            element: self.type_pretty_name(element),
        };
        let element = element.clone();
        self.get_function_builtin(builtin, |s| s.code_array(array_function, &element))
    }

    /// The type of the wasm array of `element`. The elements are mutable,
    /// so the copies can be filled, but an array is not changed after it is
    /// built.
    pub(super) fn array_type_index(&mut self, element: &Arc<Type>) -> TypeId {
        let element_vt = self.val_type(element);
        let key = WasmType::array(StorageType::Val(element_vt));
        if let Some(id) = self.wasm_types.get(&key) {
            return *id;
        }
        let comp = walrus::CompositeType::Array(walrus::ArrayType {
            field: FieldType {
                element_type: StorageType::Val(element_vt),
                mutable: true,
            },
        });
        let id = self.wasm_module.types.add_composite(comp, true, None);
        let name = format!("Array({})", self.type_pretty_name(element)).replace(" ", "");
        self.wasm_module.types.get_mut(id).name = Some(name);
        let _ = self.wasm_types.insert(key, id);
        id
    }

    fn code_array(&mut self, array_function: ArrayFunction, element: &Arc<Type>) -> FunctionId {
        let array_index = self.array_type_index(element);
        let array_vt = self.val_type_ref(array_index);
        let element_vt = self.val_type(element);
        let int_vt = self.int.val_type();
        let name = array_function.name();
        match array_function {
            ArrayFunction::Size => {
                let array = self.wasm_local(array_vt);
                let mut function = Function::new(self, name, &[array], &[int_vt]);
                let _ = function
                    .extend_instructions(self)
                    .local_get(array)
                    .array_len()
                    .i32_to_int()
                    .end();
                function.finish(self)
            }
            ArrayFunction::Get => {
                let array = self.wasm_local(array_vt);
                let index = self.wasm_local(int_vt);
                let mut function = Function::new(self, name, &[array, index], &[element_vt]);
                let _ = function
                    .extend_instructions(self)
                    .local_get(array)
                    .local_get(index)
                    .int_to_i32()
                    .array_get(array_index)
                    .end();
                function.finish(self)
            }
            ArrayFunction::Set => {
                let array = self.wasm_local(array_vt);
                let index = self.wasm_local(int_vt);
                let value = self.wasm_local(element_vt);
                let result = self.wasm_local(array_vt);
                let mut function = Function::new(self, name, &[array, index, value], &[array_vt]);
                #[rustfmt::skip]
                let _ = function
                    .extend_instructions(self)
                    .array_out_of_range(array, index, false, |then_s| {
                        let _ = then_s.local_get(array).return_();
                    })
                    // result = a copy of array
                    .local_get(value)
                    .local_get(array)
                    .array_len()
                    .array_new(array_index)
                    .local_tee(result)
                    .i32_const(0)
                    .local_get(array)
                    .i32_const(0)
                    .local_get(array)
                    .array_len()
                    .array_copy(array_index, array_index)
                    // result[index] = value
                    .local_get(result)
                    .local_get(index)
                    .int_to_i32()
                    .local_get(value)
                    .array_set(array_index)
                    .local_get(result)
                    .end();
                function.finish(self)
            }
            ArrayFunction::Insert => {
                let array = self.wasm_local(array_vt);
                let index = self.wasm_local(int_vt);
                let value = self.wasm_local(element_vt);
                let at = self.wasm_local(ValType::I32);
                let result = self.wasm_local(array_vt);
                let mut function = Function::new(self, name, &[array, index, value], &[array_vt]);
                #[rustfmt::skip]
                let _ = function
                    .extend_instructions(self)
                    .array_out_of_range(array, index, true, |then_s| {
                        let _ = then_s.local_get(array).return_();
                    })
                    .local_get(index)
                    .int_to_i32()
                    .local_set(at)
                    // result = [value] * (len + 1)
                    .local_get(value)
                    .local_get(array)
                    .array_len()
                    .i32_const(1)
                    .i32_add()
                    .array_new(array_index)
                    // result[0..at] = array[0..at]
                    .local_tee(result)
                    .i32_const(0)
                    .local_get(array)
                    .i32_const(0)
                    .local_get(at)
                    .array_copy(array_index, array_index)
                    // result[at + 1..] = array[at..]
                    .local_get(result)
                    .local_get(at)
                    .i32_const(1)
                    .i32_add()
                    .local_get(array)
                    .local_get(at)
                    .local_get(array)
                    .array_len()
                    .local_get(at)
                    .i32_sub()
                    .array_copy(array_index, array_index)
                    .local_get(result)
                    .end();
                function.finish(self)
            }
            ArrayFunction::Remove => {
                let array = self.wasm_local(array_vt);
                let index = self.wasm_local(int_vt);
                let at = self.wasm_local(ValType::I32);
                let len = self.wasm_local(ValType::I32);
                let result = self.wasm_local(array_vt);
                let mut function = Function::new(self, name, &[array, index], &[array_vt]);
                #[rustfmt::skip]
                let _ = function
                    .extend_instructions(self)
                    .array_out_of_range(array, index, false, |then_s| {
                        let _ = then_s.local_get(array).return_();
                    })
                    .local_get(index)
                    .int_to_i32()
                    .local_set(at)
                    // len = array.len - 1
                    .local_get(array)
                    .array_len()
                    .i32_const(1)
                    .i32_sub()
                    .local_set(len)
                    // result = [array[0]] * len, array has an element at `at`
                    .local_get(array)
                    .i32_const(0)
                    .array_get(array_index)
                    .local_get(len)
                    .array_new(array_index)
                    // result[0..at] = array[0..at]
                    .local_tee(result)
                    .i32_const(0)
                    .local_get(array)
                    .i32_const(0)
                    .local_get(at)
                    .array_copy(array_index, array_index)
                    // result[at..] = array[at + 1..]
                    .local_get(result)
                    .local_get(at)
                    .local_get(array)
                    .local_get(at)
                    .i32_const(1)
                    .i32_add()
                    .local_get(len)
                    .local_get(at)
                    .i32_sub()
                    .array_copy(array_index, array_index)
                    .local_get(result)
                    .end();
                function.finish(self)
            }
            ArrayFunction::FromList => self.code_array_from_list(element, array_index),
        }
    }

    fn code_array_from_list(&mut self, element: &Arc<Type>, array_index: TypeId) -> FunctionId {
        let list_type = type_::list(element.clone());
        let (custom_type, cons, _, args) = self.list_type_cons(&list_type);
        let (_, cons_index, _) =
            self.mono_union_subtype_index(&list_type, &custom_type, &cons, &args);
        let (rest_field, first_field) = self.list_cons_fields(&list_type);
        let list_vt = self.val_type(&list_type);
        let array_vt = self.val_type_ref(array_index);
        // params
        let list = self.wasm_local(list_vt);
        // locals
        let current = self.wasm_local(list_vt);
        let len = self.wasm_local(ValType::I32);
        let i = self.wasm_local(ValType::I32);
        let result = self.wasm_local(array_vt);
        let first = |instructions: &mut Instructions<'_, '_>, list: LocalId| {
            let _ = instructions
                .local_get(list)
                .variant_field_get(cons_index, first_field);
        };
        let rest = |instructions: &mut Instructions<'_, '_>, list: LocalId| {
            let _ = instructions
                .local_get(list)
                .ref_cast_non_null(HeapType::Concrete(cons_index))
                .struct_get(cons_index, rest_field)
                .local_set(list);
        };
        let mut function = Function::new(self, "_array_from_list", &[list], &[array_vt]);
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(list)
            .ref_is_null()
            .if_(InstrSeqType::Simple(None), |then_s| {
                let _ = then_s.array_new_fixed(array_index, 0).return_();
            })
            // len = the length of the list
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
                        .i32_inc(len);
                    rest(loop_s, current);
                    let _ = loop_s.br(loop_id);
                });
            });
        // result = [first] * len
        first(&mut instructions, list);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(len)
            .array_new(array_index)
            .local_set(result)
            .block_(InstrSeqType::Simple(None), |block_s| {
                let block_id = block_s.id();
                let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                    let loop_id = loop_s.id();
                    let _ = loop_s
                        .local_get(list)
                        .ref_is_null()
                        .br_if(block_id)
                        .local_get(result)
                        .local_get(i);
                    first(loop_s, list);
                    let _ = loop_s.array_set(array_index).i32_inc(i);
                    rest(loop_s, list);
                    let _ = loop_s.br(loop_id);
                });
            })
            .local_get(result)
            .end();
        function.finish(self)
    }

    /// `_equal(Array(a))`: the same size and equal elements.
    pub(super) fn code_array_eq(&mut self, element: &Arc<Type>) -> FunctionId {
        let array_index = self.array_type_index(element);
        let array_vt = self.val_type_ref(array_index);
        let eq = self.function_eq(element);
        // params
        let a = self.wasm_local(array_vt);
        let b = self.wasm_local(array_vt);
        // locals
        let i = self.wasm_local(ValType::I32);
        let len = self.wasm_local(ValType::I32);
        let mut function = Function::new(self, "_equal_array", &[a, b], &[BOOL_VALTYPE]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .local_get(a)
            .local_get(b)
            .ref_eq()
            .if_(InstrSeqType::Simple(None), |body| {
                let _ = body.bool_const(true).return_();
            })
            .local_get(a)
            .array_len()
            .local_tee(len)
            .local_get(b)
            .array_len()
            .i32_ne()
            .if_(InstrSeqType::Simple(None), |body| {
                let _ = body.bool_const(false).return_();
            })
            .loop_(InstrSeqType::Simple(None), |loop_s| {
                let loop_id = loop_s.id();
                let _ = loop_s
                    .local_get(i)
                    .local_get(len)
                    .i32_ge_u()
                    .if_(InstrSeqType::Simple(None), |body| {
                        let _ = body.bool_const(true).return_();
                    })
                    .local_get(a)
                    .local_get(i)
                    .array_get(array_index)
                    .local_get(b)
                    .local_get(i)
                    .array_get(array_index)
                    .eq(eq)
                    .bool_not()
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

    /// `_hash(Array(a))`: combines the hashes of the elements in order, as
    /// for a tuple.
    pub(super) fn code_array_hash(&mut self, element: &Arc<Type>) -> FunctionId {
        let array_index = self.array_type_index(element);
        let array_vt = self.val_type_ref(array_index);
        let hash = self.function_hash(element);
        let mix = self.function_hash_mix();
        // params
        let array = self.wasm_local(array_vt);
        // locals
        let i = self.wasm_local(ValType::I32);
        let h = self.wasm_local(ValType::I32);
        let mut function = Function::new(self, "_hash_array", &[array], &[ValType::I32]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions
            .i32_const(SEED)
            .local_set(h)
            .block_(InstrSeqType::Simple(None), |block_s| {
                let block_id = block_s.id();
                let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                    let loop_id = loop_s.id();
                    // h = (h ^ hash(array[i])) * MULTIPLIER
                    let _ = loop_s
                        .local_get(i)
                        .local_get(array)
                        .array_len()
                        .i32_ge_u()
                        .br_if(block_id)
                        .local_get(h)
                        .local_get(array)
                        .local_get(i)
                        .array_get(array_index);
                    self.hash_instructions(loop_s, hash);
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

    /// The repr of an `Array(a)`, `#(a, b, c)`, as a tuple.
    pub(super) fn code_array_repr(&mut self, element: &Arc<Type>) -> FunctionId {
        let array_index = self.array_type_index(element);
        let array_vt = self.val_type_ref(array_index);
        let element_repr = self.function_repr(element);
        // params
        let array = self.wasm_local(array_vt);
        let ptr = self.wasm_local(ValType::I32);
        // locals
        let dest = self.wasm_local(ValType::I32);
        let i = self.wasm_local(ValType::I32);
        // return I32 - number of written bytes
        let mut function = Function::new(self, "_repr_array", &[array, ptr], &[ValType::I32]);
        #[rustfmt::skip]
        let _ = function
            .extend_instructions(self)
            .local_get(ptr)
            .local_set(dest)
            .write_byte(dest, b'#')
            .write_byte(dest, b'(')
            .block_(InstrSeqType::Simple(None), |block_s| {
                let block_id = block_s.id();
                let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                    let loop_id = loop_s.id();
                    let _ = loop_s
                        .local_get(i)
                        .local_get(array)
                        .array_len()
                        .i32_ge_u()
                        .br_if(block_id)
                        .local_get(i)
                        .if_(InstrSeqType::Simple(None), |then_s| {
                            let _ = then_s.write_byte(dest, b',').write_byte(dest, b' ');
                        })
                        .local_get(array)
                        .local_get(i)
                        .array_get(array_index)
                        .local_get(dest)
                        .call(element_repr)
                        .local_get(dest)
                        .i32_add()
                        .local_set(dest)
                        .i32_inc(i)
                        .br(loop_id);
                });
            })
            .write_byte(dest, b')')
            .local_get(dest)
            .local_get(ptr)
            .i32_sub()
            .end();
        function.finish(self)
    }
}

impl<'a, 'b> Instructions<'a, 'b> {
    /// Runs `then`, which returns, unless the `Int` in `index` is an index
    /// of `array`, or its size when `may_be_size`.
    fn array_out_of_range(
        &mut self,
        array: LocalId,
        index: LocalId,
        may_be_size: bool,
        then: impl FnOnce(&mut Instructions<'_, '_>),
    ) -> &mut Self {
        let _ = self.local_get(index).local_get(array).array_len();
        if may_be_size {
            let _ = self.i32_const(1).i32_add();
        }
        self.i32_to_int()
            .int_ge_u()
            .if_(InstrSeqType::Simple(None), then)
    }
}
