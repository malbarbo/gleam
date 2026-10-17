// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! The functions of the host, declared as
//! `@external(webassembly, "./file.mjs", "name")`, and the values they
//! receive and return, which are the values of the JavaScript target.
//!
//! The function is imported with `externref` params and result, and called
//! through a function that converts each argument to JavaScript and the
//! result back, with functions generated for each type, like
//! `_dynamic_cast`, that call the functions of `runtime/js.gleam`. A value
//! whose type is a type variable of the declaration is not converted: the
//! host only keeps it, like `fn id(a: a) -> a`. A value of another type is
//! also kept as it is, for now.
//!
//! The values of the external types whose values are wasm values cross as
//! they are, like a JavaScript string when `string = "JSString"`.

use super::*;

pub(super) const JS_MODULE: &str = "builtins/js";

/// The direction in which a value crosses.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(super) enum Direction {
    ToJs,
    FromJs,
}

/// How a value is converted.
enum Conversion {
    /// It crosses as it is.
    Same,
    /// Calls this function with the value.
    Call(FunctionId),
    /// Calls `function` with the value and the closures that convert its
    /// items.
    With {
        function: FunctionId,
        closures: Vec<GlobalId>,
    },
    /// A tuple is an array. Each element has its field and its conversion,
    /// and `array` is `js_array_push` to JavaScript and `js_array_get` back.
    Tuple {
        type_index: TypeId,
        elements: Vec<(Option<u32>, Option<FunctionId>)>,
        new: FunctionId,
        array: FunctionId,
    },
    /// A JavaScript string, checked to be one.
    JsString(FunctionId),
    /// A value of this type that the host only keeps, as an `externref`.
    Opaque(Arc<Type>),
}

impl<'a> Generator<'a> {
    /// The function of the host `name` of the module `host_module`, declared
    /// with type `declared` and used with type `type_`: its import, called
    /// with the arguments converted to JavaScript and whose result is
    /// converted back.
    pub(super) fn function_host_call(
        &mut self,
        host_module: &EcoString,
        name: &EcoString,
        declared: &Arc<Type>,
        type_: &Arc<Type>,
    ) -> FunctionId {
        let (declared_params, declared_return) = declared.fn_types().expect("a function");
        let (params, return_) = type_.fn_types().expect("a function");
        let params: Vec<_> = declared_params
            .iter()
            .zip(&params)
            .map(|(declared, param)| self.host_type(declared, &self.represent(param)))
            .collect();
        let return_ = self.host_type(&declared_return, &self.represent(&return_));
        let to_js: Vec<_> = params
            .iter()
            .map(|param| self.conversion_call(param, Direction::ToJs))
            .collect();
        let from_js = self.conversion_call(&return_, Direction::FromJs);
        let param_types: Vec<_> = params
            .iter()
            .map(|param| self.val_type(&unmarked(param)))
            .collect();
        let return_type = self.val_type(&unmarked(&return_));
        let import_params: Vec<_> = param_types
            .iter()
            .zip(&to_js)
            .map(|(val_type, to_js)| {
                if to_js.is_some() {
                    EXTERNREF
                } else {
                    *val_type
                }
            })
            .collect();
        let import_return = if from_js.is_some() {
            EXTERNREF
        } else {
            return_type
        };
        let import_type = self.wasm_module.types.add(&import_params, &[import_return]);
        let (import, _) = self
            .wasm_module
            .add_import_func(host_module, name, import_type);
        let params: Vec<_> = param_types.iter().map(|vt| self.wasm_local(*vt)).collect();
        let mut function = Function::new(
            self,
            &format!("{host_module}.{name}"),
            &params,
            &[return_type],
        );
        let mut instructions = function.extend_instructions(self);
        for (param, to_js) in params.iter().zip(to_js) {
            let _ = instructions.local_get(*param);
            if let Some(to_js) = to_js {
                let _ = instructions.call(to_js);
            }
        }
        let _ = instructions.call(import);
        if let Some(from_js) = from_js {
            let _ = instructions.call(from_js);
        }
        let _ = instructions.end();
        function.finish(self)
    }

    /// `type_`, with the types at the positions of the type variables of
    /// `declared` marked as opaque, see `opaque`.
    fn host_type(&self, declared: &Arc<Type>, type_: &Arc<Type>) -> Arc<Type> {
        match (declared.as_ref(), type_.as_ref()) {
            (Type::Var { type_: var }, _) => match &*var.borrow() {
                TypeVar::Link { type_: declared } => self.host_type(declared, type_),
                TypeVar::Unbound { .. } | TypeVar::Generic { .. } => {
                    self.runtime_named_type(JS_MODULE, OPAQUE, vec![type_.clone()])
                }
            },
            (
                Type::Named {
                    arguments: declared,
                    ..
                },
                Type::Named {
                    publicity,
                    package,
                    module,
                    name,
                    arguments,
                    inferred_variant,
                },
            ) if declared.len() == arguments.len() => Arc::new(Type::Named {
                publicity: *publicity,
                package: package.clone(),
                module: module.clone(),
                name: name.clone(),
                arguments: declared
                    .iter()
                    .zip(arguments)
                    .map(|(declared, argument)| self.host_type(declared, argument))
                    .collect(),
                inferred_variant: *inferred_variant,
            }),
            (Type::Tuple { elements: declared }, Type::Tuple { elements }) => {
                Arc::new(Type::Tuple {
                    elements: declared
                        .iter()
                        .zip(elements)
                        .map(|(declared, element)| self.host_type(declared, element))
                        .collect(),
                })
            }
            _ => type_.clone(),
        }
    }

    /// The function that converts a value of the host type `type_` (see
    /// `host_type`) in `direction`, unless it crosses as it is.
    fn conversion_call(&mut self, type_: &Arc<Type>, direction: Direction) -> Option<FunctionId> {
        if self.crosses_as_is(type_, direction) {
            None
        } else {
            Some(self.function_conversion(type_, direction))
        }
    }

    /// Whether a value of the host type `type_` crosses as it is.
    fn crosses_as_is(&self, type_: &Arc<Type>, direction: Direction) -> bool {
        let type_ = opaque(type_).unwrap_or(type_);
        matches!(
            self.custom_type(type_),
            Some((CustomType::External { .. }, _))
        ) || (type_.is_string()
            && matches!(self.string, StringType::Js(_))
            && direction == Direction::ToJs)
    }

    /// `fn(T) -> JsValue` to JavaScript, `fn(JsValue) -> T` back.
    fn function_conversion(&mut self, type_: &Arc<Type>, direction: Direction) -> FunctionId {
        let val_type = self.val_type(&unmarked(type_));
        let name = qualified_type_name(type_);
        let builtin = match direction {
            Direction::ToJs => BuiltinFunction::ToJs(val_type, name),
            Direction::FromJs => BuiltinFunction::FromJs(val_type, name),
        };
        let type_ = type_.clone();
        self.get_function_builtin(builtin, |s| s.code_conversion(&type_, direction))
    }

    fn code_conversion(&mut self, type_: &Arc<Type>, direction: Direction) -> FunctionId {
        let conversion = self.conversion(type_, direction);
        let value_vt = self.val_type(&unmarked(type_));
        let (param_vt, result_vt, name) = match direction {
            Direction::ToJs => (value_vt, EXTERNREF, "_to_js"),
            Direction::FromJs => (EXTERNREF, value_vt, "_from_js"),
        };
        // params
        let value = self.wasm_local(param_vt);
        // locals
        let array = self.wasm_local(EXTERNREF);
        let mut function = Function::new(self, name, &[value], &[result_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = match conversion {
            Conversion::Same => instructions.local_get(value),
            Conversion::Call(call) => instructions.local_get(value).call(call),
            Conversion::With { function, closures } => {
                let _ = instructions.local_get(value);
                for closure in closures {
                    let _ = instructions.global_get(closure);
                }
                instructions.call(function)
            }
            Conversion::Tuple {
                type_index,
                elements,
                new,
                array: push,
            } if direction == Direction::ToJs => {
                let _ = instructions.call(new).local_set(array);
                for (field, to_js) in elements {
                    let _ = instructions
                        .local_get(array)
                        .local_get(value)
                        .field_get(type_index, field);
                    if let Some(to_js) = to_js {
                        let _ = instructions.call(to_js);
                    }
                    let _ = instructions.call(push).drop();
                }
                instructions.local_get(array)
            }
            Conversion::Tuple {
                type_index,
                elements,
                array: get,
                ..
            } => {
                for (index, (field, from_js)) in elements.into_iter().enumerate() {
                    let _ = instructions
                        .local_get(value)
                        .int_const(&index.into())
                        .call(get);
                    if let Some(from_js) = from_js {
                        let _ = instructions.call(from_js);
                    }
                    if field.is_none() {
                        let _ = instructions.drop();
                    }
                }
                instructions.struct_new(type_index)
            }
            Conversion::JsString(check) => {
                instructions.local_get(value).call(check).ref_as_non_null()
            }
            Conversion::Opaque(type_) if direction == Direction::ToJs => {
                let _ = instructions.local_get(value);
                if let Some(box_) = self.dynamic_box(&type_) {
                    let _ = instructions.struct_new(box_);
                }
                instructions.extern_convert_any()
            }
            Conversion::Opaque(type_) => {
                let _ = instructions.local_get(value).any_convert_extern();
                self.unbox_instructions(&mut instructions, &type_);
                &mut instructions
            }
        };
        let _ = instructions.end();
        function.finish(self)
    }

    fn conversion(&mut self, type_: &Arc<Type>, direction: Direction) -> Conversion {
        if self.crosses_as_is(type_, direction) {
            return Conversion::Same;
        }
        if let Some(type_) = opaque(type_) {
            // A JavaScript string is a value of the host, not a reference to
            // a wasm value.
            return if type_.is_string() && matches!(self.string, StringType::Js(_)) {
                self.conversion(type_, direction)
            } else {
                Conversion::Opaque(type_.clone())
            };
        }
        let js_value = self.runtime_named_type(JS_MODULE, "JsValue", vec![]);
        let call = |s: &mut Self, name: &str| {
            let function = match direction {
                Direction::ToJs => {
                    s.runtime_function_id(JS_MODULE, name, vec![type_.clone()], js_value.clone())
                }
                Direction::FromJs => s.runtime_function_id(
                    JS_MODULE,
                    &format!("{name}_from"),
                    vec![js_value.clone()],
                    type_.clone(),
                ),
            };
            Conversion::Call(function)
        };
        if type_.is_int() {
            call(self, "js_int")
        } else if type_.is_float() {
            call(self, "js_float")
        } else if type_.is_bool() {
            call(self, "js_bool")
        } else if type_.is_nil() {
            call(self, "js_nil")
        } else if type_.is_utf_codepoint() {
            call(self, "js_utf_codepoint")
        } else if type_.is_string() {
            match self.string {
                StringType::Utf8(_) => call(self, "js_string"),
                StringType::Js(_) => Conversion::JsString(self.runtime_function_id(
                    JS_MODULE,
                    "js_expect_string",
                    vec![js_value.clone()],
                    js_value,
                )),
            }
        } else if let Some((BIT_ARRAY_MODULE, "Bits")) = type_
            .named_type_information()
            .as_ref()
            .map(|(module, name, _)| (module.as_str(), name.as_str()))
        {
            call(self, "js_bit_array")
        } else if let Some(item) = type_.list_type() {
            self.conversion_with(type_, "js_list", &[item], direction)
        } else if let Some((ok, error)) = type_.result_types() {
            self.conversion_with(type_, "js_result", &[ok, error], direction)
        } else if let Some(elements) = type_.tuple_types() {
            let unmarked_elements: Vec<_> = elements.iter().map(unmarked).collect();
            let type_index = self.tuple_type_index(unmarked_elements.clone());
            let fields = self.field_indices(&unmarked_elements);
            let elements = fields
                .into_iter()
                .zip(&elements)
                .map(|(field, element)| (field, self.conversion_call(element, direction)))
                .collect();
            let new = self.runtime_function_id(JS_MODULE, "js_array_new", vec![], js_value.clone());
            let array = match direction {
                Direction::ToJs => self.runtime_function_id(
                    JS_MODULE,
                    "js_array_push",
                    vec![js_value.clone(), js_value],
                    type_::nil(),
                ),
                Direction::FromJs => self.runtime_function_id(
                    JS_MODULE,
                    "js_array_get",
                    vec![js_value.clone(), type_::int()],
                    js_value,
                ),
            };
            Conversion::Tuple {
                type_index,
                elements,
                new,
                array,
            }
        } else {
            Conversion::Opaque(type_.clone())
        }
    }

    /// Converts a value of `type_` with the function `name` of the runtime
    /// (`name_from` back), which takes the closures that convert `items`.
    fn conversion_with(
        &mut self,
        type_: &Arc<Type>,
        name: &str,
        items: &[Arc<Type>],
        direction: Direction,
    ) -> Conversion {
        let js_value = self.runtime_named_type(JS_MODULE, "JsValue", vec![]);
        let value = unmarked(type_);
        let mut closures = vec![];
        let mut closure_types = vec![];
        for item in items {
            let function = self.function_conversion(item, direction);
            let closure_type = match direction {
                Direction::ToJs => type_::fn_(vec![unmarked(item)], js_value.clone()),
                Direction::FromJs => type_::fn_(vec![js_value.clone()], unmarked(item)),
            };
            closures.push(self.closure_of(function, &closure_type));
            closure_types.push(closure_type);
        }
        let function = match direction {
            Direction::ToJs => self.runtime_function_id(
                JS_MODULE,
                &format!("{name}_with"),
                iter::once(value).chain(closure_types).collect(),
                js_value,
            ),
            Direction::FromJs => self.runtime_function_id(
                JS_MODULE,
                &format!("{name}_from_with"),
                iter::once(js_value).chain(closure_types).collect(),
                value,
            ),
        };
        Conversion::With { function, closures }
    }
}

/// The name of the type that marks a type as opaque in a host type: its
/// values cross as references that the host only keeps. It is not a type
/// of the runtime, it only names the conversions.
const OPAQUE: &str = "Opaque";

/// The type that `type_` marks as opaque.
fn opaque(type_: &Arc<Type>) -> Option<&Arc<Type>> {
    match type_.as_ref() {
        Type::Named {
            package,
            module,
            name,
            arguments,
            ..
        } if package == RUNTIME_PACKAGE && module == JS_MODULE && name == OPAQUE => {
            arguments.first()
        }
        Type::Named { .. } | Type::Fn { .. } | Type::Var { .. } | Type::Tuple { .. } => None,
    }
}

/// `type_` without the marks of opaque types.
fn unmarked(type_: &Arc<Type>) -> Arc<Type> {
    if let Some(type_) = opaque(type_) {
        return unmarked(type_);
    }
    match type_.as_ref() {
        Type::Named {
            publicity,
            package,
            module,
            name,
            arguments,
            inferred_variant,
        } => Arc::new(Type::Named {
            publicity: *publicity,
            package: package.clone(),
            module: module.clone(),
            name: name.clone(),
            arguments: arguments.iter().map(unmarked).collect(),
            inferred_variant: *inferred_variant,
        }),
        Type::Tuple { elements } => Arc::new(Type::Tuple {
            elements: elements.iter().map(unmarked).collect(),
        }),
        Type::Fn { .. } | Type::Var { .. } => type_.clone(),
    }
}

impl<'a> Generator<'a> {
    /// `_bytes_to_memory(bytes, start, size, address)`: writes `size` bytes of
    /// `bytes` from `start` at `address` of the linear memory.
    pub(super) fn code_bytes_to_memory(&mut self) -> FunctionId {
        // params
        let bytes = self.wasm_local(self.bytes.val_type());
        let start = self.wasm_local(self.int.val_type());
        let size = self.wasm_local(self.int.val_type());
        let dest = self.wasm_local(ValType::I32);
        // locals
        let i = self.wasm_local(ValType::I32);
        let end = self.wasm_local(ValType::I32);
        let needed = self.wasm_local(ValType::I32);
        let mut function = Function::new(
            self,
            "_bytes_to_memory",
            &[bytes, start, size, dest],
            &[BOOL_VALTYPE],
        );
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(dest)
            .local_get(size)
            .int_to_i32()
            .i32_add()
            .ensure_memory(needed)
            .local_get(start)
            .int_to_i32()
            .local_tee(i)
            .local_get(size)
            .int_to_i32()
            .i32_add()
            .local_set(end)
            .block_(InstrSeqType::Simple(None), |block_s| {
                let block_id = block_s.id();
                let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                    let loop_id = loop_s.id();
                    let _ = loop_s
                        .local_get(i)
                        .local_get(end)
                        .i32_ge_u()
                        .br_if(block_id)
                        .local_get(dest)
                        .local_get(bytes)
                        .local_get(i)
                        .bytes_get()
                        .i32_store8(MemArg { offset: 0, align: 0 })
                        .i32_inc(dest)
                        .i32_inc(i)
                        .br(loop_id);
                });
            })
            // Nil
            .i32_const(0)
            .end();
        function.finish(self)
    }

    /// `_bytes_from_memory(address, size)`: the `size` bytes at `address` of
    /// the linear memory.
    pub(super) fn code_bytes_from_memory(&mut self) -> FunctionId {
        // params
        let src = self.wasm_local(ValType::I32);
        let size = self.wasm_local(self.int.val_type());
        // locals
        let bytes = self.wasm_local(self.bytes.val_type());
        let len = self.wasm_local(ValType::I32);
        let i = self.wasm_local(ValType::I32);
        let mut function = Function::new(
            self,
            "_bytes_from_memory",
            &[src, size],
            &[self.bytes.val_type()],
        );
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(size)
            .int_to_i32()
            .local_tee(len)
            .bytes_new()
            .local_set(bytes)
            .i32_const(0)
            .local_set(i)
            .block_(InstrSeqType::Simple(None), |block_s| {
                let block_id = block_s.id();
                let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                    let loop_id = loop_s.id();
                    let _ = loop_s
                        .local_get(i)
                        .local_get(len)
                        .i32_ge_u()
                        .br_if(block_id)
                        .local_get(bytes)
                        .local_get(i)
                        .local_get(src)
                        .i32_load8_u(MemArg { offset: 0, align: 0 })
                        .bytes_set()
                        .i32_inc(src)
                        .i32_inc(i)
                        .br(loop_id);
                });
            })
            .local_get(bytes)
            .end();
        function.finish(self)
    }
}
