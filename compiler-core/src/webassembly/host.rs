// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! The functions of the host, declared as
//! `@external(webassembly, "./file.mjs", "name")`, and the values they
//! receive and return, which are the values of the JavaScript target.
//!
//! The function is imported with `externref` params and result, and called
//! through a function that converts each argument to JavaScript and the
//! result back, with functions generated for each type, like
//! `_dynamic_cast`, that call the functions of `runtime/js.gleam`. A value of
//! a custom type is an instance of the class of its variant, created and
//! read with the functions of the JavaScript target for its type, from the
//! module of the custom types of its module (see `javascript::types_module`).
//! A function is a JavaScript function that calls it with its arguments
//! converted back, and converts its result. A value whose type is a type
//! variable of the declaration is converted by the type it is used with, as
//! the external is imported for each one. A dict is a `Dict` of the module
//! `dict.mjs` of the stdlib, as on JavaScript. A `Dynamic` is the value it
//! holds, read back as on JavaScript. A value of another type, like a type of
//! the runtime, is kept as it is: the host only keeps it.
//!
//! The values of the external types cross as they are, like a JavaScript
//! string when `string = "JSString"`.

use super::*;

pub(super) const JS_MODULE: &str = "builtins/js";

/// The module of the stdlib whose `dict.mjs`, next to its directory, has the
/// `Dict` of the JavaScript target.
const STDLIB_DICT: &str = "gleam/dict";

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
    /// Calls `function` with the value and the globals `closures`: values of
    /// the host and the closures that convert its items.
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
    /// A JavaScript string, with the function that converts a value to one.
    JsString(FunctionId),
    /// A value of this type that the host only keeps, as an `externref`.
    Opaque(Arc<Type>),
    /// A `Dynamic` to JavaScript: the value of the host that it holds, which
    /// is the field `field` of the struct `other` of the variant `DOther`,
    /// boxed in `box_`, or the value that `function` makes, with the
    /// closures that convert a string, an item, and another value, which
    /// crosses in a box (see `dynamic_self_box`).
    DynamicToJs {
        other: TypeId,
        field: u32,
        box_: TypeId,
        function: FunctionId,
        closures: Vec<GlobalId>,
    },
    /// A value of the host to `Dynamic`: the `Dynamic` in a `dynamic_box`,
    /// or the `Dynamic` that `function` reads from the value of the host, with
    /// the closures that convert a string and box another value.
    DynamicFromJs {
        dynamic_box: TypeId,
        function: FunctionId,
        closures: Vec<GlobalId>,
    },
}

impl<'a> Generator<'a> {
    /// The function of the host `name` of the module `host_module`, used with
    /// type `type_`: its import, called with the arguments converted to
    /// JavaScript and whose result is converted back.
    pub(super) fn function_host_call(
        &mut self,
        host_module: &EcoString,
        name: &EcoString,
        type_: &Arc<Type>,
    ) -> FunctionId {
        let (params, return_) = type_.fn_types().expect("a function");
        let params: Vec<_> = params.iter().map(|param| self.represent(param)).collect();
        let return_ = self.represent(&return_);
        let to_js: Vec<_> = params
            .iter()
            .map(|param| self.conversion_call(param, Direction::ToJs))
            .collect();
        let from_js = self.conversion_call(&return_, Direction::FromJs);
        let param_types: Vec<_> = params.iter().map(|param| self.val_type(param)).collect();
        let return_type = self.val_type(&return_);
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

    /// The function that converts a value of type `type_` in `direction`,
    /// unless it crosses as it is.
    pub(super) fn conversion_call(
        &mut self,
        type_: &Arc<Type>,
        direction: Direction,
    ) -> Option<FunctionId> {
        if self.crosses_as_is(type_, direction) {
            None
        } else {
            Some(self.function_conversion(type_, direction))
        }
    }

    /// Whether a value of the host type `type_` crosses as it is.
    fn crosses_as_is(&self, type_: &Arc<Type>, direction: Direction) -> bool {
        matches!(
            self.custom_type(type_),
            Some((CustomType::External { .. }, _))
        ) || (self.external_val_type(type_) == Some(EXTERNREF))
            || (type_.is_string()
                && matches!(self.string, StringType::Js(_))
                && direction == Direction::ToJs)
    }

    /// `fn(T) -> JsValue` to JavaScript, `fn(JsValue) -> T` back.
    fn function_conversion(&mut self, type_: &Arc<Type>, direction: Direction) -> FunctionId {
        let val_type = self.val_type(type_);
        let key = TypeKey::new(type_);
        let builtin = match direction {
            Direction::ToJs => BuiltinFunction::ToJs(val_type, key),
            Direction::FromJs => BuiltinFunction::FromJs(val_type, key),
        };
        let type_ = type_.clone();
        self.get_function_builtin(builtin, |s| {
            if s.is_class_type(&type_) {
                s.code_class_conversion(&type_, direction)
            } else if type_.fn_types().is_some() {
                s.code_function_conversion(&type_, direction)
            } else {
                s.code_conversion(&type_, direction)
            }
        })
    }

    fn code_conversion(&mut self, type_: &Arc<Type>, direction: Direction) -> FunctionId {
        let conversion = self.conversion(type_, direction);
        let value_vt = self.val_type(type_);
        let (param_vt, result_vt, name) = match direction {
            Direction::ToJs => (value_vt, EXTERNREF, "_to_js"),
            Direction::FromJs => (EXTERNREF, value_vt, "_from_js"),
        };
        // params
        let value = self.wasm_local(param_vt);
        // locals
        let array = self.wasm_local(EXTERNREF);
        let other_value = self.wasm_local(ANYREF);
        let mut function = Function::new(self, name, &[value], &[result_vt]);
        let mut instructions = function.extend_instructions(self);
        if direction == Direction::FromJs
            && !matches!(
                conversion,
                Conversion::Same | Conversion::DynamicFromJs { .. }
            )
        {
            self.return_wasm_value(&mut instructions, value, type_);
        }
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
            Conversion::JsString(to_string) => instructions
                .local_get(value)
                .call(to_string)
                .ref_as_non_null(),
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
            Conversion::DynamicToJs {
                other,
                field,
                box_,
                function,
                closures,
            } => {
                let _ = instructions
                    .local_get(value)
                    .ref_test(false, HeapType::Concrete(other))
                    .if_(InstrSeqType::Simple(None), |then| {
                        let _ = then
                            .local_get(value)
                            .ref_cast_non_null(HeapType::Concrete(other))
                            .struct_get(other, field)
                            .local_tee(other_value)
                            .ref_test(false, HeapType::Concrete(box_))
                            .if_(InstrSeqType::Simple(None), |then| {
                                let _ = then
                                    .local_get(other_value)
                                    .ref_cast_non_null(HeapType::Concrete(box_))
                                    .struct_get(box_, 0)
                                    .return_();
                            });
                    })
                    .local_get(value);
                for closure in closures {
                    let _ = instructions.global_get(closure);
                }
                instructions.call(function)
            }
            Conversion::DynamicFromJs {
                dynamic_box,
                function,
                closures,
            } => {
                let _ = instructions
                    .local_get(value)
                    .any_convert_extern()
                    .local_tee(other_value)
                    .ref_test(false, HeapType::Concrete(dynamic_box))
                    .if_(InstrSeqType::Simple(None), |then| {
                        let _ = then
                            .local_get(other_value)
                            .ref_cast_non_null(HeapType::Concrete(dynamic_box))
                            .struct_get(dynamic_box, 0)
                            .return_();
                    })
                    .local_get(value);
                for closure in closures {
                    let _ = instructions.global_get(closure);
                }
                instructions.call(function)
            }
        };
        let _ = instructions.end();
        function.finish(self)
    }

    fn conversion(&mut self, type_: &Arc<Type>, direction: Direction) -> Conversion {
        if self.crosses_as_is(type_, direction) {
            return Conversion::Same;
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
                    "js_to_string",
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
        } else if let Some((DYNAMIC_MODULE, "Dynamic")) = type_
            .named_type_information()
            .as_ref()
            .map(|(module, name, _)| (module.as_str(), name.as_str()))
        {
            let dynamic_box = self.dynamic_self_box(type_);
            match direction {
                Direction::ToJs => self.dynamic_to_js(type_, &js_value, dynamic_box),
                Direction::FromJs => self.dynamic_from_js(type_, &js_value, dynamic_box),
            }
        } else if let Some(item) = type_.list_type() {
            self.conversion_with(type_, "js_list", &[], &[item], direction)
        } else if let Some((ok, error)) = type_.result_types() {
            self.conversion_with(type_, "js_result", &[], &[ok, error], direction)
        } else if let Some((DICT_MODULE, "Dict", [key, value])) = type_
            .named_type_information()
            .as_ref()
            .map(|(module, name, arguments)| (module.as_str(), name.as_str(), arguments.as_slice()))
        {
            let export = match direction {
                Direction::ToJs => "from",
                Direction::FromJs => "fold",
            };
            match self.js_dict_export(export) {
                Some(export) => self.conversion_with(
                    type_,
                    "js_dict",
                    &[export],
                    &[key.clone(), value.clone()],
                    direction,
                ),
                None => Conversion::Opaque(type_.clone()),
            }
        } else if let Some(elements) = type_.tuple_types() {
            let type_index = self.tuple_type_index(elements.clone());
            let fields = self.field_indices(&elements);
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

    /// Returns the wasm value of `type_` that the JavaScript value of the local
    /// `value` holds when it is a `Dynamic` that crossed to the host in its box
    /// (see `dynamic_self_box`) and holds a wasm value of `type_`, so a value
    /// cast to `Dynamic` crosses back as itself, like in the JavaScript
    /// target.
    fn return_wasm_value(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        value: LocalId,
        type_: &Arc<Type>,
    ) {
        // A variant without fields may be null.
        let (nullable, heap_type) = match (self.val_type(type_), self.dynamic_box(type_)) {
            (_, Some(box_)) => (false, HeapType::Concrete(box_)),
            (
                ValType::Ref(RefType {
                    nullable,
                    heap_type: heap_type @ HeapType::Concrete(_),
                }),
                None,
            ) => (nullable, heap_type),
            (_, None) => return,
        };
        let dynamic = self.runtime_named_type(DYNAMIC_MODULE, "Dynamic", vec![]);
        let self_box = self.dynamic_self_box(&dynamic);
        let (d_other, field) = self.dynamic_other(&dynamic);
        let other = self.wasm_local(ANYREF);
        let _ = instructions
            .local_get(value)
            .any_convert_extern()
            .local_tee(other)
            .ref_test(false, HeapType::Concrete(self_box))
            .if_(InstrSeqType::Simple(None), |then| {
                let _ = then
                    .local_get(other)
                    .ref_cast_non_null(HeapType::Concrete(self_box))
                    .struct_get(self_box, 0)
                    .local_tee(other)
                    .ref_test(false, HeapType::Concrete(d_other))
                    .if_(InstrSeqType::Simple(None), |then| {
                        let _ = then
                            .local_get(other)
                            .ref_cast_non_null(HeapType::Concrete(d_other))
                            .struct_get(d_other, field)
                            .local_tee(other)
                            .ref_test(nullable, heap_type)
                            .if_(InstrSeqType::Simple(None), |then| {
                                let _ = then.local_get(other);
                                self.unbox_instructions(then, type_);
                                let _ = then.return_();
                            });
                    });
            });
    }

    /// The struct of a `Dynamic` that crosses to the host as an opaque value.
    /// A wasm value is known by its structure only, and a union is the same
    /// type as any other union with the same shared fields, so a `Dynamic` is
    /// boxed to be told apart from the other values that the host may return
    /// as a `Dynamic`, like a value of a type variable. Its field is mutable,
    /// unlike the ones of the Gleam values, so the box has a type of its own.
    fn dynamic_self_box(&mut self, dynamic: &Arc<Type>) -> TypeId {
        let dynamic_vt = self.val_type(dynamic);
        let key = WasmType::struct_("DynamicBox".into(), vec![("value".into(), dynamic_vt)]);
        if let Some(id) = self.wasm_types.get(&key) {
            return *id;
        }
        let comp = walrus::CompositeType::Struct(walrus::StructType {
            fields: Box::new([FieldType {
                element_type: StorageType::Val(dynamic_vt),
                mutable: true,
            }]),
        });
        let id = self.composite_type_add(comp, true, None, None);
        self.wasm_module.types.get_mut(id).name = Some("DynamicBox".into());
        let _ = self.wasm_types.insert(key, id);
        id
    }

    /// The struct of the variant `DOther` of `dynamic` and the field of its
    /// value.
    fn dynamic_other(&mut self, dynamic: &Arc<Type>) -> (TypeId, u32) {
        let Some((CustomType::Union { custom_type }, args)) = self.custom_type(dynamic) else {
            panic!("Dynamic to be a union");
        };
        let (index, constructor) = custom_type
            .constructors
            .iter()
            .find_position(|constructor| constructor.name == "DOther")
            .expect("the variant DOther");
        let (_, other, _) = self.union_subtype_index(dynamic, &custom_type, constructor, &args);
        let field = self
            .union_layout(dynamic)
            .fields(index)
            .first()
            .copied()
            .flatten()
            .expect("the field value");
        (other, field)
    }

    /// `Conversion::DynamicToJs` for the type `dynamic`.
    fn dynamic_to_js(
        &mut self,
        dynamic: &Arc<Type>,
        js_value: &Arc<Type>,
        dynamic_box: TypeId,
    ) -> Conversion {
        let (other, field) = self.dynamic_other(dynamic);
        let box_ = self
            .dynamic_box(js_value)
            .expect("a boxed JavaScript value");
        let string_type = type_::fn_(vec![type_::string()], js_value.clone());
        let string = self.function_conversion(&type_::string(), Direction::ToJs);
        let item_type = type_::fn_(vec![dynamic.clone()], js_value.clone());
        let item = self.function_conversion(dynamic, Direction::ToJs);
        let dict = self.runtime_named_type(DICT_MODULE, "Dict", vec![dynamic.clone(); 2]);
        let dict_type = type_::fn_(vec![dict.clone()], js_value.clone());
        let dict = self.function_conversion(&dict, Direction::ToJs);
        let boxed = self.function_dynamic_self_box(dynamic, dynamic_box);
        let closures = vec![
            self.closure_of(string, &string_type),
            self.closure_of(item, &item_type),
            self.closure_of(dict, &dict_type),
            self.closure_of(boxed, &item_type),
        ];
        let function = self.runtime_function_id(
            JS_MODULE,
            "js_dynamic_with",
            vec![
                dynamic.clone(),
                string_type,
                item_type.clone(),
                dict_type,
                item_type,
            ],
            js_value.clone(),
        );
        Conversion::DynamicToJs {
            other,
            field,
            box_,
            function,
            closures,
        }
    }

    /// `fn(Dynamic) -> JsValue` that boxes a `Dynamic` in a `dynamic_box`.
    fn function_dynamic_self_box(
        &mut self,
        dynamic: &Arc<Type>,
        dynamic_box: TypeId,
    ) -> FunctionId {
        let dynamic_vt = self.val_type(dynamic);
        let value = self.wasm_local(dynamic_vt);
        let mut function = Function::new(self, "_dynamic_box", &[value], &[EXTERNREF]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions
            .local_get(value)
            .struct_new(dynamic_box)
            .extern_convert_any()
            .end();
        function.finish(self)
    }

    /// `Conversion::DynamicFromJs` for the type `dynamic`.
    fn dynamic_from_js(
        &mut self,
        dynamic: &Arc<Type>,
        js_value: &Arc<Type>,
        dynamic_box: TypeId,
    ) -> Conversion {
        let (closures, closure_types) = self.js_dynamic_from_closures(dynamic, js_value);
        let function = self.runtime_function_id(
            JS_MODULE,
            "js_dynamic_from_with",
            iter::once(js_value.clone()).chain(closure_types).collect(),
            dynamic.clone(),
        );
        Conversion::DynamicFromJs {
            dynamic_box,
            function,
            closures,
        }
    }

    /// The closures, and their types, that `js_dynamic_from_with` and
    /// `js_fields_with` take to read a value of the host as a `Dynamic`.
    fn js_dynamic_from_closures(
        &mut self,
        dynamic: &Arc<Type>,
        js_value: &Arc<Type>,
    ) -> (Vec<GlobalId>, Vec<Arc<Type>>) {
        let string_type = type_::fn_(vec![js_value.clone()], type_::string());
        let string = self.function_conversion(&type_::string(), Direction::FromJs);
        // A value of the host, which is compared, hashed and shown by the
        // host, see `type_hooks`.
        let other_type = type_::fn_(vec![js_value.clone()], dynamic.clone());
        let other = self.function_dynamic_cast(js_value);
        // The class of the dicts is `null` when the program has none.
        let dict_class = self.js_dict_export("default").unwrap_or_else(|| {
            let name: EcoString = "_no_dict_class".into();
            if let Some(id) = self.find_global(&name) {
                return id.global_id();
            }
            let null = ConstExpr::RefNull(RefType {
                nullable: true,
                heap_type: HeapType::Abstract(walrus::AbstractHeapType::Extern),
            });
            self.add_const(&name, EXTERNREF, null, false, false)
                .global_id()
        });
        let dict = self.runtime_named_type(DICT_MODULE, "Dict", vec![dynamic.clone(); 2]);
        let dict_type = type_::fn_(vec![js_value.clone()], dict.clone());
        let dict = self.function_conversion(&dict, Direction::FromJs);
        let closures = vec![
            self.closure_of(string, &string_type),
            dict_class,
            self.closure_of(dict, &dict_type),
            self.closure_of(other, &other_type),
        ];
        let types = vec![string_type, js_value.clone(), dict_type, other_type];
        (closures, types)
    }

    /// The hook `fields` of the table of a boxed value of the host type
    /// `type_`: `fn(AnyRef) -> Result(Dict(Dynamic, Dynamic), Nil)`, the
    /// properties of a plain object (see `js_fields_with`).
    pub(super) fn function_js_fields(
        &mut self,
        type_: &Arc<Type>,
        dynamic: &Arc<Type>,
    ) -> FunctionId {
        let fields_type = self.dynamic_fields_type(dynamic);
        let builtin = BuiltinFunction::JsFields(self.val_type(&fields_type), TypeKey::new(type_));
        let type_ = type_.clone();
        let dynamic = dynamic.clone();
        self.get_function_builtin(builtin, |s| s.code_js_fields(&type_, &dynamic))
    }

    fn code_js_fields(&mut self, type_: &Arc<Type>, dynamic: &Arc<Type>) -> FunctionId {
        let js_value = self.runtime_named_type(JS_MODULE, "JsValue", vec![]);
        let (closures, closure_types) = self.js_dynamic_from_closures(dynamic, &js_value);
        let fields_type = self.dynamic_fields_type(dynamic);
        let fields = self.runtime_function_id(
            JS_MODULE,
            "js_fields_with",
            iter::once(js_value).chain(closure_types).collect(),
            fields_type.clone(),
        );
        let fields_vt = self.val_type(&fields_type);
        // params
        let value = self.wasm_local(ANYREF);
        let mut function = Function::new(self, "_js_fields", &[value], &[fields_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions.local_get(value);
        self.unbox_instructions(&mut instructions, type_);
        for closure in closures {
            let _ = instructions.global_get(closure);
        }
        let _ = instructions.call(fields).end();
        function.finish(self)
    }

    /// Converts a value of `type_` with the function `name` of the runtime
    /// (`name_from` back), which takes the values of the host in the globals
    /// `values` and the closures that convert `items`.
    fn conversion_with(
        &mut self,
        type_: &Arc<Type>,
        name: &str,
        values: &[GlobalId],
        items: &[Arc<Type>],
        direction: Direction,
    ) -> Conversion {
        let js_value = self.runtime_named_type(JS_MODULE, "JsValue", vec![]);
        let value = type_.clone();
        let mut closures = values.to_vec();
        let mut closure_types = vec![js_value.clone(); values.len()];
        for item in items {
            let function = self.function_conversion(item, direction);
            let closure_type = match direction {
                Direction::ToJs => type_::fn_(vec![item.clone()], js_value.clone()),
                Direction::FromJs => type_::fn_(vec![js_value.clone()], item.clone()),
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

    /// The export `name` of the module `dict.mjs` of the stdlib, imported as a
    /// global, unless the program does not have the stdlib.
    fn js_dict_export(&mut self, name: &str) -> Option<GlobalId> {
        let _ = self.all_modules.get(STDLIB_DICT)?;
        let module = self.host_module_path(&"../dict.mjs".into(), &STDLIB_DICT.into());
        let key = ecow::eco_format!("{module}#{name}");
        if let Some(id) = self.find_global(&key) {
            return Some(id.global_id());
        }
        let (global, _) = self
            .wasm_module
            .add_import_global(&module, name, EXTERNREF, false, false);
        self.globals.borrow_mut().push(Id::global(key, global));
        Some(global)
    }
}

impl<'a> Generator<'a> {
    /// Whether a value of the host type `type_` is an instance of the classes
    /// of the variants of its custom type, as on JavaScript: the ones of the
    /// `.mjs` module of the custom types of its module, see
    /// `javascript::types_module`. The types of the runtime are opaque.
    fn is_class_type(&self, type_: &Arc<Type>) -> bool {
        let Some((module, _, _)) = type_.named_type_information() else {
            return false;
        };
        module != PRELUDE_MODULE_NAME
            && matches!(
                self.custom_type(type_),
                Some((
                    CustomType::Enum { .. } | CustomType::Struct { .. } | CustomType::Union { .. },
                    _
                ))
            )
            && !matches!(type_.as_ref(), Type::Named { package, .. } if package == RUNTIME_PACKAGE)
    }

    /// Converts a value of a custom type to an instance of the class of its
    /// variant with the function `Type$Variant` of the JavaScript target, and
    /// back with the functions `Type$isVariant`, which tells its variant, and
    /// `Type$Variant$i`, which reads its fields. A record of a type with a
    /// single variant is read without checking its class.
    fn code_class_conversion(&mut self, type_: &Arc<Type>, direction: Direction) -> FunctionId {
        let (custom_type, _) = self.custom_type(type_).expect("a custom type");
        let (module, name, _) = type_.named_type_information().expect("a named type");
        let value_vt = self.val_type(type_);
        let (param_vt, result_vt, function_name) = match direction {
            Direction::ToJs => (value_vt, EXTERNREF, "_to_js"),
            Direction::FromJs => (EXTERNREF, value_vt, "_from_js"),
        };
        let constructors = match &custom_type {
            CustomType::Enum { values } => values.clone(),
            CustomType::Struct { constructor, .. } => vec![constructor.name.clone()],
            CustomType::Union { custom_type } => custom_type
                .constructors
                .iter()
                .map(|constructor| constructor.name.clone())
                .collect(),
            CustomType::External { .. } | CustomType::Runtime { .. } | CustomType::Array => {
                panic!("a custom type with variants")
            }
        };
        // params
        let value = self.wasm_local(param_vt);
        // locals
        let tag = self.wasm_local(ValType::I32);
        let variants: Vec<_> = (0..constructors.len())
            .map(|index| self.function_variant_conversion(type_, index, direction))
            .collect();
        let fail = self.runtime_function_id(
            JS_MODULE,
            "js_unknown_variant",
            vec![self.runtime_named_type(JS_MODULE, "JsValue", vec![])],
            type_::nil(),
        );
        let checks: Vec<_> = constructors
            .iter()
            .map(|constructor| {
                self.class_function(
                    &module,
                    &format!("{name}$is{constructor}"),
                    &[EXTERNREF],
                    ValType::I32,
                )
            })
            .collect();
        let union = match &custom_type {
            CustomType::Union { custom_type } => Some((
                self.union_layout(type_),
                self.mono_union_supertype_index(type_, custom_type),
            )),
            CustomType::Enum { .. }
            | CustomType::Struct { .. }
            | CustomType::External { .. }
            | CustomType::Runtime { .. }
            | CustomType::Array => None,
        };
        let mut function = Function::new(self, function_name, &[value], &[result_vt]);
        let mut instructions = function.extend_instructions(self);
        match direction {
            Direction::ToJs => {
                // The index of the variant of the value.
                let _ = match (&custom_type, union) {
                    (CustomType::Enum { .. }, _) => instructions.local_get(value),
                    (_, Some((layout, supertype))) => {
                        let other = (0..constructors.len())
                            .find(|index| layout.null_tag != Some(*index))
                            .expect("a variant that is not null");
                        let read_tag = |instructions: &mut Instructions<'_, '_>| {
                            let _ = if layout.tagged {
                                instructions
                                    .local_get(value)
                                    .ref_as_non_null()
                                    .struct_get(supertype, 0)
                            } else {
                                // Only one variant is not null.
                                instructions.i32_const(other as i32)
                            };
                        };
                        match layout.null_tag {
                            Some(null_tag) => instructions.local_get(value).ref_is_null().if_else(
                                ValType::I32,
                                |then| {
                                    let _ = then.i32_const(null_tag as i32);
                                },
                                read_tag,
                            ),
                            None => {
                                read_tag(&mut instructions);
                                &mut instructions
                            }
                        }
                    }
                    _ => instructions.i32_const(0),
                }
                .local_set(tag);
                for (index, (variant_type, conversion)) in variants.into_iter().enumerate() {
                    let _ = instructions
                        .local_get(tag)
                        .i32_const(index as i32)
                        .i32_eq()
                        .if_(InstrSeqType::Simple(None), |then| {
                            let _ = then.local_get(value);
                            if let Some(type_index) = variant_type {
                                let _ = then.ref_cast_non_null(HeapType::Concrete(type_index));
                            }
                            let _ = then.return_call(conversion);
                        });
                }
                let _ = instructions.unreachable();
            }
            Direction::FromJs if variants.len() == 1 => {
                self.return_wasm_value(&mut instructions, value, type_);
                let (_, conversion) = variants.into_iter().next().expect("a variant");
                let _ = instructions.local_get(value).return_call(conversion);
            }
            Direction::FromJs => {
                self.return_wasm_value(&mut instructions, value, type_);
                for ((_, conversion), check) in variants.into_iter().zip(checks) {
                    let _ = instructions.local_get(value).call(check).if_(
                        InstrSeqType::Simple(None),
                        |then| {
                            let _ = then.local_get(value).return_call(conversion);
                        },
                    );
                }
                let _ = instructions
                    .local_get(value)
                    .call(fail)
                    .drop()
                    .unreachable();
            }
        }
        let _ = instructions.end();
        function.finish(self)
    }

    /// The function that converts a value of the variant at `index` of the
    /// custom type `type_` in `direction`, and the struct type of the
    /// variant to which a value of the custom type is cast before, when it
    /// is cast. Its fields are converted to call `Type$Variant`, or read with
    /// `Type$Variant$i` and converted back to create the value.
    fn function_variant_conversion(
        &mut self,
        type_: &Arc<Type>,
        index: usize,
        direction: Direction,
    ) -> (Option<TypeId>, FunctionId) {
        let (custom_type, args) = self.custom_type(type_).expect("a custom type");
        let (module, name, _) = type_.named_type_information().expect("a named type");
        // The struct of the variant, the fields of its values and their
        // types.
        let (variant_type, value_vt, fields, types, constructor) = match &custom_type {
            CustomType::Enum { values } => {
                let constructor = values.get(index).expect("an enum value").clone();
                (None, ValType::I32, vec![], vec![], constructor)
            }
            CustomType::Struct {
                custom_type,
                constructor,
            } => {
                let (type_index, types) =
                    self.mono_struct_type_index(type_, custom_type, constructor, &args);
                let fields = self.field_indices(&types);
                let types = Monomorphizer::variant_constructor(custom_type, constructor, &args);
                (
                    None,
                    self.val_type_ref(type_index),
                    fields
                        .into_iter()
                        .map(|field| (type_index, field))
                        .collect(),
                    types,
                    constructor.name.clone(),
                )
            }
            CustomType::Union { custom_type } => {
                let constructor = custom_type.constructors.get(index).expect("a variant");
                let layout = self.union_layout(type_);
                let types = Monomorphizer::variant_constructor(custom_type, constructor, &args);
                if layout.null_tag == Some(index) {
                    // Its fields are of unit types.
                    let supertype = self.mono_union_supertype_index(type_, custom_type);
                    let fields = types.iter().map(|_| (supertype, None)).collect();
                    (
                        None,
                        self.val_type(type_),
                        fields,
                        types,
                        constructor.name.clone(),
                    )
                } else {
                    let (_, type_index, _) =
                        self.union_subtype_index(type_, custom_type, constructor, &args);
                    let fields = layout
                        .fields(index)
                        .iter()
                        .map(|field| (type_index, *field))
                        .collect();
                    (
                        Some(type_index),
                        self.val_type_ref(type_index),
                        fields,
                        types,
                        constructor.name.clone(),
                    )
                }
            }
            CustomType::External { .. } | CustomType::Runtime { .. } | CustomType::Array => {
                panic!("a custom type with variants")
            }
        };
        // The fields of external types represented by the runtime are converted
        // as their representation, like the params of the function.
        let types: Vec<_> = types.iter().map(|type_| self.represent(type_)).collect();
        let js_vts: Vec<_> = types
            .iter()
            .map(|type_| self.js_val_type(type_, direction))
            .collect();
        match direction {
            Direction::ToJs => {
                let new = self.class_function(
                    &module,
                    &format!("{name}${constructor}"),
                    &js_vts,
                    EXTERNREF,
                );
                let builtin =
                    BuiltinFunction::ToJs(value_vt, TypeKey::variant(type_, &constructor));
                let has_fields = !types.is_empty();
                let code = move |s: &mut Self| {
                    let conversions: Vec<_> = types
                        .iter()
                        .map(|type_| s.conversion_call(type_, direction))
                        .collect();
                    // params
                    let value = s.wasm_local(value_vt);
                    let mut function = Function::new(s, "_to_js", &[value], &[EXTERNREF]);
                    let mut instructions = function.extend_instructions(s);
                    for ((type_index, field), conversion) in fields.into_iter().zip(conversions) {
                        let _ = instructions.local_get(value).field_get(type_index, field);
                        if let Some(conversion) = conversion {
                            let _ = instructions.call(conversion);
                        }
                    }
                    let _ = instructions.call(new).end();
                    function.finish(s)
                };
                // The fields are read only from the values that the program
                // constructs, see `get_function_constructor`.
                let function = if has_fields {
                    self.get_function_constructor(builtin, type_, index, code)
                } else {
                    self.get_function_builtin(builtin, code)
                };
                (variant_type, function)
            }
            Direction::FromJs => {
                let getters: Vec<_> = js_vts
                    .iter()
                    .enumerate()
                    .map(|(i, js_vt)| {
                        self.class_function(
                            &module,
                            &format!("{name}${constructor}${i}"),
                            &[EXTERNREF],
                            *js_vt,
                        )
                    })
                    .collect();
                let conversions: Vec<_> = types
                    .iter()
                    .map(|type_| self.conversion_call(type_, direction))
                    .collect();
                let new = match custom_type {
                    CustomType::Enum { .. } => None,
                    CustomType::Struct { .. }
                    | CustomType::Union { .. }
                    | CustomType::External { .. }
                    | CustomType::Runtime { .. }
                    | CustomType::Array => Some(self.variant_constructor(
                        types.clone(),
                        type_.clone(),
                        self.variant_expect(module.clone(), constructor),
                    )),
                };
                // params
                let value = self.wasm_local(EXTERNREF);
                let result_vt = self.val_type(type_);
                let mut function = Function::new(self, "_from_js", &[value], &[result_vt]);
                let mut instructions = function.extend_instructions(self);
                for (getter, conversion) in getters.into_iter().zip(conversions) {
                    let _ = instructions.local_get(value).call(getter);
                    if let Some(conversion) = conversion {
                        let _ = instructions.call(conversion);
                    }
                }
                let _ = match new {
                    Some(new) => instructions.call(new),
                    None => instructions.i32_const(index as i32),
                };
                let _ = instructions.end();
                (variant_type, function.finish(self))
            }
        }
    }

    /// The function `name` of the `.mjs` module of the custom types of the
    /// module `module`, with params `params` and result `result`.
    fn class_function(
        &mut self,
        module: &EcoString,
        name: &str,
        params: &[ValType],
        result: ValType,
    ) -> FunctionId {
        let file = module.rsplit('/').next().expect("a module name");
        let path = self.host_module_path(&ecow::eco_format!("./{file}.mjs"), module);
        self.host_import(&path, name, params, &[result])
    }

    /// The import of the function `name` of the host module `module`, with
    /// params `params` and results `results`.
    pub(super) fn host_import(
        &mut self,
        module: &str,
        name: &str,
        params: &[ValType],
        results: &[ValType],
    ) -> FunctionId {
        let import_name = ecow::eco_format!("{module}#{name}{params:?}{results:?}");
        if let Some(id) = self.find_global(&import_name) {
            return id.func_id();
        }
        let type_ = self.wasm_module.types.add(params, results);
        let (import, _) = self.wasm_module.add_import_func(module, name, type_);
        let _ = self.add_function_to_globals(import_name, import);
        import
    }
}

impl<'a> Generator<'a> {
    /// Converts a function to a JavaScript function that calls its
    /// trampoline (see `function_js_trampoline`), and a JavaScript function
    /// back to a closure that calls it, converting the arguments and the
    /// result of each call. The host keeps the pairs of closures and
    /// functions, so each one crosses as the same value every time.
    fn code_function_conversion(&mut self, type_: &Arc<Type>, direction: Direction) -> FunctionId {
        let closure_vt = self.val_type(type_);
        match direction {
            Direction::ToJs => {
                let trampoline = self.function_js_trampoline(type_);
                let function =
                    self.host_import(GLEAM_JS, "function", &[EXTERNREF, FUNCREF], &[EXTERNREF]);
                // params
                let value = self.wasm_local(closure_vt);
                let mut function_ = Function::new(self, "_to_js", &[value], &[EXTERNREF]);
                let _ = function_
                    .extend_instructions(self)
                    .local_get(value)
                    .extern_convert_any()
                    .ref_func(trampoline)
                    .call(function)
                    .end();
                function_.finish(self)
            }
            Direction::FromJs => {
                let (params, return_) = type_.fn_types().expect("a function type");
                let closure = self.closure_type(params, return_);
                let capture = js_function_capture(self);
                let code = self.function_js_call(type_);
                let closure_of =
                    self.host_import(GLEAM_JS, "closure_of", &[EXTERNREF], &[EXTERNREF]);
                let register = self.host_import(GLEAM_JS, "register", &[EXTERNREF, EXTERNREF], &[]);
                // params
                let value = self.wasm_local(EXTERNREF);
                // locals
                let found = self.wasm_local(EXTERNREF);
                let new = self.wasm_local(closure_vt);
                let mut function = Function::new(self, "_from_js", &[value], &[closure_vt]);
                let mut instructions = function.extend_instructions(self);
                self.return_wasm_value(&mut instructions, value, type_);
                // A function of a closure is converted back to the closure,
                // and a closure that calls a function is registered, so the
                // function crosses back as the same function.
                let _ = instructions
                    .local_get(value)
                    .call(closure_of)
                    .local_tee(found)
                    .ref_is_null()
                    .if_else(
                        closure_vt,
                        |then| {
                            self.closure_new(then, &closure, code, &[capture], [value]);
                            let _ = then
                                .local_tee(new)
                                .extern_convert_any()
                                .local_get(value)
                                .call(register)
                                .local_get(new);
                        },
                        |else_| {
                            let _ = else_
                                .local_get(found)
                                .any_convert_extern()
                                .ref_cast_non_null(HeapType::Concrete(closure.type_index));
                        },
                    )
                    .end();
                function.finish(self)
            }
        }
    }

    /// The function that a JavaScript function calls with the closure of the
    /// function type `type_` and its arguments: it calls the closure with the
    /// arguments converted back, and converts its result to JavaScript.
    fn function_js_trampoline(&mut self, type_: &Arc<Type>) -> FunctionId {
        let (params, return_) = type_.fn_types().expect("a function type");
        let js_params: Vec<_> = iter::once(EXTERNREF)
            .chain(
                params
                    .iter()
                    .map(|param| self.js_val_type(param, Direction::FromJs)),
            )
            .collect();
        let js_result = self.js_val_type(&return_, Direction::ToJs);
        let builtin =
            BuiltinFunction::JsTrampoline(js_params.clone(), js_result, TypeKey::new(type_));
        self.get_function_builtin(builtin, move |s| {
            let closure = s.closure_type(params.clone(), return_.clone());
            let from_js: Vec<_> = params
                .iter()
                .map(|param| s.conversion_call(param, Direction::FromJs))
                .collect();
            let to_js = s.conversion_call(&return_, Direction::ToJs);
            // params
            let js_params: Vec<_> = js_params.iter().map(|vt| s.wasm_local(*vt)).collect();
            // locals
            let closure_local = s.wasm_local(s.val_type_ref(closure.type_index));
            let mut function = Function::new(s, "_js_trampoline", &js_params, &[js_result]);
            let mut instructions = function.extend_instructions(s);
            let (value, arguments) = js_params.split_first().expect("the closure");
            let _ = instructions
                .local_get(*value)
                .any_convert_extern()
                .ref_cast_non_null(HeapType::Concrete(closure.type_index))
                .local_tee(closure_local);
            for (argument, from_js) in arguments.iter().zip(from_js) {
                let _ = instructions.local_get(*argument);
                if let Some(from_js) = from_js {
                    let _ = instructions.call(from_js);
                }
            }
            let _ = instructions
                .local_get(closure_local)
                .struct_get(closure.type_index, 0)
                .call_ref(closure.code_type_index);
            if let Some(to_js) = to_js {
                let _ = instructions.call(to_js);
            }
            let _ = instructions.end();
            function.finish(s)
        })
    }

    /// The code of the closures of the function type `type_` that call the
    /// JavaScript function of their environment, with the arguments converted
    /// to JavaScript, and convert its result back.
    fn function_js_call(&mut self, type_: &Arc<Type>) -> FunctionId {
        let (params, return_) = type_.fn_types().expect("a function type");
        let closure = self.closure_type(params.clone(), return_.clone());
        let param_vts: Vec<_> = iter::once(self.val_type_ref(closure.type_index))
            .chain(params.iter().map(|param| self.val_type(param)))
            .collect();
        let result_vt = self.val_type(&return_);
        let builtin = BuiltinFunction::JsCall(param_vts.clone(), result_vt, TypeKey::new(type_));
        self.get_function_builtin(builtin, move |s| {
            let to_js: Vec<_> = params
                .iter()
                .map(|param| s.conversion_call(param, Direction::ToJs))
                .collect();
            let from_js = s.conversion_call(&return_, Direction::FromJs);
            let js_params: Vec<_> = iter::once(EXTERNREF)
                .chain(
                    params
                        .iter()
                        .map(|param| s.js_val_type(param, Direction::ToJs)),
                )
                .collect();
            let js_result = s.js_val_type(&return_, Direction::FromJs);
            let call = s.host_import(GLEAM_JS, "call", &js_params, &[js_result]);
            let capture = js_function_capture(s);
            // params
            let params: Vec<_> = param_vts.iter().map(|vt| s.wasm_local(*vt)).collect();
            let mut function = Function::new(s, "_js_call", &params, &[result_vt]);
            let mut instructions = function.extend_instructions(s);
            let (closure_param, arguments) = params.split_first().expect("the closure");
            s.closure_environment_values(&mut instructions, &closure, *closure_param, &[capture]);
            for (argument, to_js) in arguments.iter().zip(to_js) {
                let _ = instructions.local_get(*argument);
                if let Some(to_js) = to_js {
                    let _ = instructions.call(to_js);
                }
            }
            let _ = instructions.call(call);
            if let Some(from_js) = from_js {
                let _ = instructions.call(from_js);
            }
            let _ = instructions.end();
            function.finish(s)
        })
    }

    /// The wasm type of a value of the host type `type_` in JavaScript: the
    /// one of `type_` when it crosses as it is, an `externref` otherwise.
    pub(super) fn js_val_type(&mut self, type_: &Arc<Type>, direction: Direction) -> ValType {
        if self.crosses_as_is(type_, direction) {
            self.val_type(type_)
        } else {
            EXTERNREF
        }
    }
}

/// The capture of the JavaScript function of the closures that call it.
fn js_function_capture(generator: &Generator<'_>) -> Capture {
    Capture {
        name: "function".into(),
        type_: generator.runtime_named_type(JS_MODULE, "JsValue", vec![]),
        definition: SrcSpan::default(),
        is_value: true,
    }
}

/// The module of the functions of the host that create and call JavaScript
/// values, see `runtime/js.gleam`.
pub(super) const GLEAM_JS: &str = "gleam:js";

/// The representation of a JavaScript function that calls a wasm function.
const FUNCREF: ValType = ValType::Ref(RefType {
    nullable: true,
    heap_type: HeapType::Abstract(walrus::AbstractHeapType::Func),
});

impl<'a> Generator<'a> {
    /// `_bytes_to_memory(words, start, size, address)`: writes `size` bytes
    /// of `words` from `start` at `address` of the linear memory.
    pub(super) fn code_bytes_to_memory(&mut self) -> FunctionId {
        // params
        let words = self.wasm_local(self.words.val_type());
        let start_int = self.wasm_local(self.int.val_type());
        let size_int = self.wasm_local(self.int.val_type());
        let dest = self.wasm_local(ValType::I32);
        // locals
        let start = self.wasm_local(ValType::I32);
        let size = self.wasm_local(ValType::I32);
        let needed = self.wasm_local(ValType::I32);
        let mut function = Function::new(
            self,
            "_bytes_to_memory",
            &[words, start_int, size_int, dest],
            &[BOOL_VALTYPE],
        );
        let mut instructions = function.extend_instructions(self);
        #[rustfmt::skip]
        let _ = instructions
            .local_get(start_int).int_to_i32().local_set(start)
            .local_get(size_int).int_to_i32().local_set(size)
            .local_get(dest)
            .local_get(size)
            .i32_add()
            .ensure_memory(needed);
        self.words_unpack(
            &mut instructions,
            words,
            start,
            size,
            |instructions, k, byte| {
                let _ = instructions
                    .local_get(dest)
                    .local_get(k)
                    .i32_add()
                    .local_get(byte)
                    .i32_store8(MemArg {
                        offset: 0,
                        align: 0,
                    });
            },
        );
        // Nil
        let _ = instructions.i32_const(0).end();
        function.finish(self)
    }

    /// `_bytes_from_memory(address, size)`: words with the `size` bytes at
    /// `address` of the linear memory.
    pub(super) fn code_bytes_from_memory(&mut self) -> FunctionId {
        // params
        let src = self.wasm_local(ValType::I32);
        let size = self.wasm_local(self.int.val_type());
        // locals
        let words = self.wasm_local(self.words.val_type());
        let len = self.wasm_local(ValType::I32);
        let at = self.wasm_local(ValType::I32);
        let mut function = Function::new(
            self,
            "_bytes_from_memory",
            &[src, size],
            &[self.words.val_type()],
        );
        let mut instructions = function.extend_instructions(self);
        let _ = instructions
            .local_get(size)
            .int_to_i32()
            .local_tee(len)
            .words_new()
            .local_set(words)
            .i32_const(0)
            .local_set(at);
        self.words_pack(&mut instructions, words, at, len, |instructions, k| {
            let _ = instructions
                .local_get(src)
                .local_get(k)
                .i32_add()
                .i32_load8_u(MemArg {
                    offset: 0,
                    align: 0,
                });
        });
        let _ = instructions.local_get(words).end();
        function.finish(self)
    }
}
