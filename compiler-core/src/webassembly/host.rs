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
//! variable of the declaration is not converted: the host only keeps it,
//! like `fn id(a: a) -> a`. A value of another type, like a type of the
//! runtime, is also kept as it is, for now.
//!
//! The values of the external types cross as they are, like a JavaScript
//! string when `string = "JSString"`.

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
    /// A JavaScript string, with the function that converts a value to one.
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
            (
                Type::Fn {
                    arguments: declared,
                    return_: declared_return,
                },
                Type::Fn { arguments, return_ },
            ) if declared.len() == arguments.len() => Arc::new(Type::Fn {
                arguments: declared
                    .iter()
                    .zip(arguments)
                    .map(|(declared, argument)| self.host_type(declared, argument))
                    .collect(),
                return_: self.host_type(declared_return, return_),
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
        } else if direction == Direction::FromJs
            && let Some((DYNAMIC_MODULE, "Dynamic")) = type_
                .named_type_information()
                .as_ref()
                .map(|(module, name, _)| (module.as_str(), name.as_str()))
        {
            // A value of the host, which is compared, hashed and shown by the
            // host, see `type_hooks`.
            Conversion::Call(self.function_dynamic_cast(&js_value))
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

impl<'a> Generator<'a> {
    /// Whether a value of the host type `type_` is an instance of the classes
    /// of the variants of its custom type, as on JavaScript: the ones of the
    /// `.mjs` module of the custom types of its module, see
    /// `javascript::types_module`. The types of the runtime are opaque.
    fn is_class_type(&self, type_: &Arc<Type>) -> bool {
        let Some((module, _, _)) = type_.named_type_information() else {
            return false;
        };
        opaque(type_).is_none()
            && module != PRELUDE_MODULE_NAME
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
        let value_type = unmarked(type_);
        let (custom_type, _) = self.custom_type(&value_type).expect("a custom type");
        let (module, name, _) = type_.named_type_information().expect("a named type");
        let value_vt = self.val_type(&value_type);
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
                self.union_layout(&value_type),
                self.mono_union_supertype_index(&value_type, custom_type),
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
                let (_, conversion) = variants.into_iter().next().expect("a variant");
                let _ = instructions.local_get(value).return_call(conversion);
            }
            Direction::FromJs => {
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
        let value_type = unmarked(type_);
        let (custom_type, args) = self.custom_type(&value_type).expect("a custom type");
        let (module, name, marked_args) = type_.named_type_information().expect("a named type");
        // The struct of the variant, the fields of its values and their
        // types, marked as in `type_`.
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
                    self.mono_struct_type_index(&value_type, custom_type, constructor, &args);
                let fields = self.field_indices(&types);
                let types =
                    Monomorphizer::variant_constructor(custom_type, constructor, &marked_args);
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
                let layout = self.union_layout(&value_type);
                let types =
                    Monomorphizer::variant_constructor(custom_type, constructor, &marked_args);
                if layout.null_tag == Some(index) {
                    // Its fields are of unit types.
                    let supertype = self.mono_union_supertype_index(&value_type, custom_type);
                    let fields = types.iter().map(|_| (supertype, None)).collect();
                    (
                        None,
                        self.val_type(&value_type),
                        fields,
                        types,
                        constructor.name.clone(),
                    )
                } else {
                    let (_, type_index, _) =
                        self.union_subtype_index(&value_type, custom_type, constructor, &args);
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
        let variant_name = format!("{}.{constructor}", qualified_type_name(type_));
        match direction {
            Direction::ToJs => {
                let new = self.class_function(
                    &module,
                    &format!("{name}${constructor}"),
                    &js_vts,
                    EXTERNREF,
                );
                let builtin = BuiltinFunction::ToJs(value_vt, variant_name.into());
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
                    self.get_function_constructor(builtin, &value_type, index, code)
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
                        types.iter().map(unmarked).collect(),
                        value_type.clone(),
                        self.variant_expect(module.clone(), constructor),
                    )),
                };
                // params
                let value = self.wasm_local(EXTERNREF);
                let result_vt = self.val_type(&value_type);
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
    fn host_import(
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
        let value_type = unmarked(type_);
        let closure_vt = self.val_type(&value_type);
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
                let (params, return_) = value_type.fn_types().expect("a function type");
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
            BuiltinFunction::JsTrampoline(js_params.clone(), js_result, qualified_type_name(type_));
        let type_ = type_.clone();
        self.get_function_builtin(builtin, move |s| {
            let value_type = unmarked(&type_);
            let (value_params, value_return) = value_type.fn_types().expect("a function type");
            let closure = s.closure_type(value_params, value_return);
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
        let value_type = unmarked(type_);
        let (params, return_) = type_.fn_types().expect("a function type");
        let (value_params, value_return) = value_type.fn_types().expect("a function type");
        let closure = self.closure_type(value_params.clone(), value_return.clone());
        let param_vts: Vec<_> = iter::once(self.val_type_ref(closure.type_index))
            .chain(value_params.iter().map(|param| self.val_type(param)))
            .collect();
        let result_vt = self.val_type(&value_return);
        let builtin =
            BuiltinFunction::JsCall(param_vts.clone(), result_vt, qualified_type_name(type_));
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
    fn js_val_type(&mut self, type_: &Arc<Type>, direction: Direction) -> ValType {
        if self.crosses_as_is(type_, direction) {
            self.val_type(&unmarked(type_))
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
const GLEAM_JS: &str = "gleam:js";

/// The representation of a JavaScript function that calls a wasm function.
const FUNCREF: ValType = ValType::Ref(RefType {
    nullable: true,
    heap_type: HeapType::Abstract(walrus::AbstractHeapType::Func),
});

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
        Type::Fn { arguments, return_ } => Arc::new(Type::Fn {
            arguments: arguments.iter().map(unmarked).collect(),
            return_: unmarked(return_),
        }),
        Type::Var { .. } => type_.clone(),
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
