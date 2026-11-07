// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `_dynamic_cast(T)`: turns a value of type `T` into the `Dynamic` of the
//! runtime (`runtime/dynamic.gleam`), which the standard library declares as
//! `@external(webassembly, "builtins", "_dynamic_cast") fn cast(a: t) -> Dynamic`.
//!
//! Ints, floats, strings, booleans, `Nil`, codepoints, lists, tuples, arrays
//! and dicts become their variant of `Dynamic`, with their items cast too. A
//! `StringTree` is a string, as on JavaScript. Any other value is boxed in an
//! `anyref`, with a table of functions generated for its type that cast it
//! back to compare, hash, inspect and view it.

use super::*;

pub(super) const DYNAMIC_CAST: &str = "_dynamic_cast";

pub(super) const DYNAMIC_MODULE: &str = "builtins/dynamic";
pub(super) const DICT_MODULE: &str = "builtins/dict";
const STRING_TREE_MODULE: &str = "builtins/string_tree";
const ARRAY_MODULE: &str = "builtins/array";

/// The representation of `AnyRef`, a value of any type.
pub(super) const ANYREF: ValType = ValType::Ref(RefType {
    nullable: true,
    heap_type: HeapType::Abstract(walrus::AbstractHeapType::Any),
});

/// How `_dynamic_cast(T)` turns a value into a `Dynamic`.
enum Cast {
    /// `T` is `Dynamic`.
    Same,
    /// Calls these functions, one after the other.
    Calls(Vec<FunctionId>),
    /// Calls this function without the value, for `Nil`.
    Constant(FunctionId),
    /// Calls `function` with the value and the closures that cast the items.
    With {
        function: FunctionId,
        closures: Vec<GlobalId>,
    },
    /// Puts the cast elements of a tuple in an array of `Dynamic`. Each
    /// element has its field and its cast.
    Tuple {
        type_index: TypeId,
        casts: Vec<(Option<u32>, FunctionId)>,
        dynamics_index: TypeId,
        from_array: FunctionId,
    },
    /// Puts the cast elements of an `Array` in an array of `Dynamic`.
    Array {
        type_index: TypeId,
        cast: FunctionId,
        dynamics_index: TypeId,
        from_array: FunctionId,
    },
    /// Boxes the value with a table of functions for its type.
    Other {
        /// The struct that holds the value, when it is not a reference.
        boxed: Option<TypeId>,
        name: GlobalId,
        classify: GlobalId,
        equal: GlobalId,
        hash: GlobalId,
        inspect: GlobalId,
        fields: GlobalId,
        view: GlobalId,
        other: FunctionId,
    },
}

impl<'a> Generator<'a> {
    /// The `_dynamic_cast` external with the type `fn(T) -> Dynamic`.
    pub(super) fn function_dynamic_cast_external(&mut self, fn_type: &Arc<Type>) -> Id {
        let (params, _) = fn_type.fn_types().expect("cast must be a function type");
        let arg_type = params.into_iter().next().expect("cast takes one argument");
        let index = self.function_dynamic_cast(&arg_type);
        Id::func(DYNAMIC_CAST.into(), index)
    }

    /// `_dynamic_cast(T)`, `fn(T) -> Dynamic`.
    pub(super) fn function_dynamic_cast(&mut self, type_: &Arc<Type>) -> FunctionId {
        let type_ = self.represent(type_);
        let dynamic = self.runtime_named_type(DYNAMIC_MODULE, "Dynamic", vec![]);
        let builtin = BuiltinFunction::DynamicCast(
            self.val_type(&type_),
            self.val_type(&dynamic),
            TypeKey::new(&type_),
        );
        self.get_function_builtin(builtin, |s| s.code_dynamic_cast(&type_, &dynamic))
    }

    fn code_dynamic_cast(&mut self, type_: &Arc<Type>, dynamic: &Arc<Type>) -> FunctionId {
        let cast = self.dynamic_cast_plan(type_, dynamic);
        let value_vt = self.val_type(type_);
        let dynamic_vt = self.val_type(dynamic);
        // params
        let value = self.wasm_local(value_vt);
        // locals
        let dynamics_index = self.array_type_index(dynamic);
        let dynamics_vt = self.val_type_ref(dynamics_index);
        let dynamics = self.wasm_local(dynamics_vt);
        let size = self.wasm_local(ValType::I32);
        let i = self.wasm_local(ValType::I32);
        let mut function = Function::new(self, DYNAMIC_CAST, &[value], &[dynamic_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = match cast {
            Cast::Same => instructions.local_get(value),
            Cast::Calls(calls) => {
                let _ = instructions.local_get(value);
                for call in calls {
                    let _ = instructions.call(call);
                }
                &mut instructions
            }
            Cast::Constant(function) => instructions.call(function),
            Cast::With { function, closures } => {
                let _ = instructions.local_get(value);
                for closure in closures {
                    let _ = instructions.global_get(closure);
                }
                instructions.call(function)
            }
            Cast::Tuple {
                type_index,
                casts,
                dynamics_index,
                from_array,
            } => {
                let size = casts.len() as u32;
                for (field, cast) in casts {
                    let _ = instructions
                        .local_get(value)
                        .field_get(type_index, field)
                        .call(cast);
                }
                instructions
                    .array_new_fixed(dynamics_index, size)
                    .call(from_array)
            }
            Cast::Array {
                type_index,
                cast,
                dynamics_index,
                from_array,
            } => {
                // The array is filled with the first cast element, then the
                // others are set from the second.
                #[rustfmt::skip]
                let _ = instructions
                    .local_get(value)
                    .array_len()
                    .local_tee(size)
                    .i32_eqz()
                    .if_(InstrSeqType::Simple(None), |then| {
                        let _ = then
                            .array_new_fixed(dynamics_index, 0)
                            .call(from_array)
                            .return_();
                    })
                    .local_get(value)
                    .i32_const(0)
                    .array_get(type_index)
                    .call(cast)
                    .local_get(size)
                    .array_new(dynamics_index)
                    .local_set(dynamics)
                    .i32_const(1)
                    .local_set(i)
                    .block_(InstrSeqType::Simple(None), |block_s| {
                        let block_id = block_s.id();
                        let _ = block_s.loop_(InstrSeqType::Simple(None), |loop_s| {
                            let loop_id = loop_s.id();
                            let _ = loop_s
                                .local_get(i)
                                .local_get(size)
                                .i32_ge_u()
                                .br_if(block_id)
                                .local_get(dynamics)
                                .local_get(i)
                                .local_get(value)
                                .local_get(i)
                                .array_get(type_index)
                                .call(cast)
                                .array_set(dynamics_index)
                                .i32_inc(i)
                                .br(loop_id);
                        });
                    });
                instructions.local_get(dynamics).call(from_array)
            }
            Cast::Other {
                boxed,
                name,
                classify,
                equal,
                hash,
                inspect,
                fields,
                view,
                other,
            } => {
                let _ = instructions.local_get(value);
                if let Some(box_) = boxed {
                    let _ = instructions.struct_new(box_);
                }
                instructions
                    .global_as_non_null(name)
                    .global_as_non_null(classify)
                    .global_get(equal)
                    .global_get(hash)
                    .global_get(inspect)
                    .global_get(fields)
                    .global_get(view)
                    .call(other)
            }
        };
        let _ = instructions.end();
        function.finish(self)
    }

    fn dynamic_cast_plan(&mut self, type_: &Arc<Type>, dynamic: &Arc<Type>) -> Cast {
        let to_dynamic = |s: &mut Self, name: &str, argument: Arc<Type>| -> FunctionId {
            s.runtime_function_id(DYNAMIC_MODULE, name, vec![argument], dynamic.clone())
        };
        if type_.same_as(dynamic) {
            Cast::Same
        } else if type_.is_int() {
            Cast::Calls(vec![to_dynamic(self, "dynamic_int", type_.clone())])
        } else if type_.is_utf_codepoint() {
            Cast::Calls(vec![to_dynamic(
                self,
                "dynamic_utf_codepoint",
                type_.clone(),
            )])
        } else if type_.is_float() {
            Cast::Calls(vec![to_dynamic(self, "dynamic_float", type_.clone())])
        } else if type_.is_string() {
            Cast::Calls(vec![to_dynamic(self, "dynamic_string", type_.clone())])
        } else if type_.is_bool() {
            Cast::Calls(vec![to_dynamic(self, "dynamic_bool", type_.clone())])
        } else if type_.is_nil() {
            Cast::Constant(self.runtime_function_id(
                DYNAMIC_MODULE,
                "dynamic_nil",
                vec![],
                dynamic.clone(),
            ))
        } else if let Some((BIT_ARRAY_MODULE, "Bits")) = type_
            .named_type_information()
            .as_ref()
            .map(|(module, name, _)| (module.as_str(), name.as_str()))
        {
            Cast::Calls(vec![to_dynamic(self, "dynamic_bit_array", type_.clone())])
        } else if let Some(item) = type_.list_type() {
            if item.same_as(dynamic) {
                Cast::Calls(vec![to_dynamic(self, "dynamic_list", type_.clone())])
            } else {
                let closure = self.dynamic_cast_closure(&item, dynamic);
                let function = self.runtime_function_id(
                    DYNAMIC_MODULE,
                    "dynamic_list_with",
                    vec![type_.clone(), type_::fn_(vec![item], dynamic.clone())],
                    dynamic.clone(),
                );
                Cast::With {
                    function,
                    closures: vec![closure],
                }
            }
        } else if let Some(element) = self.array_element(type_) {
            Cast::Array {
                type_index: self.array_type_index(&element),
                cast: self.function_dynamic_cast(&element),
                dynamics_index: self.array_type_index(dynamic),
                from_array: self.function_dynamic_from_array(dynamic),
            }
        } else if let Some(elements) = type_.tuple_types() {
            let type_index = self.tuple_type_index(elements.clone());
            let casts = self
                .field_indices(&elements)
                .into_iter()
                .zip(&elements)
                .map(|(field, element)| (field, self.function_dynamic_cast(element)))
                .collect();
            Cast::Tuple {
                type_index,
                casts,
                dynamics_index: self.array_type_index(dynamic),
                from_array: self.function_dynamic_from_array(dynamic),
            }
        } else if let Some((DICT_MODULE, "Dict", [key, value])) = type_
            .named_type_information()
            .as_ref()
            .map(|(module, name, arguments)| (module.as_str(), name.as_str(), arguments.as_slice()))
        {
            if key.same_as(dynamic) && value.same_as(dynamic) {
                Cast::Calls(vec![to_dynamic(self, "dynamic_dict", type_.clone())])
            } else {
                let closures = vec![
                    self.dynamic_cast_closure(key, dynamic),
                    self.dynamic_cast_closure(value, dynamic),
                ];
                let function = self.runtime_function_id(
                    DYNAMIC_MODULE,
                    "dynamic_dict_with",
                    vec![
                        type_.clone(),
                        type_::fn_(vec![key.clone()], dynamic.clone()),
                        type_::fn_(vec![value.clone()], dynamic.clone()),
                    ],
                    dynamic.clone(),
                );
                Cast::With { function, closures }
            }
        } else if let Some((STRING_TREE_MODULE, "StringTree")) = type_
            .named_type_information()
            .as_ref()
            .map(|(module, name, _)| (module.as_str(), name.as_str()))
        {
            let to_string = self.runtime_function_id(
                STRING_TREE_MODULE,
                "string_tree_to_string",
                vec![type_.clone()],
                type_::string(),
            );
            Cast::Calls(vec![
                to_string,
                to_dynamic(self, "dynamic_string", type_::string()),
            ])
        } else {
            self.dynamic_cast_other(type_, dynamic)
        }
    }

    /// Boxes a value of another type, with its table of functions.
    fn dynamic_cast_other(&mut self, type_: &Arc<Type>, dynamic: &Arc<Type>) -> Cast {
        let boxed = self.dynamic_box(type_);
        let any_ref = self.runtime_named_type(DYNAMIC_MODULE, "AnyRef", vec![]);
        let name = self.string_index(&qualified_type_name(type_));
        let classify = self.string_index(&EcoString::from(if type_.fn_types().is_some() {
            "Function"
        } else if type_.is_result() {
            "Result"
        } else {
            "Object"
        }));
        let builtin = BuiltinFunction::DynamicEqual(TypeKey::new(type_));
        let equal = {
            let type_ = type_.clone();
            self.get_function_builtin(builtin, |s| s.code_dynamic_equal(&type_))
        };
        let builtin = BuiltinFunction::DynamicHash(self.int.val_type(), TypeKey::new(type_));
        let hash = {
            let type_ = type_.clone();
            self.get_function_builtin(builtin, |s| s.code_dynamic_hash(&type_))
        };
        let builtin = BuiltinFunction::DynamicInspect(self.string.val_type(), TypeKey::new(type_));
        let inspect = {
            let type_ = type_.clone();
            self.get_function_builtin(builtin, |s| s.code_dynamic_inspect(&type_))
        };
        let equal = self.closure_of(
            equal,
            &type_::fn_(vec![any_ref.clone(), any_ref.clone()], type_::bool()),
        );
        let hash = self.closure_of(hash, &type_::fn_(vec![any_ref.clone()], type_::int()));
        let inspect = self.closure_of(inspect, &type_::fn_(vec![any_ref.clone()], type_::string()));
        // Only a value of the host has fields, see `function_js_fields`.
        let fields_type = type_::fn_(vec![any_ref.clone()], self.dynamic_fields_type(dynamic));
        let fields = if self.external_val_type(type_) == Some(EXTERNREF) {
            self.function_js_fields(type_, dynamic)
        } else {
            self.runtime_function_id(
                DYNAMIC_MODULE,
                "dynamic_no_fields",
                vec![any_ref.clone()],
                self.dynamic_fields_type(dynamic),
            )
        };
        let fields = self.closure_of(fields, &fields_type);
        let reflection = self.runtime_named_type(DYNAMIC_MODULE, "Reflection", vec![]);
        let builtin = BuiltinFunction::DynamicView(self.val_type(&reflection), TypeKey::new(type_));
        let view = {
            let type_ = type_.clone();
            self.get_function_builtin(builtin, |s| s.code_dynamic_view(&type_))
        };
        let view_type = type_::fn_(vec![any_ref.clone()], reflection);
        let view = self.closure_of(view, &view_type);
        let other = self.runtime_function_id(
            DYNAMIC_MODULE,
            "dynamic_other",
            vec![
                any_ref.clone(),
                type_::string(),
                type_::string(),
                type_::fn_(vec![any_ref.clone(), any_ref.clone()], type_::bool()),
                type_::fn_(vec![any_ref.clone()], type_::int()),
                type_::fn_(vec![any_ref], type_::string()),
                fields_type,
                view_type,
            ],
            dynamic.clone(),
        );
        Cast::Other {
            boxed,
            name,
            classify,
            equal,
            hash,
            inspect,
            fields,
            view,
            other,
        }
    }

    /// `Result(Dict(Dynamic, Dynamic), Nil)`, the fields of a `Dynamic` that
    /// `decode.dict` and `decode.field` read.
    pub(super) fn dynamic_fields_type(&self, dynamic: &Arc<Type>) -> Arc<Type> {
        let dict = self.runtime_named_type(DICT_MODULE, "Dict", vec![dynamic.clone(); 2]);
        type_::result(dict, type_::nil())
    }

    /// The closure of `_dynamic_cast(T)`.
    fn dynamic_cast_closure(&mut self, type_: &Arc<Type>, dynamic: &Arc<Type>) -> GlobalId {
        let cast = self.function_dynamic_cast(type_);
        self.closure_of(cast, &type_::fn_(vec![type_.clone()], dynamic.clone()))
    }

    /// The struct that boxes the values of `type_` in an `anyref`, when they
    /// are not references to wasm values: numbers and values of the host.
    pub(super) fn dynamic_box(&mut self, type_: &Arc<Type>) -> Option<TypeId> {
        match self.val_type(type_) {
            val_type @ ValType::Ref(_) if val_type != EXTERNREF => None,
            val_type @ (ValType::Ref(_)
            | ValType::I32
            | ValType::I64
            | ValType::F32
            | ValType::F64
            | ValType::V128) => Some(self.struct_type_index(
                format!("Box({val_type})").into(),
                vec![("value".into(), val_type)],
            )),
        }
    }

    /// `[anyref] -> [T]`, the inverse of the boxing of `dynamic_cast_other`.
    pub(super) fn unbox_instructions(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        type_: &Arc<Type>,
    ) {
        let _ = match (self.val_type(type_), self.dynamic_box(type_)) {
            (_, Some(box_)) => instructions
                .ref_cast(false, HeapType::Concrete(box_))
                .struct_get(box_, 0),
            (
                ValType::Ref(RefType {
                    nullable,
                    heap_type,
                }),
                None,
            ) => instructions.ref_cast(nullable, heap_type),
            (val_type, None) => panic!("{val_type:?} to be a reference"),
        };
    }

    /// `(anyref, anyref) -> i32`: the equality of two boxed values of type `T`.
    fn code_dynamic_equal(&mut self, type_: &Arc<Type>) -> FunctionId {
        let eq = self.function_eq(type_);
        // params
        let a = self.wasm_local(ANYREF);
        let b = self.wasm_local(ANYREF);
        let mut function = Function::new(self, "_dynamic_equal", &[a, b], &[BOOL_VALTYPE]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions.local_get(a);
        self.unbox_instructions(&mut instructions, type_);
        let _ = instructions.local_get(b);
        self.unbox_instructions(&mut instructions, type_);
        let _ = instructions.eq(eq).end();
        function.finish(self)
    }

    /// `(anyref) -> Int`: the hash of a boxed value of type `T`.
    fn code_dynamic_hash(&mut self, type_: &Arc<Type>) -> FunctionId {
        let hash = self.function_hash(type_);
        let int_vt = self.int.val_type();
        // params
        let value = self.wasm_local(ANYREF);
        let mut function = Function::new(self, "_dynamic_hash", &[value], &[int_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions.local_get(value);
        self.unbox_instructions(&mut instructions, type_);
        self.hash_instructions(&mut instructions, hash);
        let _ = instructions.i32_to_int().end();
        function.finish(self)
    }

    /// `(anyref) -> String`: the text of a boxed value of type `T`.
    fn code_dynamic_inspect(&mut self, type_: &Arc<Type>) -> FunctionId {
        let inspect = self
            .function_inspect(&type_::fn_(vec![type_.clone()], type_::string()))
            .func_id();
        let string_vt = self.string.val_type();
        // params
        let value = self.wasm_local(ANYREF);
        let mut function = Function::new(self, "_dynamic_inspect", &[value], &[string_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions.local_get(value);
        self.unbox_instructions(&mut instructions, type_);
        let _ = instructions.call(inspect).end();
        function.finish(self)
    }

    /// `(anyref) -> Reflection`: one level of a boxed value of type `T`, see
    /// `dynamic_view` in the runtime.
    fn code_dynamic_view(&mut self, type_: &Arc<Type>) -> FunctionId {
        let reflection = self.runtime_named_type(DYNAMIC_MODULE, "Reflection", vec![]);
        let reflection_vt = self.val_type(&reflection);
        let value_vt = self.val_type(type_);
        // params
        let boxed = self.wasm_local(ANYREF);
        // locals
        let value = self.wasm_local(value_vt);
        let mut function = Function::new(self, "_dynamic_view", &[boxed], &[reflection_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions.local_get(boxed);
        self.unbox_instructions(&mut instructions, type_);
        let _ = instructions.local_set(value);
        let custom_type = self.custom_type(type_).filter(|(custom_type, _)| {
            matches!(
                custom_type,
                CustomType::Enum { .. } | CustomType::Struct { .. } | CustomType::Union { .. }
            )
        });
        if let Some((arguments, _)) = type_.fn_types() {
            let function = self.runtime_function_id(
                DYNAMIC_MODULE,
                "dynamic_function",
                vec![type_::int()],
                reflection.clone(),
            );
            let _ = instructions
                .int_const(&arguments.len().into())
                .call(function);
        } else if let Some((custom_type, arguments)) = custom_type {
            self.view_custom_type(&mut instructions, type_, value, &custom_type, &arguments);
        } else {
            let (module, name) = match type_.named_type_information() {
                Some((module, name, _)) => (module, name),
                None => (EcoString::new(), qualified_type_name(type_)),
            };
            let module = self.string_index(&module);
            let name = self.string_index(&name);
            let function = self.runtime_function_id(
                DYNAMIC_MODULE,
                "dynamic_external",
                vec![type_::string(), type_::string()],
                reflection.clone(),
            );
            let _ = instructions
                .global_as_non_null(module)
                .global_as_non_null(name)
                .call(function);
        }
        let _ = instructions.end();
        function.finish(self)
    }

    /// `[] -> [Reflection]`: the `view` of `value`, of the custom type
    /// `type_`, which calls the `view` of its constructor.
    fn view_custom_type(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        type_: &Arc<Type>,
        value: LocalId,
        custom_type: &CustomType,
        arguments: &[Arc<Type>],
    ) {
        let reflection = self.runtime_named_type(DYNAMIC_MODULE, "Reflection", vec![]);
        let reflection_vt = self.val_type(&reflection);
        match custom_type {
            CustomType::Enum { values } => {
                let _ = instructions.block_(InstrSeqType::Simple(Some(reflection_vt)), |block| {
                    let block_id = block.id();
                    for (index, constructor) in values.iter().enumerate() {
                        let _ = block.local_get(value).i32_const(index as i32).i32_eq().if_(
                            InstrSeqType::Simple(None),
                            |then_| {
                                self.view_constant(then_, type_, constructor, &[]);
                                let _ = then_.br(block_id);
                            },
                        );
                    }
                    let _ = block.unreachable();
                });
            }
            CustomType::Struct {
                custom_type,
                constructor,
            } => {
                let (type_index, types) =
                    self.mono_struct_type_index(type_, custom_type, constructor, arguments);
                let fields = self.field_indices(&types);
                let view = self.function_constructor_view(
                    type_,
                    type_index,
                    0,
                    constructor,
                    fields,
                    types,
                );
                let _ = instructions.local_get(value).call(view);
            }
            CustomType::Union { custom_type } => {
                let supertype_index = self.mono_union_supertype_index(type_, custom_type);
                let layout = self.union_layout(type_);
                let mut views = vec![];
                for (index, constructor) in custom_type.constructors.iter().enumerate() {
                    let (_, type_index, types) =
                        self.union_subtype_index(type_, custom_type, constructor, arguments);
                    if layout.null_tag == Some(index) {
                        let _ = instructions.local_get(value).ref_is_null().if_(
                            InstrSeqType::Simple(None),
                            |then_| {
                                self.view_constant(then_, type_, &constructor.name, &types);
                                let _ = then_.return_();
                            },
                        );
                    } else {
                        let fields = layout.fields(index).to_vec();
                        let view = self.function_constructor_view(
                            type_,
                            type_index,
                            index,
                            constructor,
                            fields,
                            types,
                        );
                        views.push((index, type_index, view));
                    }
                }
                if !layout.tagged {
                    let (_, type_index, view) = views
                        .into_iter()
                        .exactly_one()
                        .expect("one variant that is not null");
                    let _ = instructions
                        .local_get(value)
                        .ref_cast_non_null(HeapType::Concrete(type_index))
                        .call(view);
                    return;
                }
                let tag = self.wasm_local(ValType::I32);
                let _ = instructions
                    .local_get(value)
                    .ref_as_non_null()
                    .struct_get(supertype_index, 0)
                    .local_set(tag);
                for (index, type_index, view) in views {
                    let _ = instructions
                        .local_get(tag)
                        .i32_const(index as i32)
                        .i32_eq()
                        .if_(InstrSeqType::Simple(None), |then_| {
                            let _ = then_
                                .local_get(value)
                                .ref_cast_non_null(HeapType::Concrete(type_index))
                                .return_call(view);
                        });
                }
                let _ = instructions.unreachable();
            }
            CustomType::External { .. } | CustomType::Runtime { .. } | CustomType::Array => {
                panic!("a custom type with constructors")
            }
        }
    }

    /// `[] -> [Reflection]`: the `view` of a constructor whose fields all
    /// have unit types, whose only value is `0`.
    fn view_constant(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        type_: &Arc<Type>,
        constructor: &EcoString,
        types: &[Arc<Type>],
    ) {
        let casts = types
            .iter()
            .map(|type_| self.function_dynamic_cast(type_))
            .collect_vec();
        self.view_record_start(instructions, type_, constructor, &vec![None; types.len()]);
        for cast in &casts {
            let _ = instructions.i32_const(0).call(*cast);
        }
        self.view_record_end(instructions, type_, casts.len());
    }

    /// The `view` of the constructor at `index` of `type_`, whose values have
    /// the struct `type_index`, with its fields at `fields`.
    fn function_constructor_view(
        &mut self,
        type_: &Arc<Type>,
        type_index: TypeId,
        index: usize,
        constructor: &TypedRecordConstructor,
        fields: Vec<Option<u32>>,
        types: Vec<Arc<Type>>,
    ) -> FunctionId {
        let reflection = self.runtime_named_type(DYNAMIC_MODULE, "Reflection", vec![]);
        let builtin = BuiltinFunction::ConstructorView(
            self.val_type_ref(type_index),
            self.val_type(&reflection),
            TypeKey::variant(type_, &constructor.name),
        );
        let constructor = constructor.clone();
        let view_type = type_.clone();
        self.get_function_constructor(builtin, type_, index, move |s| {
            s.code_constructor_view(&view_type, type_index, &constructor, &fields, &types)
        })
    }

    fn code_constructor_view(
        &mut self,
        type_: &Arc<Type>,
        type_index: TypeId,
        constructor: &TypedRecordConstructor,
        fields: &[Option<u32>],
        types: &[Arc<Type>],
    ) -> FunctionId {
        let reflection = self.runtime_named_type(DYNAMIC_MODULE, "Reflection", vec![]);
        let reflection_vt = self.val_type(&reflection);
        let value_vt = self.val_type_ref(type_index);
        // params
        let value = self.wasm_local(value_vt);
        let mut function = Function::new(self, "_view", &[value], &[reflection_vt]);
        let mut instructions = function.extend_instructions(self);
        self.view_record_start(
            &mut instructions,
            type_,
            &constructor.name,
            &labels(constructor),
        );
        for (type_, field) in types.iter().zip(fields) {
            let cast = self.function_dynamic_cast(type_);
            let _ = instructions
                .local_get(value)
                .field_get(type_index, *field)
                .call(cast);
        }
        self.view_record_end(&mut instructions, type_, types.len());
        let _ = instructions.end();
        function.finish(self)
    }

    /// `[] -> [String, String, String, Array(String)]`: the module, the
    /// name, the constructor and the labels of a record, for
    /// `view_record_end`.
    fn view_record_start(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        type_: &Arc<Type>,
        constructor: &EcoString,
        labels: &[Option<EcoString>],
    ) {
        let (module, name, _) = type_
            .named_type_information()
            .expect("a custom type to have a name");
        let module = self.string_index(&module);
        let name = self.string_index(&name);
        let constructor = self.string_index(constructor);
        let labels = labels
            .iter()
            .map(|label| self.string_index(&label.clone().unwrap_or_default()))
            .collect_vec();
        let strings_index = self.array_type_index(&type_::string());
        let _ = instructions
            .global_as_non_null(module)
            .global_as_non_null(name)
            .global_as_non_null(constructor);
        for label in &labels {
            let _ = instructions.global_as_non_null(*label);
        }
        let _ = instructions.array_new_fixed(strings_index, labels.len() as u32);
    }

    /// `[String, String, String, Array(String), Dynamic...] -> [Reflection]`:
    /// the record with the `size` fields on the stack.
    fn view_record_end(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        type_: &Arc<Type>,
        size: usize,
    ) {
        let dynamic = self.runtime_named_type(DYNAMIC_MODULE, "Dynamic", vec![]);
        let reflection = self.runtime_named_type(DYNAMIC_MODULE, "Reflection", vec![]);
        let dynamics_index = self.array_type_index(&dynamic);
        let opaque = type_
            .named_type_information()
            .is_some_and(|(module, name, _)| self.opaque_types.contains(&(module, name)));
        let array =
            |element: Arc<Type>| self.runtime_named_type(ARRAY_MODULE, "Array", vec![element]);
        let arguments = vec![
            type_::string(),
            type_::string(),
            type_::string(),
            array(type_::string()),
            array(dynamic.clone()),
            type_::bool(),
        ];
        let record =
            self.runtime_function_id(DYNAMIC_MODULE, "dynamic_record", arguments, reflection);
        let _ = instructions
            .array_new_fixed(dynamics_index, size as u32)
            .i32_const(i32::from(opaque))
            .call(record);
    }

    /// `dynamic_from_array`, which makes a `Dynamic` of an array of `Dynamic`.
    pub(super) fn function_dynamic_from_array(&mut self, dynamic: &Arc<Type>) -> FunctionId {
        let array = self.runtime_named_type(ARRAY_MODULE, "Array", vec![dynamic.clone()]);
        self.runtime_function_id(
            DYNAMIC_MODULE,
            "dynamic_from_array",
            vec![array],
            dynamic.clone(),
        )
    }

    /// The type `name` of the runtime module `module`.
    pub(super) fn runtime_named_type(
        &self,
        module: &str,
        name: &str,
        arguments: Vec<Arc<Type>>,
    ) -> Arc<Type> {
        Arc::new(Type::Named {
            publicity: Publicity::Public,
            package: RUNTIME_PACKAGE.into(),
            module: module.into(),
            name: name.into(),
            arguments,
            inferred_variant: None,
        })
    }

    /// The function `name` of the runtime module `module`, with the type
    /// `fn(arguments) -> return_` (see `represent`).
    pub(super) fn runtime_function_id(
        &mut self,
        module: &str,
        name: &str,
        arguments: Vec<Arc<Type>>,
        return_: Arc<Type>,
    ) -> FunctionId {
        let type_ = self.represent(&type_::fn_(arguments, return_));
        self.function_imported(&module.into(), &name.into(), &type_)
            .func_id()
    }
}
