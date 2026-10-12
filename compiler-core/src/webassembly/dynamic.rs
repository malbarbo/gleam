// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `_dynamic_cast(T)`: turns a value of type `T` into the `Dynamic` of the
//! runtime (`runtime/dynamic.gleam`), which the standard library declares as
//! `@external(webassembly, "builtins", "_dynamic_cast") fn cast(a: t) -> Dynamic`.
//!
//! Ints, floats, strings, booleans, `Nil`, lists, tuples and dicts become
//! their variant of `Dynamic`, with their items cast too. A `StringTree` is
//! a string and a `UtfCodepoint` an int, as on JavaScript. Any other value is
//! boxed in an `anyref`, with a table of functions generated for its type
//! that cast it back to compare, hash and inspect it.

use super::*;

pub(super) const DYNAMIC_CAST: &str = "_dynamic_cast";

pub(super) const DYNAMIC_MODULE: &str = "builtins/dynamic";
const DICT_MODULE: &str = "builtins/dict";
const STRING_TREE_MODULE: &str = "builtins/string_tree";

/// The representation of `AnyRef`, a value of any type.
pub(super) const ANYREF: ValType = ValType::Ref(RefType {
    nullable: true,
    heap_type: HeapType::Abstract(walrus::AbstractHeapType::Any),
});

/// How `_dynamic_cast(T)` turns a value into a `Dynamic`.
enum Cast {
    /// `T` is `Dynamic`.
    Same,
    /// Calls these functions, one after the other, after converting the
    /// value from `i32` to `Int` when `from_i32`.
    Calls {
        from_i32: bool,
        calls: Vec<FunctionId>,
    },
    /// Calls this function without the value, for `Nil`.
    Constant(FunctionId),
    /// Calls `function` with the value and the closures that cast the items.
    With {
        function: FunctionId,
        closures: Vec<GlobalId>,
    },
    /// Prepends the cast elements to an empty list, from the last.
    Tuple {
        type_index: TypeId,
        casts: Vec<FunctionId>,
        empty: FunctionId,
        prepend: FunctionId,
        array: FunctionId,
    },
    /// Boxes the value with a table of functions for its type.
    Other {
        boxed: bool,
        name: GlobalId,
        classify: GlobalId,
        equal: GlobalId,
        hash: GlobalId,
        inspect: GlobalId,
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
            qualified_type_name(&type_),
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
        let list_vt = self.val_type(&type_::list(dynamic.clone()));
        let list = self.wasm_local(list_vt);
        let mut function = Function::new(self, DYNAMIC_CAST, &[value], &[dynamic_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = match cast {
            Cast::Same => instructions.local_get(value),
            Cast::Calls { from_i32, calls } => {
                let _ = instructions.local_get(value);
                if from_i32 {
                    let _ = instructions.i32_to_int();
                }
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
                empty,
                prepend,
                array,
            } => {
                let _ = instructions.call(empty).local_set(list);
                for (index, cast) in casts.into_iter().enumerate().rev() {
                    let _ = instructions
                        .local_get(list)
                        .local_get(value)
                        .struct_get(type_index, index as u32)
                        .call(cast)
                        .call(prepend)
                        .local_set(list);
                }
                instructions.local_get(list).call(array)
            }
            Cast::Other {
                boxed,
                name,
                classify,
                equal,
                hash,
                inspect,
                other,
            } => {
                let _ = instructions.local_get(value);
                if boxed {
                    let _ = instructions.ref_i31();
                }
                instructions
                    .global_as_non_null(name)
                    .global_as_non_null(classify)
                    .global_get(equal)
                    .global_get(hash)
                    .global_get(inspect)
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
            Cast::Calls {
                from_i32: false,
                calls: vec![to_dynamic(self, "dynamic_int", type_.clone())],
            }
        } else if type_.is_utf_codepoint() {
            Cast::Calls {
                from_i32: true,
                calls: vec![to_dynamic(self, "dynamic_int", type_::int())],
            }
        } else if type_.is_float() {
            Cast::Calls {
                from_i32: false,
                calls: vec![to_dynamic(self, "dynamic_float", type_.clone())],
            }
        } else if type_.is_string() {
            Cast::Calls {
                from_i32: false,
                calls: vec![to_dynamic(self, "dynamic_string", type_.clone())],
            }
        } else if type_.is_bool() {
            Cast::Calls {
                from_i32: false,
                calls: vec![to_dynamic(self, "dynamic_bool", type_.clone())],
            }
        } else if type_.is_nil() {
            Cast::Constant(self.runtime_function_id(
                DYNAMIC_MODULE,
                "dynamic_nil",
                vec![],
                dynamic.clone(),
            ))
        } else if let Some(item) = type_.list_type() {
            if item.same_as(dynamic) {
                Cast::Calls {
                    from_i32: false,
                    calls: vec![to_dynamic(self, "dynamic_list", type_.clone())],
                }
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
        } else if let Some(elements) = type_.tuple_types() {
            let type_index = self.tuple_type_index(elements.clone());
            let casts = elements
                .iter()
                .map(|element| self.function_dynamic_cast(element))
                .collect();
            let dynamics = type_::list(dynamic.clone());
            let empty =
                self.runtime_function_id(DYNAMIC_MODULE, "dynamic_empty", vec![], dynamics.clone());
            let prepend = self.runtime_function_id(
                DYNAMIC_MODULE,
                "dynamic_prepend",
                vec![dynamics.clone(), dynamic.clone()],
                dynamics.clone(),
            );
            let array = to_dynamic(self, "dynamic_array", dynamics);
            Cast::Tuple {
                type_index,
                casts,
                empty,
                prepend,
                array,
            }
        } else if let Some((DICT_MODULE, "Dict", [key, value])) = type_
            .named_type_information()
            .as_ref()
            .map(|(module, name, arguments)| (module.as_str(), name.as_str(), arguments.as_slice()))
        {
            if key.same_as(dynamic) && value.same_as(dynamic) {
                Cast::Calls {
                    from_i32: false,
                    calls: vec![to_dynamic(self, "dynamic_dict", type_.clone())],
                }
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
            Cast::Calls {
                from_i32: false,
                calls: vec![
                    to_string,
                    to_dynamic(self, "dynamic_string", type_::string()),
                ],
            }
        } else {
            self.dynamic_cast_other(type_, dynamic)
        }
    }

    /// Boxes a value of another type, with its table of functions.
    fn dynamic_cast_other(&mut self, type_: &Arc<Type>, dynamic: &Arc<Type>) -> Cast {
        let boxed = match self.val_type(type_) {
            ValType::Ref(_) => false,
            ValType::I32 => true,
            val_type @ (ValType::I64 | ValType::F32 | ValType::F64 | ValType::V128) => {
                panic!("values of type {val_type:?} can't be dynamic yet")
            }
        };
        let any_ref = self.runtime_named_type(DYNAMIC_MODULE, "AnyRef", vec![]);
        let name = self.string_index(&qualified_type_name(type_));
        let classify = self.string_index(&EcoString::from(if type_.fn_types().is_some() {
            "Function"
        } else if type_.is_result() {
            "Result"
        } else {
            "Object"
        }));
        let builtin = BuiltinFunction::DynamicEqual(qualified_type_name(type_));
        let equal = {
            let type_ = type_.clone();
            self.get_function_builtin(builtin, |s| s.code_dynamic_equal(&type_))
        };
        let builtin = BuiltinFunction::DynamicHash(self.int.val_type(), qualified_type_name(type_));
        let hash = {
            let type_ = type_.clone();
            self.get_function_builtin(builtin, |s| s.code_dynamic_hash(&type_))
        };
        let builtin =
            BuiltinFunction::DynamicInspect(self.string.val_type(), qualified_type_name(type_));
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
            other,
        }
    }

    /// The closure of `_dynamic_cast(T)`.
    fn dynamic_cast_closure(&mut self, type_: &Arc<Type>, dynamic: &Arc<Type>) -> GlobalId {
        let cast = self.function_dynamic_cast(type_);
        self.closure_of(cast, &type_::fn_(vec![type_.clone()], dynamic.clone()))
    }

    /// `[anyref] -> [T]`, the inverse of the boxing of `dynamic_cast_other`.
    fn unbox_instructions(&mut self, instructions: &mut Instructions<'_, '_>, type_: &Arc<Type>) {
        let _ = match self.val_type(type_) {
            ValType::Ref(RefType {
                nullable,
                heap_type,
            }) => instructions.ref_cast(nullable, heap_type),
            ValType::I32 => instructions
                .ref_cast(false, HeapType::Abstract(walrus::AbstractHeapType::I31))
                .i31_get_s(),
            val_type @ (ValType::I64 | ValType::F32 | ValType::F64 | ValType::V128) => {
                panic!("values of type {val_type:?} can't be dynamic yet")
            }
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
    /// `fn(arguments) -> return_`.
    fn runtime_function_id(
        &mut self,
        module: &str,
        name: &str,
        arguments: Vec<Arc<Type>>,
        return_: Arc<Type>,
    ) -> FunctionId {
        self.function_imported(
            &module.into(),
            &name.into(),
            &type_::fn_(arguments, return_),
        )
        .func_id()
    }
}
