// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Specialization of the functions with function parameters by the code of
//! their arguments.
//!
//! A function like `list.fold` is generated once for each type, and calling
//! its function parameter is a `call_ref` through the closure. When the code
//! of the argument is known where the function is called, the function is
//! also specialized for that code, and calling the parameter is a direct
//! `call`, which `wasm-opt` can inline:
//!
//! ```gleam
//! list.fold(xs, 0, int.add)               // fold with f = int.add
//! list.fold(xs, 0, fn(acc, x) { acc + k }) // fold with f = the literal code
//! ```
//!
//! The argument is still the same closure value, so equality, storing and
//! returning it behave as without the specialization. The known arguments
//! are kept by the location of the parameter definition, and not in its type,
//! because unification can make two parameters share a type.

use super::*;
use crate::ast::{ArgNames, CallArg};

/// The most specializations of a function for a type. The next ones use the
/// function specialized only by the type.
const MAX_SPECIALIZATIONS: usize = 32;

/// A function argument whose code is known where the function is called.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum Known {
    /// A function, called directly, without the closure.
    Function(FunctionId),
    /// A record constructor, called directly, but not as a tail call.
    Constructor(FunctionId),
    /// The code of a closure, called directly with the closure as its first
    /// argument.
    Code(FunctionId),
}

#[derive(PartialEq, Eq, Hash)]
struct SpecializationKey {
    base_name: EcoString,
    type_name: EcoString,
    knowns: Vec<Option<Known>>,
}

#[derive(Default)]
pub(super) struct Specializations {
    /// The known function arguments of the function being generated, by the
    /// location where the parameter, or a variable bound to it, is defined.
    pub(super) known_arguments: HashMap<SrcSpan, Known>,
    /// The base names of the specializations being generated.
    generating: Vec<EcoString>,
    /// The name of each specialization, registered before its generation, so
    /// a recursive call finds its placeholder.
    names: HashMap<SpecializationKey, EcoString>,
    /// The number of specializations of each function and type.
    counts: HashMap<(EcoString, EcoString), usize>,
    /// Which parameters of each function are specialized, by base name.
    parameters: HashMap<EcoString, Rc<[bool]>>,
}

/// A function with a body that a call names.
pub(super) struct Callee<'a> {
    module: EcoString,
    function: &'a TypedFunction,
    base_name: EcoString,
}

impl<'a> Generator<'a> {
    /// The specialization of the function that `fun` names for the known
    /// function arguments of `arguments`, if any is known.
    pub(super) fn specialized_callee(
        &mut self,
        scope: &Scope,
        fun: &TypedExpr,
        arguments: &[CallArg<TypedExpr>],
    ) -> Option<FunctionId> {
        let callee = self.callee_definition(fun)?;
        let parameters = self.specialized_parameters(&callee);
        if !parameters.iter().any(|parameter| *parameter) {
            return None;
        }
        // A new code passed to a function being specialized could make a new
        // specialization of it, which makes a new code, and so on. The
        // generation is depth first, so every such chain goes through this
        // stack.
        let recursive = self.specializations.generating.contains(&callee.base_name);
        let knowns = arguments
            .iter()
            .zip(parameters.iter())
            .map(|(argument, specialized)| {
                if *specialized {
                    self.known_argument(scope, &argument.value, recursive)
                } else {
                    None
                }
            })
            .collect_vec();
        if knowns.iter().all(Option::is_none) {
            return None;
        }
        let type_ = fun.type_();
        let key = SpecializationKey {
            base_name: callee.base_name.clone(),
            type_name: qualified_type_name(&type_),
            knowns,
        };
        if let Some(name) = self.specializations.names.get(&key) {
            return Some(self.find_global_expect(name).func_id());
        }
        let count = self
            .specializations
            .counts
            .entry((key.base_name.clone(), key.type_name.clone()))
            .or_default();
        if *count >= MAX_SPECIALIZATIONS {
            return None;
        }
        *count += 1;
        let specialization = self.allocate_specialization();
        let generic = is_generic_type(&function_type(callee.function));
        let base_name = if generic {
            mangle(&callee.base_name, &type_)
        } else {
            callee.base_name.clone()
        };
        let name: EcoString = format!("{base_name}${specialization}").into();
        let knowns = key.knowns.clone();
        let _ = self.specializations.names.insert(key, name.clone());
        let id = self.function_specialized(&callee, &type_, &knowns, name, specialization);
        Some(id.func_id())
    }

    /// The function with a body that `fun` names, if any.
    fn callee_definition(&self, fun: &TypedExpr) -> Option<Callee<'a>> {
        if let TypedExpr::Var { constructor, .. } = fun {
            match &constructor.variant {
                ValueConstructorVariant::ModuleFn { module, name, .. } => {
                    self.module_function(module, name)
                }
                ValueConstructorVariant::ModuleConstant { literal, .. } => {
                    self.constant_callee(literal)
                }
                ValueConstructorVariant::LocalVariable { .. }
                | ValueConstructorVariant::Record { .. } => None,
            }
        } else if let TypedExpr::ModuleSelect {
            constructor,
            module_name,
            label,
            ..
        } = fun
        {
            match constructor {
                ModuleValueConstructor::Fn { .. } => self.module_function(module_name, label),
                ModuleValueConstructor::Constant { literal, .. } => self.constant_callee(literal),
                ModuleValueConstructor::Record { .. } => None,
            }
        } else {
            None
        }
    }

    /// The function with a body that the constant `constant` names, if any.
    fn constant_callee(&self, constant: &TypedConstant) -> Option<Callee<'a>> {
        let Constant::Var {
            constructor: Some(constructor),
            ..
        } = constant
        else {
            return None;
        };
        match &constructor.variant {
            ValueConstructorVariant::ModuleFn { module, name, .. } => {
                self.module_function(module, name)
            }
            ValueConstructorVariant::ModuleConstant { literal, .. } => {
                self.constant_callee(literal)
            }
            ValueConstructorVariant::LocalVariable { .. }
            | ValueConstructorVariant::Record { .. } => None,
        }
    }

    /// The function `name` of the module `module`, if it has a body.
    fn module_function(&self, module: &EcoString, name: &EcoString) -> Option<Callee<'a>> {
        let imported = *module != self.module.name;
        let definitions = if imported {
            self.module_imported(module)
        } else {
            self.module
        };
        let function = definitions
            .definitions
            .functions
            .iter()
            .find(|function| function_name(function) == name)?;
        if function.external_webassembly.is_some() {
            return None;
        }
        // The same base names as `var` and `function_imported`
        let base_name = if imported {
            format!("{module}.{name}").into()
        } else {
            name.clone()
        };
        Some(Callee {
            module: module.clone(),
            function,
            base_name,
        })
    }

    /// Which parameters of `callee` are specialized: the ones declared with a
    /// function type that its body calls or passes to a call. A parameter
    /// that is only stored gains nothing from it.
    fn specialized_parameters(&mut self, callee: &Callee<'a>) -> Rc<[bool]> {
        if let Some(parameters) = self.specializations.parameters.get(&callee.base_name) {
            return parameters.clone();
        }
        let function = callee.function;
        let used = captures::called_or_passed(&function.body);
        let parameters: Rc<[bool]> = function
            .arguments
            .iter()
            .map(|argument| {
                argument.type_.fn_types().is_some()
                    && parameter_location(argument).is_some_and(|location| used.contains(&location))
            })
            .collect();
        let _ = self
            .specializations
            .parameters
            .insert(callee.base_name.clone(), parameters.clone());
        parameters
    }

    /// The known code of the function argument `argument`, if any. With
    /// `recursive`, a code created by the function being generated is not
    /// known, see `specialized_callee`.
    fn known_argument(
        &mut self,
        scope: &Scope,
        argument: &TypedExpr,
        recursive: bool,
    ) -> Option<Known> {
        let type_ = argument.type_();
        let _ = type_.fn_types()?;
        if let Some(location) = captures::local_variable_definition(argument) {
            if let Some(known) = self.specializations.known_arguments.get(&location) {
                return Some(*known);
            }
            let info = self.local_functions.get(&location)?.clone();
            if recursive {
                return None;
            }
            let id = self.function_local_generic(&info, &type_).func_id();
            let captures = self.local_function_captures(&info);
            return Some(if captures.is_empty() {
                Known::Function(id)
            } else {
                Known::Code(self.closure_adapter(id, &type_, &captures))
            });
        }
        if let TypedExpr::Fn {
            location,
            arguments,
            body,
            ..
        } = argument
        {
            if recursive {
                return None;
            }
            // The same code that the argument creates its closure with
            let name = self.function_literal_name(*location, &type_);
            let captures = self.function_literal_captures(*location, arguments, body);
            let (params, return_) = type_.fn_types().expect("function type");
            let closure = self.closure_type(params, return_);
            let id = self.function_local(name, &type_, arguments, body, Some(closure), &captures);
            return Some(Known::Code(id.func_id()));
        }
        // A function named by a variable or a module select
        let id = self.static_callee(scope, argument)?;
        Some(if argument.is_record_constructor_function() {
            Known::Constructor(id)
        } else {
            Known::Function(id)
        })
    }

    /// The known code of `value`, bound to a variable, if it is a known
    /// argument or a function. A local function is not, as it would not be
    /// checked for `recursive` in `known_argument`.
    pub(super) fn known_value(&mut self, scope: &Scope, value: &TypedExpr) -> Option<Known> {
        let _ = value.type_().fn_types()?;
        if let Some(location) = captures::local_variable_definition(value) {
            return self.specializations.known_arguments.get(&location).copied();
        }
        if let TypedExpr::Fn { .. } = value {
            // A local function, see `local_functions`
            None
        } else {
            self.known_argument(scope, value, false)
        }
    }

    /// The specialization `name` of `callee` used with type `type_` for the
    /// known function arguments `knowns`.
    fn function_specialized(
        &mut self,
        callee: &Callee<'a>,
        type_: &Arc<Type>,
        knowns: &[Option<Known>],
        name: EcoString,
        specialization: u32,
    ) -> Id {
        let function = callee.function;
        let mut mono = if is_generic_type(&function_type(function)) {
            Monomorphizer::with_bound(&function_type(function), type_)
        } else {
            Monomorphizer::new()
        };
        let variables = Variables::of(&function.arguments, &function.body);
        mono.bind_local_functions(&variables);
        let local_functions = collect_local_functions(&variables, specialization, &mono);
        let mut function = mono.function(function);
        set_function_name(&mut function, name.clone());
        let known_arguments = function
            .arguments
            .iter()
            .zip(knowns)
            .filter_map(|(argument, known)| Some((parameter_location(argument)?, (*known)?)))
            .collect();
        let module = std::mem::replace(&mut self.current_module_name, callee.module.clone());
        self.specializations
            .generating
            .push(callee.base_name.clone());
        let id = self._function(
            function,
            name,
            false,
            specialization,
            local_functions,
            known_arguments,
        );
        let _ = self.specializations.generating.pop();
        self.current_module_name = module;
        id
    }
}

/// Where the parameter `argument` is defined, which its references name.
fn parameter_location(argument: &TypedArg) -> Option<SrcSpan> {
    match &argument.names {
        ArgNames::Named { location, .. } => Some(*location),
        ArgNames::NamedLabelled { name_location, .. } => Some(*name_location),
        ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => None,
    }
}
