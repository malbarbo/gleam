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
//! The code of any expression is its identity, see `Generator::identity`:
//! each generated function records the identity of the value it returns,
//! so a call is known as well, and a case is known when all its clauses
//! have the same identity. A callee whose identity is known is also called
//! directly:
//!
//! ```gleam
//! list.sort(xs, order.reverse(int.compare)) // sort with the literal code of
//!                                           // reverse with int.compare
//! twice(g)(1)                               // the literal code of twice
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

/// The identity of a function value: the code that calling it runs, when a
/// single one reaches it. Without one, the value is boxed: calling it is a
/// `call_ref` through the closure.
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

impl Known {
    fn id(self) -> FunctionId {
        match self {
            Known::Function(id) | Known::Constructor(id) | Known::Code(id) => id,
        }
    }
}

/// How a call is emitted, see `Generator::call_target`.
pub(super) enum CallTarget {
    /// A function literal called directly.
    Literal,
    /// A callee whose identity is known.
    Known(Known),
    /// A function called directly: a specialization or a static callee.
    Direct(FunctionId),
    /// A local function, whose captures are the first arguments.
    Local(Rc<LocalFunction>, FunctionId),
    /// A `call_ref` through the closure.
    Dynamic,
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
    /// The base names of the specializations being generated, with the
    /// identities of their parameters.
    generating: Vec<(EcoString, Vec<Known>)>,
    /// The codes made by the functions: literals and local functions. An
    /// identity that is not one of them is a module function or constructor.
    created: HashSet<FunctionId>,
    /// The name of each specialization, registered before its generation, so
    /// a recursive call finds its placeholder.
    names: HashMap<SpecializationKey, EcoString>,
    /// The number of specializations of each function and type.
    counts: HashMap<(EcoString, EcoString), usize>,
    /// Which parameters of each function are specialized, by base name.
    parameters: HashMap<EcoString, Rc<[bool]>>,
    /// The identity of the value that each generated function returns. A
    /// function being generated has none yet.
    pub(super) returns: HashMap<FunctionId, Known>,
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
        let recursive = self
            .specializations
            .generating
            .iter()
            .any(|(name, _)| *name == callee.base_name);
        let knowns = arguments
            .iter()
            .zip(parameters.iter())
            .map(|(argument, specialized)| {
                if !*specialized {
                    return None;
                }
                let known = self.identity(scope, &argument.value)?;
                (!recursive || self.stable(&callee.base_name, known)).then_some(known)
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
        let mut used = captures::called_or_passed(&function.body);
        if function.return_type.fn_types().is_some() {
            returned_variables(&function.body, &mut used);
        }
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

    /// Whether `known` is stable for a specialization of the function
    /// `base_name` requested while it is being generated: a module function
    /// or constructor, or the identity of a parameter of a generation of it.
    /// Any other is new, and could make a new specialization, which makes a
    /// new code, and so on.
    fn stable(&self, base_name: &EcoString, known: Known) -> bool {
        if !self.specializations.created.contains(&known.id()) {
            return true;
        }
        self.specializations
            .generating
            .iter()
            .any(|(name, knowns)| name == base_name && knowns.contains(&known))
    }

    /// The identity of the value of `expr`, if a single one reaches it.
    pub(super) fn identity(&mut self, scope: &Scope, expr: &TypedExpr) -> Option<Known> {
        let type_ = expr.type_();
        let _ = type_.fn_types()?;
        match expr {
            TypedExpr::Var { .. } | TypedExpr::ModuleSelect { .. } => {
                if let Some(location) = captures::local_variable_definition(expr) {
                    if let Some(known) = self.specializations.known_arguments.get(&location) {
                        return Some(*known);
                    }
                    let info = self.local_functions.get(&location)?.clone();
                    return Some(self.local_function_identity(&info, &type_));
                }
                let id = self.static_callee(scope, expr)?;
                Some(if expr.is_record_constructor_function() {
                    Known::Constructor(id)
                } else {
                    Known::Function(id)
                })
            }
            TypedExpr::Fn {
                location,
                arguments,
                body,
                ..
            } => {
                // The same code that the literal creates its closure with. It
                // is generated now, but the literal is generated next with the
                // same state: its identity is only asked where it is a value.
                let name = self.function_literal_name(*location, &type_);
                let captures = self.function_literal_captures(*location, arguments, body);
                let (params, return_) = type_.fn_types().expect("function type");
                let closure = self.closure_type(params, return_);
                let id = self
                    .function_local(name, &type_, arguments, body, Some(closure), &captures)
                    .func_id();
                let _ = self.specializations.created.insert(id);
                Some(Known::Code(id))
            }
            TypedExpr::Call { fun, arguments, .. } => {
                let id = match self.call_target(scope, fun, arguments) {
                    CallTarget::Known(known) => known.id(),
                    CallTarget::Direct(id) | CallTarget::Local(_, id) => id,
                    CallTarget::Literal | CallTarget::Dynamic => return None,
                };
                let known = self.specializations.returns.get(&id).copied()?;
                self.has_signature(known, &type_).then_some(known)
            }
            TypedExpr::Block { statements, .. } => self.statements_identity(scope, statements),
            TypedExpr::Case { clauses, .. } => {
                // A clause that does not return gives no value to join
                let knowns = clauses
                    .iter()
                    .filter(|clause| {
                        !matches!(
                            clause.then,
                            TypedExpr::Panic { .. } | TypedExpr::Todo { .. }
                        )
                    })
                    .map(|clause| self.identity(scope, &clause.then))
                    .collect_vec();
                let first = (*knowns.first()?)?;
                knowns
                    .iter()
                    .all(|known| *known == Some(first))
                    .then_some(first)
            }
            TypedExpr::Pipeline {
                first_value,
                assignments,
                finally,
                ..
            } => {
                self.bind(scope, first_value.value.location(), &first_value.value);
                for (assignment, _) in assignments {
                    self.bind(scope, assignment.value.location(), &assignment.value);
                }
                self.identity(scope, finally)
            }
            TypedExpr::Echo {
                expression: Some(expression),
                ..
            } => self.identity(scope, expression),
            // Boxed
            TypedExpr::Int { .. }
            | TypedExpr::Float { .. }
            | TypedExpr::String { .. }
            | TypedExpr::List { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::BitArray { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::NegateInt { .. }
            | TypedExpr::Invalid { .. } => None,
        }
    }

    /// Whether the code of `known` has the wasm signature that calling a value
    /// of type `type_` requires.
    fn has_signature(&mut self, known: Known, type_: &Arc<Type>) -> bool {
        let (params, return_) = type_.fn_types().expect("function type");
        let closure = match known {
            Known::Code(_) => {
                let closure = self.closure_type(params.clone(), return_.clone());
                Some(self.val_type_ref(closure.type_index))
            }
            Known::Function(_) | Known::Constructor(_) => None,
        };
        let expected_params = closure
            .into_iter()
            .chain(self.val_types(params))
            .collect_vec();
        let expected_results = self.val_types(iter::once(return_));
        let ty = self
            .wasm_module
            .types
            .get(self.wasm_module.funcs.get(known.id()).ty());
        ty.params() == expected_params && ty.results() == expected_results
    }

    /// The identity of the value of `statements`, binding their variables.
    pub(super) fn statements_identity(
        &mut self,
        scope: &Scope,
        statements: &[TypedStatement],
    ) -> Option<Known> {
        let (last, rest) = statements.split_last()?;
        for statement in rest {
            if let Statement::Assignment(assignment) = statement
                && let Pattern::Variable { location, .. } = &assignment.pattern
                && !matches!(assignment.kind, AssignmentKind::Assert { .. })
                && !self.local_functions.contains_key(location)
            {
                self.bind(scope, *location, &assignment.value);
            }
        }
        match last {
            Statement::Expression(expr) => self.identity(scope, expr),
            Statement::Assignment(assignment) => {
                if let Some(info) = self.local_function_definition(assignment).cloned() {
                    let type_ = assignment.value.type_();
                    return Some(self.local_function_identity(&info, &type_));
                }
                self.identity(scope, &assignment.value)
            }
            Statement::Use(_) | Statement::Assert(_) => None,
        }
    }

    /// The identity of the local function `info` used with type `type_`.
    fn local_function_identity(&mut self, info: &LocalFunction, type_: &Arc<Type>) -> Known {
        let id = self.function_local_generic(info, type_).func_id();
        let captures = self.local_function_captures(info);
        let known = if captures.is_empty() {
            Known::Function(id)
        } else {
            Known::Code(self.closure_adapter(id, type_, &captures))
        };
        let _ = self.specializations.created.insert(known.id());
        known
    }

    /// Binds the variable defined at `location` to the identity of `value`.
    /// A variable can be bound more than once, as the body of a clause with
    /// alternative patterns is generated for each, so a boxed value removes
    /// the identity of a previous binding.
    pub(super) fn bind(&mut self, scope: &Scope, location: SrcSpan, value: &TypedExpr) {
        let known = self.identity(scope, value);
        let known_arguments = &mut self.specializations.known_arguments;
        let _ = match known {
            Some(known) => known_arguments.insert(location, known),
            None => known_arguments.remove(&location),
        };
    }

    /// How the call of `fun` with `arguments` is emitted.
    pub(super) fn call_target(
        &mut self,
        scope: &Scope,
        fun: &TypedExpr,
        arguments: &[CallArg<TypedExpr>],
    ) -> CallTarget {
        if let TypedExpr::Fn { .. } = fun {
            return CallTarget::Literal;
        }
        if let Some(id) = self.specialized_callee(scope, fun, arguments) {
            return CallTarget::Direct(id);
        }
        if let Some(info) = self.local_function_of(fun).cloned() {
            let id = self.function_local_generic(&info, &fun.type_()).func_id();
            return CallTarget::Local(info, id);
        }
        if let Some(id) = self.static_callee(scope, fun) {
            return CallTarget::Direct(id);
        }
        match self.identity(scope, fun) {
            Some(known) => CallTarget::Known(known),
            None => CallTarget::Dynamic,
        }
    }

    /// Records the identity of the value that the function `id`, whose body
    /// is `body` and whose return type is `return_`, returns. It must be
    /// called before the state of the body is restored.
    pub(super) fn record_return(
        &mut self,
        scope: &Scope,
        id: FunctionId,
        return_: &Arc<Type>,
        body: &[TypedStatement],
    ) {
        if return_.fn_types().is_none() {
            return;
        }
        if let Some(known) = self.statements_identity(scope, body) {
            let _ = self.specializations.returns.insert(id, known);
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
        self.specializations.generating.push((
            callee.base_name.clone(),
            knowns.iter().flatten().copied().collect(),
        ));
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

/// Adds to `definitions` the definitions of the local variables that
/// `statements` returns.
fn returned_variables(statements: &[TypedStatement], definitions: &mut HashSet<SrcSpan>) {
    match statements.last() {
        Some(Statement::Expression(expr)) => returned_variable(expr, definitions),
        Some(Statement::Assignment(assignment)) => {
            returned_variable(&assignment.value, definitions);
        }
        Some(Statement::Use(_) | Statement::Assert(_)) | None => {}
    }
}

fn returned_variable(expr: &TypedExpr, definitions: &mut HashSet<SrcSpan>) {
    match expr {
        TypedExpr::Block { statements, .. } => returned_variables(statements, definitions),
        TypedExpr::Case { clauses, .. } => {
            for clause in clauses {
                returned_variable(&clause.then, definitions);
            }
        }
        TypedExpr::Pipeline { finally, .. } => returned_variable(finally, definitions),
        TypedExpr::Echo {
            expression: Some(expression),
            ..
        } => returned_variable(expression, definitions),
        TypedExpr::Var { .. } => definitions.extend(captures::local_variable_definition(expr)),
        TypedExpr::Int { .. }
        | TypedExpr::Float { .. }
        | TypedExpr::String { .. }
        | TypedExpr::Fn { .. }
        | TypedExpr::List { .. }
        | TypedExpr::Call { .. }
        | TypedExpr::BinOp { .. }
        | TypedExpr::RecordAccess { .. }
        | TypedExpr::PositionalAccess { .. }
        | TypedExpr::ModuleSelect { .. }
        | TypedExpr::Tuple { .. }
        | TypedExpr::TupleIndex { .. }
        | TypedExpr::Todo { .. }
        | TypedExpr::Panic { .. }
        | TypedExpr::Echo { .. }
        | TypedExpr::BitArray { .. }
        | TypedExpr::RecordUpdate { .. }
        | TypedExpr::NegateBool { .. }
        | TypedExpr::NegateInt { .. }
        | TypedExpr::Invalid { .. } => {}
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
