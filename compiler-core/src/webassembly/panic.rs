// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! The errors of `panic`, `todo`, `let assert` and `assert`, which call
//! `_panic` with the error of the JavaScript target: its kind, file, module,
//! function, line, message, whether the program gave the message, and the
//! details of its kind, like the value that did not match. In a program that
//! runs in a JavaScript host, one that imports anything other than WASI,
//! `_panic` throws the `Error` of the JavaScript target, which the host can
//! catch, like on JavaScript. Otherwise it prints the error and exits,
//! without the details, which are only made when the program can run in a
//! JavaScript host: it uses JavaScript strings or a module declares a
//! function of the host.
//!
//! With `failure = "exception"`, `_panic` throws instead an exception of the
//! tag of a failure, whose values are its params, and `_panic_exit` does
//! what `_panic` does otherwise. `_catch` catches the exception, and `_start`
//! calls `_panic_exit` with an exception that nothing caught.

use super::dynamic::DYNAMIC_MODULE;
use super::glue::{ARENA_MARK, ARENA_RELEASE};
use super::host::{Direction, GLEAM_JS};
use super::*;

/// The failure of an error, with the details that the host makes the fields of the error of
/// the JavaScript target from. They are a `Dynamic` tuple:
///
/// - `let assert`: `#(value, start, end, pattern_start, pattern_end)`;
/// - `assert`: `#(start, end, expression_start, kind, ...)`, where the kind
///   `"binary_operator"` is followed by the operator and the left and right
///   expressions, `"function_call"` by a tuple of the arguments and
///   `"expression"` by the expression, each expression a tuple
///   `#(kind, value, start, end)`;
/// - `panic`, `todo` and an `Int` overflow have none, `Nil`.
pub(super) enum Failure<'e> {
    Panic,
    Todo,
    LetAssert {
        value: LocalId,
        type_: Arc<Type>,
        subject: &'e TypedExpr,
        pattern: SrcSpan,
    },
    Assert {
        value: &'e TypedExpr,
        kind: AssertKind<'e>,
    },
    /// An `Int` operation whose result does not fit, see `overflow`.
    IntOverflow,
}

impl Failure<'_> {
    /// The `gleam_error` of the JavaScript target.
    fn name(&self) -> &'static str {
        match self {
            Failure::Panic => "panic",
            Failure::Todo => "todo",
            Failure::LetAssert { .. } => "let_assert",
            Failure::Assert { .. } => "assert",
            Failure::IntOverflow => "int_overflow",
        }
    }

    /// The message of the JavaScript target, of an error without one.
    fn default_message(&self) -> &'static str {
        match self {
            Failure::Panic => "`panic` expression evaluated.",
            Failure::Todo => "`todo` expression evaluated. This code has not yet been implemented.",
            Failure::LetAssert { .. } => "Pattern match failed, no pattern matched the value.",
            Failure::Assert { .. } => "Assertion failed.",
            Failure::IntOverflow => "Int overflow.",
        }
    }
}

/// The asserted expression of an `assert`.
pub(super) enum AssertKind<'e> {
    BinaryOperator {
        operator: &'static str,
        left: Asserted<'e>,
        right: Asserted<'e>,
    },
    FunctionCall(Vec<Asserted<'e>>),
    Expression(Asserted<'e>),
}

/// An operand of an asserted expression, with its value.
pub(super) struct Asserted<'e> {
    pub(super) expression: &'e TypedExpr,
    pub(super) value: AssertedValue,
}

pub(super) enum AssertedValue {
    /// Stored in a local.
    Local(LocalId),
    /// A variable, read again.
    Var,
    Bool(bool),
    Unevaluated,
}

const PANIC: &str = "_panic";

const PANIC_EXIT: &str = "_panic_exit";

/// `_catch(run, ok, error)`, which calls `run`, and then `ok` with its
/// result, or `error` with the values of the failure that `run` raised.
pub(super) const CATCH: &str = "_catch";

/// Returns `true` if `function` has the shape of the type of `_catch`,
/// `false` otherwise.
pub(super) fn is_catch_signature(function: &TypedFunction) -> bool {
    let arities: Vec<_> = function
        .arguments
        .iter()
        .map(|argument| argument.type_.fn_types().map(|(params, _)| params.len()))
        .collect();
    arities == [Some(0), Some(1), Some(8)] && is_generic_type(&function.return_type)
}

/// The strings that `_panic` prints between its params.
const PRINTED: [&str; 4] = [" at ", ":", "\n  ", "\n"];

impl<'a> Generator<'a> {
    /// Calls `_panic` for `error` at `start`, with the message `message`, or
    /// the one of the JavaScript target when it has none.
    pub(super) fn panic(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        locals: &Locals,
        scope: Scope,
        error: Failure<'_>,
        start: u32,
        message: Option<&TypedExpr>,
    ) {
        let index = self.string_index(&error.name().into());
        let _ = instructions.global_as_non_null(index);
        self.push_failure_location(instructions, start);
        match message {
            Some(message) => {
                assert!(message.type_().is_string());
                let _ = instructions.expression(self, locals, scope.clone(), message);
            }
            None => {
                let index = self.string_index(&error.default_message().into());
                let _ = instructions.global_as_non_null(index);
            }
        }
        let _ = instructions.bool_const(message.is_some());
        self.panic_details(instructions, locals, &scope, start, error);
        let panic = self.function_panic();
        let _ = instructions.call(panic).unreachable();
    }

    /// Pushes the file, the module, the function and the line of a failure
    /// at `start`, the params of `_panic` after the kind.
    pub(super) fn push_failure_location(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        start: u32,
    ) {
        let (module, line) = self.source_location(start);
        let file = self.source_file(&module);
        let function = self.current_function_name.clone();
        for string in [file, module, function, line.to_string().into()] {
            let index = self.string_index(&string);
            let _ = instructions.global_as_non_null(index);
        }
    }

    /// Calls `_panic` for an `Int` overflow at `location`, the locals of the
    /// file, the module, the function and the line.
    pub(super) fn int_overflow_failure(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        location: &[LocalId],
    ) {
        let error = Failure::IntOverflow;
        let kind = self.string_index(&error.name().into());
        let _ = instructions.global_as_non_null(kind);
        for local in location {
            let _ = instructions.local_get(*local);
        }
        let message = self.string_index(&error.default_message().into());
        let _ = instructions
            .global_as_non_null(message)
            .bool_const(false)
            .call(self.dynamic_nil());
        let panic = self.function_panic();
        let _ = instructions.call(panic).unreachable();
    }

    /// Pushes the `Dynamic` of the details of `error`, at `start`.
    fn panic_details(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        locals: &Locals,
        scope: &Scope,
        start: u32,
        error: Failure<'_>,
    ) {
        match error {
            _ if !self.javascript_host => {
                let _ = instructions.call(self.dynamic_nil());
            }
            Failure::Panic | Failure::Todo | Failure::IntOverflow => {
                let _ = instructions.call(self.dynamic_nil());
            }
            Failure::LetAssert {
                value,
                type_,
                subject,
                pattern,
            } => {
                let _ = instructions.local_get(value);
                self.push_dynamic_of(instructions, &type_);
                for position in [start, subject.location().end, pattern.start, pattern.end] {
                    self.push_dynamic_int(instructions, position);
                }
                self.push_dynamic_tuple(instructions, 5);
            }
            Failure::Assert { value, kind } => {
                for position in [start, value.location().end, value.location().start] {
                    self.push_dynamic_int(instructions, position);
                }
                let size = match kind {
                    AssertKind::BinaryOperator {
                        operator,
                        left,
                        right,
                    } => {
                        self.push_dynamic_string(instructions, "binary_operator");
                        self.push_dynamic_string(instructions, operator);
                        self.push_asserted(instructions, locals, scope, left);
                        self.push_asserted(instructions, locals, scope, right);
                        7
                    }
                    AssertKind::FunctionCall(arguments) => {
                        self.push_dynamic_string(instructions, "function_call");
                        let size = arguments.len() as u32;
                        for argument in arguments {
                            self.push_asserted(instructions, locals, scope, argument);
                        }
                        self.push_dynamic_tuple(instructions, size);
                        5
                    }
                    AssertKind::Expression(expression) => {
                        self.push_dynamic_string(instructions, "expression");
                        self.push_asserted(instructions, locals, scope, expression);
                        5
                    }
                };
                self.push_dynamic_tuple(instructions, size);
            }
        }
    }

    /// Pushes the `Dynamic` tuple `#(kind, value, start, end)` of `asserted`.
    fn push_asserted(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        locals: &Locals,
        scope: &Scope,
        asserted: Asserted<'_>,
    ) {
        let Asserted { expression, value } = asserted;
        let kind = match value {
            AssertedValue::Unevaluated => "unevaluated",
            AssertedValue::Local(_) | AssertedValue::Var | AssertedValue::Bool(_)
                if expression.is_literal() =>
            {
                "literal"
            }
            AssertedValue::Local(_) | AssertedValue::Var | AssertedValue::Bool(_) => "expression",
        };
        self.push_dynamic_string(instructions, kind);
        match value {
            AssertedValue::Local(local) => {
                let _ = instructions.local_get(local);
                self.push_dynamic_of(instructions, &expression.type_());
            }
            AssertedValue::Var => {
                let _ = instructions.expression(self, locals, scope.clone(), expression);
                self.push_dynamic_of(instructions, &expression.type_());
            }
            AssertedValue::Bool(value) => {
                let _ = instructions.bool_const(value);
                self.push_dynamic_of(instructions, &type_::bool());
            }
            AssertedValue::Unevaluated => {
                let _ = instructions.call(self.dynamic_nil());
            }
        }
        let location = expression.location();
        self.push_dynamic_int(instructions, location.start);
        self.push_dynamic_int(instructions, location.end);
        self.push_dynamic_tuple(instructions, 4);
    }

    /// Makes the `Dynamic` of the value of type `type_` on the stack.
    fn push_dynamic_of(&mut self, instructions: &mut Instructions<'_, '_>, type_: &Arc<Type>) {
        let cast = self.function_dynamic_cast(type_);
        let _ = instructions.call(cast);
    }

    fn push_dynamic_int(&mut self, instructions: &mut Instructions<'_, '_>, value: u32) {
        let _ = instructions.int_const(&value.into());
        self.push_dynamic_of(instructions, &type_::int());
    }

    fn push_dynamic_string(&mut self, instructions: &mut Instructions<'_, '_>, value: &str) {
        let index = self.string_index(&value.into());
        let _ = instructions.global_as_non_null(index);
        self.push_dynamic_of(instructions, &type_::string());
    }

    /// Makes the `Dynamic` tuple of the `size` `Dynamic`s on the stack.
    fn push_dynamic_tuple(&mut self, instructions: &mut Instructions<'_, '_>, size: u32) {
        let dynamic = self.runtime_named_type(DYNAMIC_MODULE, "Dynamic", vec![]);
        let array = self.array_type_index(&dynamic);
        let from_array = self.function_dynamic_from_array(&dynamic);
        let _ = instructions.array_new_fixed(array, size).call(from_array);
    }

    pub(super) fn dynamic_nil(&mut self) -> FunctionId {
        let dynamic = self.runtime_named_type(DYNAMIC_MODULE, "Dynamic", vec![]);
        self.runtime_function_id(DYNAMIC_MODULE, "dynamic_nil", vec![], dynamic)
    }

    /// `_panic(kind, file, module, function, line, message, has_message,
    /// details)`, whose code is generated by `code_panic` once the other
    /// functions are, as it depends on their imports.
    fn function_panic(&mut self) -> FunctionId {
        if let Some(panic) = self.panic {
            return panic;
        }
        let params = self.panic_params();
        let panic = self.allocate_placeholder_function(PANIC, &params, &[]);
        if self.failure == WasmFailure::Exception {
            let _ = self.failure_tag();
            self.panic_exit = Some(self.allocate_placeholder_function(PANIC_EXIT, &params, &[]));
        }
        // The strings are initialized with the constants, which are
        // generated before the code of `_panic`.
        for string in PRINTED {
            let _ = self.string_index(&string.into());
        }
        self.panic = Some(panic);
        panic
    }

    /// The wasm types of the params of `_panic`, which are also the values
    /// of the tag of a failure.
    pub(super) fn panic_params(&mut self) -> Vec<ValType> {
        let string = self.val_type(&type_::string());
        let dynamic = self.runtime_named_type(DYNAMIC_MODULE, "Dynamic", vec![]);
        let dynamic = self.val_type(&dynamic);
        vec![
            string,
            string,
            string,
            string,
            string,
            string,
            BOOL_VALTYPE,
            dynamic,
        ]
    }

    /// The tag of the exception of a failure.
    pub(super) fn failure_tag(&mut self) -> walrus::TagId {
        if let Some(tag) = self.failure_tag {
            return tag;
        }
        let params = self.panic_params();
        let type_ = self.wasm_module.types.add(&params, &[]);
        let tag = self.wasm_module.tags.add(type_);
        self.failure_tag = Some(tag);
        tag
    }

    /// Generates the code of `_panic`, and of `_panic_exit`, when it is
    /// called.
    pub(super) fn code_panic(&mut self) {
        let Some(panic) = self.panic else {
            return;
        };
        let Some(panic_exit) = self.panic_exit else {
            let code = self.code_panic_exit(PANIC);
            replace_function_body(&mut self.wasm_module, panic, code);
            return;
        };
        let code = self.code_panic_exit(PANIC_EXIT);
        replace_function_body(&mut self.wasm_module, panic_exit, code);
        let tag = self.failure_tag();
        let params: Vec<_> = self
            .panic_params()
            .into_iter()
            .map(|val_type| self.wasm_local(val_type))
            .collect();
        let mut function = Function::new(self, PANIC, &params, &[]);
        let mut instructions = function.extend_instructions(self);
        for param in &params {
            let _ = instructions.local_get(*param);
        }
        let _ = instructions.throw(tag).end();
        let code = function.finish(self);
        replace_function_body(&mut self.wasm_module, panic, code);
    }

    /// The function `name` that prints the error of its params and exits,
    /// or throws the `Error` of the JavaScript target in a JavaScript host.
    fn code_panic_exit(&mut self, name: &str) -> FunctionId {
        let string_type = type_::string();
        let bool_type = type_::bool();
        let dynamic_type = self.runtime_named_type(DYNAMIC_MODULE, "Dynamic", vec![]);
        let types = [&string_type; 6]
            .into_iter()
            .chain([&bool_type, &dynamic_type]);
        // params
        let params: Vec<_> = types
            .clone()
            .map(|type_| {
                let val_type = self.val_type(type_);
                self.wasm_local(val_type)
            })
            .collect();
        let mut function = Function::new(self, name, &params, &[]);
        let mut instructions = function.extend_instructions(self);
        if self.javascript_host {
            let mut js_types = vec![];
            // The `Error` of the JavaScript target has the default message
            // when the program gave none.
            let has_message = 6;
            for (index, (param, type_)) in params.iter().zip(types).enumerate() {
                if index == has_message {
                    continue;
                }
                let _ = instructions.local_get(*param);
                if let Some(to_js) = self.conversion_call(type_, Direction::ToJs) {
                    let _ = instructions.call(to_js);
                }
                js_types.push(self.js_val_type(type_, Direction::ToJs));
            }
            let throw = self.host_import(GLEAM_JS, "panic", &js_types, &[]);
            let _ = instructions.call(throw);
        } else {
            let string_to_memory =
                self.get_function_builtin_external(BuiltinFunctionExternal::StringToMemory);
            let heap_base = self.find_global_expect(HEAP_BASE).func_id();
            let print = self.find_global_expect(PRINT).func_id();
            let exit = self.find_global_expect(EXIT).func_id();
            // kind at file:line
            //   message
            let [kind, file, _, _, line, message, _, _] = params[..] else {
                unreachable!("the params of `_panic`")
            };
            let [at, colon, indent, newline] =
                PRINTED.map(|string| self.string_index(&string.into()));
            let pieces = [
                Ok(kind),
                Err(at),
                Ok(file),
                Err(colon),
                Ok(line),
                Err(indent),
                Ok(message),
                Err(newline),
            ];
            for piece in pieces {
                let _ = instructions.i32_const(STDERR).call(heap_base);
                let _ = match piece {
                    Ok(param) => instructions.local_get(param),
                    Err(literal) => instructions.global_as_non_null(literal),
                };
                let _ = instructions
                    .call(heap_base)
                    .call(string_to_memory)
                    .call(print)
                    .drop();
            }
            let _ = instructions.i32_const(1).call(exit);
        }
        let _ = instructions.unreachable().end();
        function.finish(self)
    }

    /// `_catch` for its type `type_`, declared by `function` in `module`.
    pub(super) fn function_catch_external(
        &mut self,
        function: &TypedFunction,
        module: &EcoString,
        type_: &Arc<Type>,
    ) -> Id {
        if self.failure != WasmFailure::Exception && self.error.is_none() {
            self.error = Some(Error::CatchWithoutException {
                module: module.clone(),
                location: function.location,
            });
        }
        let (params, return_) = type_.fn_types().expect("catch must be a function type");
        let param_vts = self.val_types(params.iter().cloned());
        let result_vt = self.val_type(&return_);
        let builtin = BuiltinFunction::Catch(param_vts, result_vt, TypeKey::new(type_));
        let index = self.get_function_builtin(builtin, |s| s.code_catch(&params, &return_));
        Id::func(CATCH.into(), index)
    }

    /// The code of `_catch`. The arena goes back to where it was when
    /// `_catch` started, when it catches the failure and when an exception
    /// of another tag goes on.
    fn code_catch(&mut self, params: &[Arc<Type>], return_: &Arc<Type>) -> FunctionId {
        let [run_type, ok_type, error_type] = params else {
            panic!("the three params of `_catch`")
        };
        let (_, value_type) = run_type.fn_types().expect("a function to run");
        let (error_params, _) = error_type.fn_types().expect("a function of the error");
        let run_closure = self.closure_type([], value_type.clone());
        let ok_closure = self.closure_type([value_type.clone()], return_.clone());
        let error_closure = self.closure_type(error_params, return_.clone());
        let value_vt = self.val_type(&value_type);
        let tag = self.failure_tag();
        let failure_vts = self.panic_params();
        let failure_seq = InstrSeqType::new(&mut self.wasm_module.types, &[], &failure_vts);
        let arena_mark = self.rust_function(ARENA_MARK);
        let arena_release = self.rust_function(ARENA_RELEASE);
        let result_vt = self.val_type(return_);
        // params
        let [run, ok, error] = [run_type, ok_type, error_type].map(|type_| {
            let val_type = self.val_type(type_);
            self.wasm_local(val_type)
        });
        // locals
        let mark = self.wasm_local(ValType::I32);
        let value = self.wasm_local(value_vt);
        let exception = self.wasm_local(ValType::Ref(RefType::EXNREF));
        let failure: Vec<_> = failure_vts
            .iter()
            .map(|val_type| self.wasm_local(*val_type))
            .collect();
        let mut function = Function::new(self, CATCH, &[run, ok, error], &[result_vt]);
        let mut instructions = function.extend_instructions(self);
        let _ = instructions
            .call(arena_mark)
            .local_set(mark)
            .block_(failure_seq, |caught| {
                let caught_label = caught.id();
                let _ = caught
                    .block_(
                        InstrSeqType::Simple(Some(ValType::Ref(RefType::EXNREF))),
                        |other| {
                            let catches = vec![
                                walrus::ir::TryTableCatch::Catch {
                                    tag,
                                    label: caught_label,
                                },
                                walrus::ir::TryTableCatch::CatchAllRef { label: other.id() },
                            ];
                            let _ = other
                                .try_table(InstrSeqType::Simple(Some(value_vt)), catches, |body| {
                                    let _ = body
                                        .local_get(run)
                                        .local_get(run)
                                        .struct_get(run_closure.type_index, 0)
                                        .call_ref(run_closure.code_type_index);
                                })
                                .local_set(value)
                                .local_get(ok)
                                .local_get(value)
                                .local_get(ok)
                                .struct_get(ok_closure.type_index, 0)
                                .return_call_ref(ok_closure.code_type_index);
                        },
                    )
                    .local_set(exception)
                    .local_get(mark)
                    .call(arena_release)
                    .local_get(exception)
                    .throw_ref();
            });
        for local in failure.iter().rev() {
            let _ = instructions.local_set(*local);
        }
        let _ = instructions
            .local_get(mark)
            .call(arena_release)
            .local_get(error);
        for local in &failure {
            let _ = instructions.local_get(*local);
        }
        let _ = instructions
            .local_get(error)
            .struct_get(error_closure.type_index, 0)
            .return_call_ref(error_closure.code_type_index)
            .end();
        function.finish(self)
    }
}
