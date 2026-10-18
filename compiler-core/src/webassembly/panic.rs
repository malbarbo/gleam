// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! The errors of `panic`, `todo`, `let assert` and `assert`, which call
//! `_panic` with the error of the JavaScript target: its kind, file, module,
//! function, line, message and the details of its kind, like the value that
//! did not match. In a program that runs in a JavaScript host, one that
//! imports anything other than WASI, `_panic` throws the `Error` of the
//! JavaScript target, which the host can catch, like on JavaScript. Otherwise
//! it prints the error and exits, without the details, which are only made
//! when the program can run in a JavaScript host: it uses JavaScript strings
//! or a module declares a function of the host.

use super::dynamic::DYNAMIC_MODULE;
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
/// - `panic` and `todo` have none, `Nil`.
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
}

impl Failure<'_> {
    /// The `gleam_error` of the JavaScript target.
    fn name(&self) -> &'static str {
        match self {
            Failure::Panic => "panic",
            Failure::Todo => "todo",
            Failure::LetAssert { .. } => "let_assert",
            Failure::Assert { .. } => "assert",
        }
    }

    /// The message of the JavaScript target, of an error without one.
    fn default_message(&self) -> &'static str {
        match self {
            Failure::Panic => "`panic` expression evaluated.",
            Failure::Todo => "`todo` expression evaluated. This code has not yet been implemented.",
            Failure::LetAssert { .. } => "Pattern match failed, no pattern matched the value.",
            Failure::Assert { .. } => "Assertion failed.",
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

/// The strings that `_panic` prints between its params.
const PRINTED: [&str; 4] = [" at ", ":", "\n  ", "\n"];

/// The module of the imports of WASI.
const WASI_MODULE: &str = "wasi_snapshot_preview1";

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
        let (module, line) = self.source_location(start);
        let file = self.source_file(&module);
        let function = self.current_function_name.clone();
        let strings = [
            error.name().into(),
            file,
            module,
            function,
            line.to_string().into(),
        ];
        for string in strings {
            let index = self.string_index(&string);
            let _ = instructions.global_as_non_null(index);
        }
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
        self.panic_details(instructions, locals, &scope, start, error);
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
            Failure::Panic | Failure::Todo => {
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

    fn dynamic_nil(&mut self) -> FunctionId {
        let dynamic = self.runtime_named_type(DYNAMIC_MODULE, "Dynamic", vec![]);
        self.runtime_function_id(DYNAMIC_MODULE, "dynamic_nil", vec![], dynamic)
    }

    /// `_panic(kind, file, module, function, line, message, details)`, whose code is
    /// generated by `code_panic` once the other functions are, as it depends
    /// on their imports.
    fn function_panic(&mut self) -> FunctionId {
        if let Some(panic) = self.panic {
            return panic;
        }
        let string = self.val_type(&type_::string());
        let dynamic = self.runtime_named_type(DYNAMIC_MODULE, "Dynamic", vec![]);
        let dynamic = self.val_type(&dynamic);
        let params = [string, string, string, string, string, string, dynamic];
        let panic = self.allocate_placeholder_function(PANIC, &params, &[]);
        // The strings are initialized with the constants, which are
        // generated before the code of `_panic`.
        for string in PRINTED {
            let _ = self.string_index(&string.into());
        }
        self.panic = Some(panic);
        panic
    }

    /// Generates the code of `_panic`, when it is called.
    pub(super) fn code_panic(&mut self) {
        let Some(panic) = self.panic else {
            return;
        };
        let js_host = self
            .wasm_module
            .imports
            .iter()
            .any(|import| import.module != WASI_MODULE);
        let string_type = type_::string();
        let dynamic_type = self.runtime_named_type(DYNAMIC_MODULE, "Dynamic", vec![]);
        let types = [&string_type; 6].into_iter().chain([&dynamic_type]);
        // params
        let params: Vec<_> = types
            .clone()
            .map(|type_| {
                let val_type = self.val_type(type_);
                self.wasm_local(val_type)
            })
            .collect();
        let mut function = Function::new(self, PANIC, &params, &[]);
        let mut instructions = function.extend_instructions(self);
        if js_host {
            let mut js_types = vec![];
            for (param, type_) in params.iter().zip(types) {
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
            let [kind, file, _, _, line, message, _] = params[..] else {
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
        let code = function.finish(self);
        replace_function_body(&mut self.wasm_module, panic, code);
    }
}
