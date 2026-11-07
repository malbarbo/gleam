// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `echo = { module, function }`, see `expression_echo_hook`.

use super::{TEST_PACKAGE, WasmOutput, codegen_in, run_wasm_in};
use crate::config::{WasmFunction, WebAssemblyConfig};
use crate::webassembly::Error;

fn config() -> WebAssemblyConfig {
    WebAssemblyConfig {
        echo: Some(WasmFunction {
            module: "my/debug".into(),
            function: "print".into(),
        }),
        ..WebAssemblyConfig::default()
    }
}

const OPTION: &str = r#"
pub type Option(a) {
  Some(a)
  None
}
"#;

const DYNAMIC: &str = r#"
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic
"#;

/// `print` adds a line for each `echo` to the global, which `printed`
/// returns.
const DEBUG: &str = r#"
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option, None, Some}

@external(webassembly, "builtins", "_global_get")
fn global_get() -> Dynamic
@external(webassembly, "builtins", "_global_set")
fn global_set(value: Dynamic) -> Nil
@external(webassembly, "builtins", "dynamic_string")
fn string(value: String) -> Dynamic
@external(webassembly, "builtins", "dynamic_decode_string")
fn decode_string(data: Dynamic) -> Result(String, String)
@external(webassembly, "builtins", "dynamic_inspect")
fn inspect(data: Dynamic) -> String
@external(webassembly, "builtins", "_inspect")
fn inspect_int(value: Int) -> String

pub fn print(file: String, line: Int, message: Option(String), value: Dynamic) -> Nil {
  let message = case message {
    Some(message) -> " " <> message
    None -> ""
  }
  let line = file <> ":" <> inspect_int(line) <> message <> " " <> inspect(value)
  global_set(string(printed() <> line <> "\n"))
}

pub fn printed() -> String {
  case decode_string(global_get()) {
    Ok(printed) -> printed
    Error(_) -> ""
  }
}
"#;

/// Runs `src` with the modules of `print` and `other`.
#[track_caller]
fn run_ok(src: &str, debug: &str, other: &str) -> WasmOutput {
    let output = run_wasm_in(
        TEST_PACKAGE,
        src,
        vec![
            (TEST_PACKAGE, "gleam/option", OPTION),
            (TEST_PACKAGE, "gleam/dynamic", DYNAMIC),
            (TEST_PACKAGE, "my/debug", debug),
            (TEST_PACKAGE, "other", other),
        ],
        config(),
    );
    assert!(
        output.status.success(),
        "WASM execution failed:\n{}",
        output.stderr
    );
    output
}

#[test]
fn echo_calls_the_function() {
    let output = run_ok(
        r#"
import my/debug

type Point {
  Point(x: Int, y: Int)
}

pub fn main() {
  let two = echo 1 + 1
  assert two == 2
  assert echo Point(1, 2) as "point" == Point(1, 2)
  let x = "a"
  let y = echo x
  assert y == "a"
  assert 3 |> echo |> fn(n) { n + 1 } == 4
  assert debug.printed() == "src/my/mod.gleam:9 2
src/my/mod.gleam:11 point Point(x: 1, y: 2)
src/my/mod.gleam:13 \"a\"
src/my/mod.gleam:15 3
"
}
"#,
        DEBUG,
        "",
    );
    assert_eq!(output.stderr, "");
}

/// `other` calls the function without importing its module.
#[test]
fn echo_in_a_module_that_does_not_import_the_function() {
    let _ = run_ok(
        r#"
import my/debug
import other

pub fn main() {
  assert other.f() == 1
  assert debug.printed() == "src/other.gleam:3 1\n"
}
"#,
        DEBUG,
        "pub fn f() {\n  // `echo`\n  echo 1\n}",
    );
}

/// An `echo` in the module of the function would call it again without
/// end, so it prints.
#[test]
fn echo_in_the_module_of_the_function_prints() {
    let output = run_ok(
        r#"
import my/debug

pub fn main() {
  assert echo debug.f() == 1
}
"#,
        r#"
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

pub fn print(_file: String, _line: Int, _message: Option(String), _value: Dynamic) -> Nil {
  f()
  Nil
}

pub fn f() -> Int {
  echo 1
}
"#,
        "",
    );
    assert!(
        output.stderr.contains("src/my/debug.gleam:11"),
        "{}",
        output.stderr
    );
}

fn codegen_error(debug: &str) -> Error {
    codegen_in(
        TEST_PACKAGE,
        "pub fn main() { echo 1 }",
        vec![
            (TEST_PACKAGE, "gleam/option", OPTION),
            (TEST_PACKAGE, "gleam/dynamic", DYNAMIC),
            (TEST_PACKAGE, "my/debug", debug),
        ],
        config(),
    )
    .expect_err("expected codegen error")
}

#[test]
fn unknown_echo_function() {
    let error = codegen_error("fn print() { Nil }");
    assert!(
        matches!(error, Error::UnknownHookFunction { .. }),
        "got {error:?}"
    );
}

#[test]
fn wrong_echo_function_signature() {
    let error = codegen_error("pub fn print(value: Int) -> Nil { Nil }");
    assert!(
        matches!(error, Error::WrongHookFunctionSignature { .. }),
        "got {error:?}"
    );
}
