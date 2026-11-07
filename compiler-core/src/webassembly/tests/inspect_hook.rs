// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `inspect = { module, function }`, see `function_inspect_hook`.

use super::{TEST_PACKAGE, codegen_in, run_wasm_in};
use crate::config::{WasmFunction, WebAssemblyConfig};
use crate::webassembly::Error;

fn config() -> WebAssemblyConfig {
    WebAssemblyConfig {
        inspect: Some(WasmFunction {
            module: "my/show".into(),
            function: "show".into(),
        }),
        ..WebAssemblyConfig::default()
    }
}

const DYNAMIC: &str = r#"
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic
"#;

/// `show` puts the text of the runtime between `<` and `>`.
const SHOW: &str = r#"
import gleam/dynamic.{type Dynamic}

@external(webassembly, "builtins", "dynamic_inspect")
fn inspect(data: Dynamic) -> String

pub fn show(value: Dynamic) -> String {
  "<" <> inspect(value) <> ">"
}
"#;

#[track_caller]
fn run_ok(src: &str, show: &str) {
    let output = run_wasm_in(
        TEST_PACKAGE,
        src,
        vec![
            (TEST_PACKAGE, "gleam/dynamic", DYNAMIC),
            (TEST_PACKAGE, "my/show", show),
        ],
        config(),
    );
    assert!(
        output.status.success(),
        "WASM execution failed:\n{}",
        output.stderr
    );
}

/// The runtime shows the values inside a value with its own text. The
/// program does not import the module of `show`.
#[test]
fn inspect_calls_the_function() {
    run_ok(
        r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

type Point {
  Point(x: Int, y: Int)
}

pub fn main() {
  assert inspect(1) == "<1>"
  assert inspect("a") == "<\"a\">"
  assert inspect([Point(1, 2)]) == "<[Point(x: 1, y: 2)]>"
  let f = inspect
  assert f(#(1, 2.0)) == "<#(1, 2.0)>"
}
"#,
        SHOW,
    );
}

fn codegen_error(show: &str) -> Error {
    codegen_in(
        TEST_PACKAGE,
        "pub fn main() { Nil }",
        vec![
            (TEST_PACKAGE, "gleam/dynamic", DYNAMIC),
            (TEST_PACKAGE, "my/show", show),
        ],
        config(),
    )
    .expect_err("expected codegen error")
}

#[test]
fn unknown_inspect_function() {
    let error = codegen_error("fn show() { Nil }");
    assert!(
        matches!(error, Error::UnknownHookFunction { .. }),
        "got {error:?}"
    );
}

#[test]
fn wrong_inspect_function_signature() {
    let error = codegen_error("pub fn show(value: Int) -> String { \"\" }");
    assert!(
        matches!(error, Error::WrongHookFunctionSignature { .. }),
        "got {error:?}"
    );
}
