// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `start = { module, function }`, see `code_command_start`.

use super::{TEST_PACKAGE, WasmOutput, codegen_in, run_wasm_in};
use crate::config::{WasmFailure, WasmFunction, WebAssemblyConfig};
use crate::webassembly::Error;

fn config() -> WebAssemblyConfig {
    WebAssemblyConfig {
        failure: WasmFailure::Exception,
        start: Some(WasmFunction {
            module: "my/run".into(),
            function: "start".into(),
        }),
        ..WebAssemblyConfig::default()
    }
}

/// `start` fails with the text of the result of `main`, so the test sees
/// it in stderr.
const SHOW_RESULT: &str = r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

pub fn start(main: fn() -> a) -> Nil {
  let result = main()
  panic as { "main gave " <> inspect(result) }
}
"#;

/// Runs `src`, which the module of `start` does not import.
fn run(src: &str, run: &str) -> WasmOutput {
    run_wasm_in(
        TEST_PACKAGE,
        src,
        vec![(TEST_PACKAGE, "my/run", run)],
        config(),
    )
}

#[test]
fn start_calls_main() {
    let output = run("pub fn main() { 1 + 2 }", SHOW_RESULT);
    assert!(output.stderr.contains("main gave 3"), "{}", output.stderr);
    let output = run("pub fn main() { \"a\" }", SHOW_RESULT);
    assert!(
        output.stderr.contains("main gave \"a\""),
        "{}",
        output.stderr
    );
}

#[test]
fn start_catches_a_failure_of_main() {
    let output = run(
        r#"
pub fn main() {
  panic as "boom"
}
"#,
        r#"
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic

@external(webassembly, "builtins", "_catch")
fn catch_with(
  run: fn() -> a,
  ok: fn(a) -> b,
  error: fn(String, String, String, String, String, String, Bool, Dynamic) -> b,
) -> b

pub fn start(main: fn() -> a) -> Nil {
  let text =
    catch_with(main, fn(_) { "ok" }, fn(kind, _, _, _, _, message, _, _) {
      kind <> ": " <> message
    })
  assert text == "panic: boom"
}
"#,
    );
    assert!(output.status.success(), "{}", output.stderr);
    assert_eq!(output.stderr, "");
}

/// Nothing catches a failure of `start`, which `_start` prints.
#[test]
fn failure_of_start_is_printed() {
    let output = run(
        "pub fn main() { 1 }",
        "pub fn start(main: fn() -> a) -> Nil {\n  panic as \"in start\"\n}",
    );
    assert!(!output.status.success());
    assert!(
        output.stderr.contains("src/my/run.gleam:2\n  in start"),
        "{}",
        output.stderr
    );
}

fn codegen_error(run: &str) -> Error {
    codegen_in(
        TEST_PACKAGE,
        "pub fn main() { Nil }",
        vec![(TEST_PACKAGE, "my/run", run)],
        config(),
    )
    .expect_err("expected codegen error")
}

#[test]
fn unknown_start_function() {
    let error = codegen_error("fn start(main: fn() -> a) -> Nil { Nil }");
    assert!(
        matches!(error, Error::UnknownHookFunction { .. }),
        "got {error:?}"
    );
}

/// `main` may give a value of any type, so `start` must take any.
#[test]
fn wrong_start_function_signature() {
    let error = codegen_error("pub fn start(main: fn() -> Nil) -> Nil { main() }");
    assert!(
        matches!(error, Error::WrongHookFunctionSignature { .. }),
        "got {error:?}"
    );
}
