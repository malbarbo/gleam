// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `assert = { module, function }`, see `asserts`.

use super::{TEST_PACKAGE, WasmOutput, codegen_in, run_wasm_in};
use crate::config::{WasmFailure, WasmFunction, WebAssemblyConfig};
use crate::webassembly::Error;

fn config() -> WebAssemblyConfig {
    WebAssemblyConfig {
        failure: WasmFailure::Exception,
        assert: Some(WasmFunction {
            module: "my/check".into(),
            function: "run".into(),
        }),
        ..WebAssemblyConfig::default()
    }
}

/// `run` counts in the global the `assert`s that pass, by 1, and the ones
/// that fail, by 100, and keeps the line of the last failure.
const CHECK: &str = r#"
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic

@external(webassembly, "builtins", "_catch")
fn catch_with(
  run: fn() -> a,
  ok: fn(a) -> b,
  error: fn(String, String, String, String, String, String, Bool, Dynamic) -> b,
) -> b
@external(webassembly, "builtins", "_global_get")
fn global_get() -> Dynamic
@external(webassembly, "builtins", "_global_set")
fn global_set(value: Dynamic) -> Nil
@external(webassembly, "builtins", "dynamic_array")
fn array(values: List(Dynamic)) -> Dynamic
@external(webassembly, "builtins", "dynamic_int")
fn int(value: Int) -> Dynamic
@external(webassembly, "builtins", "dynamic_string")
fn string(value: String) -> Dynamic
@external(webassembly, "builtins", "dynamic_decode_list_items")
fn items(data: Dynamic) -> Result(List(Dynamic), Nil)
@external(webassembly, "builtins", "dynamic_decode_int")
fn decode_int(data: Dynamic) -> Result(Int, Int)
@external(webassembly, "builtins", "dynamic_decode_string")
fn decode_string(data: Dynamic) -> Result(String, String)

pub fn run(assertion: fn() -> Nil) -> Nil {
  catch_with(assertion, fn(_) { add(1, "") }, fn(_, _, _, function, line, _, _, _) {
    add(100, function <> ":" <> line)
  })
}

fn add(n: Int, failure: String) -> Nil {
  let #(count, last) = state()
  let last = case failure {
    "" -> last
    _ -> failure
  }
  global_set(array([int(count + n), string(last)]))
}

pub fn state() -> #(Int, String) {
  case items(global_get()) {
    Ok([count, last]) -> {
      let assert Ok(count) = decode_int(count)
      let assert Ok(last) = decode_string(last)
      #(count, last)
    }
    _ -> #(0, "")
  }
}
"#;

fn run(src: &str, deps: Vec<(&str, &str, &str)>) -> WasmOutput {
    let mut deps = deps;
    deps.push((TEST_PACKAGE, "my/check", CHECK));
    run_wasm_in(TEST_PACKAGE, src, deps, config())
}

#[track_caller]
fn run_ok(src: &str, deps: Vec<(&str, &str, &str)>) {
    let result = run(src, deps);
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

/// The `assert`s after one that fails run too, and a `panic` in the value
/// of an `assert` counts only for that `assert`. The `panic` of `main` is
/// not wrapped, so the program fails when the counts are not right.
#[test]
fn each_assert_runs() {
    run_ok(
        r#"
import my/check

fn boom() -> Int {
  panic
}

pub fn main() {
  assert 1 == 1
  assert 1 == 2
  assert boom() == 3
  let x = 2
  assert x == 2
  case check.state() {
    #(202, _) -> Nil
    _ -> panic
  }
}
"#,
        vec![],
    );
}

/// The `assert`s inside a block, an anonymous function and a `case` are
/// wrapped too, and the failure has the line and the function of the source.
#[test]
fn nested_asserts_keep_their_location() {
    run_ok(
        r#"
import my/check

fn helper(n: Int) -> Nil {
  case n {
    0 -> Nil
    _ -> {
      let f = fn() {
        assert n == 0
      }
      f()
    }
  }
}

pub fn main() {
  helper(0)
  helper(1)
  case check.state() {
    #(100, "helper:9") -> Nil
    _ -> panic
  }
}
"#,
        vec![],
    );
}

/// The `assert`s of another package are not wrapped, so the failure ends
/// the program.
#[test]
fn asserts_of_dependencies_are_not_wrapped() {
    let other = r#"
pub fn fails() -> Nil {
  assert 1 == 2
}
"#;
    let result = run(
        r#"
import other

pub fn main() {
  other.fails()
}
"#,
        vec![("other_package", "other", other)],
    );
    assert!(!result.status.success());
}

/// The module of the function does not need to be imported.
#[test]
fn module_of_the_function_is_added() {
    run_ok(
        r#"
pub fn main() {
  assert 1 == 2
}
"#,
        vec![],
    );
}

fn codegen_error(config: WebAssemblyConfig, check: &str) -> Error {
    codegen_in(
        TEST_PACKAGE,
        "pub fn main() { assert True }",
        vec![(TEST_PACKAGE, "my/check", check)],
        config,
    )
    .expect_err("expected codegen error")
}

#[test]
fn assert_without_exception() {
    let config = WebAssemblyConfig {
        failure: WasmFailure::Exit,
        ..config()
    };
    let error = codegen_error(config, CHECK);
    assert!(
        matches!(error, Error::AssertWithoutException),
        "got {error:?}"
    );
}

#[test]
fn unknown_assert_function() {
    let error = codegen_error(
        config(),
        "fn run(assertion: fn() -> Nil) -> Nil { assertion() }",
    );
    assert!(
        matches!(error, Error::UnknownHookFunction { .. }),
        "got {error:?}"
    );
}

#[test]
fn wrong_assert_function_signature() {
    let error = codegen_error(config(), "pub fn run(assertion: fn() -> Nil) -> Int { 1 }");
    assert!(
        matches!(error, Error::WrongHookFunctionSignature { .. }),
        "got {error:?}"
    );
}
