// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! The failures with `failure = "exception"`, see `panic`.

use super::{TEST_PACKAGE, WasmOutput, run_wasm_in};
use crate::config::{WasmFailure, WebAssemblyConfig};

fn exception() -> WebAssemblyConfig {
    WebAssemblyConfig {
        failure: WasmFailure::Exception,
        ..WebAssemblyConfig::default()
    }
}

fn run_exception(src: &str) -> WasmOutput {
    run_wasm_in(TEST_PACKAGE, src, vec![], exception())
}

#[track_caller]
fn run_exception_ok(src: &str) {
    let result = run_exception(&format!("{CATCH}{src}"));
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

const CATCH: &str = r#"
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic

pub type Failure {
  Failure(
    kind: String,
    file: String,
    module: String,
    function: String,
    line: String,
    message: String,
    has_message: Bool,
  )
}

@external(webassembly, "builtins", "_catch")
fn catch_with(
  run: fn() -> a,
  ok: fn(a) -> b,
  error: fn(String, String, String, String, String, String, Bool, Dynamic) -> b,
) -> b

fn catch(run: fn() -> a) -> Result(a, Failure) {
  catch_with(run, Ok, fn(kind, file, module, function, line, message, has_message, _) {
    Error(Failure(kind, file, module, function, line, message, has_message))
  })
}
"#;

/// A failure that nothing catches is printed as without the option.
#[test]
fn uncaught_failure_is_printed() {
    let src = r#"
pub fn main() {
  panic as "crashed"
}
"#;
    let exit = super::run_wasm(src, vec![]);
    let exception = run_exception(src);
    assert!(!exception.status.success());
    assert_eq!(exception.status.code(), exit.status.code());
    assert_eq!(exception.stderr, exit.stderr);
}

#[test]
fn catch_returns_the_value() {
    run_exception_ok(
        r#"
pub fn main() {
  assert catch(fn() { 1 + 2 }) == Ok(3)
}
"#,
    );
}

#[test]
fn catch_a_panic() {
    run_exception_ok(
        r#"
fn fail() -> Int {
  panic as "crashed"
}

pub fn main() {
  let assert Error(failure) = catch(fail)
  assert failure
    == Failure("panic", "src/my/mod.gleam", "my/mod", "fail", "31", "crashed", True)
}
"#,
    );
}

#[test]
fn catch_the_kinds() {
    run_exception_ok(
        r#"
pub fn main() {
  let assert Error(Failure(kind: "todo", message:, has_message: False, ..)) =
    catch(fn() { todo })
  assert message == "`todo` expression evaluated. This code has not yet been implemented."
  let assert Error(Failure(kind: "let_assert", ..)) =
    catch(fn() {
      let assert Ok(n) = Error(1)
      n
    })
  let assert Error(Failure(kind: "assert", has_message: False, ..)) =
    catch(fn() { assert 1 == 2 })
  let assert Error(Failure(kind: "assert", message: "no", has_message: True, ..)) =
    catch(fn() { assert 1 == 2 as "no" })
}
"#,
    );
}

/// A failure deep in the calls of `run` goes to the innermost `catch`, and
/// the program goes on after it.
#[test]
fn nested_catch() {
    run_exception_ok(
        r#"
fn deep(n: Int) -> Int {
  case n {
    0 -> panic as "bottom"
    _ -> 1 + deep(n - 1)
  }
}

pub fn main() {
  let outer =
    catch(fn() {
      let inner = catch(fn() { deep(100) })
      let assert Error(Failure(message: "bottom", ..)) = inner
      deep(5)
    })
  let assert Error(Failure(message: "bottom", ..)) = outer
  assert catch(fn() { 7 }) == Ok(7)
}
"#,
    );
}

/// `catch` gives back the arena that `run` took before it failed.
#[test]
fn catch_releases_the_arena() {
    run_exception_ok(
        r#"
type I32

@external(webassembly, "builtins", "_arena_mark")
fn arena_mark() -> I32
@external(webassembly, "builtins", "_arena_alloc")
fn alloc(size: I32) -> I32
@external(webassembly, "builtins", "_int_to_i32")
fn to_i32(n: Int) -> I32

pub fn main() {
  let mark = arena_mark()
  let _ =
    catch(fn() {
      let _ = alloc(to_i32(1024))
      panic
    })
  assert arena_mark() == mark
}
"#,
    );
}

#[test]
fn catch_without_exception() {
    let src = format!(
        "{CATCH}{}",
        r#"
pub fn main() {
  catch(fn() { 1 })
}
"#
    );
    let error = super::codegen_in(TEST_PACKAGE, &src, vec![], WebAssemblyConfig::default())
        .expect_err("expected codegen error");
    assert!(
        matches!(
            error,
            crate::webassembly::Error::CatchWithoutException { .. }
        ),
        "got {error:?}"
    );
}

/// The global keeps what `run` set before it failed.
#[test]
fn catch_keeps_the_global() {
    run_exception_ok(
        r#"
@external(webassembly, "builtins", "_global_get")
fn global_get() -> Dynamic
@external(webassembly, "builtins", "_global_set")
fn global_set(value: Dynamic) -> Nil
@external(webassembly, "builtins", "dynamic_int")
fn int(value: Int) -> Dynamic
@external(webassembly, "builtins", "dynamic_decode_int")
fn decode_int(data: Dynamic) -> Result(Int, Int)

pub fn main() {
  let _ =
    catch(fn() {
      global_set(int(1))
      panic
    })
  assert decode_int(global_get()) == Ok(1)
}
"#,
    );
}
