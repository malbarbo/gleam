// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `stack_overflow = "fail"`, see `stack`.

use super::{TEST_PACKAGE, WasmOutput, compile_wasm_in, run_wasm_in};
use crate::config::{WasmFailure, WasmStackOverflow, WebAssemblyConfig};

fn fail(limit: u32) -> WebAssemblyConfig {
    WebAssemblyConfig {
        stack_overflow: WasmStackOverflow::Fail,
        stack_limit: limit,
        ..WebAssemblyConfig::default()
    }
}

fn run(src: &str, config: WebAssemblyConfig) -> WasmOutput {
    run_wasm_in(TEST_PACKAGE, src, vec![], config)
}

#[track_caller]
fn runs(src: &str, config: WebAssemblyConfig) {
    let result = run(src, config);
    assert!(result.status.success(), "{}", result.stderr);
}

const SUM: &str = r#"
fn sum(n: Int) -> Int {
  case n {
    0 -> 0
    _ -> n + sum(n - 1)
  }
}
"#;

/// A call past the limit fails at the function it calls, before the stack
/// of the engine ends.
#[test]
fn call_past_the_limit() {
    let result = run(
        &format!("{SUM}\npub fn main() {{ sum(1_000_000) }}"),
        fail(1000),
    );
    assert!(!result.status.success());
    assert!(
        result.stderr.contains("stack_overflow at ")
            && result.stderr.contains(".gleam:2\n  Stack overflow."),
        "{}",
        result.stderr
    );
}

/// `main` and the 99 calls of `sum`, from 98 to 0, are the 100 calls of the
/// limit, and one more call fails.
#[test]
fn calls_up_to_the_limit() {
    runs(
        &format!("{SUM}\npub fn main() {{ assert sum(98) == 4851 }}"),
        fail(100),
    );
    let result = run(
        &format!("{SUM}\npub fn main() {{ assert sum(99) == 4950 }}"),
        fail(100),
    );
    assert!(
        result.stderr.contains("Stack overflow."),
        "{}",
        result.stderr
    );
}

/// A tail call does not add to the depth.
#[test]
fn tail_calls() {
    runs(
        r#"
fn count(n: Int, acc: Int) -> Int {
  case n {
    0 -> acc
    _ -> count(n - 1, acc + 1)
  }
}

pub fn main() {
  assert count(100_000, 0) == 100_000
}
"#,
        fail(10),
    );
}

/// The calls that returned do not count, also the ones through a closure.
#[test]
fn returned_calls() {
    runs(
        &format!(
            r#"{SUM}
fn apply(f: fn(Int) -> Int, n: Int) -> Int {{ f(n) }}

pub fn main() {{
  assert sum(90) == 4095
  assert apply(sum, 90) == 4095
  assert apply(fn(n) {{ sum(n) }}, 90) == 4095
  assert sum(90) == 4095
}}"#
        ),
        fail(100),
    );
}

/// An anonymous function counts, so a recursion that goes only through
/// anonymous functions fails at the limit.
#[test]
fn anonymous_function() {
    let result = run(
        r#"
type F {
  F(run: fn(F, Int) -> Int)
}

pub fn main() {
  let f =
    F(fn(self: F, n: Int) -> Int {
      case n {
        0 -> 0
        _ -> 1 + self.run(self, n - 1)
      }
    })
  f.run(f, 1_000_000)
}
"#,
        fail(1000),
    );
    assert!(
        result.stderr.contains(".gleam:8\n  Stack overflow."),
        "{}",
        result.stderr
    );
}

/// With `failure = "exception"`, `catch` catches an overflow, and the depth
/// goes back to where it was.
#[test]
fn catch() {
    let src = format!(
        r#"{SUM}
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic

@external(webassembly, "builtins", "_catch")
fn catch_with(
  run: fn() -> a,
  ok: fn(a) -> b,
  error: fn(String, String, String, String, String, String, Bool, Dynamic) -> b,
) -> b

fn kind(run: fn() -> a) -> String {{
  catch_with(run, fn(_) {{ "none" }}, fn(kind, _, _, _, _, _, _, _) {{ kind }})
}}

pub fn main() {{
  assert kind(fn() {{ sum(1000) }}) == "stack_overflow"
  assert kind(fn() {{ sum(1000) }}) == "stack_overflow"
  assert sum(90) == 4095
}}"#
    );
    runs(
        &src,
        WebAssemblyConfig {
            failure: WasmFailure::Exception,
            ..fail(100)
        },
    );
}

/// A limit that does not fit in the `i32` of the depth is the largest
/// `i32`.
#[test]
fn limit_larger_than_the_depth() {
    runs(
        &format!("{SUM}\npub fn main() {{ assert sum(10) == 55 }}"),
        fail(u32::MAX),
    );
}

/// Without the option, the stack of the engine ends in a trap.
#[test]
fn traps_by_default() {
    let result = run(
        &format!("{SUM}\npub fn main() {{ sum(100_000_000) }}"),
        WebAssemblyConfig::default(),
    );
    assert!(!result.status.success());
    assert!(
        !result.stderr.contains("Stack overflow."),
        "{}",
        result.stderr
    );
}

/// The host can change the limit, which is an exported global.
#[test]
fn limit_is_exported() {
    let wasm = compile_wasm_in(
        TEST_PACKAGE,
        &format!("{SUM}\npub fn main() {{ sum(10) }}"),
        vec![],
        fail(100),
    );
    let exports: Vec<_> = wasmparser::Parser::new(0)
        .parse_all(&wasm)
        .filter_map(|payload| {
            if let wasmparser::Payload::ExportSection(reader) = payload.expect("a valid module") {
                Some(reader)
            } else {
                None
            }
        })
        .flatten()
        .map(|export| export.expect("an export"))
        .filter(|export| export.kind == wasmparser::ExternalKind::Global)
        .map(|export| export.name.to_string())
        .collect();
    assert_eq!(exports, vec!["_stack_limit"]);
}
