// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `int_overflow = "fail"`, see `overflow`.

use super::{TEST_PACKAGE, WasmOutput, run_wasm_in, wasm_config};
use crate::config::{WasmFailure, WasmFloat, WasmInt, WasmIntOverflow, WebAssemblyConfig};

fn fail(int: WasmInt) -> WebAssemblyConfig {
    WebAssemblyConfig {
        int_overflow: WasmIntOverflow::Fail,
        ..wasm_config(int, WasmFloat::F64)
    }
}

fn run(src: &str, config: WebAssemblyConfig) -> WasmOutput {
    run_wasm_in(TEST_PACKAGE, src, vec![], config)
}

/// `src` with the largest and the smallest `Int` of `int` for `MAX` and
/// `MIN`, called `max()` and `min()` so the operations happen when the
/// program runs.
fn with_bounds(src: &str, int: WasmInt) -> String {
    let (max, min) = match int {
        WasmInt::I32 => (i64::from(i32::MAX), i64::from(i32::MIN)),
        WasmInt::I64 => (i64::MAX, i64::MIN),
    };
    format!("fn max() -> Int {{ {max} }}\nfn min() -> Int {{ {min} }}\n{src}")
}

/// The program fails with an `Int` overflow at `line` of `src`, with each
/// `Int`.
#[track_caller]
fn overflows_at(src: &str, line: u32) {
    for int in [WasmInt::I32, WasmInt::I64] {
        let result = run(&with_bounds(src, int), fail(int));
        assert!(!result.status.success(), "{int:?} did not fail");
        // `with_bounds` adds two lines.
        let expected = format!(".gleam:{}\n  Int overflow.", line + 2);
        assert!(
            result.stderr.contains("int_overflow at ") && result.stderr.contains(&expected),
            "{int:?}: expected {expected:?} in:\n{}",
            result.stderr
        );
    }
}

/// The program runs, with each `Int`.
#[track_caller]
fn runs(src: &str, config: impl Fn(WasmInt) -> WebAssemblyConfig) {
    for int in [WasmInt::I32, WasmInt::I64] {
        let result = run(&with_bounds(src, int), config(int));
        assert!(result.status.success(), "{int:?}:\n{}", result.stderr);
    }
}

#[test]
fn add() {
    overflows_at("pub fn main() {\n  max() + 1\n}", 2);
}

#[test]
fn sub() {
    overflows_at("pub fn main() {\n  min() - 1\n}", 2);
}

#[test]
fn mul() {
    overflows_at("pub fn main() {\n  max() * 2\n}", 2);
    overflows_at("pub fn main() {\n  min() * -1\n}", 2);
    overflows_at("pub fn main() {\n  -1 * min()\n}", 2);
}

#[test]
fn negate() {
    overflows_at("pub fn main() {\n  let n = min()\n  let m = -n\n  m\n}", 3);
}

#[test]
fn div() {
    overflows_at("pub fn main() {\n  let d = -1\n  min() / d\n}", 3);
}

#[test]
fn guard() {
    overflows_at(
        "pub fn main() {\n  let n = max()\n  case n {\n    _ if n + 1 > 0 -> 1\n    _ -> 0\n  }\n}",
        4,
    );
}

/// The results that fit are the ones of the modular operations.
#[test]
fn results_that_fit() {
    runs(
        r#"pub fn main() {
  assert max() - 1 + 1 == max()
  assert min() + 1 - 1 == min()
  assert min() + max() == -1
  assert -1 * { min() + 1 } == max()
  assert max() * -1 == min() + 1
  assert min() * 1 == min()
  assert 0 * min() == 0
  assert min() / 1 == min()
  assert min() / 0 == 0
  assert 7 / -1 == -7
  let n = max()
  assert -n == min() + 1
}"#,
        fail,
    );
}

/// Without the option, the operations wrap.
#[test]
fn wraps_by_default() {
    runs(
        r#"pub fn main() {
  assert max() + 1 == min()
  assert min() - 1 == max()
  let d = -1
  assert min() / d == min()
}"#,
        |int| wasm_config(int, WasmFloat::F64),
    );
}

/// The hashes of the runtime wrap, also with the option.
#[test]
fn runtime_wraps() {
    let src = r#"
@external(webassembly, "builtins", "Dict")
pub type Dict(key, value)

@external(webassembly, "builtins", "dict_from_list")
fn from_list(list: List(#(k, v))) -> Dict(k, v)

@external(webassembly, "builtins", "dict_size")
fn size(dict: Dict(k, v)) -> Int

pub fn main() {
  // The hash of a dict, a key here, multiplies a hash of 32 bits by 31.
  let dicts =
    from_list([
      #(from_list([#("a", max())]), 1),
      #(from_list([#("b", min())]), 2),
      #(from_list([#("c", max())]), 3),
    ])
  assert size(dicts) == 3
}
"#;
    runs(src, fail);
}

/// With `failure = "exception"`, `catch` catches an overflow.
#[test]
fn catch() {
    let src = r#"
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic

@external(webassembly, "builtins", "_catch")
fn catch_with(
  run: fn() -> a,
  ok: fn(a) -> b,
  error: fn(String, String, String, String, String, String, Bool, Dynamic) -> b,
) -> b

pub fn main() {
  let kind =
    catch_with(fn() { max() + 1 }, fn(_) { "none" }, fn(kind, _, _, _, _, _, _, _) {
      kind
    })
  assert kind == "int_overflow"
}
"#;
    runs(src, |int| WebAssemblyConfig {
        failure: WasmFailure::Exception,
        ..fail(int)
    });
}

const FLOAT_TO_INT: &str = r#"
@external(webassembly, "builtins", "_float_truncate")
fn truncate(x: Float) -> Int

@external(webassembly, "builtins", "_float_round")
fn round(x: Float) -> Int

fn apply(f: fn(Float) -> Int, x: Float) -> Int {
  f(x)
}
"#;

/// A `Float` that is too large for the `Int` fails at the declaration of
/// the function that converts it, also when the function is a value.
#[test]
fn float_to_int() {
    let main = |body: &str| format!("{FLOAT_TO_INT}\npub fn main() {{ {body} }}");
    overflows_at(&main("truncate(1.0e30)"), 3);
    overflows_at(&main("truncate(-1.0e30)"), 3);
    overflows_at(&main("round(1.0e30)"), 6);
    overflows_at(&main("round(-1.0e30)"), 6);
    overflows_at(&main("apply(truncate, 1.0e30)"), 3);
    runs(
        &main(
            "assert truncate(2147483647.9) == 2147483647
  assert truncate(-2147483648.9) == -2147483648
  assert round(2147483647.4) == 2147483647
  assert round(-2147483648.4) == -2147483648
  assert apply(round, -2.5) == -3",
        ),
        fail,
    );
    let result = run(&main("round(2147483647.5)"), fail(WasmInt::I32));
    assert!(
        result.stderr.contains(".gleam:6\n  Int overflow."),
        "{}",
        result.stderr
    );
}

/// Without the option, the conversion gives the nearest `Int`.
#[test]
fn float_to_int_saturates_by_default() {
    runs(
        &format!("{FLOAT_TO_INT}\npub fn main() {{ assert truncate(1.0e30) == max() }}"),
        |int| wasm_config(int, WasmFloat::F64),
    );
}

/// The size of a segment times its unit, and the operations in the size.
#[test]
fn bit_array_segment_size() {
    overflows_at(
        "pub fn main() {\n  let n = max() / 2 + 1\n  <<1:size(n)-unit(4)>>\n}",
        3,
    );
    overflows_at(
        "pub fn main() {
  let n = max()
  case <<1>> {
    <<_:size(n)-unit(2), _:bits>> -> 1
    _ -> 0
  }
}",
        4,
    );
    overflows_at(
        "pub fn main() {
  let n = max()
  case <<1>> {
    <<_:size(n + 1), _:bits>> -> 1
    _ -> 0
  }
}",
        4,
    );
    runs(
        "pub fn main() {
  let n = 2
  assert <<1:size(n)-unit(4)>> == <<1>>
  let assert <<a:size(n * 2)-unit(2)>> = <<7>>
  assert a == 7
}",
        fail,
    );
}
