// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! The global `Dynamic`, see `global`.

use super::run_ok;

const GLOBAL: &str = r#"
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic

@external(webassembly, "builtins", "_global_get")
fn global_get() -> Dynamic
@external(webassembly, "builtins", "_global_set")
fn global_set(value: Dynamic) -> Nil
@external(webassembly, "builtins", "dynamic_int")
fn int(value: Int) -> Dynamic
@external(webassembly, "builtins", "dynamic_decode_int")
fn decode_int(data: Dynamic) -> Result(Int, Int)
@external(webassembly, "builtins", "dynamic_classify")
fn classify(data: Dynamic) -> String
"#;

#[track_caller]
fn run_global_ok(src: &str) {
    run_ok(&format!("{GLOBAL}{src}"));
}

#[test]
fn global_is_nil_before_a_set() {
    run_global_ok(
        r#"
pub fn main() {
  assert classify(global_get()) == "Nil"
}
"#,
    );
}

/// A value that one function sets is the value that another one gets.
#[test]
fn global_keeps_the_value_between_calls() {
    run_global_ok(
        r#"
fn count() -> Nil {
  let assert Ok(n) = decode_int(global_get())
  global_set(int(n + 1))
}

pub fn main() {
  global_set(int(0))
  count()
  count()
  count()
  assert decode_int(global_get()) == Ok(3)
}
"#,
    );
}
