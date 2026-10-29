// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use super::{run_fail, run_fail_stderr, run_wasm};

#[test]
fn assert_failure() {
    run_fail(
        r#"
pub fn main() {
  assert 1 == 1 + 2
  0
}
"#,
    );
}

#[test]
fn let_assert_failure() {
    run_fail(
        r#"
pub fn main() {
  let assert 1 = 1 + 1
}
"#,
    );
}

#[test]
fn panic_with_message() {
    run_fail_stderr(
        r#"
pub fn main() {
  case 0 == 0 + 1 {
    True -> 0
    False -> panic as "crashed"
  }
}
"#,
        "crashed",
    );
}

#[test]
fn todo_with_message() {
    run_fail_stderr(
        r#"
pub fn main() {
  todo as "missing"
  0
}
"#,
        "missing",
    );
}

#[test]
fn assert_failure_in_imported_function() {
    let result = run_wasm(
        r#"
import dep/mod

pub fn main() {
  mod.fail()
}
"#,
        vec![(
            "thepackage",
            "dep/mod",
            r#"
pub fn fail() {
  assert 1 == 2
}
"#,
        )],
    );
    assert!(!result.status.success());
    assert!(
        result.stderr.contains("src/dep/mod.gleam:3"),
        "Expected location 'src/dep/mod.gleam:3', got:\n{}",
        result.stderr
    );
}

/// The message of a `let assert` can have code that needs locals, like a
/// variable, a `case` or an `echo`.
#[test]
fn let_assert_with_message_that_needs_locals() {
    run_fail_stderr(
        r#"
pub fn main() {
  let n = 2
  let assert Ok(_) = Error(1) as {
    let m = case echo n {
      2 -> "boom"
      _ -> "other"
    }
    m
  }
  0
}
"#,
        "boom",
    );
}
