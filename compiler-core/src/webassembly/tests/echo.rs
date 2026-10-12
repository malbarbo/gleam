// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use super::run_ok;

#[test]
fn inspect_int() {
    run_ok(
        r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

pub fn main() {
    assert inspect(42) == "42"
}
"#,
    );
}

#[test]
fn inspect_string() {
    run_ok(
        r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

pub fn main() {
    assert inspect("hello") == "\"hello\""
}
"#,
    );
}

#[test]
fn inspect_string_control_characters() {
    run_ok(
        r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

pub fn main() {
    assert inspect("\t\f\r\n") == "\"\\t\\f\\r\\n\""
    assert inspect("a\u{0008}b") == "\"a\\u{0008}b\""
    assert inspect("\u{001B}\u{0000}") == "\"\\u{001B}\\u{0000}\""
    assert inspect("\u{007F}\u{0080}\u{009F}") == "\"\\u{007F}\\u{0080}\\u{009F}\""
    assert inspect("\u{00A0}é") == "\"\u{00A0}é\""
}
"#,
    );
}

#[test]
fn inspect_list() {
    run_ok(
        r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

pub fn main() {
    assert inspect([1, 2, 3]) == "[1, 2, 3]"
}
"#,
    );
}

#[test]
fn inspect_bool() {
    run_ok(
        r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

pub fn main() {
    assert inspect(True) == "True"
    assert inspect(False) == "False"
}
"#,
    );
}

#[test]
fn echo_various_types() {
    assert_wasm_echo!(
        r#"
pub fn main() {
    let x = 10
    echo echo x
    echo ["1\n2", echo "10", "9"]
    echo []
    echo #(1, 2.0, "home", #(), main)
    True
}
"#,
    );
}

#[test]
fn echo_in_pipes() {
    assert_wasm_echo!(
        r#"
fn inc(x: Int) -> Int {
    x + 1
}

pub fn main() {
    let n = 1
    let f = fn(x: Int) { x + n }
    let y = 1 |> echo
    let z = y |> echo |> inc |> echo as "inc"
    let g = f |> echo
    assert g(z) == 3
}
"#,
    );
}

/// The text of a big value needs more memory than there is at the start.
#[test]
fn inspect_grows_the_memory() {
    run_ok(
        r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

@external(webassembly, "builtins", "_string_byte_size")
fn byte_size(string: String) -> Int

type Nested {
  Nested(Nested, Int)
  Leaf
}

fn nest(n: Int, acc: Nested) -> Nested {
  case n {
    0 -> acc
    _ -> nest(n - 1, Nested(acc, 123_456_789))
  }
}

fn numbers(n: Int, acc: List(Int)) -> List(Int) {
  case n {
    0 -> acc
    _ -> numbers(n - 1, [n * 1000, ..acc])
  }
}

pub fn main() {
  let assert 38_004 = byte_size(inspect(nest(2000, Leaf)))
  let assert True = byte_size(inspect(numbers(20_000, []))) > 150_000
}
"#,
    );
}
