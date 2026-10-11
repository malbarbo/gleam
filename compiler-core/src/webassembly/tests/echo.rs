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
