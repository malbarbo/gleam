// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use super::{run_ok_i64, run_wasm, run_wasm_js};

/// Runs `src` with UTF8 and with JavaScript strings.
#[track_caller]
fn run_ok(src: &str) {
    super::run_ok(src);
    let result = run_wasm_js(src, &[]);
    assert!(
        result.status.success(),
        "WASM execution failed (JSString):\n{}",
        result.stderr
    );
}

/// The global of a string literal is named after its first characters, the
/// ones that are not ASCII letters or digits replaced by `_`.
#[test]
fn string_literal_global_names() {
    let wasm = super::compile_wasm(
        r#"
pub fn main() {
  ["hello, world!", "abcdefghijklmnopqrstuvwxyz"]
}
"#,
        vec![],
    );
    let tags = super::wasm_global_names(&wasm)
        .into_iter()
        .filter(|name| name.starts_with("string@"))
        .filter_map(|name| Some(name.split_once('<')?.1.trim_end_matches('>').to_string()))
        .collect::<Vec<_>>();
    assert!(tags.contains(&"hello__world_".to_string()), "{tags:?}");
    assert!(tags.contains(&"abcdefghijklmnop".to_string()), "{tags:?}");
}

/// As on JavaScript.
#[test]
fn inspect_utf_codepoint() {
    run_ok(
        r##"
@external(webassembly, "builtins", "_unsafe_int_to_utf_codepoint")
fn utf_codepoint(a: Int) -> UtfCodepoint

@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

pub fn main() {
    let a = utf_codepoint(65)
    let e = utf_codepoint(0xE9)
    let smile = utf_codepoint(0x1F600)
    let nul = utf_codepoint(0)
    let assert "//utfcodepoint(A)" = inspect(a)
    let assert "//utfcodepoint(é)" = inspect(e)
    let assert "//utfcodepoint(😀)" = inspect(smile)
    let assert "//utfcodepoint(\u{0000})" = inspect(nul)
    let assert "[//utfcodepoint(A), //utfcodepoint(é)]" = inspect([a, e])
    let assert "#(//utfcodepoint(😀), 1)" = inspect(#(smile, 1))
}
"##,
    );
}

#[test]
fn string_const() {
    run_ok(
        r#"
const mystring = "string"

pub fn main() {
    let assert "string" = mystring
}
"#,
    );
}

#[test]
fn string_concat() {
    run_ok(
        r#"
const mystring = "string"

pub fn main() {
    let assert "ab cd" = "ab" <> " " <> "cd"
    let assert "string" = mystring <> ""
    let assert "string" = "" <> mystring
    let assert "string-string" = mystring <> "-" <> "string"
}
"#,
    );
}

#[test]
fn string_concat_in_place() {
    run_ok(
        r#"
fn repeat(s: String, n: Int, acc: String) -> String {
    case n {
        0 -> acc
        _ -> repeat(s, n - 1, acc <> s)
    }
}

pub fn main() {
    // `c` is written after `a`, in its bytes, and `d` is not
    let a = "a" <> "b"
    let c = a <> "c"
    let d = a <> "d"
    let assert "abc" = c
    let assert "abd" = d
    let assert "ab" = a
    let assert "abcx" = c <> "x"
    let assert "abce" = c <> "e"
    let assert "abdy" = d <> "y"
    // a big slice that ends where the bytes are written
    let long = repeat("0123456789", 100, "")
    let assert "0123456789" <> rest = long
    let r = rest <> "!"
    let l = long <> "?"
    let assert True = r == repeat("0123456789", 99, "") <> "!"
    let assert True = l == repeat("0123456789", 100, "") <> "?"
    let assert True = rest == repeat("0123456789", 99, "")
}
"#,
    );
}

#[test]
fn string_comparison() {
    run_ok(
        r#"
pub fn main() {
    let assert True = "abc" == "ab" <> "c"
    let assert False = "ab" == "cd" <> "a"
    let assert True = "ab" != "cd" <> "a"
    let assert False = "ab" != "a" <> "b"
}
"#,
    );
}

#[test]
fn string_escape() {
    let src = r#"
pub fn main() {
    echo "\" \n \r \t \f \\ "
}
"#;
    assert_wasm_echo!(src);
    assert_eq!(run_wasm(src, vec![]).stderr, run_wasm_js(src, &[]).stderr);
}

#[test]
fn string_global_const() {
    run_ok(
        r#"
pub const s = "a"

pub fn main() {
    let assert "a" = s
}
"#,
    );
}

#[test]
fn string_prefix_basic() {
    run_ok(
        r#"
pub fn main() {
    assert case "Hello, World" {
        "Hello, " <> name -> name == "World"
        _ -> False
    }
}
"#,
    );
}

#[test]
fn string_prefix_empty() {
    run_ok(
        r#"
pub fn main() {
    assert case "anything" {
        "" <> rest -> rest == "anything"
        _ -> False
    }
}
"#,
    );
}

#[test]
fn string_prefix_with_assignment() {
    run_ok(
        r#"
pub fn main() {
    assert case "Hello, World" {
        "Hello, " as greeting <> name -> greeting == "Hello, " && name == "World"
        _ -> False
    }
}
"#,
    );
}

#[test]
fn string_prefix_no_match() {
    run_ok(
        r#"
pub fn main() {
    assert case "Goodbye" {
        "Hello, " <> _ -> False
        _ -> True
    }
}
"#,
    );
}
