// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `@external(webassembly, "rust", ...)`: glue to the functions of
//! builtins-wasm.

use super::{run_ok, run_ok_i64};

const RUST: &str = r#"
@external(webassembly, "rust", "int_parse")
fn int_parse(a: String) -> Result(Int, Nil)
@external(webassembly, "rust", "int_to_base_string")
fn to_base_string(a: Int, b: Int) -> String
@external(webassembly, "rust", "float_parse")
fn float_parse(a: String) -> Result(Float, Nil)
@external(webassembly, "rust", "float_to_string")
fn float_to_string(a: Float) -> String
@external(webassembly, "rust", "float_power")
fn power(a: Float, b: Float) -> Float
@external(webassembly, "rust", "string_length")
fn length(a: String) -> Int
@external(webassembly, "rust", "string_lowercase")
fn lowercase(a: String) -> String
@external(webassembly, "rust", "string_trim")
fn trim(a: String) -> String
@external(webassembly, "rust", "string_contains")
fn contains(a: String, b: String) -> Bool
@external(webassembly, "rust", "string_split")
fn split(a: String, b: String) -> List(String)
@external(webassembly, "rust", "string_split_once")
fn split_once(a: String, b: String) -> Result(#(String, String), Nil)
@external(webassembly, "rust", "string_replace")
fn replace(a: String, b: String, c: String) -> String
@external(webassembly, "rust", "string_to_graphemes")
fn to_graphemes(a: String) -> List(String)
@external(webassembly, "rust", "string_to_utf_codepoints")
fn to_utf_codepoints(a: String) -> List(UtfCodepoint)
@external(webassembly, "rust", "string_from_utf_codepoints")
fn from_utf_codepoints(a: List(UtfCodepoint)) -> String
@external(webassembly, "rust", "string_inspect")
fn string_inspect(a: String) -> String
@external(webassembly, "rust", "uri_parse_query")
fn parse_query(a: String) -> Result(List(#(String, String)), Nil)
@external(webassembly, "builtins", "_string_byte_slice")
fn byte_slice(a: String, start: Int, length: Int) -> String
"#;

#[test]
fn rust_numbers() {
    run_ok(&format!(
        r#"{RUST}
pub fn main() {{
    let assert Ok(42) = int_parse("+42")
    let assert Ok(-7) = int_parse("-007")
    let assert Error(Nil) = int_parse("1_000")
    let assert Error(Nil) = int_parse("2147483648")
    let assert Ok(-2147483648) = int_parse("-2147483648")
    let assert "-1C" = to_base_string(-28, 16)
    let assert Ok(1.5) = float_parse("1.5")
    let assert Error(Nil) = float_parse("1")
    let assert Error(Nil) = float_parse("1.0e400")
    let assert "100000.0" = float_to_string(100000.0)
    let assert "1.0e21" = float_to_string(1.0e21)
    let assert "1.5e-7" = float_to_string(1.5e-7)
    let assert "0.1" = float_to_string(0.1)
    let assert 8.0 = power(2.0, 3.0)
}}
"#
    ));
}

#[test]
fn rust_numbers_i64() {
    run_ok_i64(&format!(
        r#"{RUST}
pub fn main() {{
    let assert Ok(2147483648) = int_parse("2147483648")
    let assert Error(Nil) = int_parse("9223372036854775808")
}}
"#
    ));
}

#[test]
fn rust_strings() {
    run_ok(&format!(
        r#"{RUST}
pub fn main() {{
    let assert 2 = length("e\u{{0301}}x")
    let assert "οδος" = lowercase("ΟΔΟΣ")
    let assert "a b" = trim(" \ta b\n")
    let assert True = contains("hello", "ll")
    let assert False = contains("hello", "x")
    let assert ["a", "b", "", "c"] = split("a,b,,c", ",")
    let assert ["a", "é", "b"] = split("aéb", "")
    let assert Ok(#("k", "v=1")) = split_once("k=v=1", "=")
    let assert Error(Nil) = split_once("kv", "=")
    let assert "h3ll0" = replace(replace("hello", "e", "3"), "o", "0")
    let assert ["e\u{{0301}}", "x"] = to_graphemes("e\u{{0301}}x")
    let assert "aé" = from_utf_codepoints(to_utf_codepoints("aé"))
    let assert "\"a\\nb\"" = string_inspect("a\nb")
    let assert Ok([#("a", "1"), #("b", "x y")]) = parse_query("a=1&b=x+y")
    let assert Error(Nil) = parse_query("a=%ZZ")
}}
"#
    ));
}

#[test]
fn rust_with_views() {
    run_ok(&format!(
        r#"{RUST}
pub fn main() {{
    let s = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ,abcdefghijklmnopqrstuvwxyz,0123456789abcdefghijklmnopqrstuvwxyz"
    let view = byte_slice(s, 10, 80)
    let assert 80 = length(view)
    let assert ["ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", "0123456789abcdefghijklmnop"] = split(view, ",")
    let assert "abcdefghijklmnopqrstuvwxyz,abcdefghijklmnopqrstuvwxyz,0123456789abcdefghijklmnop" = lowercase(view)
}}
"#
    ));
}

#[test]
fn pop_grapheme() {
    run_ok(
        r#"
@external(webassembly, "builtins", "_string_pop_grapheme")
fn pop_grapheme(a: String) -> Result(#(String, String), Nil)
@external(webassembly, "rust", "string_length")
fn length(a: String) -> Int

fn count(s: String, n: Int) -> Int {
    case pop_grapheme(s) {
        Ok(#(_, rest)) -> count(rest, n + 1)
        Error(Nil) -> n
    }
}

pub fn main() {
    let assert Error(Nil) = pop_grapheme("")
    let assert Ok(#("a", "bc")) = pop_grapheme("abc")
    let assert Ok(#("e\u{0301}", "x")) = pop_grapheme("e\u{0301}x")
    let assert Ok(#("👨‍👩‍👧", "!")) = pop_grapheme("👨‍👩‍👧!")
    // a grapheme bigger than the first window of 64 bytes
    let big = "e\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}\u{0301}"
    let assert Ok(#(first, "z")) = pop_grapheme(big <> "z")
    let assert True = first == big
    let s = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzé👍🏽e\u{0301}"
    let assert True = count(s, 0) == length(s)
    let assert 101 = count(s, 0)
}
"#,
    );
}

#[test]
fn unused_builtins_are_removed() {
    // The Rust exports and their data are not roots of the program.
    let size = super::compile_wasm("pub fn main() { 1 }", vec![]).len();
    assert!(size < 1000, "{size} bytes");
}
