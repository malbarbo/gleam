// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `string = "JSString"`: the strings are JavaScript strings, so the programs
//! run in Node.js, which also checks the names and the types of the builtins
//! when it instantiates them.

use super::{WasmOutput, run_wasm_js};

#[track_caller]
fn run_ok(src: &str, files: &[(&str, &str)]) -> WasmOutput {
    let result = run_wasm_js(src, files);
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
    result
}

const BUILTINS: &str = r#"
@external(webassembly, "builtins", "_string_byte_size")
fn byte_size(a: String) -> Int
@external(webassembly, "builtins", "_string_byte_slice")
fn byte_slice(a: String, start: Int, len: Int) -> String
@external(webassembly, "builtins", "_string_codeunit_slice")
fn codeunit_slice(a: String, start: Int, len: Int) -> String
@external(webassembly, "builtins", "_string_pop_codeunit")
fn pop_codeunit(a: String) -> #(Int, String)
@external(webassembly, "builtins", "_string_get_byte")
fn get_byte(a: String, i: Int) -> Result(Int, Nil)
@external(webassembly, "builtins", "_string_starts_with")
fn starts_with(a: String, b: String) -> Bool
@external(webassembly, "builtins", "_string_ends_with")
fn ends_with(a: String, b: String) -> Bool
@external(webassembly, "builtins", "_string_remove_prefix")
fn remove_prefix(a: String, b: String) -> String
@external(webassembly, "builtins", "_string_remove_suffix")
fn remove_suffix(a: String, b: String) -> String
@external(webassembly, "builtins", "_string_less_than")
fn less_than(a: String, b: String) -> Bool
@external(webassembly, "builtins", "_string_concat_list")
fn concat(a: List(String)) -> String
@external(webassembly, "builtins", "_string_join")
fn join(a: List(String), separator: String) -> String
@external(webassembly, "builtins", "_string_pop_grapheme")
fn pop_grapheme(a: String) -> Result(#(String, String), Nil)
@external(webassembly, "builtins", "_inspect")
fn inspect(a: a) -> String
@external(webassembly, "builtins", "_hash")
fn hash(a: a) -> Int
@external(webassembly, "builtins", "_parse_int")
fn parse_int(a: String) -> Result(Int, Nil)
@external(webassembly, "builtins", "string_lowercase")
fn lowercase(a: String) -> String
"#;

#[test]
fn literals_and_constants() {
    let _ = run_ok(
        &format!(
            r#"{BUILTINS}
const greeting = "olá"
const both = greeting <> ", mundo"
const pair = #("a", "😀")

pub fn main() {{
    let assert "olá, mundo" = both
    let assert True = both == "olá" <> ", " <> "mundo"
    let assert False = both == "olá, mundo!"
    let assert #("a", "😀") = pair
    let assert "" = ""
    let assert 5 = byte_size("a😀")
    let assert 4 = byte_size(greeting)
}}
"#
        ),
        &[],
    );
}

#[test]
fn patterns() {
    let _ = run_ok(
        r#"
fn rest(s: String) -> String {
    case s {
        "😀" <> rest -> "smile " <> rest
        "olá" <> rest -> rest
        "abc" -> "exactly abc"
        _ -> "other"
    }
}

pub fn main() {
    let assert "smile !" = rest("😀!")
    let assert ", mundo" = rest("olá, mundo")
    let assert "exactly abc" = rest("abc")
    let assert "other" = rest("ab")
    let assert "" = rest("olá")
}
"#,
        &[],
    );
}

#[test]
fn operations() {
    let _ = run_ok(
        &format!(
            r#"{BUILTINS}
pub fn main() {{
    let s = "a😀é"
    let assert True = starts_with(s, "a😀")
    let assert False = starts_with(s, "😀")
    let assert False = starts_with("a", "ab")
    let assert True = ends_with(s, "😀é")
    let assert False = ends_with("é", "aé")
    let assert "é" = remove_prefix(s, "a😀")
    let assert "a😀é" = remove_prefix(s, "b")
    let assert "a" = remove_suffix(s, "😀é")
    let assert True = less_than("a", "b")
    let assert True = less_than("a", "ab")
    let assert False = less_than("b", "a")
    let assert False = less_than("a", "a")
    // in the order of the code points, not of the UTF-16 units
    let assert True = less_than("\u{{FF61}}", "😀")
    let assert False = less_than("😀", "\u{{FF61}}")
    let assert True = less_than("a😀", "a😁")
    let assert True = less_than("\u{{D7FF}}", "😀")
    let assert "a😀bc" = concat(["a", "😀", "b", "c"])
    let assert "" = concat([])
    let assert "a, 😀, b" = join(["a", "😀", "b"], ", ")
    let assert "a" = join(["a"], ", ")
    let assert "" = join([], ", ")
    let assert "😀" = byte_slice(s, 1, 4)
    let assert "é" = byte_slice(s, 5, 2)
    let assert #(97, "😀é") = pop_codeunit(s)
    let assert #(0, "") = pop_codeunit("")
    // the code units are UTF-16 units, so a pair is split
    let assert #(0xE9, "") = pop_codeunit("é")
    let assert #(0xD83D, rest) = pop_codeunit("😀é")
    let assert #(0xDE00, "é") = pop_codeunit(rest)
    let assert "😀" = codeunit_slice(s, 1, 2)
    let assert "é" = codeunit_slice(s, 3, 1)
    let assert Ok(0xF0) = get_byte(s, 1)
    let assert Error(Nil) = get_byte(s, 7)
    let assert Ok(-42) = parse_int("-42")
    let assert "\"a😀é\\n\"" = inspect(s <> "\n")
    let assert True = hash(s) == hash("a😀" <> "é")
    let assert "olá 😀" = lowercase("OLÁ 😀")
}}
"#
        ),
        &[],
    );
}

#[test]
fn pop_grapheme() {
    let _ = run_ok(
        &format!(
            r#"{BUILTINS}
fn graphemes(s: String, acc: List(String)) -> List(String) {{
    case pop_grapheme(s) {{
        Ok(#(g, rest)) -> graphemes(rest, [g, ..acc])
        Error(Nil) -> acc
    }}
}}

fn repeat(s: String, n: Int) -> String {{
    case n {{
        0 -> ""
        _ -> s <> repeat(s, n - 1)
    }}
}}

pub fn main() {{
    let assert ["e\u{{301}}", "👨‍👩‍👧", "a"] = graphemes("a👨‍👩‍👧e\u{{301}}", [])
    let assert Error(Nil) = pop_grapheme("")
    // a grapheme across the first windows, and a pair at their end
    let long = "e" <> repeat("\u{{301}}", 100)
    let assert Ok(#(g, "x")) = pop_grapheme(long <> "x")
    let assert True = g == long
    let pairs = "a" <> repeat("😀", 40)
    let assert 41 = graphemes(pairs, []) |> count(0)
}}

fn count(l: List(a), n: Int) -> Int {{
    case l {{
        [] -> n
        [_, ..rest] -> count(rest, n + 1)
    }}
}}
"#
        ),
        &[],
    );
}

/// The output goes through the UTF-8 bytes of the strings.
#[test]
fn echo() {
    let result = run_ok(
        r#"
pub fn main() {
    echo "olá 😀"
    echo #("a", ["b"])
}
"#,
        &[],
    );
    assert!(result.stderr.contains("\"olá 😀\""), "{}", result.stderr);
    assert!(
        result.stderr.contains("#(\"a\", [\"b\"])"),
        "{}",
        result.stderr
    );
}

/// A string crosses to the host as is, and a value that the host returns as
/// a `String` is checked to be one.
#[test]
fn host() {
    let host = r#"
export function shout(s) { return s.toUpperCase() + "!"; }
export function length(s) { return s.length; }
export function not_a_string() { return 42; }
"#;
    let _ = run_ok(
        r#"
@external(webassembly, "./host_strings.mjs", "shout")
fn shout(s: String) -> String
@external(webassembly, "./host_strings.mjs", "length")
fn length(s: String) -> Int

pub fn main() {
    let assert "OLÁ 😀!" = shout("olá 😀")
    let assert 3 = length("a😀")
}
"#,
        &[("host_strings.mjs", host)],
    );
    let result = run_wasm_js(
        r#"
@external(webassembly, "./host_strings.mjs", "not_a_string")
fn not_a_string() -> String

pub fn main() {
    not_a_string()
}
"#,
        &[("host_strings.mjs", host)],
    );
    assert!(!result.status.success());
    assert!(
        result
            .stderr
            .contains("External function did not return a String: ./host_strings.mjs.not_a_string"),
        "{}",
        result.stderr
    );
}

/// A lone surrogate from the host becomes U+FFFD in the UTF-8 bytes, and a
/// high surrogate followed by a low one is a pair.
#[test]
fn lone_surrogate() {
    let host = r#"
export function lone() { return "a\uD800b"; }
export function mixed() { return "\uD800\uD83D\uDE00\uDE00"; }
"#;
    let _ = run_ok(
        &format!(
            r#"{BUILTINS}
@external(webassembly, "./host_strings.mjs", "lone")
fn lone() -> String
@external(webassembly, "./host_strings.mjs", "mixed")
fn mixed() -> String

pub fn main() {{
    let assert 5 = byte_size(lone())
    let assert "a\u{{FFFD}}b" = byte_slice(lone(), 0, 5)
    let assert 10 = byte_size(mixed())
    let assert "\u{{FFFD}}😀\u{{FFFD}}" = byte_slice(mixed(), 0, 10)
}}
"#
        ),
        &[("host_strings.mjs", host)],
    );
}
