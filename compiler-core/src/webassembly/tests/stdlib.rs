// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Compiler builtins used by the standard library.

use super::{run_ok, run_ok_i64};

const INT: &str = r#"
@external(webassembly, "builtins", "_int_bitwise_and")
fn and(a: Int, b: Int) -> Int
@external(webassembly, "builtins", "_int_bitwise_or")
fn or(a: Int, b: Int) -> Int
@external(webassembly, "builtins", "_int_bitwise_exclusive_or")
fn xor(a: Int, b: Int) -> Int
@external(webassembly, "builtins", "_int_bitwise_not")
fn not(a: Int) -> Int
@external(webassembly, "builtins", "_int_bitwise_shift_left")
fn shl(a: Int, b: Int) -> Int
@external(webassembly, "builtins", "_int_bitwise_shift_right")
fn shr(a: Int, b: Int) -> Int
@external(webassembly, "builtins", "_int_to_float")
fn to_float(a: Int) -> Float
"#;

#[test]
fn int_bitwise() {
    run_ok(&format!(
        r#"{INT}
pub fn main() {{
    let assert 8 = and(12, 10)
    let assert 14 = or(12, 10)
    let assert 6 = xor(12, 10)
    let assert -13 = not(12)
    let assert 48 = shl(12, 2)
    let assert 3 = shr(12, 2)
    let assert -4 = shr(-13, 2)
    let assert 3 = shl(12, -2)
    let assert 48 = shr(12, -2)
    let assert 0 = shl(1, 32)
    let assert 0 = shl(1, 100)
    let assert 0 = shr(12, 32)
    let assert -1 = shr(-12, 32)
    let assert -1 = shr(-12, 1000)
    let assert 0 = shl(12, -32)
    let assert -1 = shl(-12, -2147483648)
    let assert 0 = shr(12, -2147483648)
    let assert -2147483648 = shl(1, 31)
    let assert 3.0 = to_float(3)
    let assert -2147483648.0 = to_float(-2147483648)
}}
"#
    ));
}

#[test]
fn int_bitwise_i64() {
    run_ok_i64(&format!(
        r#"{INT}
pub fn main() {{
    let assert 4294967296 = shl(1, 32)
    let assert 0 = shl(1, 64)
    let assert -1 = shr(-1, 64)
    let assert 1 = shr(4294967296, 32)
    let assert 4294967296.0 = to_float(4294967296)
}}
"#
    ));
}

#[test]
fn int_division_does_not_trap() {
    run_ok(
        r#"
pub fn main() {
    let zero = 0
    let minus_one = -1
    let min = -2147483648
    let assert 0 = 7 % zero
    let assert 0 = 7 / zero
    let assert -2147483648 = min / minus_one
    let assert 0 = min % minus_one
    let assert -2 = -7 / 3
    let assert -1 = -7 % 3
    let assert True = case 1 {
        _ if 7 % zero == 0 -> True
        _ -> False
    }
}
"#,
    );
}

#[test]
fn int_division_does_not_trap_i64() {
    run_ok_i64(
        r#"
pub fn main() {
    let zero = 0
    let minus_one = -1
    let min = -9223372036854775808
    let assert 0 = 7 % zero
    let assert -9223372036854775808 = min / minus_one
    let assert 0 = min % minus_one
}
"#,
    );
}

const FLOAT: &str = r#"
@external(webassembly, "builtins", "_float_floor")
fn floor(a: Float) -> Float
@external(webassembly, "builtins", "_float_ceiling")
fn ceiling(a: Float) -> Float
@external(webassembly, "builtins", "_float_truncate")
fn truncate(a: Float) -> Int
@external(webassembly, "builtins", "_float_round")
fn round(a: Float) -> Int
@external(webassembly, "builtins", "_float_square_root")
fn sqrt(a: Float) -> Float
"#;

#[test]
fn float_rounding() {
    run_ok(&format!(
        r#"{FLOAT}
pub fn main() {{
    let assert 1.0 = floor(1.7)
    let assert -2.0 = floor(-1.2)
    let assert 2.0 = ceiling(1.2)
    let assert -1.0 = ceiling(-1.7)
    let assert 1 = truncate(1.7)
    let assert -1 = truncate(-1.7)
    let assert 2147483647 = truncate(1.0e20)
    let assert -2147483648 = truncate(-1.0e20)
    let assert 2 = round(1.5)
    let assert 3 = round(2.5)
    let assert -2 = round(-1.5)
    let assert 1 = round(1.4999)
    let assert 0 = round(0.49999999999999994)
    let assert 0 = round(-0.4)
    let assert 3.0 = sqrt(9.0)
}}
"#
    ));
}

#[test]
fn float_round_i64() {
    run_ok_i64(&format!(
        r#"{FLOAT}
pub fn main() {{
    let assert 4503599627370497 = round(4503599627370497.0)
    let assert 100000000000 = truncate(1.0e11)
}}
"#
    ));
}

const STRING: &str = r#"
@external(webassembly, "builtins", "_string_byte_size")
fn byte_size(a: String) -> Int
@external(webassembly, "builtins", "_string_byte_slice")
fn byte_slice(a: String, start: Int, length: Int) -> String
@external(webassembly, "builtins", "_string_pop_codeunit")
fn pop_codeunit(a: String) -> #(Int, String)
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
fn join(a: List(String), b: String) -> String
@external(webassembly, "builtins", "_utf_codepoint_to_int")
fn codepoint_to_int(a: UtfCodepoint) -> Int
@external(webassembly, "builtins", "_unsafe_int_to_utf_codepoint")
fn int_to_codepoint(a: Int) -> UtfCodepoint
"#;

#[test]
fn string_bytes() {
    run_ok(&format!(
        r#"{STRING}
pub fn main() {{
    let assert 0 = byte_size("")
    let assert 6 = byte_size("héllo")
    let assert "ll" = byte_slice("hello", 2, 2)
    let assert "" = byte_slice("hello", 5, 0)
    let assert #(104, "ello") = pop_codeunit("hello")
    let assert #(0, "") = pop_codeunit("")
    let assert True = starts_with("hello", "he")
    let assert True = starts_with("hello", "")
    let assert False = starts_with("he", "hello")
    let assert True = ends_with("hello", "llo")
    let assert True = ends_with("hello", "")
    let assert False = ends_with("hello", "he")
    let assert False = ends_with("lo", "hello")
    let assert "llo" = remove_prefix("hello", "he")
    let assert "hello" = remove_prefix("hello", "x")
    let assert "he" = remove_suffix("hello", "llo")
    let assert "hello" = remove_suffix("hello", "x")
    let assert True = less_than("abc", "abd")
    let assert True = less_than("ab", "abc")
    let assert False = less_than("abc", "abc")
    let assert False = less_than("b", "abc")
    let assert True = less_than("", "a")
    let assert False = less_than("a", "")
    let assert True = less_than("z", "é")
    let assert 233 = codepoint_to_int(int_to_codepoint(233))
}}
"#
    ));
}

#[test]
fn string_join() {
    run_ok(&format!(
        r#"{STRING}
pub fn main() {{
    let assert "" = concat([])
    let assert "abc" = concat(["a", "", "bc"])
    let assert "" = join([], ", ")
    let assert "a" = join(["a"], ", ")
    let assert "a, b, c" = join(["a", "b", "c"], ", ")
    let assert ", b" = join(["", "b"], ", ")
    let assert ", " = join(["", ""], ", ")
    let assert "ab" = join(["a", "b"], "")
}}
"#
    ));
}
