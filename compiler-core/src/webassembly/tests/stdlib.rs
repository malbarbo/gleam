// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Compiler builtins used by the standard library.

use super::{compile_wasm, for_each_operator, run_ok, run_ok_i64, run_wasm_with_config};

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

/// The amounts are variables, so the builtin functions are called.
#[test]
fn int_shift_by_variable() {
    run_ok(&format!(
        r#"{INT}
pub fn main() {{
    let #(a, b, c, d, e, f) = #(2, -2, 32, 1000, -32, -2147483648)
    let assert 48 = shl(12, a)
    let assert 3 = shr(12, a)
    let assert -4 = shr(-13, a)
    let assert 3 = shl(12, b)
    let assert 48 = shr(12, b)
    let assert 0 = shl(1, c)
    let assert 0 = shr(12, c)
    let assert -1 = shr(-12, c)
    let assert -1 = shr(-12, d)
    let assert 0 = shl(12, e)
    let assert -1 = shl(-12, f)
    let assert 0 = shr(12, f)
    let assert 48 = {{ shl }}(12, 2)
}}
"#
    ));
}

/// Builtins of a few instructions and shifts by a literal are emitted in
/// place of the call.
#[test]
fn builtins_are_inlined() {
    let wasm = compile_wasm(
        &format!(
            r#"{INT}
pub fn main() {{
    let a = 12
    let b = and(a, 10) + or(a, 10) + xor(a, 10) + not(a)
    let c = shl(a, 2) + shr(a, -3) + shl(a, 40) + shr(a, 40)
    to_float(b + c)
}}
"#
        ),
        vec![],
    );
    for_each_operator(&wasm, |function, op| {
        if function == "main" {
            assert!(
                !matches!(op, wasmparser::Operator::Call { .. }),
                "main calls a builtin"
            );
        }
    });
}

#[test]
fn division_by_literal() {
    run_ok(
        r#"
pub fn main() {
    let #(a, b, x) = #(-7, 7, -2147483648)
    let assert -2 = a / 3
    let assert -1 = a % 3
    let assert 2 = b / 3
    let assert 1 = b % -3
    let assert 0 = x % -1
    let assert 1073741824 = x / -2
    let assert -3.5 = -7.0 /. 2.0
    let f = 7.0
    let assert -3.5 = f /. -2.0
    let assert 0 = b / 0
    let assert 0 = b % 0
    let assert 0.0 = f /. 0.0
}
"#,
    );
}

/// `1.0e-50` is 0 as an `f32`, so the division gives 0 too.
#[test]
fn division_by_literal_zero_in_f32() {
    let result = run_wasm_with_config(
        r#"
pub fn main() {
    let f = 7.0
    let assert 0.0 = f /. 1.0e-50
}
"#,
        vec![],
        super::wasm_config(crate::config::WasmInt::I32, crate::config::WasmFloat::F32),
    );
    assert!(result.status.success(), "{}", result.stderr);
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
@external(webassembly, "builtins", "_string_codeunit_slice")
fn codeunit_slice(a: String, start: Int, length: Int) -> String
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
    // the code units are the bytes
    let assert "á" = codeunit_slice("olá", 2, 2)
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

#[test]
fn string_views() {
    // Slices bigger than 64 bytes and a quarter of the original share its
    // bytes; every operation must respect their offset.
    run_ok(&format!(
        r#"{STRING}
pub fn main() {{
    let s = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz"
    let view = byte_slice(s, 10, 80)
    let assert 80 = byte_size(view)
    let assert True = view == "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqr"
    let assert True = starts_with(view, "abc")
    let assert True = ends_with(view, "pqr")
    let assert #(97, rest) = pop_codeunit(view)
    let assert 79 = byte_size(rest)
    let assert True = starts_with(rest, "bcd")
    let assert "bcd" = byte_slice(rest, 0, 3)
    let assert "abc!" = byte_slice(view, 0, 3) <> "!"
    let assert "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqr" = view
    let assert "abc" <> tail = view
    let assert True = starts_with(tail, "defg")
    let assert True = less_than(view, rest)
    let assert "x-abc-qr" = join(["x", byte_slice(view, 0, 3), byte_slice(view, 78, 2)], "-")
    let assert "bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqr" = remove_prefix(view, "a")
    let assert "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopq" = remove_suffix(view, "r")
}}
"#
    ));
}

#[test]
fn string_views_echo() {
    let output = super::run_wasm(
        &format!(
            r#"{STRING}
@external(webassembly, "builtins", "_inspect")
fn inspect(a: a) -> String

pub fn main() {{
    let s = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz"
    let view = byte_slice(s, 10, 80)
    echo view
    echo #(byte_slice(view, 70, 10), inspect(view) == "\"" <> view <> "\"")
}}
"#
        ),
        vec![],
    );
    assert!(output.status.success(), "{}", output.stderr);
    assert!(
        output.stderr.contains(
            "\"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqr\""
        ),
        "{}",
        output.stderr
    );
    assert!(
        output.stderr.contains("#(\"ijklmnopqr\", True)"),
        "{}",
        output.stderr
    );
}

#[test]
fn hash() {
    run_ok(&format!(
        r#"{STRING}
@external(webassembly, "builtins", "_hash")
fn hash(a: a) -> Int

type Shape {{
    Circle(Float)
    Rect(width: Int, height: Int)
    Empty
}}

pub fn main() {{
    let s = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz"
    let view = byte_slice(s, 10, 80)
    let copy = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqr"
    let assert True = hash(view) == hash(copy)
    let assert False = hash("a") == hash("b")
    let assert True = hash(1) == hash(1)
    let assert False = hash(1) == hash(2)
    let assert True = hash(0.0) == hash(-0.0)
    let assert False = hash(1.0) == hash(2.0)
    let assert True = hash([1, 2, 3]) == hash([1, 2, 3])
    let assert False = hash([1, 2, 3]) == hash([1, 3, 2])
    let assert False = hash([]) == hash([0])
    let assert True = hash(#(1, "a", [view])) == hash(#(1, "a", [copy]))
    let assert False = hash(#(1, 2)) == hash(#(2, 1))
    let assert True = hash(Rect(1, 2)) == hash(Rect(width: 1, height: 2))
    let assert False = hash(Rect(1, 2)) == hash(Rect(2, 1))
    let assert False = hash(Empty) == hash(Circle(0.0))
    let assert True = hash(Ok(Empty)) == hash(Ok(Empty))
    let assert False = hash(Ok(1)) == hash(Error(1))
    let assert True = hash(True) == hash(True)
    let assert False = hash(True) == hash(False)
    let assert True = hash(fn(x) {{ x + 1 }}) == hash(fn(x) {{ x }})
}}
"#
    ));
}

/// With a 64 bit `Int`, both halves of an `Int` and of a `Float` are hashed.
#[test]
fn hash_i64() {
    run_ok_i64(
        r#"
@external(webassembly, "builtins", "_hash")
fn hash(a: a) -> Int

pub fn main() {
    let assert False = hash(1) == hash(2)
    let assert False = hash(1) == hash(7919)
    let assert False = hash(0) == hash(4294967296)
    let assert False = hash(-1) == hash(4294967295)
    let assert False = hash(1.0) == hash(2.0)
    let assert True = hash(0.0) == hash(-0.0)
}
"#,
    );
}

/// The external floats hash `-0.0` like `0.0`, as they are equal.
#[test]
fn hash_external_floats_negative_zero() {
    run_ok(
        r#"
@external(webassembly, "builtins", "F32")
type F32

@external(webassembly, "builtins", "F64")
type F64

@external(webassembly, "builtins", "_float_to_f32")
fn to_f32(value: Float) -> F32

@external(webassembly, "builtins", "_float_to_f64")
fn to_f64(value: Float) -> F64

@external(webassembly, "builtins", "_hash")
fn hash(a: a) -> Int

pub fn main() {
    let assert True = to_f32(0.0) == to_f32(-0.0)
    let assert True = hash(to_f32(0.0)) == hash(to_f32(-0.0))
    let assert True = to_f64(0.0) == to_f64(-0.0)
    let assert True = hash(to_f64(0.0)) == hash(to_f64(-0.0))
    let assert False = hash(to_f64(1.0)) == hash(to_f64(2.0))
}
"#,
    );
}
