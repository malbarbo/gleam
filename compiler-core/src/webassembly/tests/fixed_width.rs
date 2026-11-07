// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! The operations of `I32` and `I64`, which wrap with any `Int` and with
//! `int_overflow = "fail"`.

use super::{TEST_PACKAGE, run_wasm_in, wasm_config};
use crate::config::{WasmFloat, WasmInt, WasmIntOverflow, WebAssemblyConfig};

const I32: &str = r#"
type I32

@external(webassembly, "builtins", "_int_to_i32")
fn i32(n: Int) -> I32
@external(webassembly, "builtins", "_i32_to_int")
fn int(n: I32) -> Int
@external(webassembly, "builtins", "_i32_add")
fn add(a: I32, b: I32) -> I32
@external(webassembly, "builtins", "_i32_sub")
fn sub(a: I32, b: I32) -> I32
@external(webassembly, "builtins", "_i32_mul")
fn mul(a: I32, b: I32) -> I32
@external(webassembly, "builtins", "_i32_and")
fn and(a: I32, b: I32) -> I32
@external(webassembly, "builtins", "_i32_or")
fn or(a: I32, b: I32) -> I32
@external(webassembly, "builtins", "_i32_xor")
fn xor(a: I32, b: I32) -> I32
@external(webassembly, "builtins", "_i32_shift_left")
fn shift_left(a: I32, bits: Int) -> I32
@external(webassembly, "builtins", "_i32_shift_right")
fn shift_right(a: I32, bits: Int) -> I32
@external(webassembly, "builtins", "_i32_shift_right_unsigned")
fn shift_right_unsigned(a: I32, bits: Int) -> I32
"#;

const I64: &str = r#"
type I64

@external(webassembly, "builtins", "_int_to_i64")
fn i64(n: Int) -> I64
@external(webassembly, "builtins", "_i64_to_int")
fn int(n: I64) -> Int
@external(webassembly, "builtins", "_i64_add")
fn add(a: I64, b: I64) -> I64
@external(webassembly, "builtins", "_i64_sub")
fn sub(a: I64, b: I64) -> I64
@external(webassembly, "builtins", "_i64_mul")
fn mul(a: I64, b: I64) -> I64
@external(webassembly, "builtins", "_i64_and")
fn and(a: I64, b: I64) -> I64
@external(webassembly, "builtins", "_i64_or")
fn or(a: I64, b: I64) -> I64
@external(webassembly, "builtins", "_i64_xor")
fn xor(a: I64, b: I64) -> I64
@external(webassembly, "builtins", "_i64_shift_left")
fn shift_left(a: I64, bits: Int) -> I64
@external(webassembly, "builtins", "_i64_shift_right")
fn shift_right(a: I64, bits: Int) -> I64
@external(webassembly, "builtins", "_i64_shift_right_unsigned")
fn shift_right_unsigned(a: I64, bits: Int) -> I64
"#;

/// `src` runs with `int` and `int_overflow = "fail"`, which the operations
/// do not check.
#[track_caller]
fn runs(src: &str, int: WasmInt) {
    let config = WebAssemblyConfig {
        int_overflow: WasmIntOverflow::Fail,
        ..wasm_config(int, WasmFloat::F64)
    };
    let result = run_wasm_in(TEST_PACKAGE, src, vec![], config);
    assert!(
        result.status.success(),
        "{int:?} failed:\n{}",
        result.stderr
    );
}

#[test]
fn i32_operations() {
    let src = format!(
        "{I32}{}",
        r#"
pub fn main() {
  let max = i32(2147483647)
  let min = i32(-2147483648)
  assert int(add(max, i32(1))) == -2147483648
  assert int(sub(min, i32(1))) == 2147483647
  assert int(mul(i32(0x6d2b79f5), i32(0x6d2b79f5))) == -731413383
  assert int(and(i32(0b1100), i32(0b1010))) == 0b1000
  assert int(or(i32(0b1100), i32(0b1010))) == 0b1110
  assert int(xor(i32(0b1100), i32(0b1010))) == 0b0110
  assert int(shift_left(i32(1), 31)) == -2147483648
  assert int(shift_right(i32(-8), 1)) == -4
  assert int(shift_right_unsigned(i32(-8), 1)) == 2147483644
}
"#
    );
    runs(&src, WasmInt::I32);
    runs(&src, WasmInt::I64);
}

/// wasm takes the amount of a shift modulo the width.
#[test]
fn i32_shift_amount() {
    let src = format!(
        "{I32}{}",
        r#"
pub fn main() {
  assert int(shift_left(i32(1), 33)) == 2
  assert int(shift_right_unsigned(i32(4), 32)) == 4
}
"#
    );
    runs(&src, WasmInt::I32);
}

#[test]
fn i64_operations() {
    let src = format!(
        "{I64}{}",
        r#"
pub fn main() {
  let max = i64(9223372036854775807)
  let min = i64(-9223372036854775808)
  let a = i64(0x6d2b79f5d2b79f5)
  assert int(add(max, i64(1))) == -9223372036854775808
  assert int(sub(min, i64(1))) == 9223372036854775807
  assert int(mul(a, a)) == -7477556117499313031
  assert int(and(i64(0b1100), i64(0b1010))) == 0b1000
  assert int(or(i64(0b1100), i64(0b1010))) == 0b1110
  assert int(xor(i64(0b1100), i64(0b1010))) == 0b0110
  assert int(shift_left(i64(1), 63)) == -9223372036854775808
  assert int(shift_right(i64(-8), 1)) == -4
  assert int(shift_right_unsigned(i64(-8), 1)) == 9223372036854775804
}
"#
    );
    runs(&src, WasmInt::I64);
}

/// An operation passed as a value is a function of the builtin.
#[test]
fn operation_as_value() {
    let src = format!(
        "{I32}{}",
        r#"
fn apply(f: fn(I32, I32) -> I32, a: Int, b: Int) -> Int {
  int(f(i32(a), i32(b)))
}

fn apply_shift(f: fn(I32, Int) -> I32, a: Int, bits: Int) -> Int {
  int(f(i32(a), bits))
}

pub fn main() {
  assert apply(add, 2147483647, 1) == -2147483648
  assert apply(mul, 65536, 65536) == 0
  assert apply_shift(shift_right_unsigned, -1, 28) == 15
}
"#
    );
    runs(&src, WasmInt::I32);
}
