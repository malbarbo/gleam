//// The values that the functions of the host receive and return, declared
//// with `@external(webassembly, "./file.mjs", "name")`: each Gleam value
//// crosses as the value of the JavaScript target, built and read by the
//// functions of the module `"gleam:js"` of the host, with the classes of the
//// JavaScript prelude.
////
//// The compiler converts a value with the functions of this module for its
//// type, see `webassembly/host.rs`: an `Int` or a `Float` is a number, a
//// `Bool` a boolean, `Nil` is `undefined`, a list is a `List` of the prelude,
//// a `Result` an `Ok` or an `Error`, and a bit array a `BitArray`. The
//// values from the host are not checked, as on JavaScript, except to tell
//// the variants apart, and a list or a bit array, whose items are read; a
//// value that is not a string becomes one, as with `String(value)`.

// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

import builtins/bit_array.{type Bits, type Bytes, type Cell, Bits}

/// A value of the host, an `externref`.
@external(webassembly, "builtins", "JsValue")
pub type JsValue

@external(webassembly, "builtins", "I32")
type I32

@external(webassembly, "builtins", "F64")
type F64

// -- Scalars ------------------------------------------------------------------

pub fn js_int(value: Int) -> JsValue {
  number(float_to_f64(int_to_float(value)))
}

/// The integer part of the number `value`; `0` when it is not a number.
pub fn js_int_from(value: JsValue) -> Int {
  truncate(f64_to_float(to_number(value)))
}

pub fn js_float(value: Float) -> JsValue {
  number(float_to_f64(value))
}

pub fn js_float_from(value: JsValue) -> Float {
  f64_to_float(to_number(value))
}

pub fn js_bool(value: Bool) -> JsValue {
  case value {
    True -> boolean(int_to_i32(1))
    False -> boolean(int_to_i32(0))
  }
}

/// Whether `value` is truthy.
pub fn js_bool_from(value: JsValue) -> Bool {
  i32_to_int(truthy(value)) != 0
}

pub fn js_nil(_value: Nil) -> JsValue {
  undefined()
}

pub fn js_nil_from(_value: JsValue) -> Nil {
  Nil
}

pub fn js_utf_codepoint(value: UtfCodepoint) -> JsValue {
  codepoint(js_int(utf_codepoint_to_int(value)))
}

pub fn js_utf_codepoint_from(value: JsValue) -> UtfCodepoint {
  unsafe_int_to_utf_codepoint(js_int_from(codepoint_value(value)))
}

// -- Strings and bit arrays ---------------------------------------------------

/// The string of the bytes of `value`, copied through the linear memory.
/// Only with UTF-8 strings: JavaScript strings are values of the host.
pub fn js_string(value: String) -> JsValue {
  let base = heap_base()
  string_from_memory(base, string_to_memory(value, base))
}

/// The string of `value`, as `String(value)` of JavaScript. JavaScript
/// strings are values of the host, and they are not copied.
pub fn js_to_string(value: JsValue) -> JsValue {
  to_string(value)
}

/// The bytes of the string of `value`, see `js_to_string`, copied through the
/// linear memory.
pub fn js_string_from(value: JsValue) -> String {
  let base = heap_base()
  memory_to_string(base, string_into_memory(value, base))
}

/// The bytes of `value` from the one of its first bit, copied through the
/// linear memory.
pub fn js_bit_array(value: Bits) -> JsValue {
  let first = value.start / 8
  let offset = value.start - first * 8
  let size = value.end - value.start
  let byte_size = { offset + size + 7 } / 8
  let base = heap_base()
  bytes_to_memory(value.bytes, first, byte_size, base)
  bit_array_from_memory(
    base,
    int_to_i32(byte_size),
    int_to_i32(size),
    int_to_i32(offset),
  )
}

/// The bytes of the bit array `value`, copied through the linear memory.
/// The bits after them are not known to be zero, so they are not written.
pub fn js_bit_array_from(value: JsValue) -> Bits {
  let base = heap_base()
  let byte_size = i32_to_int(bit_array_into_memory(value, base))
  let offset = i32_to_int(bit_array_offset(value))
  let size = i32_to_int(bit_array_size(value))
  Bits(bytes_from_memory(base, byte_size), offset, offset + size, cell_new(-1))
}

// -- Lists, results and arrays ------------------------------------------------

pub fn js_list_with(list: List(a), to_js: fn(a) -> JsValue) -> JsValue {
  let array = js_array_new()
  push_all(array, list, to_js)
  list_from_array(array)
}

fn push_all(array: JsValue, list: List(a), to_js: fn(a) -> JsValue) -> Nil {
  case list {
    [] -> Nil
    [first, ..rest] -> {
      js_array_push(array, to_js(first))
      push_all(array, rest, to_js)
    }
  }
}

pub fn js_list_from_with(value: JsValue, from_js: fn(JsValue) -> a) -> List(a) {
  let array = list_to_array(value)
  prepend_from(array, i32_to_int(array_length(array)) - 1, [], from_js)
}

/// The items of `array` up to `index`, prepended to `list`.
fn prepend_from(
  array: JsValue,
  index: Int,
  list: List(a),
  from_js: fn(JsValue) -> a,
) -> List(a) {
  case index < 0 {
    True -> list
    False -> {
      let item = from_js(js_array_get(array, index))
      prepend_from(array, index - 1, [item, ..list], from_js)
    }
  }
}

pub fn js_result_with(
  result: Result(a, e),
  ok: fn(a) -> JsValue,
  error: fn(e) -> JsValue,
) -> JsValue {
  case result {
    Ok(value) -> new_ok(ok(value))
    Error(value) -> new_error(error(value))
  }
}

pub fn js_result_from_with(
  value: JsValue,
  ok: fn(JsValue) -> a,
  error: fn(JsValue) -> e,
) -> Result(a, e) {
  case i32_to_int(result_is_ok(value)) {
    1 -> Ok(ok(result_value(value)))
    _ -> Error(error(result_value(value)))
  }
}

/// A new JavaScript array, for a tuple.
pub fn js_array_new() -> JsValue {
  array_new()
}

pub fn js_array_push(array: JsValue, item: JsValue) -> Nil {
  array_push(array, item)
}

pub fn js_array_get(array: JsValue, index: Int) -> JsValue {
  array_get(array, int_to_i32(index))
}

/// Throws: `value` is not an instance of the class of a variant of the custom
/// type it is converted to.
pub fn js_unknown_variant(value: JsValue) -> Nil {
  unknown_variant(value)
}

// -- Values of the host ------------------------------------------------------

/// Whether `a` and `b` are equal, as on JavaScript: the hook of `==` of the
/// external types of values of the host.
pub fn js_equal(a: JsValue, b: JsValue) -> Bool {
  i32_to_int(equal(a, b)) != 0
}

/// A hash of `value` consistent with `js_equal`.
pub fn js_hash(value: JsValue) -> Int {
  i32_to_int(hash(value))
}

/// The text of `value`, as on JavaScript.
pub fn js_inspect(value: JsValue) -> String {
  js_string_from(inspect(value))
}

// -- The functions of the host ------------------------------------------------

@external(webassembly, "gleam:js", "number")
fn number(value: F64) -> JsValue

@external(webassembly, "gleam:js", "to_number")
fn to_number(value: JsValue) -> F64

@external(webassembly, "gleam:js", "boolean")
fn boolean(value: I32) -> JsValue

@external(webassembly, "gleam:js", "truthy")
fn truthy(value: JsValue) -> I32

@external(webassembly, "gleam:js", "undefined")
fn undefined() -> JsValue

@external(webassembly, "gleam:js", "codepoint")
fn codepoint(value: JsValue) -> JsValue

@external(webassembly, "gleam:js", "codepoint_value")
fn codepoint_value(value: JsValue) -> JsValue

@external(webassembly, "gleam:js", "to_string")
fn to_string(value: JsValue) -> JsValue

@external(webassembly, "gleam:js", "string_from_memory")
fn string_from_memory(address: I32, size: I32) -> JsValue

/// Writes the bytes of the string of `value` at `address`, and returns their
/// number.
@external(webassembly, "gleam:js", "string_into_memory")
fn string_into_memory(value: JsValue, address: I32) -> I32

@external(webassembly, "gleam:js", "bit_array_from_memory")
fn bit_array_from_memory(
  address: I32,
  byte_size: I32,
  size: I32,
  offset: I32,
) -> JsValue

/// Writes the bytes of the bit array `value` at `address`, and returns their
/// number. Throws when `value` is not a bit array.
@external(webassembly, "gleam:js", "bit_array_into_memory")
fn bit_array_into_memory(value: JsValue, address: I32) -> I32

@external(webassembly, "gleam:js", "bit_array_offset")
fn bit_array_offset(value: JsValue) -> I32

@external(webassembly, "gleam:js", "bit_array_size")
fn bit_array_size(value: JsValue) -> I32

@external(webassembly, "gleam:js", "array_new")
fn array_new() -> JsValue

@external(webassembly, "gleam:js", "array_push")
fn array_push(array: JsValue, item: JsValue) -> Nil

@external(webassembly, "gleam:js", "array_get")
fn array_get(array: JsValue, index: I32) -> JsValue

@external(webassembly, "gleam:js", "array_length")
fn array_length(array: JsValue) -> I32

@external(webassembly, "gleam:js", "list_from_array")
fn list_from_array(array: JsValue) -> JsValue

/// The items of the list `value`. Throws when it is not a list.
@external(webassembly, "gleam:js", "list_to_array")
fn list_to_array(value: JsValue) -> JsValue

@external(webassembly, "gleam:js", "ok")
fn new_ok(value: JsValue) -> JsValue

@external(webassembly, "gleam:js", "error")
fn new_error(value: JsValue) -> JsValue

/// `1` for an `Ok`, `0` for an `Error`. Throws for another value.
@external(webassembly, "gleam:js", "result_is_ok")
fn result_is_ok(value: JsValue) -> I32

@external(webassembly, "gleam:js", "result_value")
fn result_value(value: JsValue) -> JsValue

@external(webassembly, "gleam:js", "unknown_variant")
fn unknown_variant(value: JsValue) -> Nil

@external(webassembly, "gleam:js", "equal")
fn equal(a: JsValue, b: JsValue) -> I32

@external(webassembly, "gleam:js", "hash")
fn hash(value: JsValue) -> I32

@external(webassembly, "gleam:js", "inspect")
fn inspect(value: JsValue) -> JsValue

// -- Builtins -----------------------------------------------------------------

@external(webassembly, "builtins", "_heap_base")
fn heap_base() -> I32

@external(webassembly, "builtins", "_string_to_memory")
fn string_to_memory(string: String, address: I32) -> I32

@external(webassembly, "builtins", "_memory_to_string")
fn memory_to_string(address: I32, size: I32) -> String

@external(webassembly, "builtins", "_bytes_to_memory")
fn bytes_to_memory(bytes: Bytes, start: Int, size: Int, address: I32) -> Nil

@external(webassembly, "builtins", "_bytes_from_memory")
fn bytes_from_memory(address: I32, size: Int) -> Bytes

@external(webassembly, "builtins", "_cell_new")
fn cell_new(value: Int) -> Cell

@external(webassembly, "builtins", "_i32_to_int")
fn i32_to_int(value: I32) -> Int

@external(webassembly, "builtins", "_int_to_i32")
fn int_to_i32(value: Int) -> I32

@external(webassembly, "builtins", "_f64_to_float")
fn f64_to_float(value: F64) -> Float

@external(webassembly, "builtins", "_float_to_f64")
fn float_to_f64(value: Float) -> F64

@external(webassembly, "builtins", "_int_to_float")
fn int_to_float(value: Int) -> Float

@external(webassembly, "builtins", "_float_truncate")
fn truncate(value: Float) -> Int

@external(webassembly, "builtins", "_utf_codepoint_to_int")
fn utf_codepoint_to_int(value: UtfCodepoint) -> Int

@external(webassembly, "builtins", "_unsafe_int_to_utf_codepoint")
fn unsafe_int_to_utf_codepoint(value: Int) -> UtfCodepoint
