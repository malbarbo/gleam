//// The values that the functions of the host receive and return, declared
//// with `@external(webassembly, "./file.mjs", "name")`: each Gleam value
//// crosses as the value of the JavaScript target, built and read by the
//// functions of the module `"gleam:js"` of the host, with the classes of the
//// JavaScript prelude.
////
//// The compiler converts a value with the functions of this module for its
//// type, see `webassembly/host.rs`: an `Int` or a `Float` is a number, a
//// `Bool` a boolean, `Nil` is `undefined`, a list is a `List` of the prelude,
//// a `Result` an `Ok` or an `Error`, a bit array a `BitArray`, and a dict a
//// `Dict` of the module `dict.mjs` of the stdlib. The
//// values from the host are not checked, as on JavaScript, except to tell
//// the variants apart, and a list or a bit array, whose items are read; a
//// value that is not a string becomes one, as with `String(value)`.

// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

import builtins/array.{array_to_list}
import builtins/bit_array.{type Bits, type Cell, type Words, Bits}
import builtins/dict.{type Dict, dict_fold, dict_insert, dict_new}
import builtins/dynamic.{
  type Dynamic, DArray, DBitArray, DBool, DDict, DFloat, DInt, DList, DNil,
  DString, DUtfCodepoint, dynamic_array, dynamic_bit_array, dynamic_bool,
  dynamic_dict, dynamic_float, dynamic_int, dynamic_list, dynamic_nil,
  dynamic_string,
}

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

// -- Dicts --------------------------------------------------------------------

/// The `Dict` of the JavaScript target with the entries of `dict`, made by
/// `from` of the module `dict.mjs` of the stdlib from an array with an array
/// `[key, value]` for each entry, whose key is converted with `key` and whose
/// value with `value`.
pub fn js_dict_with(
  dict: Dict(k, v),
  from: JsValue,
  key: fn(k) -> JsValue,
  value: fn(v) -> JsValue,
) -> JsValue {
  let entries = js_array_new()
  dict_fold(dict, Nil, fn(_, k, v) {
    let entry = js_array_new()
    js_array_push(entry, key(k))
    js_array_push(entry, value(v))
    js_array_push(entries, entry)
  })
  call(from, entries)
}

/// The dict with the entries of the `Dict` `dict` of the JavaScript target,
/// read with `fold` of the module `dict.mjs` of the stdlib.
pub fn js_dict_from_with(
  dict: JsValue,
  fold: JsValue,
  key: fn(JsValue) -> k,
  value: fn(JsValue) -> v,
) -> Dict(k, v) {
  let entries = entries(fold, dict)
  insert_from(
    entries,
    i32_to_int(array_length(entries)) - 2,
    dict_new(),
    key,
    value,
  )
}

/// Inserts in `dict` the entries of `entries`, its keys and values one after
/// the other, up to the one at `index`.
fn insert_from(
  entries: JsValue,
  index: Int,
  dict: Dict(k, v),
  key: fn(JsValue) -> k,
  value: fn(JsValue) -> v,
) -> Dict(k, v) {
  case index < 0 {
    True -> dict
    False -> {
      let k = key(js_array_get(entries, index))
      let v = value(js_array_get(entries, index + 1))
      insert_from(entries, index - 2, dict_insert(dict, k, v), key, value)
    }
  }
}

/// Throws: `value` is not an instance of the class of a variant of the custom
/// type it is converted to.
pub fn js_unknown_variant(value: JsValue) -> Nil {
  unknown_variant(value)
}

// -- Dynamic ------------------------------------------------------------------

/// The JavaScript value of the `Dynamic` `value`, the one that
/// `js_dynamic_from_with` reads back: a tuple or an array is an array, and
/// its items are converted with `item`. `string` converts a string, `dict` a
/// dict, and `other` any other value, like a value of another type.
pub fn js_dynamic_with(
  value: Dynamic,
  string: fn(String) -> JsValue,
  item: fn(Dynamic) -> JsValue,
  dict: fn(Dict(Dynamic, Dynamic)) -> JsValue,
  other: fn(Dynamic) -> JsValue,
) -> JsValue {
  case value {
    DInt(value) -> js_int(value)
    DFloat(value) -> js_float(value)
    DString(value) -> string(value)
    DBool(value) -> js_bool(value)
    DNil -> undefined()
    DBitArray(value) -> js_bit_array(value)
    DUtfCodepoint(value) -> js_utf_codepoint(value)
    DList(items) -> js_list_with(items, item)
    DArray(items) -> {
      let array = js_array_new()
      push_all(array, array_to_list(items), item)
      array
    }
    DDict(entries) -> dict(entries)
    _ -> other(value)
  }
}

/// The `Dynamic` of `value`, read as on JavaScript: a number is an `Int` when
/// it is an integer and a `Float` otherwise, a string a `String`, a boolean a
/// `Bool`, `undefined` and `null` are `Nil`, a `BitArray` a bit array, a
/// `List` a list and an array a tuple, with their items read too, and an
/// instance of `dict_class`, the `Dict` of the stdlib, a dict, converted with
/// `dict`. Any other value, like an instance of a custom type or a plain
/// object (see `js_fields_with`), is a value of the host, made by `other`.
/// `string` converts a string.
pub fn js_dynamic_from_with(
  value: JsValue,
  string: fn(JsValue) -> String,
  dict_class: JsValue,
  dict: fn(JsValue) -> Dict(Dynamic, Dynamic),
  other: fn(JsValue) -> Dynamic,
) -> Dynamic {
  let item = js_dynamic_from_with(_, string, dict_class, dict, other)
  case i32_to_int(kind(value, dict_class)) {
    1 -> dynamic_int(js_int_from(value))
    2 -> dynamic_float(js_float_from(value))
    3 -> dynamic_string(string(value))
    4 -> dynamic_bool(js_bool_from(value))
    5 -> dynamic_nil()
    6 -> dynamic_bit_array(js_bit_array_from(value))
    7 -> dynamic_list(js_list_from_with(value, item))
    8 ->
      dynamic_array(prepend_from(
        value,
        i32_to_int(array_length(value)) - 1,
        [],
        item,
      ))
    9 -> dynamic_dict(dict(value))
    _ -> other(value)
  }
}

/// The properties of `value` when it is a plain object, read by `decode.dict`
/// and `decode.field` as on JavaScript: the hook `fields` of the values of
/// the host, whose values are read with `js_dynamic_from_with`. The object
/// itself stays a value of the host, so it is the same object when it goes
/// back to the host.
pub fn js_fields_with(
  value: JsValue,
  string: fn(JsValue) -> String,
  dict_class: JsValue,
  dict: fn(JsValue) -> Dict(Dynamic, Dynamic),
  other: fn(JsValue) -> Dynamic,
) -> Result(Dict(Dynamic, Dynamic), Nil) {
  case i32_to_int(kind(value, dict_class)) {
    10 -> {
      let entries = object_entries(value)
      Ok(insert_from(
        entries,
        i32_to_int(array_length(entries)) - 2,
        dict_new(),
        fn(key) { dynamic_string(string(key)) },
        js_dynamic_from_with(_, string, dict_class, dict, other),
      ))
    }
    _ -> Error(Nil)
  }
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

/// What `value` is read as by `js_dynamic_from_with`: 1 an integer, 2 another
/// number, 3 a string, 4 a boolean, 5 `undefined` or `null`, 6 a `BitArray`,
/// 7 a `List`, 8 an array, 9 an instance of `dict_class` (unless it is `null`,
/// when the program has no dicts), 10 a plain object and 0 another value.
@external(webassembly, "gleam:js", "kind")
fn kind(value: JsValue, dict_class: JsValue) -> I32

/// The keys and values of the `Dict` `dict`, one after the other in an array,
/// read with `fold` of the module `dict.mjs` of the stdlib.
@external(webassembly, "gleam:js", "entries")
fn entries(fold: JsValue, dict: JsValue) -> JsValue

/// The keys and values of the properties of the plain object `value`, one
/// after the other in an array.
@external(webassembly, "gleam:js", "object_entries")
fn object_entries(value: JsValue) -> JsValue

/// Calls the function `f` with `argument`.
@external(webassembly, "gleam:js", "call")
fn call(f: JsValue, argument: JsValue) -> JsValue

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
fn bytes_to_memory(bytes: Words, start: Int, size: Int, address: I32) -> Nil

@external(webassembly, "builtins", "_bytes_from_memory")
fn bytes_from_memory(address: I32, size: Int) -> Words

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
