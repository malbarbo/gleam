//// An immutable array, the external type `Array` of the WebAssembly target,
//// with its builtins. Reading an index out of range traps, as the runtime
//// only reads the positions it knows, and changing one gives the same array.
//// On Erlang it is a tuple, so the modules that use it can be tested there.

// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

@external(webassembly, "builtins", "Array")
pub type Array(a)

@external(erlang, "erlang", "tuple_size")
@external(webassembly, "builtins", "_array_size")
pub fn array_size(array: Array(a)) -> Int

@external(webassembly, "builtins", "_array_get")
pub fn array_get(array: Array(a), index: Int) -> a {
  element(index + 1, array)
}

@external(webassembly, "builtins", "_array_set")
pub fn array_set(array: Array(a), index: Int, value: a) -> Array(a) {
  case is_index(array, index) {
    True -> set_element(index + 1, array, value)
    False -> array
  }
}

@external(webassembly, "builtins", "_array_insert")
pub fn array_insert(array: Array(a), index: Int, value: a) -> Array(a) {
  case index == array_size(array) || is_index(array, index) {
    True -> insert_element(index + 1, array, value)
    False -> array
  }
}

@external(webassembly, "builtins", "_array_remove")
pub fn array_remove(array: Array(a), index: Int) -> Array(a) {
  case is_index(array, index) {
    True -> delete_element(index + 1, array)
    False -> array
  }
}

fn is_index(array: Array(a), index: Int) -> Bool {
  index >= 0 && index < array_size(array)
}

@external(erlang, "erlang", "list_to_tuple")
@external(webassembly, "builtins", "_array_from_list")
pub fn array_from_list(list: List(a)) -> Array(a)

@external(erlang, "erlang", "element")
fn element(index: Int, array: Array(a)) -> a

@external(erlang, "erlang", "setelement")
fn set_element(index: Int, array: Array(a), value: a) -> Array(a)

@external(erlang, "erlang", "insert_element")
fn insert_element(index: Int, array: Array(a), value: a) -> Array(a)

@external(erlang, "erlang", "delete_element")
fn delete_element(index: Int, array: Array(a)) -> Array(a)

/// The elements of `array`, in order.
pub fn array_to_list(array: Array(a)) -> List(a) {
  array_to_list_loop(array, array_size(array) - 1, [])
}

fn array_to_list_loop(array: Array(a), i: Int, acc: List(a)) -> List(a) {
  case i < 0 {
    True -> acc
    False -> array_to_list_loop(array, i - 1, [array_get(array, i), ..acc])
  }
}
