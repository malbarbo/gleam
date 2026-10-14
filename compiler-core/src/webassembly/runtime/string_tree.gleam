//// A rope: the `StringTree` that the WebAssembly target provides to
//// `@external(webassembly, "builtins", "StringTree")`, as strings are flat
//// arrays and repeated appends would copy. Its public functions are reached
//// with `@external(webassembly, "builtins", "string_tree_...")`.
////
//// Every node stores its size in bytes, so `byte_size` is constant time.
//// Converting a rope to a string collects the leaves in a list (tail
//// recursively, with an explicit stack) and concatenates them with a single
//// builtin call.
////
//// Operations that need the whole content (case conversion, reversal,
//// splitting, replacing) convert the rope to a string, make one call and
//// build a new leaf: they cannot work leaf by leaf, because graphemes, final
//// sigma and patterns can cross leaves.
////
//// Equality, hash and inspect are those of the content: `append("a", "b")`
//// and `"ab"` are the same text, as on JavaScript.
////
//// It is plain Gleam that imports only `builtins/base`, so it also compiles
//// (and can be tested) on Erlang.

// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

import builtins/base.{
  concat_strings, hash, inspect, reverse_and_prepend, string_byte_size,
}

pub opaque type StringTree {
  Leaf(size: Int, string: String)
  Pair(size: Int, left: StringTree, right: StringTree)
  Many(size: Int, ropes: List(StringTree))
}

pub fn string_tree_from_string(string: String) -> StringTree {
  Leaf(string_byte_size(string), string)
}

pub fn string_tree_from_strings(strings: List(String)) -> StringTree {
  from_strings_loop(strings, [], 0)
}

fn from_strings_loop(
  strings: List(String),
  acc: List(StringTree),
  size: Int,
) -> StringTree {
  case strings {
    [] -> Many(size, reverse_and_prepend(acc, []))
    [string, ..rest] -> {
      let leaf = string_tree_from_string(string)
      from_strings_loop(rest, [leaf, ..acc], size + leaf.size)
    }
  }
}

pub fn string_tree_concat(ropes: List(StringTree)) -> StringTree {
  Many(sum_sizes(ropes, 0), ropes)
}

fn sum_sizes(ropes: List(StringTree), acc: Int) -> Int {
  case ropes {
    [] -> acc
    [rope, ..rest] -> sum_sizes(rest, acc + rope.size)
  }
}

pub fn string_tree_append(rope: StringTree, suffix: StringTree) -> StringTree {
  Pair(rope.size + suffix.size, rope, suffix)
}

pub fn string_tree_byte_size(rope: StringTree) -> Int {
  rope.size
}

pub fn string_tree_to_string(rope: StringTree) -> String {
  case rope {
    Leaf(string:, ..) -> string
    _ -> concat_strings(leaves([rope], []))
  }
}

/// The strings of the leaves, in order. The stack holds the ropes still to
/// visit, the rightmost on top, so the leaves are prepended in order.
fn leaves(stack: List(StringTree), acc: List(String)) -> List(String) {
  case stack {
    [] -> acc
    [Leaf(string:, ..), ..stack] -> leaves(stack, [string, ..acc])
    [Pair(left:, right:, ..), ..stack] -> leaves([right, left, ..stack], acc)
    [Many(ropes:, ..), ..stack] ->
      leaves(reverse_and_prepend(ropes, stack), acc)
  }
}

pub fn string_tree_is_equal(a: StringTree, b: StringTree) -> Bool {
  a.size == b.size && string_tree_to_string(a) == string_tree_to_string(b)
}

pub fn string_tree_is_empty(rope: StringTree) -> Bool {
  rope.size == 0
}

pub fn string_tree_lowercase(rope: StringTree) -> StringTree {
  string_tree_from_string(string_lowercase(string_tree_to_string(rope)))
}

pub fn string_tree_uppercase(rope: StringTree) -> StringTree {
  string_tree_from_string(string_uppercase(string_tree_to_string(rope)))
}

pub fn string_tree_reverse(rope: StringTree) -> StringTree {
  string_tree_from_string(string_reverse(string_tree_to_string(rope)))
}

pub fn string_tree_replace(
  rope: StringTree,
  pattern: String,
  substitute: String,
) -> StringTree {
  string_tree_from_string(string_replace(
    string_tree_to_string(rope),
    pattern,
    substitute,
  ))
}

pub fn string_tree_split(
  rope: StringTree,
  pattern: String,
) -> List(StringTree) {
  from_strings_each(string_split(string_tree_to_string(rope), pattern), [])
}

fn from_strings_each(
  strings: List(String),
  acc: List(StringTree),
) -> List(StringTree) {
  case strings {
    [] -> reverse_and_prepend(acc, [])
    [string, ..rest] ->
      from_strings_each(rest, [string_tree_from_string(string), ..acc])
  }
}

// -- Type hooks ---------------------------------------------------------------

/// The hash of the content, consistent with `string_tree_is_equal`.
pub fn string_tree_hash(rope: StringTree) -> Int {
  hash(string_tree_to_string(rope))
}

/// The text of the content, as `string.inspect` shows a `String`.
pub fn string_tree_inspect(rope: StringTree) -> String {
  inspect(string_tree_to_string(rope))
}

// -- Builtins -----------------------------------------------------------------

@external(erlang, "string", "lowercase")
@external(webassembly, "builtins", "string_lowercase")
fn string_lowercase(string: String) -> String

@external(erlang, "string", "uppercase")
@external(webassembly, "builtins", "string_uppercase")
fn string_uppercase(string: String) -> String

@external(webassembly, "builtins", "string_reverse")
fn string_reverse(string: String) -> String {
  string
  |> graphemes
  |> reverse_and_prepend([])
  |> concat_chardata
}

@external(webassembly, "builtins", "string_replace")
fn string_replace(
  string: String,
  pattern: String,
  substitute: String,
) -> String {
  chardata_to_string(chardata_replace(string, pattern, substitute))
}

@external(webassembly, "builtins", "string_split")
fn string_split(string: String, pattern: String) -> List(String) {
  split_all(string, pattern, All)
}

// Fallbacks for Erlang, where the `string` functions return chardata, which
// is converted back to binaries.

type Chardata

type Direction {
  All
}

@external(erlang, "unicode", "characters_to_binary")
fn chardata_to_string(chardata: Chardata) -> String

@external(erlang, "unicode", "characters_to_binary")
fn concat_chardata(list: List(Chardata)) -> String

@external(erlang, "string", "to_graphemes")
fn graphemes(string: String) -> List(Chardata)

@external(erlang, "gleam_stdlib", "string_replace")
fn chardata_replace(
  string: String,
  pattern: String,
  substitute: String,
) -> Chardata

// For binary arguments, `string:split` returns binaries.
@external(erlang, "string", "split")
fn split_all(
  string: String,
  pattern: String,
  direction: Direction,
) -> List(String)
