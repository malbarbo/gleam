//// The functions of `gleam/string` that search a string for another. They
//// are reached with `@external(webassembly, "builtins", "string_...")`.
////
//// They search with `_string_find` and cut with `_string_codeunit_slice`,
//// in the code units of the representation (bytes, or UTF-16 units with
//// `string = "JSString"`), so a part is a view of the string (or a substring
//// of a JavaScript string), not a copy, and the cost of a search is the
//// distance to the match.

// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

import builtins/base.{concat_strings, reverse_and_prepend}

pub fn string_contains(haystack: String, needle: String) -> Bool {
  find(haystack, needle, 0) >= 0
}

pub fn string_crop(string: String, before: String) -> String {
  case find(string, before, 0) {
    -1 -> string
    i -> drop(string, i)
  }
}

pub fn string_split_once(
  string: String,
  pattern: String,
) -> Result(#(String, String), Nil) {
  case find(string, pattern, 0) {
    -1 -> Error(Nil)
    i -> Ok(#(slice(string, 0, i), drop(string, i + size(pattern))))
  }
}

/// An empty pattern gives the graphemes.
pub fn string_split(string: String, pattern: String) -> List(String) {
  case pattern {
    "" -> to_graphemes(string)
    _ -> reverse_and_prepend(split_loop(string, pattern, 0, []), [])
  }
}

/// The parts of `string` from `start`, reversed and prepended to `parts`.
fn split_loop(
  string: String,
  pattern: String,
  start: Int,
  parts: List(String),
) -> List(String) {
  case find(string, pattern, start) {
    -1 -> [drop(string, start), ..parts]
    i -> {
      let part = slice(string, start, i - start)
      split_loop(string, pattern, i + size(pattern), [part, ..parts])
    }
  }
}

/// An empty pattern gives `string`.
pub fn string_replace(
  string: String,
  pattern: String,
  replacement: String,
) -> String {
  case pattern {
    "" -> string
    _ -> {
      let parts = replace_loop(string, pattern, replacement, 0, [])
      concat_strings(reverse_and_prepend(parts, []))
    }
  }
}

/// The parts of the result from `start`, reversed and prepended to `parts`.
fn replace_loop(
  string: String,
  pattern: String,
  replacement: String,
  start: Int,
  parts: List(String),
) -> List(String) {
  case find(string, pattern, start) {
    -1 -> [drop(string, start), ..parts]
    i -> {
      let parts = [replacement, slice(string, start, i - start), ..parts]
      replace_loop(string, pattern, replacement, i + size(pattern), parts)
    }
  }
}

/// `string` without its first `n` code units.
fn drop(string: String, n: Int) -> String {
  slice(string, n, size(string) - n)
}

@external(webassembly, "builtins", "_string_find")
fn find(haystack: String, needle: String, from: Int) -> Int

@external(webassembly, "builtins", "_string_codeunit_size")
fn size(string: String) -> Int

@external(webassembly, "builtins", "_string_codeunit_slice")
fn slice(string: String, start: Int, length: Int) -> String

@external(webassembly, "builtins", "string_to_graphemes")
fn to_graphemes(string: String) -> List(String)
