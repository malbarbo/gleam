// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

import gleam/io
import gleam/string

pub type Dynamic

// On WebAssembly the functions are written in Gleam when they can be, as its
// strings, bit arrays and code points are not values of the host.

@external(erlang, "ffi_erlang", "print")
@external(javascript, "./ffi_javascript.mjs", "print")
pub fn print(a: String) -> Nil {
  io.print(a)
}

@external(erlang, "ffi_erlang", "append")
@external(javascript, "./ffi_javascript.mjs", "append")
pub fn append(a: String, b: String) -> String {
  a <> b
}

@external(erlang, "ffi_erlang", "to_string")
@external(javascript, "./ffi_javascript.mjs", "toString")
pub fn to_string(a: anything) -> String {
  string.inspect(a)
}

/// Only the tests of the build files of Erlang and JavaScript use it.
@external(erlang, "ffi_erlang", "file_exists")
@external(javascript, "./ffi_javascript.mjs", "fileExists")
pub fn file_exists(_a: String) -> Bool {
  panic as "file_exists is not available on WebAssembly"
}

@external(erlang, "ffi_erlang", "halt")
@external(javascript, "./ffi_javascript.mjs", "halt")
@external(webassembly, "./ffi_webassembly.mjs", "halt")
pub fn halt(a: Int) -> Nil

@external(erlang, "ffi_erlang", "to_dynamic")
@external(javascript, "./ffi_javascript.mjs", "toDynamic")
@external(webassembly, "./ffi_webassembly.mjs", "toDynamic")
pub fn to_dynamic(a: x) -> Dynamic

@external(erlang, "ffi_erlang", "to_codepoint")
@external(javascript, "./ffi_javascript.mjs", "toCodepoint")
pub fn utf_codepoint(a: Int) -> UtfCodepoint {
  let assert Ok(codepoint) = string.utf_codepoint(a)
  codepoint
}

@external(erlang, "ffi_erlang", "read_uint16_from_bit_array")
@external(javascript, "./ffi_javascript.mjs", "readUint16FromBitArray")
pub fn read_uint16_from_bit_array(a: BitArray) -> Int {
  let assert <<value:16, _:bits>> = a
  value
}
