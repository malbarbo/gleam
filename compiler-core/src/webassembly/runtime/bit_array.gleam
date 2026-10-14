//// The `BitArray` of the prelude on the WebAssembly target, represented by
//// `Bits`, and the functions of `gleam/bit_array`. They are reached with
//// `@external(webassembly, "builtins", "bit_array_...")`.
////
//// The bits of a bit array are a range of a `Bytes`, the byte array of
//// strings. Bytes are only changed by the function that creates them, before
//// the bit array is returned, so slices and the bit arrays made from strings
//// share them.
////
//// A position or a size is a number of bits. The bits of a byte are counted
//// from the most significant one, as in a bit array literal.

// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

import builtins/base.{
  bitwise_and, bitwise_exclusive_or, bitwise_or, bitwise_shift_left,
  bitwise_shift_right, concat_strings, inspect, low_32_bits,
  reverse_and_prepend,
}

/// An array of bytes, the `(array (mut i8))` of strings.
@external(webassembly, "builtins", "Bytes")
pub type Bytes

/// The bits of `bytes` from `start` to `end`, not included.
pub type Bits {
  Bits(bytes: Bytes, start: Int, end: Int)
}

// -- Construction -------------------------------------------------------------

/// The first `size` bits of `bytes`, which are no longer changed. The
/// compiler creates the bit arrays of the literals with it.
pub fn bit_array_from_bytes(bytes: Bytes, size: Int) -> Bits {
  Bits(bytes, 0, size)
}

/// The bytes of `string`, shared.
pub fn bit_array_from_string(string: String) -> Bits {
  let start = string_offset(string) * 8
  Bits(string_bytes(string), start, start + string_byte_size(string) * 8)
}

pub fn bit_array_concat(arrays: List(Bits)) -> Bits {
  case arrays {
    [bits] -> bits
    _ -> {
      let size = sum_sizes(arrays, 0)
      let bytes = bytes_new(byte_count(size))
      let _ = concat_loop(bytes, 0, arrays)
      Bits(bytes, 0, size)
    }
  }
}

fn sum_sizes(arrays: List(Bits), acc: Int) -> Int {
  case arrays {
    [] -> acc
    [bits, ..rest] -> sum_sizes(rest, acc + bits.end - bits.start)
  }
}

fn concat_loop(bytes: Bytes, at: Int, arrays: List(Bits)) -> Nil {
  case arrays {
    [] -> Nil
    [bits, ..rest] -> {
      let size = bits.end - bits.start
      let _ = copy_bits(bytes, at, bits.bytes, bits.start, size)
      concat_loop(bytes, at + size, rest)
    }
  }
}

/// `bits` followed by zeros up to a whole number of bytes.
pub fn bit_array_pad_to_bytes(bits: Bits) -> Bits {
  let size = bits.end - bits.start
  case bitwise_and(size, 7) {
    0 -> bits
    _ -> {
      let count = byte_count(size)
      let bytes = bytes_new(count)
      let _ = copy_bits(bytes, 0, bits.bytes, bits.start, size)
      Bits(bytes, 0, count * 8)
    }
  }
}

/// `bits` starting at a whole byte, copied when it does not.
fn aligned(bits: Bits) -> Bits {
  case bitwise_and(bits.start, 7) {
    0 -> bits
    _ -> {
      let size = bits.end - bits.start
      let bytes = bytes_new(byte_count(size))
      let _ = copy_bits(bytes, 0, bits.bytes, bits.start, size)
      Bits(bytes, 0, size)
    }
  }
}

// -- Queries ------------------------------------------------------------------

pub fn bit_array_bit_size(bits: Bits) -> Int {
  bits.end - bits.start
}

pub fn bit_array_byte_size(bits: Bits) -> Int {
  byte_count(bits.end - bits.start)
}

/// The bytes from `position` to `position + length`, or from
/// `position + length` to `position` when `length` is negative.
pub fn bit_array_slice(bits: Bits, position: Int, length: Int) -> Result(Bits, Nil) {
  let #(from, to) = case length < 0 {
    True -> #(position + length, position)
    False -> #(position, position + length)
  }
  case from < 0 || to * 8 > bits.end - bits.start {
    True -> Error(Nil)
    False -> Ok(Bits(bits.bytes, bits.start + from * 8, bits.start + to * 8))
  }
}

pub fn bit_array_starts_with(bits: Bits, prefix: Bits) -> Bool {
  let size = prefix.end - prefix.start
  size <= bits.end - bits.start
  && equal_loop(bits.bytes, bits.start, prefix.bytes, prefix.start, size)
}

/// The order of `a` and `b` as `-1`, `0` or `1`: byte by byte, and then by
/// the value and size of the bits after the last whole byte of the shortest.
pub fn bit_array_compare(a: Bits, b: Bits) -> Int {
  compare_loop(a.bytes, a.start, a.end, b.bytes, b.start, b.end)
}

fn compare_loop(
  a: Bytes,
  i: Int,
  a_end: Int,
  b: Bytes,
  j: Int,
  b_end: Int,
) -> Int {
  case a_end - i >= 8 && b_end - j >= 8 {
    True -> {
      let x = byte_at(a, i)
      let y = byte_at(b, j)
      case x == y {
        True -> compare_loop(a, i + 8, a_end, b, j + 8, b_end)
        False -> sign(x - y)
      }
    }
    False ->
      case a_end - i, b_end - j {
        0, 0 -> 0
        _, 0 -> 1
        0, _ -> -1
        a_size, b_size ->
          case compare_values(a, i, a_size, b, j, b_size, 0) {
            0 -> sign(a_size - b_size)
            order -> order
          }
      }
  }
}

/// Compares the unsigned integers of `a_size` bits of `a` at `i` and
/// `b_size` bits of `b` at `j`, from the most significant bit of the
/// largest, at `bit` from the left.
fn compare_values(
  a: Bytes,
  i: Int,
  a_size: Int,
  b: Bytes,
  j: Int,
  b_size: Int,
  bit: Int,
) -> Int {
  let size = max(a_size, b_size)
  case bit == size {
    True -> 0
    False -> {
      let x = bit_or_zero(a, i, bit - { size - a_size })
      let y = bit_or_zero(b, j, bit - { size - b_size })
      case x == y {
        True -> compare_values(a, i, a_size, b, j, b_size, bit + 1)
        False -> sign(x - y)
      }
    }
  }
}

/// The bit at `offset` from `position`, or 0 when `offset` is negative.
fn bit_or_zero(bytes: Bytes, position: Int, offset: Int) -> Int {
  case offset < 0 {
    True -> 0
    False -> bits_at(bytes, position + offset, 1)
  }
}

/// The integer of the bits of `bits`, unsigned, and their number.
pub fn bit_array_to_int_and_size(bits: Bits) -> #(Int, Int) {
  let size = bits.end - bits.start
  #(to_int_loop(bits.bytes, bits.start, size, 0), size)
}

fn to_int_loop(bytes: Bytes, position: Int, remaining: Int, acc: Int) -> Int {
  case remaining >= 8 {
    True ->
      to_int_loop(
        bytes,
        position + 8,
        remaining - 8,
        acc * 256 + byte_at(bytes, position),
      )
    False ->
      case remaining {
        0 -> acc
        _ ->
          bitwise_shift_left(acc, remaining)
          + bits_at(bytes, position, remaining)
      }
  }
}

// -- Strings ------------------------------------------------------------------

pub fn bit_array_is_utf8(bits: Bits) -> Bool {
  bitwise_and(bits.end - bits.start, 7) == 0
  && utf8_loop(bits.bytes, bits.start, bits.end)
}

/// The string of the bytes of `bits`, which shares them when `bits` starts
/// at a whole byte.
pub fn bit_array_to_string(bits: Bits) -> Result(String, Nil) {
  case bit_array_is_utf8(bits) {
    True -> {
      let bits = aligned(bits)
      let start = bitwise_shift_right(bits.start, 3)
      Ok(bytes_to_string(bits.bytes, start, byte_count(bits.end - bits.start)))
    }
    False -> Error(Nil)
  }
}

/// Whether the bytes from `position` to `end` are UTF-8, without overlong
/// encodings, surrogates or code points after U+10FFFF.
fn utf8_loop(bytes: Bytes, position: Int, end: Int) -> Bool {
  case position == end {
    True -> True
    False -> {
      let byte = byte_at(bytes, position)
      case byte {
        _ if byte < 0x80 -> utf8_loop(bytes, position + 8, end)
        _ if byte < 0xC2 -> False
        _ if byte < 0xE0 -> utf8_tail(bytes, position, end, 1, 0x80, 0xBF)
        0xE0 -> utf8_tail(bytes, position, end, 2, 0xA0, 0xBF)
        0xED -> utf8_tail(bytes, position, end, 2, 0x80, 0x9F)
        _ if byte < 0xF0 -> utf8_tail(bytes, position, end, 2, 0x80, 0xBF)
        0xF0 -> utf8_tail(bytes, position, end, 3, 0x90, 0xBF)
        0xF4 -> utf8_tail(bytes, position, end, 3, 0x80, 0x8F)
        _ if byte < 0xF4 -> utf8_tail(bytes, position, end, 3, 0x80, 0xBF)
        _ -> False
      }
    }
  }
}

/// Checks the `count` bytes after the first byte of a code point at
/// `position`, the first of them from `low` to `high`, and continues after
/// them.
fn utf8_tail(
  bytes: Bytes,
  position: Int,
  end: Int,
  count: Int,
  low: Int,
  high: Int,
) -> Bool {
  let next = position + 8
  case next + count * 8 <= end {
    False -> False
    True -> {
      let byte = byte_at(bytes, next)
      case low <= byte && byte <= high {
        False -> False
        True -> utf8_continuation(bytes, next + 8, end, count - 1)
      }
    }
  }
}

fn utf8_continuation(bytes: Bytes, position: Int, end: Int, count: Int) -> Bool {
  case count {
    0 -> utf8_loop(bytes, position, end)
    _ -> {
      let byte = byte_at(bytes, position)
      case 0x80 <= byte && byte <= 0xBF {
        False -> False
        True -> utf8_continuation(bytes, position + 8, end, count - 1)
      }
    }
  }
}

// -- Equality, hash and inspect -----------------------------------------------

pub fn bit_array_equal(a: Bits, b: Bits) -> Bool {
  let size = a.end - a.start
  size == b.end - b.start && equal_loop(a.bytes, a.start, b.bytes, b.start, size)
}

/// Whether the `size` bits of `a` at `i` and of `b` at `j` are equal.
fn equal_loop(a: Bytes, i: Int, b: Bytes, j: Int, size: Int) -> Bool {
  case size >= 8 {
    True ->
      case byte_at(a, i) == byte_at(b, j) {
        True -> equal_loop(a, i + 8, b, j + 8, size - 8)
        False -> False
      }
    False -> size == 0 || bits_at(a, i, size) == bits_at(b, j, size)
  }
}

/// FNV-1a of the bytes, then of the bits after the last whole byte and
/// their number.
pub fn bit_array_hash(bits: Bits) -> Int {
  hash_loop(bits.bytes, bits.start, bits.end - bits.start, -2_128_831_035)
}

fn hash_loop(bytes: Bytes, position: Int, remaining: Int, acc: Int) -> Int {
  case remaining >= 8 {
    True ->
      hash_loop(
        bytes,
        position + 8,
        remaining - 8,
        fnv(acc, byte_at(bytes, position)),
      )
    False ->
      case remaining {
        0 -> acc
        _ -> fnv(fnv(acc, bits_at(bytes, position, remaining)), remaining)
      }
  }
}

fn fnv(acc: Int, byte: Int) -> Int {
  low_32_bits(bitwise_exclusive_or(acc, byte) * 16_777_619)
}

/// `<<1, 2, 3:size(2)>>`, as `string.inspect` and `bit_array.inspect` show
/// it on the other targets.
pub fn bit_array_inspect(bits: Bits) -> String {
  let segments = inspect_loop(bits.bytes, bits.start, bits.end - bits.start, [])
  concat_strings(["<<", ..reverse_and_prepend(segments, [">>"])])
}

fn inspect_loop(
  bytes: Bytes,
  position: Int,
  remaining: Int,
  acc: List(String),
) -> List(String) {
  case remaining >= 8 {
    True -> {
      let byte = inspect(byte_at(bytes, position))
      inspect_loop(bytes, position + 8, remaining - 8, [byte, ..separate(acc)])
    }
    False ->
      case remaining {
        0 -> acc
        _ -> [
          ":size(" <> inspect(remaining) <> ")",
          inspect(bits_at(bytes, position, remaining)),
          ..separate(acc)
        ]
      }
  }
}

fn separate(acc: List(String)) -> List(String) {
  case acc {
    [] -> []
    _ -> [", ", ..acc]
  }
}

// -- Bits ---------------------------------------------------------------------

/// The number of bytes that hold `size` bits.
fn byte_count(size: Int) -> Int {
  bitwise_shift_right(size + 7, 3)
}

/// The 8 bits of `bytes` at `position`.
fn byte_at(bytes: Bytes, position: Int) -> Int {
  let index = bitwise_shift_right(position, 3)
  case bitwise_and(position, 7) {
    0 -> bytes_get(bytes, index)
    shift ->
      bitwise_and(
        bitwise_or(
          bitwise_shift_left(bytes_get(bytes, index), shift),
          bitwise_shift_right(bytes_get(bytes, index + 1), 8 - shift),
        ),
        0xFF,
      )
  }
}

/// The `count` bits of `bytes` at `position`, from 1 to 8, as an unsigned
/// integer.
fn bits_at(bytes: Bytes, position: Int, count: Int) -> Int {
  let index = bitwise_shift_right(position, 3)
  let shift = bitwise_and(position, 7)
  let high = bitwise_shift_left(bytes_get(bytes, index), shift)
  let window = case shift + count > 8 {
    True -> bitwise_or(high, bitwise_shift_right(bytes_get(bytes, index + 1), 8 - shift))
    False -> high
  }
  bitwise_shift_right(bitwise_and(window, 0xFF), 8 - count)
}

/// Writes `count` bits of `src` at `from` to `dst` at `at`. The bits of
/// `dst` from `at` on are zero.
fn copy_bits(dst: Bytes, at: Int, src: Bytes, from: Int, count: Int) -> Nil {
  case bitwise_and(at, 7) == 0 && bitwise_and(from, 7) == 0 {
    True -> {
      let whole = bitwise_shift_right(count, 3)
      let _ =
        bytes_copy(
          dst,
          bitwise_shift_right(at, 3),
          src,
          bitwise_shift_right(from, 3),
          whole,
        )
      copy_bits_loop(dst, at + whole * 8, src, from + whole * 8, count - whole * 8)
    }
    False -> copy_bits_loop(dst, at, src, from, count)
  }
}

fn copy_bits_loop(dst: Bytes, at: Int, src: Bytes, from: Int, count: Int) -> Nil {
  case count >= 8 {
    True -> {
      let _ = or_bits(dst, at, byte_at(src, from), 8)
      copy_bits_loop(dst, at + 8, src, from + 8, count - 8)
    }
    False ->
      case count {
        0 -> Nil
        _ -> or_bits(dst, at, bits_at(src, from, count), count)
      }
  }
}

/// Sets the `count` bits of `dst` at `at`, which are zero, to the bits of
/// `value`, from 1 to 8 of them.
fn or_bits(dst: Bytes, at: Int, value: Int, count: Int) -> Nil {
  let index = bitwise_shift_right(at, 3)
  let shift = bitwise_and(at, 7)
  // The bits in a window of two bytes, from `index`.
  let window = bitwise_shift_left(value, 16 - shift - count)
  let _ =
    bytes_set(
      dst,
      index,
      bitwise_or(bytes_get(dst, index), bitwise_shift_right(window, 8)),
    )
  case shift + count > 8 {
    True -> bytes_set(dst, index + 1, bitwise_and(window, 0xFF))
    False -> Nil
  }
}

fn sign(value: Int) -> Int {
  case value {
    0 -> 0
    _ if value < 0 -> -1
    _ -> 1
  }
}

fn max(a: Int, b: Int) -> Int {
  case a > b {
    True -> a
    False -> b
  }
}

// -- Builtins -----------------------------------------------------------------

/// A new array of `size` zero bytes.
@external(webassembly, "builtins", "_bytes_new")
fn bytes_new(size: Int) -> Bytes

@external(webassembly, "builtins", "_bytes_get")
fn bytes_get(bytes: Bytes, index: Int) -> Int

/// Only on the bytes the caller has just created.
@external(webassembly, "builtins", "_bytes_set")
fn bytes_set(bytes: Bytes, index: Int, byte: Int) -> Nil

/// Copies `count` bytes of `src` from `from` to `dst` at `at`. Only on the
/// bytes the caller has just created.
@external(webassembly, "builtins", "_bytes_copy")
fn bytes_copy(dst: Bytes, at: Int, src: Bytes, from: Int, count: Int) -> Nil

/// The bytes of `string`, which start at `string_offset(string)`.
@external(webassembly, "builtins", "_string_bytes")
fn string_bytes(string: String) -> Bytes

@external(webassembly, "builtins", "_string_offset")
fn string_offset(string: String) -> Int

@external(webassembly, "builtins", "_string_byte_size")
fn string_byte_size(string: String) -> Int

/// The string of the `size` bytes of `bytes` from `start`, which are UTF-8.
@external(webassembly, "builtins", "_bytes_to_string")
fn bytes_to_string(bytes: Bytes, start: Int, size: Int) -> String
