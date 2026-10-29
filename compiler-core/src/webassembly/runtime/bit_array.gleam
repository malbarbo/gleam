//// The `BitArray` of the prelude on the WebAssembly target, represented by
//// `Bits`, and the functions of `gleam/bit_array`. They are reached with
//// `@external(webassembly, "builtins", "bit_array_...")`.
////
//// The bits of a bit array are a range of a `Words`, whose bytes are in
//// `i64`s, so it is copied a word at a time, and slices share them. The
//// bits of a `Words` are written in order and only once: the bit arrays of
//// the same bytes share a `Cell` with the end of the bits written so far, and
//// the bits after it are zero. A bit array that ends there is extended by
//// writing after it, in the same bytes, which changes no bit array: this
//// makes `<<bits:bits, ...>>` in a loop linear, as on Erlang.
////
//// A position or a size is a number of bits. The bits of a byte are counted
//// from the most significant one, as in a bit array literal.

// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

import builtins/base.{
  bitwise_and, bitwise_exclusive_or, bitwise_or, bitwise_shift_left,
  bitwise_shift_right, concat_strings, inspect, low_32_bits, reverse_and_prepend,
  string_byte_size,
}

/// An array of bytes, 8 in each `i64` of an `(array (mut i64))` from the
/// most significant one. Its functions take the index of a byte.
@external(webassembly, "builtins", "Words")
pub type Words

/// An `Int` that can be changed, `(struct (field (mut i64)))`.
@external(webassembly, "builtins", "Cell")
pub type Cell

/// The bits of `bytes` from `start` to `end`, not included, and the end of
/// the bits written to `bytes`, which is `-1` when they are not written
/// anymore.
pub type Bits {
  Bits(bytes: Words, start: Int, end: Int, written: Cell)
}

// -- Construction -------------------------------------------------------------

/// The first `size` bits of `bytes`, which are no longer changed. The
/// compiler creates the bit arrays of the literals with it.
pub fn bit_array_from_bytes(bytes: Words, size: Int) -> Bits {
  Bits(bytes, 0, size, cell_new(size))
}

/// The bytes of `string`.
pub fn bit_array_from_string(string: String) -> Bits {
  string_bits(string)
}

/// `bits` followed by `size` bits, which are zero, to be written after
/// `bits`: in the same bytes when `bits` ends where they are written and
/// they have room, and otherwise in new bytes with room for as many bits
/// more. The compiler creates `<<bits:bits, ...>>` with it.
pub fn bit_array_extend(bits: Bits, size: Int) -> Bits {
  let end = bits.end + size
  case cell_get(bits.written) == bits.end && end <= bytes_size(bits.bytes) * 8 {
    True -> {
      let _ = cell_set(bits.written, end)
      Bits(bits.bytes, bits.start, end, bits.written)
    }
    False -> {
      let size = bits.end - bits.start + size
      copy(bits, size, byte_count(size) * 2)
    }
  }
}

pub fn bit_array_concat(arrays: List(Bits)) -> Bits {
  case arrays {
    [bits] -> bits
    _ -> {
      let size = sum_sizes(arrays, 0)
      let bytes = bytes_new(byte_count(size))
      let _ = concat_loop(bytes, 0, arrays)
      bit_array_from_bytes(bytes, size)
    }
  }
}

fn sum_sizes(arrays: List(Bits), acc: Int) -> Int {
  case arrays {
    [] -> acc
    [bits, ..rest] -> sum_sizes(rest, acc + bits.end - bits.start)
  }
}

fn concat_loop(bytes: Words, at: Int, arrays: List(Bits)) -> Nil {
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
      copy(bits, count * 8, count)
    }
  }
}

/// `bits` starting at a whole byte, copied when it does not.
fn aligned(bits: Bits) -> Bits {
  case bitwise_and(bits.start, 7) {
    0 -> bits
    _ -> {
      let size = bits.end - bits.start
      copy(bits, size, byte_count(size))
    }
  }
}

/// The bits of `bits` in new bytes of `capacity` bytes, followed by zeros
/// up to `size` bits.
fn copy(bits: Bits, size: Int, capacity: Int) -> Bits {
  let bytes = bytes_new(capacity)
  let _ = copy_bits(bytes, 0, bits.bytes, bits.start, bits.end - bits.start)
  bit_array_from_bytes(bytes, size)
}

// -- Construction from values -------------------------------------------------
//
// The compiler creates a bit array whose segments are not all literals with
// the sizes of its segments, by constants or by the `bit_array_*_size`
// functions, and then writes them to the new bytes with the
// `bit_array_write_*` functions, which return the position after what they
// write. A size function fails when the segment cannot be created.

/// The size of an integer segment of `size` bits: a negative size is an
/// empty segment.
pub fn bit_array_int_size(size: Int) -> Int {
  max(size, 0)
}

pub fn bit_array_float_size(size: Int) -> Int {
  case size {
    16 | 32 | 64 -> size
    _ -> panic as "Sized floats must be 16-bit, 32-bit or 64-bit"
  }
}

/// The size of a segment with the first `size` bits of `bits`.
pub fn bit_array_bits_size(bits: Bits, size: Int) -> Int {
  case size < 0 || size > bits.end - bits.start {
    True -> panic as "Bit array segment size is larger than its value"
    False -> size
  }
}

pub fn bit_array_utf8_size(string: String) -> Int {
  string_byte_size(string) * 8
}

pub fn bit_array_utf16_size(string: String) -> Int {
  unicode_size(string, 16)
}

pub fn bit_array_utf32_size(string: String) -> Int {
  unicode_size(string, 32)
}

/// The size of the UTF-16 or UTF-32 (`unit` 16 or 32) code units of
/// `string`.
fn unicode_size(string: String, unit: Int) -> Int {
  let bits = string_bits(string)
  unicode_size_loop(bits.bytes, bits.start / 8, bits.end / 8, unit, 0)
}

/// The size of the code units of the UTF-8 bytes from `index` to `end`.
fn unicode_size_loop(
  bytes: Words,
  index: Int,
  end: Int,
  unit: Int,
  acc: Int,
) -> Int {
  case index < end {
    False -> acc
    True -> {
      let byte = bytes_get(bytes, index)
      let size = case unit == 16 && byte >= 0xF0 {
        True -> 32
        False -> unit
      }
      unicode_size_loop(bytes, index + utf8_length(byte), end, unit, acc + size)
    }
  }
}

pub fn bit_array_codepoint_utf8_size(codepoint: UtfCodepoint) -> Int {
  let codepoint = codepoint_to_int(codepoint)
  case codepoint {
    _ if codepoint < 0x80 -> 8
    _ if codepoint < 0x800 -> 16
    _ if codepoint < 0x10000 -> 24
    _ -> 32
  }
}

pub fn bit_array_codepoint_utf16_size(codepoint: UtfCodepoint) -> Int {
  case codepoint_to_int(codepoint) < 0x10000 {
    True -> 16
    False -> 32
  }
}

/// Writes `value` in `size` bits, in two's complement. A little endian
/// value has its bytes from the least significant, and then the bits that
/// are left.
pub fn bit_array_write_int(
  bytes: Words,
  at: Int,
  value: Int,
  size: Int,
  little: Bool,
) -> Int {
  let rest = bitwise_and(size, 7)
  case little {
    False -> {
      let at = put_bits(bytes, at, int_bits(value, size - rest, rest), rest)
      write_int_big(bytes, at, value, size - rest)
    }
    True -> {
      let whole = size - rest
      let at = write_int_little(bytes, at, value, 0, whole)
      put_bits(bytes, at, int_bits(value, whole, rest), rest)
    }
  }
}

/// Writes the `size` bits of `value` below bit `size`, a multiple of 8,
/// from the most significant byte.
fn write_int_big(bytes: Words, at: Int, value: Int, size: Int) -> Int {
  case size {
    0 -> at
    _ -> {
      let size = size - 8
      let at = put_bits(bytes, at, int_bits(value, size, 8), 8)
      write_int_big(bytes, at, value, size)
    }
  }
}

/// Writes the bytes of `value` from bit `shift` to bit `end`, from the
/// least significant.
fn write_int_little(
  bytes: Words,
  at: Int,
  value: Int,
  shift: Int,
  end: Int,
) -> Int {
  case shift == end {
    True -> at
    False -> {
      let at = put_bits(bytes, at, int_bits(value, shift, 8), 8)
      write_int_little(bytes, at, value, shift + 8, end)
    }
  }
}

/// The `count` bits of `value` from bit `shift`, from 0 to 8 of them.
fn int_bits(value: Int, shift: Int, count: Int) -> Int {
  bitwise_and(
    bitwise_shift_right(value, shift),
    bitwise_shift_left(1, count) - 1,
  )
}

/// Writes `value` as a float of `size` bits, 16, 32 or 64.
pub fn bit_array_write_float(
  bytes: Words,
  at: Int,
  value: Float,
  size: Int,
  little: Bool,
) -> Int {
  case size {
    16 -> bit_array_write_int(bytes, at, float16_bits(value), 16, little)
    _ -> write_float_loop(bytes, at, value, size, little, 0)
  }
}

/// Writes the bytes of the float of `size` bits of `value` from the byte
/// `index`.
fn write_float_loop(
  bytes: Words,
  at: Int,
  value: Float,
  size: Int,
  little: Bool,
  index: Int,
) -> Int {
  case index * 8 == size {
    True -> at
    False -> {
      let shift = case little {
        True -> index * 8
        False -> size - 8 - index * 8
      }
      let byte = case size {
        64 -> float64_byte(value, shift)
        _ -> float32_byte(value, shift)
      }
      let at = put_bits(bytes, at, byte, 8)
      write_float_loop(bytes, at, value, size, little, index + 1)
    }
  }
}

/// The bits of the 16 bits float nearest to `value`, rounded half to even.
fn float16_bits(value: Float) -> Int {
  let high = float64_byte(value, 56)
  let sign = bitwise_shift_left(bitwise_shift_right(high, 7), 15)
  let exponent =
    bitwise_or(
      bitwise_shift_left(bitwise_and(high, 0x7F), 4),
      bitwise_shift_right(float64_byte(value, 48), 4),
    )
  // The 24 most significant bits of the fraction, and whether any of the
  // others is set.
  let fraction =
    bitwise_or(
      bitwise_shift_left(bitwise_and(float64_byte(value, 48), 0x0F), 20),
      bitwise_or(
        bitwise_shift_left(float64_byte(value, 40), 12),
        bitwise_or(
          bitwise_shift_left(float64_byte(value, 32), 4),
          bitwise_shift_right(float64_byte(value, 24), 4),
        ),
      ),
    )
  let sticky =
    bitwise_and(float64_byte(value, 24), 0x0F) != 0
    || float64_byte(value, 16) != 0
    || float64_byte(value, 8) != 0
    || float64_byte(value, 0) != 0
  let half = case exponent {
    0x7FF ->
      case fraction != 0 || sticky {
        True -> 0x7E00
        False -> 0x7C00
      }
    _ -> {
      // The significand, with 24 bits after the point, and the exponent of
      // the half float.
      let significand = bitwise_or(bitwise_shift_left(1, 24), fraction)
      let exponent = exponent - 1023 + 15
      case exponent >= 1 {
        // The rounding may carry into the exponent, up to infinity.
        True ->
          min(
            { exponent - 1 } * 1024 + round_shift(significand, 14, sticky),
            0x7C00,
          )
        False -> round_shift(significand, 15 - exponent, sticky)
      }
    }
  }
  case half {
    0x7E00 -> half
    _ -> bitwise_or(sign, half)
  }
}

/// `value` shifted right by `shift`, rounded half to even. `sticky` is
/// whether there are bits set after the ones of `value`, which is less than
/// `2^25`.
fn round_shift(value: Int, shift: Int, sticky: Bool) -> Int {
  case shift > 26 {
    True -> 0
    False -> {
      let quotient = bitwise_shift_right(value, shift)
      let remainder = bitwise_and(value, bitwise_shift_left(1, shift) - 1)
      let half = bitwise_shift_left(1, shift - 1)
      case
        remainder > half
        || remainder == half
        && { sticky || bitwise_and(quotient, 1) == 1 }
      {
        True -> quotient + 1
        False -> quotient
      }
    }
  }
}

/// Writes the first `size` bits of `bits`.
pub fn bit_array_write_bits(
  bytes: Words,
  at: Int,
  bits: Bits,
  size: Int,
) -> Int {
  let _ = copy_bits(bytes, at, bits.bytes, bits.start, size)
  at + size
}

pub fn bit_array_write_utf8(bytes: Words, at: Int, string: String) -> Int {
  case bitwise_and(at, 7) {
    0 -> {
      let _ = bytes_write_string(bytes, bitwise_shift_right(at, 3), string)
      at + string_byte_size(string) * 8
    }
    _ -> {
      let bits = string_bits(string)
      let size = bits.end - bits.start
      let _ = copy_bits(bytes, at, bits.bytes, bits.start, size)
      at + size
    }
  }
}

pub fn bit_array_write_utf16(
  bytes: Words,
  at: Int,
  string: String,
  little: Bool,
) -> Int {
  write_unicode(bytes, at, string, 16, little)
}

pub fn bit_array_write_utf32(
  bytes: Words,
  at: Int,
  string: String,
  little: Bool,
) -> Int {
  write_unicode(bytes, at, string, 32, little)
}

/// Writes the code points of `string` in UTF-16 or UTF-32 (`unit` 16 or
/// 32).
fn write_unicode(
  bytes: Words,
  at: Int,
  string: String,
  unit: Int,
  little: Bool,
) -> Int {
  let bits = string_bits(string)
  let start = bits.start / 8
  write_unicode_loop(bytes, at, bits.bytes, start, bits.end / 8, unit, little)
}

/// Writes the code points of the UTF-8 bytes of `string` from `index` to
/// `end`.
fn write_unicode_loop(
  bytes: Words,
  at: Int,
  string: Words,
  index: Int,
  end: Int,
  unit: Int,
  little: Bool,
) -> Int {
  case index < end {
    False -> at
    True -> {
      let byte = bytes_get(string, index)
      let length = utf8_length(byte)
      let codepoint = utf8_decode(string, index, length)
      let at = case unit {
        16 -> write_utf16(bytes, at, codepoint, little)
        _ -> bit_array_write_int(bytes, at, codepoint, 32, little)
      }
      write_unicode_loop(bytes, at, string, index + length, end, unit, little)
    }
  }
}

pub fn bit_array_write_codepoint_utf8(
  bytes: Words,
  at: Int,
  codepoint: UtfCodepoint,
) -> Int {
  let codepoint = codepoint_to_int(codepoint)
  case codepoint {
    _ if codepoint < 0x80 -> put_bits(bytes, at, codepoint, 8)
    _ if codepoint < 0x800 ->
      at
      |> put_bits(bytes, _, 0xC0 + bitwise_shift_right(codepoint, 6), 8)
      |> put_continuation(bytes, _, codepoint, 0)
    _ if codepoint < 0x10000 ->
      at
      |> put_bits(bytes, _, 0xE0 + bitwise_shift_right(codepoint, 12), 8)
      |> put_continuation(bytes, _, codepoint, 6)
      |> put_continuation(bytes, _, codepoint, 0)
    _ ->
      at
      |> put_bits(bytes, _, 0xF0 + bitwise_shift_right(codepoint, 18), 8)
      |> put_continuation(bytes, _, codepoint, 12)
      |> put_continuation(bytes, _, codepoint, 6)
      |> put_continuation(bytes, _, codepoint, 0)
  }
}

/// Writes the UTF-8 continuation byte of the 6 bits of `codepoint` from bit
/// `shift`.
fn put_continuation(bytes: Words, at: Int, codepoint: Int, shift: Int) -> Int {
  put_bits(bytes, at, 0x80 + int_bits(codepoint, shift, 6), 8)
}

pub fn bit_array_write_codepoint_utf16(
  bytes: Words,
  at: Int,
  codepoint: UtfCodepoint,
  little: Bool,
) -> Int {
  write_utf16(bytes, at, codepoint_to_int(codepoint), little)
}

pub fn bit_array_write_codepoint_utf32(
  bytes: Words,
  at: Int,
  codepoint: UtfCodepoint,
  little: Bool,
) -> Int {
  bit_array_write_int(bytes, at, codepoint_to_int(codepoint), 32, little)
}

fn write_utf16(bytes: Words, at: Int, codepoint: Int, little: Bool) -> Int {
  case codepoint < 0x10000 {
    True -> bit_array_write_int(bytes, at, codepoint, 16, little)
    False -> {
      let codepoint = codepoint - 0x10000
      let high = 0xD800 + bitwise_shift_right(codepoint, 10)
      let low = 0xDC00 + bitwise_and(codepoint, 0x3FF)
      let at = bit_array_write_int(bytes, at, high, 16, little)
      bit_array_write_int(bytes, at, low, 16, little)
    }
  }
}

/// The number of bytes of the UTF-8 code point whose first byte is `byte`.
fn utf8_length(byte: Int) -> Int {
  case byte {
    _ if byte < 0x80 -> 1
    _ if byte < 0xE0 -> 2
    _ if byte < 0xF0 -> 3
    _ -> 4
  }
}

/// The code point of the `length` UTF-8 bytes of `bytes` from `index`.
fn utf8_decode(bytes: Words, index: Int, length: Int) -> Int {
  let first = bytes_get(bytes, index)
  case length {
    1 -> first
    2 -> utf8_continue(bytes, index + 1, 1, bitwise_and(first, 0x1F))
    3 -> utf8_continue(bytes, index + 1, 2, bitwise_and(first, 0x0F))
    _ -> utf8_continue(bytes, index + 1, 3, bitwise_and(first, 0x07))
  }
}

fn utf8_continue(bytes: Words, index: Int, count: Int, acc: Int) -> Int {
  case count {
    0 -> acc
    _ ->
      utf8_continue(
        bytes,
        index + 1,
        count - 1,
        acc * 64 + bitwise_and(bytes_get(bytes, index), 0x3F),
      )
  }
}

// -- Pattern matching ---------------------------------------------------------

// The compiler matches a bit array pattern segment by segment, from the
// position 0 of the bit array, checking that the bit array has the bits of a
// segment before reading them with the `bit_array_read_*` functions, or
// before comparing them to the bits of consecutive literal segments with
// `bit_array_match`. A code point is read with the check of its bits, and is
// -1 when they are not one.

/// Whether the bits of `bits` at `at` are the bits of `literal`.
pub fn bit_array_match(bits: Bits, at: Int, literal: Bits) -> Bool {
  equal_loop(
    bits.bytes,
    bits.start + at,
    literal.bytes,
    literal.start,
    literal.end - literal.start,
  )
}

/// The integer of the `size` bits of `bits` at `at`, in two's complement
/// when it is `signed`. A little endian integer has its bytes from the least
/// significant, and then the bits that are left.
pub fn bit_array_read_int(
  bits: Bits,
  at: Int,
  size: Int,
  little: Bool,
  signed: Bool,
) -> Int {
  let position = bits.start + at
  let value = case size, bitwise_and(position, 7) {
    8, 0 -> bytes_get(bits.bytes, bitwise_shift_right(position, 3))
    _, _ -> read_int(bits.bytes, position, size, little)
  }
  case signed {
    False -> value
    // The shifts are 0 when `size` is at least the number of bits of `Int`.
    True ->
      case
        size > 0 && bitwise_and(bitwise_shift_right(value, size - 1), 1) == 1
      {
        True -> value - bitwise_shift_left(1, size)
        False -> value
      }
  }
}

/// The unsigned integer of the `size` bits of `bytes` at `position`.
fn read_int(bytes: Words, position: Int, size: Int, little: Bool) -> Int {
  case bitwise_and(position, 7), little {
    0, False -> read_big(bytes, bitwise_shift_right(position, 3), size, 0)
    0, True -> {
      // From the most significant byte, after the whole bytes.
      let index = bitwise_shift_right(position, 3)
      let whole = bitwise_shift_right(size, 3)
      let high = read_big(bytes, index + whole, bitwise_and(size, 7), 0)
      read_little(bytes, index, index + whole, high)
    }
    _, False -> to_int_loop(bytes, position, size, 0)
    _, True -> read_int_little(bytes, position, size, 0, 0)
  }
}

/// The integer `acc` followed by the `remaining` bits from the byte
/// `index`.
fn read_big(bytes: Words, index: Int, remaining: Int, acc: Int) -> Int {
  case remaining >= 8 {
    True ->
      read_big(
        bytes,
        index + 1,
        remaining - 8,
        acc * 256 + bytes_get(bytes, index),
      )
    False ->
      case remaining {
        0 -> acc
        _ ->
          bitwise_shift_left(acc, remaining)
          + bitwise_shift_right(bytes_get(bytes, index), 8 - remaining)
      }
  }
}

/// The integer `acc` followed by the bytes from `end - 1` to `start`.
fn read_little(bytes: Words, start: Int, end: Int, acc: Int) -> Int {
  case end == start {
    True -> acc
    False ->
      read_little(bytes, start, end - 1, acc * 256 + bytes_get(bytes, end - 1))
  }
}

fn read_int_little(
  bytes: Words,
  position: Int,
  remaining: Int,
  shift: Int,
  acc: Int,
) -> Int {
  case remaining >= 8 {
    True ->
      read_int_little(
        bytes,
        position + 8,
        remaining - 8,
        shift + 8,
        bitwise_or(acc, bitwise_shift_left(byte_at(bytes, position), shift)),
      )
    False ->
      case remaining {
        0 -> acc
        _ ->
          bitwise_or(
            acc,
            bitwise_shift_left(bits_at(bytes, position, remaining), shift),
          )
      }
  }
}

/// The float of the `size` bits of `bits` at `at`, or the 0.0 of no bits.
/// It is not finite when the bits are not those of a finite float, or when
/// `size` is not 0, 16, 32 or 64, so that the pattern does not match.
pub fn bit_array_read_float(
  bits: Bits,
  at: Int,
  size: Int,
  little: Bool,
) -> Float {
  let position = bits.start + at
  case size {
    0 -> 0.0
    16 ->
      float32_from_bits(
        float16_to_float32(read_int(bits.bytes, position, 16, little)),
      )
    32 -> float32_from_bits(read_int(bits.bytes, position, 32, little))
    64 -> {
      let first = read_int(bits.bytes, position, 32, little)
      let second = read_int(bits.bytes, position + 32, 32, little)
      case little {
        False -> float64_from_bits(first, second)
        True -> float64_from_bits(second, first)
      }
    }
    // A NaN.
    _ -> float64_from_bits(0x7FF80000, 0)
  }
}

/// The bits of the 32 bits float of the 16 bits float of the bits `half`.
fn float16_to_float32(half: Int) -> Int {
  let sign = bitwise_shift_left(bitwise_shift_right(half, 15), 31)
  let exponent = bitwise_and(bitwise_shift_right(half, 10), 0x1F)
  let fraction = bitwise_and(half, 0x3FF)
  case exponent, fraction {
    0, 0 -> sign
    0, _ -> float16_subnormal(sign, fraction, 113)
    // An infinity or a NaN.
    0x1F, _ ->
      bitwise_or(sign, bitwise_or(0x7F800000, bitwise_shift_left(fraction, 13)))
    _, _ ->
      bitwise_or(
        sign,
        bitwise_or(
          bitwise_shift_left(exponent + 112, 23),
          bitwise_shift_left(fraction, 13),
        ),
      )
  }
}

/// Normalizes the `fraction` of a subnormal 16 bits float, whose value is
/// `fraction * 2^-24`, decrementing the `exponent` of the 32 bits float.
fn float16_subnormal(sign: Int, fraction: Int, exponent: Int) -> Int {
  case bitwise_and(fraction, 0x400) {
    0 -> float16_subnormal(sign, fraction * 2, exponent - 1)
    _ ->
      bitwise_or(
        sign,
        bitwise_or(
          bitwise_shift_left(exponent, 23),
          bitwise_shift_left(bitwise_and(fraction, 0x3FF), 13),
        ),
      )
  }
}

/// The `size` bits of `bits` at `at`.
pub fn bit_array_read_bits(bits: Bits, at: Int, size: Int) -> Bits {
  let start = bits.start + at
  Bits(bits.bytes, start, start + size, bits.written)
}

/// The code point of the UTF-8 bytes of `bits` at `at`, without overlong
/// encodings or surrogates, or -1.
pub fn bit_array_read_codepoint_utf8(bits: Bits, at: Int) -> Int {
  let position = bits.start + at
  case position + 8 <= bits.end {
    False -> -1
    True -> {
      let byte = byte_at(bits.bytes, position)
      case byte {
        _ if byte < 0x80 -> byte
        _ if byte < 0xC2 -> -1
        _ if byte < 0xE0 ->
          codepoint_tail(bits, position, 1, 0x80, 0xBF, bitwise_and(byte, 0x1F))
        0xE0 -> codepoint_tail(bits, position, 2, 0xA0, 0xBF, 0)
        0xED -> codepoint_tail(bits, position, 2, 0x80, 0x9F, 0x0D)
        _ if byte < 0xF0 ->
          codepoint_tail(bits, position, 2, 0x80, 0xBF, bitwise_and(byte, 0x0F))
        0xF0 -> codepoint_tail(bits, position, 3, 0x90, 0xBF, 0)
        0xF4 -> codepoint_tail(bits, position, 3, 0x80, 0x8F, 4)
        _ if byte < 0xF4 ->
          codepoint_tail(bits, position, 3, 0x80, 0xBF, bitwise_and(byte, 0x07))
        _ -> -1
      }
    }
  }
}

/// The code point of the `count` bytes after the first one at `position`,
/// the first of them from `low` to `high`, whose first bits are `acc`, or
/// -1.
fn codepoint_tail(
  bits: Bits,
  position: Int,
  count: Int,
  low: Int,
  high: Int,
  acc: Int,
) -> Int {
  let next = position + 8
  case next + count * 8 <= bits.end {
    False -> -1
    True -> {
      let byte = byte_at(bits.bytes, next)
      case low <= byte && byte <= high {
        False -> -1
        True ->
          codepoint_continuation(
            bits.bytes,
            next + 8,
            count - 1,
            acc * 64 + bitwise_and(byte, 0x3F),
          )
      }
    }
  }
}

fn codepoint_continuation(
  bytes: Words,
  position: Int,
  count: Int,
  acc: Int,
) -> Int {
  case count {
    0 -> acc
    _ -> {
      let byte = byte_at(bytes, position)
      case 0x80 <= byte && byte <= 0xBF {
        False -> -1
        True ->
          codepoint_continuation(
            bytes,
            position + 8,
            count - 1,
            acc * 64 + bitwise_and(byte, 0x3F),
          )
      }
    }
  }
}

/// The code point of the UTF-16 code units of `bits` at `at`, or -1.
pub fn bit_array_read_codepoint_utf16(
  bits: Bits,
  at: Int,
  little: Bool,
) -> Int {
  let position = bits.start + at
  case position + 16 <= bits.end {
    False -> -1
    True -> {
      let unit = read_int(bits.bytes, position, 16, little)
      case unit {
        _ if unit < 0xD800 || unit > 0xDFFF -> unit
        _ if unit >= 0xDC00 || position + 32 > bits.end -> -1
        _ -> {
          let low = read_int(bits.bytes, position + 16, 16, little)
          case 0xDC00 <= low && low <= 0xDFFF {
            True -> 0x10000 + { unit - 0xD800 } * 0x400 + { low - 0xDC00 }
            False -> -1
          }
        }
      }
    }
  }
}

/// The code point of the UTF-32 code unit of `bits` at `at`, or -1.
pub fn bit_array_read_codepoint_utf32(
  bits: Bits,
  at: Int,
  little: Bool,
) -> Int {
  let position = bits.start + at
  case position + 32 <= bits.end {
    False -> -1
    True -> {
      let unit = read_int(bits.bytes, position, 32, little)
      // `unit` is negative from bit 31 when `Int` has 32 bits.
      case 0 <= unit && unit < 0xD800 || 0xE000 <= unit && unit <= 0x10FFFF {
        True -> unit
        False -> -1
      }
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
pub fn bit_array_slice(
  bits: Bits,
  position: Int,
  length: Int,
) -> Result(Bits, Nil) {
  let #(from, to) = case length < 0 {
    True -> #(position + length, position)
    False -> #(position, position + length)
  }
  case from < 0 || to * 8 > bits.end - bits.start {
    True -> Error(Nil)
    False -> {
      let start = bits.start + from * 8
      Ok(Bits(bits.bytes, start, bits.start + to * 8, bits.written))
    }
  }
}

pub fn bit_array_starts_with(bits: Bits, prefix: Bits) -> Bool {
  prefix.end - prefix.start <= bits.end - bits.start
  && bit_array_match(bits, 0, prefix)
}

/// The order of `a` and `b` as `-1`, `0` or `1`: byte by byte, and then by
/// the value and size of the bits after the last whole byte of the shortest.
pub fn bit_array_compare(a: Bits, b: Bits) -> Int {
  let a_size = a.end - a.start
  let b_size = b.end - b.start
  let whole = bitwise_shift_right(min(a_size, b_size), 3)
  let order = case bitwise_and(bitwise_or(a.start, b.start), 7) {
    0 ->
      compare_bytes(
        a.bytes,
        bitwise_shift_right(a.start, 3),
        b.bytes,
        bitwise_shift_right(b.start, 3),
        whole,
      )
    _ -> compare_bits(a.bytes, a.start, b.bytes, b.start, whole)
  }
  case order {
    0 -> {
      let a_rest = a_size - whole * 8
      let b_rest = b_size - whole * 8
      let at = whole * 8
      case a_rest, b_rest {
        0, 0 -> 0
        _, 0 -> 1
        0, _ -> -1
        _, _ ->
          case
            compare_values(
              a.bytes,
              a.start + at,
              a_rest,
              b.bytes,
              b.start + at,
              b_rest,
              0,
            )
          {
            0 -> sign(a_rest - b_rest)
            order -> order
          }
      }
    }
    _ -> order
  }
}

/// The order of the `count` bytes of `a` from the byte `i` and of `b` from
/// the byte `j`.
fn compare_bytes(a: Words, i: Int, b: Words, j: Int, count: Int) -> Int {
  case count {
    0 -> 0
    _ -> {
      let x = bytes_get(a, i)
      let y = bytes_get(b, j)
      case x == y {
        True -> compare_bytes(a, i + 1, b, j + 1, count - 1)
        False -> sign(x - y)
      }
    }
  }
}

/// The order of the `count` bytes of `a` at `i` and of `b` at `j`.
fn compare_bits(a: Words, i: Int, b: Words, j: Int, count: Int) -> Int {
  case count {
    0 -> 0
    _ -> {
      let x = byte_at(a, i)
      let y = byte_at(b, j)
      case x == y {
        True -> compare_bits(a, i + 8, b, j + 8, count - 1)
        False -> sign(x - y)
      }
    }
  }
}

/// Compares the unsigned integers of `a_size` bits of `a` at `i` and
/// `b_size` bits of `b` at `j`, from the most significant bit of the
/// largest, at `bit` from the left.
fn compare_values(
  a: Words,
  i: Int,
  a_size: Int,
  b: Words,
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
fn bit_or_zero(bytes: Words, position: Int, offset: Int) -> Int {
  case offset < 0 {
    True -> 0
    False -> bits_at(bytes, position + offset, 1)
  }
}

fn to_int_loop(bytes: Words, position: Int, remaining: Int, acc: Int) -> Int {
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
  bitwise_and(bits.end - bits.start, 7) == 0 && is_utf8(aligned(bits))
}

/// The string of the bytes of `bits`, which shares them when `bits` starts
/// at a whole byte.
pub fn bit_array_to_string(bits: Bits) -> Result(String, Nil) {
  case bitwise_and(bits.end - bits.start, 7) == 0 {
    True -> {
      let bits = aligned(bits)
      case is_utf8(bits) {
        True -> {
          let start = bitwise_shift_right(bits.start, 3)
          let size = bitwise_shift_right(bits.end - bits.start, 3)
          Ok(bytes_to_string(bits.bytes, start, size))
        }
        False -> Error(Nil)
      }
    }
    False -> Error(Nil)
  }
}

/// Whether the bytes of `bits`, which start and end at whole bytes, are
/// UTF-8.
fn is_utf8(bits: Bits) -> Bool {
  let start = bitwise_shift_right(bits.start, 3)
  bytes_is_utf8(
    bits.bytes,
    start,
    bitwise_shift_right(bits.end - bits.start, 3),
  )
}

// -- Base 64 and base 16 ------------------------------------------------------
//
// As on Erlang: the bits of the last byte are padded with zeros, decoding
// skips spaces, tabs and line breaks, a base 64 string whose size is not a
// multiple of 4 is completed with `=`, and the bits after the last byte of a
// group with `=` are ignored.

pub fn bit_array_base64_encode(bits: Bits, padding: Bool) -> String {
  base64_encode(bits, padding, False)
}

pub fn bit_array_base64_url_encode(bits: Bits, padding: Bool) -> String {
  base64_encode(bits, padding, True)
}

fn base64_encode(bits: Bits, padding: Bool, url: Bool) -> String {
  let bits = aligned(bit_array_pad_to_bytes(bits))
  let start = bitwise_shift_right(bits.start, 3)
  let size = bitwise_shift_right(bits.end - bits.start, 3)
  let length = case size % 3, padding {
    0, _ -> size / 3 * 4
    _, True -> size / 3 * 4 + 4
    rest, False -> size / 3 * 4 + rest + 1
  }
  let out = bytes_new(length)
  let encoder = Base64Encoder(bits.bytes, start + size, out, length, url)
  let _ = base64_encode_loop(encoder, start, 0)
  bytes_to_string(out, 0, length)
}

/// The bytes to encode, which end at `end`, and the `length` bytes of the
/// encoded string.
type Base64Encoder {
  Base64Encoder(bytes: Words, end: Int, out: Words, length: Int, url: Bool)
}

/// Encodes the groups of 3 bytes from `index`, the last one with the `=`
/// that fit in the encoded string.
fn base64_encode_loop(encoder: Base64Encoder, index: Int, at: Int) -> Nil {
  let Base64Encoder(bytes:, end:, ..) = encoder
  case index < end {
    False -> Nil
    True -> {
      let digits = min(end - index, 3) + 1
      let group =
        bitwise_or(
          bitwise_or(
            bitwise_shift_left(bytes_get(bytes, index), 16),
            bitwise_shift_left(byte_or_zero(bytes, index + 1, end), 8),
          ),
          byte_or_zero(bytes, index + 2, end),
        )
      let _ = put_base64(encoder, at, group, 0, digits)
      let _ = put_base64(encoder, at, group, 1, digits)
      let _ = put_base64(encoder, at, group, 2, digits)
      let _ = put_base64(encoder, at, group, 3, digits)
      base64_encode_loop(encoder, index + 3, at + 4)
    }
  }
}

fn byte_or_zero(bytes: Words, index: Int, end: Int) -> Int {
  case index < end {
    True -> bytes_get(bytes, index)
    False -> 0
  }
}

/// Writes the digit `i` of the 4 of `group` at `at`, or `=` when it is not
/// one of the first `digits` and the encoded string has room for it.
fn put_base64(
  encoder: Base64Encoder,
  at: Int,
  group: Int,
  i: Int,
  digits: Int,
) -> Nil {
  case i < digits, at + i < encoder.length {
    True, _ -> {
      let value = bitwise_and(bitwise_shift_right(group, 18 - 6 * i), 63)
      bytes_set(encoder.out, at + i, base64_digit(value, encoder.url))
    }
    False, True -> bytes_set(encoder.out, at + i, 61)
    False, False -> Nil
  }
}

fn base64_digit(value: Int, url: Bool) -> Int {
  case value {
    _ if value < 26 -> value + 65
    _ if value < 52 -> value + 71
    _ if value < 62 -> value - 4
    62 if url -> 45
    62 -> 43
    _ if url -> 95
    _ -> 47
  }
}

pub fn bit_array_base64_decode(encoded: String) -> Result(Bits, Nil) {
  base64_decode(encoded, False)
}

/// Accepts both alphabets, as `gleam/bit_array` does.
pub fn bit_array_base64_url_decode(encoded: String) -> Result(Bits, Nil) {
  base64_decode(encoded, True)
}

fn base64_decode(encoded: String, url: Bool) -> Result(Bits, Nil) {
  let bits = string_bits(encoded)
  let start = bits.start / 8
  let size = bits.end / 8 - start
  let missing = case size % 4 {
    0 -> 0
    rest -> 4 - rest
  }
  let out = bytes_new({ size + missing } / 4 * 3)
  let decoder = Base64Decoder(bits.bytes, start + size, out, url)
  case base64_decode_loop(decoder, start, start + size + missing, 0, 0, 0, 0) {
    Ok(at) -> Ok(bit_array_from_bytes(out, at * 8))
    Error(Nil) -> Error(Nil)
  }
}

/// The bytes of the string to decode, which end at `last`, and the bytes
/// to which they are decoded.
type Base64Decoder {
  Base64Decoder(bytes: Words, last: Int, out: Words, url: Bool)
}

/// Decodes the characters from `index` to `end`, which are `=` after
/// `last`. `group` has the `count` digits of the current group, followed by
/// `pads` `=`. Returns the number of bytes written.
fn base64_decode_loop(
  decoder: Base64Decoder,
  index: Int,
  end: Int,
  at: Int,
  group: Int,
  count: Int,
  pads: Int,
) -> Result(Int, Nil) {
  let next = index + 1
  case index < end {
    False ->
      case pads {
        0 if count == 0 -> Ok(at)
        0 -> Error(Nil)
        _ if count + pads == 4 -> {
          // 2 digits have 1 byte and 4 bits more, 3 digits 2 bytes and 2
          // bits more.
          let value = bitwise_shift_right(group, 2 * { 4 - count })
          Ok(put_big(decoder.out, at, value, count - 1))
        }
        _ -> Error(Nil)
      }
    True -> {
      let char = case index < decoder.last {
        True -> bytes_get(decoder.bytes, index)
        False -> 61
      }
      case char {
        32 | 9 | 10 | 13 ->
          base64_decode_loop(decoder, next, end, at, group, count, pads)
        61 if count >= 2 && count + pads < 4 ->
          base64_decode_loop(decoder, next, end, at, group, count, pads + 1)
        61 -> Error(Nil)
        _ ->
          case base64_value(char, decoder.url) {
            -1 -> Error(Nil)
            _ if pads > 0 -> Error(Nil)
            value -> {
              let group = bitwise_or(bitwise_shift_left(group, 6), value)
              case count {
                3 -> {
                  let at = put_big(decoder.out, at, group, 3)
                  base64_decode_loop(decoder, next, end, at, 0, 0, 0)
                }
                _ ->
                  base64_decode_loop(
                    decoder,
                    next,
                    end,
                    at,
                    group,
                    count + 1,
                    0,
                  )
              }
            }
          }
      }
    }
  }
}

/// The value of a base 64 digit, or -1. `url` also accepts `-` and `_`.
fn base64_value(char: Int, url: Bool) -> Int {
  case char {
    _ if char >= 65 && char <= 90 -> char - 65
    _ if char >= 97 && char <= 122 -> char - 71
    _ if char >= 48 && char <= 57 -> char + 4
    43 -> 62
    47 -> 63
    45 if url -> 62
    95 if url -> 63
    _ -> -1
  }
}

/// Writes the `count` low bytes of `value` at `at`, the most significant
/// first, and returns the index after them.
fn put_big(out: Words, at: Int, value: Int, count: Int) -> Int {
  case count {
    0 -> at
    _ -> {
      let shift = 8 * { count - 1 }
      let byte = bitwise_and(bitwise_shift_right(value, shift), 0xFF)
      let _ = bytes_set(out, at, byte)
      put_big(out, at + 1, value, count - 1)
    }
  }
}

/// The digits are uppercase.
pub fn bit_array_base16_encode(bits: Bits) -> String {
  let bits = aligned(bit_array_pad_to_bytes(bits))
  let start = bitwise_shift_right(bits.start, 3)
  let size = bitwise_shift_right(bits.end - bits.start, 3)
  let out = bytes_new(2 * size)
  let _ = base16_encode_loop(bits.bytes, start, out, 0, 2 * size)
  bytes_to_string(out, 0, 2 * size)
}

fn base16_encode_loop(
  bytes: Words,
  index: Int,
  out: Words,
  at: Int,
  end: Int,
) -> Nil {
  case at < end {
    False -> Nil
    True -> {
      let byte = bytes_get(bytes, index)
      let _ = bytes_set(out, at, hex_digit(bitwise_shift_right(byte, 4)))
      let _ = bytes_set(out, at + 1, hex_digit(bitwise_and(byte, 15)))
      base16_encode_loop(bytes, index + 1, out, at + 2, end)
    }
  }
}

fn hex_digit(value: Int) -> Int {
  case value < 10 {
    True -> value + 48
    False -> value + 55
  }
}

/// Accepts uppercase and lowercase digits.
pub fn bit_array_base16_decode(encoded: String) -> Result(Bits, Nil) {
  let bits = string_bits(encoded)
  let start = bits.start / 8
  let size = bits.end / 8 - start
  case bitwise_and(size, 1) {
    0 -> {
      let out = bytes_new(size / 2)
      case base16_decode_loop(bits.bytes, start, start + size, out, 0) {
        True -> Ok(bit_array_from_bytes(out, size * 4))
        False -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

fn base16_decode_loop(
  bytes: Words,
  index: Int,
  end: Int,
  out: Words,
  at: Int,
) -> Bool {
  case index < end {
    False -> True
    True -> {
      let high = hex_value(bytes_get(bytes, index))
      let low = hex_value(bytes_get(bytes, index + 1))
      case high >= 0 && low >= 0 {
        True -> {
          let _ =
            bytes_set(out, at, bitwise_or(bitwise_shift_left(high, 4), low))
          base16_decode_loop(bytes, index + 2, end, out, at + 1)
        }
        False -> False
      }
    }
  }
}

/// The value of a base 16 digit, or -1.
fn hex_value(char: Int) -> Int {
  case char {
    _ if char >= 48 && char <= 57 -> char - 48
    _ if char >= 65 && char <= 70 -> char - 55
    _ if char >= 97 && char <= 102 -> char - 87
    _ -> -1
  }
}

// -- Equality, hash and inspect -----------------------------------------------

pub fn bit_array_equal(a: Bits, b: Bits) -> Bool {
  a.end - a.start == b.end - b.start && bit_array_match(a, 0, b)
}

/// Whether the `size` bits of `a` at `i` and of `b` at `j` are equal.
fn equal_loop(a: Words, i: Int, b: Words, j: Int, size: Int) -> Bool {
  case bitwise_and(bitwise_or(i, j), 7) {
    0 ->
      equal_bytes(
        a,
        bitwise_shift_right(i, 3),
        b,
        bitwise_shift_right(j, 3),
        size,
      )
    _ -> equal_bits(a, i, b, j, size)
  }
}

/// Whether the `size` bits of `a` from the byte `i` and of `b` from the
/// byte `j` are equal.
fn equal_bytes(a: Words, i: Int, b: Words, j: Int, size: Int) -> Bool {
  case size >= 8 {
    True ->
      case bytes_get(a, i) == bytes_get(b, j) {
        True -> equal_bytes(a, i + 1, b, j + 1, size - 8)
        False -> False
      }
    False ->
      size == 0
      || bitwise_shift_right(bytes_get(a, i), 8 - size)
      == bitwise_shift_right(bytes_get(b, j), 8 - size)
  }
}

fn equal_bits(a: Words, i: Int, b: Words, j: Int, size: Int) -> Bool {
  case size >= 8 {
    True ->
      case byte_at(a, i) == byte_at(b, j) {
        True -> equal_bits(a, i + 8, b, j + 8, size - 8)
        False -> False
      }
    False -> size == 0 || bits_at(a, i, size) == bits_at(b, j, size)
  }
}

/// FNV-1a of the bytes, then of the bits after the last whole byte and
/// their number.
pub fn bit_array_hash(bits: Bits) -> Int {
  let size = bits.end - bits.start
  let whole = bitwise_shift_right(size, 3)
  let acc = case bitwise_and(bits.start, 7) {
    0 ->
      hash_bytes(
        bits.bytes,
        bitwise_shift_right(bits.start, 3),
        whole,
        -2_128_831_035,
      )
    _ -> hash_bits(bits.bytes, bits.start, whole, -2_128_831_035)
  }
  case size - whole * 8 {
    0 -> acc
    rest ->
      fnv(fnv(acc, bits_at(bits.bytes, bits.start + whole * 8, rest)), rest)
  }
}

/// The hash `acc` of the `count` bytes of `bytes` from the byte `index`.
fn hash_bytes(bytes: Words, index: Int, count: Int, acc: Int) -> Int {
  case count {
    0 -> acc
    _ ->
      hash_bytes(bytes, index + 1, count - 1, fnv(acc, bytes_get(bytes, index)))
  }
}

/// The hash `acc` of the `count` bytes of `bytes` at `position`.
fn hash_bits(bytes: Words, position: Int, count: Int, acc: Int) -> Int {
  case count {
    0 -> acc
    _ ->
      hash_bits(
        bytes,
        position + 8,
        count - 1,
        fnv(acc, byte_at(bytes, position)),
      )
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
  bytes: Words,
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
fn byte_at(bytes: Words, position: Int) -> Int {
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
fn bits_at(bytes: Words, position: Int, count: Int) -> Int {
  let index = bitwise_shift_right(position, 3)
  let shift = bitwise_and(position, 7)
  let high = bitwise_shift_left(bytes_get(bytes, index), shift)
  let window = case shift + count > 8 {
    True ->
      bitwise_or(
        high,
        bitwise_shift_right(bytes_get(bytes, index + 1), 8 - shift),
      )
    False -> high
  }
  bitwise_shift_right(bitwise_and(window, 0xFF), 8 - count)
}

/// Sets the `count` bits of `dst` at `at`, which are zero, to the bits of
/// `value`, from 1 to 8 of them.
fn or_bits(dst: Words, at: Int, value: Int, count: Int) -> Nil {
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

/// Writes the `count` bits of `value`, from 0 to 8 of them, to `dst` at
/// `at`, whose bits are zero, and returns the position after them.
fn put_bits(dst: Words, at: Int, value: Int, count: Int) -> Int {
  let _ = case count, bitwise_and(at, 7) {
    0, _ -> Nil
    8, 0 -> bytes_set(dst, bitwise_shift_right(at, 3), value)
    _, _ -> or_bits(dst, at, value, count)
  }
  at + count
}

fn sign(value: Int) -> Int {
  case value {
    0 -> 0
    _ if value < 0 -> -1
    _ -> 1
  }
}

fn min(a: Int, b: Int) -> Int {
  case a < b {
    True -> a
    False -> b
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
fn bytes_new(size: Int) -> Words

/// Whether the `size` bytes of `bytes` from `start` are UTF-8, without
/// overlong encodings, surrogates or code points after U+10FFFF.
@external(webassembly, "builtins", "_bytes_is_utf8")
fn bytes_is_utf8(bytes: Words, start: Int, size: Int) -> Bool

@external(webassembly, "builtins", "_bytes_size")
fn bytes_size(bytes: Words) -> Int

@external(webassembly, "builtins", "_bytes_get")
fn bytes_get(bytes: Words, index: Int) -> Int

/// Only on the bytes the caller has just created, or after the bits written
/// to them.
@external(webassembly, "builtins", "_bytes_set")
fn bytes_set(bytes: Words, index: Int, byte: Int) -> Nil

/// Writes `count` bits of `src` at `from` to `dst` at `at`. The bits of
/// `dst` from `at` on are zero.
@external(webassembly, "builtins", "_bits_copy")
fn copy_bits(dst: Words, at: Int, src: Words, from: Int, count: Int) -> Nil

/// Writes the UTF-8 bytes of `string` to `bytes` from the byte `at`. Only
/// after the bits written to them.
@external(webassembly, "builtins", "_bytes_write_string")
fn bytes_write_string(bytes: Words, at: Int, string: String) -> Nil

@external(webassembly, "builtins", "_cell_new")
fn cell_new(value: Int) -> Cell

@external(webassembly, "builtins", "_cell_get")
fn cell_get(cell: Cell) -> Int

@external(webassembly, "builtins", "_cell_set")
fn cell_set(cell: Cell, value: Int) -> Nil

/// The bit array of the UTF-8 bytes of `string`.
@external(webassembly, "builtins", "_string_bits")
fn string_bits(string: String) -> Bits

/// The string of the `size` bytes of `bytes` from `start`, which are UTF-8.
@external(webassembly, "builtins", "_bytes_to_string")
fn bytes_to_string(bytes: Words, start: Int, size: Int) -> String

@external(webassembly, "builtins", "_utf_codepoint_to_int")
fn codepoint_to_int(codepoint: UtfCodepoint) -> Int

/// The byte of the bits of `value` as a 64 bits float from bit `shift`.
@external(webassembly, "builtins", "_float64_byte")
fn float64_byte(value: Float, shift: Int) -> Int

/// The byte of the bits of `value` as a 32 bits float from bit `shift`.
@external(webassembly, "builtins", "_float32_byte")
fn float32_byte(value: Float, shift: Int) -> Int

/// The float of the high and the low 32 bits of a 64 bits float.
@external(webassembly, "builtins", "_float64_from_bits")
fn float64_from_bits(high: Int, low: Int) -> Float

/// The float of the bits of a 32 bits float.
@external(webassembly, "builtins", "_float32_from_bits")
fn float32_from_bits(bits: Int) -> Float
