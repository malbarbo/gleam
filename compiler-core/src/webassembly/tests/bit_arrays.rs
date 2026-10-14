// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Bit arrays: the literals, and the functions of the runtime module
//! `builtins/bit_array`.

use super::{TEST_PACKAGE, compile_wasm, for_each_operator, run_wasm_with_config};
use crate::config::{WasmFloat, WasmInt};

const BIT_ARRAY: &str = r#"
@external(webassembly, "builtins", "bit_array_from_string")
pub fn from_string(x: String) -> BitArray

@external(webassembly, "builtins", "bit_array_bit_size")
pub fn bit_size(x: BitArray) -> Int

@external(webassembly, "builtins", "bit_array_byte_size")
pub fn byte_size(x: BitArray) -> Int

@external(webassembly, "builtins", "bit_array_pad_to_bytes")
pub fn pad_to_bytes(x: BitArray) -> BitArray

@external(webassembly, "builtins", "bit_array_slice")
pub fn slice(x: BitArray, position: Int, length: Int) -> Result(BitArray, Nil)

@external(webassembly, "builtins", "bit_array_concat")
pub fn concat(arrays: List(BitArray)) -> BitArray

@external(webassembly, "builtins", "bit_array_is_utf8")
pub fn is_utf8(x: BitArray) -> Bool

@external(webassembly, "builtins", "bit_array_to_string")
pub fn to_string(x: BitArray) -> Result(String, Nil)

@external(webassembly, "builtins", "bit_array_inspect")
pub fn inspect(x: BitArray) -> String

@external(webassembly, "builtins", "bit_array_compare")
pub fn compare(a: BitArray, b: BitArray) -> Int

@external(webassembly, "builtins", "bit_array_starts_with")
pub fn starts_with(x: BitArray, prefix: BitArray) -> Bool

@external(webassembly, "builtins", "bit_array_to_int_and_size")
pub fn to_int_and_size(x: BitArray) -> #(Int, Int)

@external(webassembly, "builtins", "_inspect")
pub fn show(value: a) -> String

@external(webassembly, "builtins", "_hash")
pub fn hash(value: a) -> Int
"#;

/// Runs `src` with `Int` of 32 and of 64 bits, and returns what it writes to
/// stderr with 32 bits.
#[track_caller]
fn run(src: &str) -> String {
    let mut stderr = vec![];
    for int in [WasmInt::I32, WasmInt::I64] {
        let result = run_wasm_with_config(
            src,
            vec![(TEST_PACKAGE, "bit_array", BIT_ARRAY)],
            int,
            WasmFloat::F64,
        );
        assert!(
            result.status.success(),
            "WASM execution failed ({int:?}):\n{}",
            result.stderr
        );
        stderr.push(result.stderr);
    }
    assert_eq!(stderr[0], stderr[1]);
    stderr.swap_remove(0)
}

#[test]
fn literals() {
    let _ = run(r#"
import bit_array

pub fn main() {
  let assert 0 = bit_array.bit_size(<<>>)
  let assert 24 = bit_array.bit_size(<<1, 2, 3>>)
  let assert 3 = bit_array.byte_size(<<1, 2, 3>>)
  let assert 12 = bit_array.bit_size(<<1:4, 2:8>>)
  let assert 2 = bit_array.byte_size(<<1:4, 2:8>>)
  let assert "<<1, 2, 3>>" = bit_array.inspect(<<1, 2, 3>>)
  let assert "<<1, 2, 3:size(2)>>" = bit_array.inspect(<<1, 2, 3:2>>)
  let assert "<<16, 2:size(4)>>" = bit_array.inspect(<<1:4, 2:8>>)
  let assert "<<255, 254>>" = bit_array.inspect(<<-2:16>>)
  let assert "<<2, 1>>" = bit_array.inspect(<<258:16-little>>)
  let assert "<<0, 0, 1, 0>>" = bit_array.inspect(<<256:32>>)
  let assert "<<97, 98, 195, 169>>" = bit_array.inspect(<<"abé":utf8>>)
  let assert "<<0, 97, 216, 61, 222, 0>>" = bit_array.inspect(<<"a😀":utf16>>)
  let assert "<<97, 0, 0, 0>>" = bit_array.inspect(<<"a":utf32-little>>)
  let assert "<<63, 240, 0, 0, 0, 0, 0, 0>>" = bit_array.inspect(<<1.0:float>>)
  let assert "<<0, 0, 128, 63>>" = bit_array.inspect(<<1.0:32-float-little>>)
  let assert "<<1, 2, 3, 4>>" = bit_array.inspect(<<<<1, 2>>:bits, 3, <<4>>:bits>>)
  let assert "<<1, 0:size(4)>>" = bit_array.inspect(<<<<1, 2>>:bits-size(12)>>)
  let assert "<<1, 2>>" = bit_array.inspect(<<<<1, 2, 3>>:bits-size(2)-unit(8)>>)
  Nil
}
"#);
}

#[test]
fn constants() {
    let _ = run(r#"
import bit_array

const key = <<1, 2, "a":utf8>>

const keys = [key, <<3:4>>]

const sized = <<1:size(12)>>

pub fn main() {
  let assert "<<1, 2, 97>>" = bit_array.inspect(key)
  let assert [_, b] = keys
  let assert "<<3:size(4)>>" = bit_array.inspect(b)
  let assert 12 = bit_array.bit_size(sized)
  let assert "<<1, 2>>" = bit_array.inspect(<<key:bits-size(16)>>)
  Nil
}
"#);
}

/// Equal bit arrays are equal and have the same hash whatever the position
/// of their bits in their bytes.
#[test]
fn equality_and_hash() {
    let _ = run(r#"
import bit_array

pub fn main() {
  let assert Ok(a) = bit_array.slice(<<0, 1, 2, 3>>, 1, 2)
  let b = <<1, 2>>
  let assert True = a == b
  let assert True = bit_array.hash(a) == bit_array.hash(b)
  // `c` starts at bit 4 of its bytes.
  let c = bit_array.concat([<<5:4>>, <<1, 2>>, <<3:4>>])
  let assert Ok(d) = bit_array.slice(c, 1, 1)
  let assert False = d == <<1>>
  let assert True = c == <<5:4, 1, 2, 3:4>>
  let assert True = bit_array.hash(c) == bit_array.hash(<<5:4, 1, 2, 3:4>>)
  let assert False = <<1:4>> == <<1:5>>
  let assert False = <<1, 2>> == <<1, 3>>
  let assert False = <<1, 2:4>> == <<1, 3:4>>
  let assert True = bit_array.hash(<<1:4>>) != bit_array.hash(<<1:5>>)
  let assert True = #(<<1>>, [<<2, 3>>]) == #(<<1>>, [<<2, 3>>])
  let assert False = Ok(<<1>>) == Ok(<<2>>)
  Nil
}
"#);
}

#[test]
fn inspect() {
    let stderr = run(r#"
import bit_array

pub type Box {
  Box(bits: BitArray)
}

pub fn main() {
  echo <<>>
  echo <<1, 2, 3:2>>
  echo #(Box(<<"a":utf8>>), [<<255>>])
  let _ = bit_array.show(<<1>>)
  Nil
}
"#);
    insta::assert_snapshot!(stderr, @r#"
    src/my/mod.gleam:9
    <<>>
    src/my/mod.gleam:10
    <<1, 2, 3:size(2)>>
    src/my/mod.gleam:11
    #(Box(<<97>>), [<<255>>])
    "#);
}

#[test]
fn strings() {
    let _ = run(r#"
import bit_array

pub fn main() {
  let bits = bit_array.from_string("héllo")
  let assert 6 = bit_array.byte_size(bits)
  let assert True = bits == <<"héllo":utf8>>
  let assert Ok("héllo") = bit_array.to_string(bits)
  let assert Ok(part) = bit_array.slice(bits, 1, 2)
  let assert Ok("é") = bit_array.to_string(part)
  let assert Ok(part) = bit_array.slice(bits, 1, 1)
  let assert Error(Nil) = bit_array.to_string(part)
  let assert True = bit_array.is_utf8(<<"a€😀":utf8>>)
  let assert False = bit_array.is_utf8(<<0xC0, 0x80>>)
  let assert False = bit_array.is_utf8(<<0xED, 0xA0, 0x80>>)
  let assert False = bit_array.is_utf8(<<0xF4, 0x90, 0x80, 0x80>>)
  let assert False = bit_array.is_utf8(<<0xE2, 0x82>>)
  let assert False = bit_array.is_utf8(<<97:7>>)
  // Not at a whole byte.
  let shifted = bit_array.concat([<<1:4>>, <<"ab":utf8>>, <<0:4>>])
  let assert Ok(ab) = bit_array.slice(bit_array.concat([<<0:4>>, shifted]), 1, 2)
  let assert Ok("ab") = bit_array.to_string(ab)
  let assert Ok("") = bit_array.to_string(<<>>)
  Nil
}
"#);
}

#[test]
fn slice_concat_and_pad() {
    let _ = run(r#"
import bit_array

pub fn main() {
  let bits = <<1, 2, 3, 4>>
  let assert Ok(a) = bit_array.slice(bits, 1, 2)
  let assert True = a == <<2, 3>>
  let assert Ok(b) = bit_array.slice(bits, 4, -2)
  let assert True = b == <<3, 4>>
  let assert Ok(c) = bit_array.slice(bits, 4, 0)
  let assert True = c == <<>>
  let assert Error(Nil) = bit_array.slice(bits, 3, 2)
  let assert Error(Nil) = bit_array.slice(bits, 1, -2)
  let assert True = bit_array.concat([]) == <<>>
  let assert True = bit_array.concat([<<1>>, <<>>, <<2, 3>>]) == <<1, 2, 3>>
  let assert True =
    bit_array.concat([<<1:1>>, <<0:2>>, <<255>>, <<5:3>>]) == <<1:1, 0:2, 255, 5:3>>
  let assert "<<159, 61:size(6)>>" =
    bit_array.inspect(bit_array.concat([<<1:1>>, <<0:2>>, <<255>>, <<5:3>>]))
  let assert True = bit_array.pad_to_bytes(<<1, 2>>) == <<1, 2>>
  let assert True = bit_array.pad_to_bytes(<<1, 3:3>>) == <<1, 96>>
  let assert True = bit_array.pad_to_bytes(bit_array.concat([<<15:4>>, <<1:3>>])) == <<242>>
  Nil
}
"#);
}

#[test]
fn compare_and_starts_with() {
    let _ = run(r#"
import bit_array

pub fn main() {
  let assert -1 = bit_array.compare(<<1>>, <<2>>)
  let assert 1 = bit_array.compare(<<"AB":utf8>>, <<"AA":utf8>>)
  let assert 0 = bit_array.compare(<<1, 2:2>>, <<1, 2:2>>)
  let assert 1 = bit_array.compare(<<1, 2>>, <<1>>)
  let assert -1 = bit_array.compare(<<>>, <<0>>)
  let assert 1 = bit_array.compare(<<1, 2>>, <<1, 1:1>>)
  let assert -1 = bit_array.compare(<<1:2>>, <<1:3>>)
  let assert 1 = bit_array.compare(<<3:2>>, <<1:3>>)
  // The remaining bits as unsigned integers, then the size.
  let assert -1 = bit_array.compare(<<1:1>>, <<1:2>>)
  let assert -1 = bit_array.compare(<<1:1>>, <<2:2>>)
  let assert -1 = bit_array.compare(<<3:2>>, <<3:3>>)
  let assert 1 = bit_array.compare(<<1:3>>, <<1:2>>)
  let assert 1 = bit_array.compare(<<0:3>>, <<0:2>>)
  let assert 0 = bit_array.compare(<<0:2>>, <<0:2>>)
  let assert 1 = bit_array.compare(<<7:3>>, <<3:2>>)
  let assert 1 = bit_array.compare(<<7:3>>, <<0, 0>>)
  let assert -1 = bit_array.compare(<<7:3>>, <<0, 7>>)
  let assert -1 = bit_array.compare(<<7:3>>, <<0, 8>>)
  let assert 1 = bit_array.compare(<<1:3>>, <<0>>)
  let assert -1 = bit_array.compare(<<0:3>>, <<0>>)
  let assert 0 = bit_array.compare(<<5, 1:1>>, <<5, 1:1>>)
  let assert -1 = bit_array.compare(<<5, 1:1>>, <<5, 2:2>>)
  let assert -1 = bit_array.compare(<<4, 1:1>>, <<5, 1:1>>)
  let assert True = bit_array.starts_with(<<1, 2, 3, 4>>, <<1, 2>>)
  let assert True = bit_array.starts_with(<<1, 2>>, <<>>)
  let assert False = bit_array.starts_with(<<1, 2>>, <<1, 2, 3>>)
  let assert True = bit_array.starts_with(<<1, 2>>, <<0:7>>)
  let assert False = bit_array.starts_with(<<1, 2>>, <<1:7>>)
  let assert #(258, 16) = bit_array.to_int_and_size(<<1, 2>>)
  let assert #(5, 3) = bit_array.to_int_and_size(<<5:3>>)
  Nil
}
"#);
}

/// Bit arrays are values like any other: in arguments, results, records,
/// closures and generic functions.
#[test]
fn values() {
    let _ = run(r#"
import bit_array

pub type Box {
  Box(bits: BitArray)
}

fn identity(a: a) -> a {
  a
}

fn twice(bits: BitArray) -> BitArray {
  bit_array.concat([bits, bits])
}

pub fn main() {
  let f = fn(bits) { bit_array.bit_size(bits) }
  let assert 16 = f(twice(<<1>>))
  let assert Box(bits) = identity(Box(<<1, 2>>))
  let assert True = bits == <<1, 2>>
  let assert [3, 2] = [<<1, 2, 3>>, <<1, 2>>] |> map(bit_array.byte_size)
  Nil
}

fn map(list: List(a), f: fn(a) -> b) -> List(b) {
  case list {
    [] -> []
    [x, ..rest] -> [f(x), ..map(rest, f)]
  }
}
"#);
}

/// The builtins of `Bytes` are emitted in place of their calls, so the
/// module has no function for them.
#[test]
fn bytes_builtins_are_inlined() {
    let wasm = compile_wasm(
        r#"
import bit_array

pub fn main() {
  let bits = bit_array.concat([<<1:4>>, bit_array.from_string("ab")])
  bit_array.to_string(bits)
}
"#,
        vec![(TEST_PACKAGE, "bit_array", BIT_ARRAY)],
    );
    let mut names = vec![];
    for_each_operator(&wasm, |function, _| names.push(function.to_string()));
    names.dedup();
    let builtins = names
        .iter()
        .filter(|name| name.starts_with("_bytes") || name.starts_with("_string_offset"))
        .collect::<Vec<_>>();
    assert!(builtins.is_empty(), "{builtins:?}");
}
