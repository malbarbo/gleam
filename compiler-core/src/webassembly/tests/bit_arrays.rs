// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Bit arrays: the literals, their construction from values, and the
//! functions of the runtime module `builtins/bit_array`.

use super::{
    TEST_PACKAGE, compile_wasm, for_each_operator, run_wasm, run_wasm_js_with_config,
    run_wasm_with_config,
};
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

@external(webassembly, "builtins", "bit_array_base64_encode")
pub fn base64_encode(x: BitArray, padding: Bool) -> String

@external(webassembly, "builtins", "bit_array_base64_decode")
pub fn base64_decode(encoded: String) -> Result(BitArray, Nil)

@external(webassembly, "builtins", "bit_array_base64_url_encode")
pub fn base64_url_encode(x: BitArray, padding: Bool) -> String

@external(webassembly, "builtins", "bit_array_base64_url_decode")
pub fn base64_url_decode(encoded: String) -> Result(BitArray, Nil)

@external(webassembly, "builtins", "bit_array_base16_encode")
pub fn base16_encode(x: BitArray) -> String

@external(webassembly, "builtins", "bit_array_base16_decode")
pub fn base16_decode(encoded: String) -> Result(BitArray, Nil)

@external(webassembly, "builtins", "_inspect")
pub fn show(value: a) -> String

@external(webassembly, "builtins", "_hash")
pub fn hash(value: a) -> Int
"#;

/// Runs `src` with `Int` of 32 and of 64 bits, and with JavaScript strings,
/// and returns what it writes to stderr with 32 bits.
#[track_caller]
fn run(src: &str) -> String {
    run_with(src, vec![])
}

/// Like `run`, with the modules `modules` of the test package.
#[track_caller]
fn run_with(src: &str, modules: Vec<(&str, &str)>) -> String {
    let mut deps = vec![(TEST_PACKAGE, "bit_array", BIT_ARRAY)];
    deps.extend(
        modules
            .iter()
            .map(|(name, src)| (TEST_PACKAGE, *name, *src)),
    );
    let mut stderr = vec![];
    for int in [WasmInt::I32, WasmInt::I64] {
        let config = super::wasm_config(int, WasmFloat::F64);
        let result = run_wasm_with_config(src, deps.clone(), config);
        assert!(
            result.status.success(),
            "WASM execution failed ({int:?}):\n{}",
            result.stderr
        );
        stderr.push(result.stderr_without_colour());
    }
    let config = super::wasm_config(WasmInt::I64, WasmFloat::F64);
    let result = run_wasm_js_with_config(src, deps, config, &[]);
    assert!(
        result.status.success(),
        "WASM execution failed (JSString):\n{}",
        result.stderr
    );
    assert_eq!(stderr[0], stderr[1]);
    assert_eq!(stderr[0], result.stderr_without_colour());
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
  let assert "<<5:size(3)>>" = bit_array.inspect(<<5:3-little>>)
  let assert "<<232, 3:size(4)>>" = bit_array.inspect(<<1000:12-little>>)
  let assert "<<222, 188, 10:size(4)>>" = bit_array.inspect(<<0xABCDE:20-little>>)
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

/// A constant bit array whose segments are not literals is created by the
/// start function.
#[test]
fn constants_with_values() {
    let _ = run(r#"
import bit_array

const greeting = "hi, " <> "you"

const header = <<1, greeting:utf8>>

const nested = <<header:bits, 2:4>>

const later = <<before:bits, 4>>

const before = <<nested:bits-size(12), 3:4>>

const coded = <<greeting:utf16-little>>

const sized = <<greeting:utf8, 5:size(4)>>

const joined = <<"a" <> "b":utf8, header:bits-size(8)>>

const all = [header, nested, later]

fn get_nested() -> BitArray {
  nested
}

pub fn main() {
  let assert "<<1, 104, 105, 44, 32, 121, 111, 117>>" = bit_array.inspect(header)
  let assert "<<1, 104, 105, 44, 32, 121, 111, 117, 2:size(4)>>" = bit_array.inspect(nested)
  let assert "<<1, 99>>" = bit_array.inspect(before)
  let assert "<<1, 99, 4>>" = bit_array.inspect(later)
  let assert 14 = bit_array.byte_size(coded)
  let assert "<<104, 0, 105, 0>>" = bit_array.inspect(<<coded:bits-size(32)>>)
  let assert 60 = bit_array.bit_size(sized)
  let assert "<<97, 98, 1>>" = bit_array.inspect(joined)
  let assert [h, n, l] = all
  let assert True = h == header && n == get_nested() && l == later
  let assert True = <<nested:bits>> == <<header:bits, 2:4>>
  Nil
}
"#);
}

/// A constant bit array with values of another module is created where it
/// is used.
#[test]
fn constants_with_values_of_other_modules() {
    let _ = run_with(
        r#"
import bit_array
import other

pub fn main() {
  let assert "<<1, 104, 105, 2:size(4)>>" = bit_array.inspect(other.nested)
  let assert "<<1, 104, 105, 35>>" = bit_array.inspect(<<other.nested:bits, 3:4>>)
  Nil
}
"#,
        vec![(
            "other",
            r#"
const greeting = "h" <> "i"

const header = <<1, greeting:utf8>>

pub const nested = <<header:bits, 2:4>>
"#,
        )],
    );
}

/// The size of a pattern segment can be a constant of another module.
#[test]
fn pattern_size_of_other_modules() {
    let _ = run_with(
        r#"
import other.{width, width as w}

pub fn main() {
  let assert <<5, 6>> = case <<5, 6>> {
    <<x:size(width), y:size(w)>> -> <<x, y>>
    _ -> <<>>
  }
  Nil
}
"#,
        vec![("other", "pub const width = 8\n")],
    );
}

/// A literal integer segment too large to be computed when compiling is
/// written by the runtime.
#[test]
fn construction_large_literal_ints() {
    let _ = run(r#"
import bit_array

pub fn main() {
  let b = <<0:size(100_000), 1:size(70_000), 2>>
  let assert 170_008 = bit_array.bit_size(b)
  let assert <<0:size(100_000), 1:size(70_000), 2>> = b
  Nil
}
"#);
}

/// A literal segment of a pattern is matched with the value of its literal,
/// which does not need to fit in an Int.
#[test]
fn pattern_large_literal_ints() {
    let _ = run(r#"
pub fn main() {
  let assert <<0xFFF0000000000000:64>> = <<0xFFF0000000000000:64>>
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
  // Bits that do not start at the start of their bytes.
  let assert <<_:3, u:bits>> = <<0:3, 1, 2, 3:2>>
  let assert <<_:5, v:bits>> = <<0:5, 1, 3>>
  let assert 0 = bit_array.compare(u, <<1, 2, 3:2>>)
  let assert -1 = bit_array.compare(u, v)
  let assert 1 = bit_array.compare(v, <<1, 2, 3:2>>)
  let assert 1 = bit_array.compare(u, <<1, 2>>)
  let assert True = bit_array.hash(u) == bit_array.hash(<<1, 2, 3:2>>)
  let assert True = bit_array.starts_with(<<1, 2, 3, 4>>, <<1, 2>>)
  let assert True = bit_array.starts_with(<<1, 2>>, <<>>)
  let assert False = bit_array.starts_with(<<1, 2>>, <<1, 2, 3>>)
  let assert True = bit_array.starts_with(<<1, 2>>, <<0:7>>)
  let assert False = bit_array.starts_with(<<1, 2>>, <<1:7>>)
  Nil
}
"#);
}

/// The results of Erlang and JavaScript.
#[test]
fn base64_and_base16() {
    let _ = run(r#"
import bit_array

pub fn main() {
  let assert "" = bit_array.base64_encode(<<>>, True)
  let assert "QQ==" = bit_array.base64_encode(<<"A">>, True)
  let assert "QQ" = bit_array.base64_encode(<<"A">>, False)
  let assert "QUI=" = bit_array.base64_encode(<<"AB">>, True)
  let assert "QUI" = bit_array.base64_encode(<<"AB">>, False)
  let assert "QUJD" = bit_array.base64_encode(<<"ABC">>, False)
  let assert "QUJDRA==" = bit_array.base64_encode(<<"ABCD">>, True)
  let assert "//79/Ps=" = bit_array.base64_encode(<<255, 254, 253, 252, 251>>, True)
  let assert "__79_Ps" = bit_array.base64_url_encode(<<255, 254, 253, 252, 251>>, False)
  let assert "-_8=" = bit_array.base64_url_encode(<<251, 255>>, True)
  // The last byte is padded with zeros.
  let assert "gA==" = bit_array.base64_encode(<<1:1>>, True)
  let assert "v+g=" = bit_array.base64_encode(<<5:3, 255, 1:2>>, True)
  let assert <<_:3, rest:bits>> = <<1:3, "hello", 0:5>>
  let assert "aGVsbG8A" = bit_array.base64_encode(rest, True)
  let assert "68656C6C6F00" = bit_array.base16_encode(rest)
  let assert "" = bit_array.base16_encode(<<>>)
  let assert "FBFF" = bit_array.base16_encode(<<251, 255>>)
  let assert "FFE0" = bit_array.base16_encode(<<255, 7:3>>)

  let assert Ok(<<>>) = bit_array.base64_decode("")
  let assert Ok(<<"A">>) = bit_array.base64_decode("QQ==")
  let assert Ok(<<"A">>) = bit_array.base64_decode("QR==")
  let assert Ok(<<"AB">>) = bit_array.base64_decode("QUJ=")
  let assert Ok(<<"ABC">>) = bit_array.base64_decode("QUJD")
  let assert Ok(<<"AB">>) = bit_array.base64_decode("QUJ")
  let assert Ok(<<"hello world">>) = bit_array.base64_decode("aGVsbG8gd29ybGQ")
  let assert Ok(<<251>>) = bit_array.base64_decode("+/==")
  let assert Error(Nil) = bit_array.base64_decode("-_==")
  let assert Ok(<<251>>) = bit_array.base64_url_decode("-_==")
  let assert Ok(<<251>>) = bit_array.base64_url_decode("+/==")
  let assert Ok(<<255, 254, 253, 252, 251>>) =
    bit_array.base64_url_decode("__79/Ps")
  // Spaces, tabs and line breaks are skipped, but the missing `=` are
  // counted with them.
  let assert Ok(<<"ABC">>) = bit_array.base64_decode("QU JD\n\n\n")
  let assert Ok(<<"A">>) = bit_array.base64_decode("QQ=  =\r\n")
  let assert Ok(<<"ABCA">>) = bit_array.base64_decode("QUJD\nQQ==\n\n\n")
  let assert Error(Nil) = bit_array.base64_decode("QQ== ")
  let assert Error(Nil) = bit_array.base64_decode("Q Q")
  let assert Error(Nil) = bit_array.base64_decode("QQ =")
  let assert Error(Nil) = bit_array.base64_decode("QQ\u{C}==")
  let assert Error(Nil) = bit_array.base64_decode("Q")
  let assert Error(Nil) = bit_array.base64_decode("Q===")
  let assert Error(Nil) = bit_array.base64_decode("====")
  let assert Error(Nil) = bit_array.base64_decode("=QQ=")
  let assert Error(Nil) = bit_array.base64_decode("QQ=A")
  let assert Error(Nil) = bit_array.base64_decode("QQ==QQ==")
  let assert Error(Nil) = bit_array.base64_decode("ab$d")
  let assert Error(Nil) = bit_array.base64_decode("é===")

  let assert Ok(<<>>) = bit_array.base16_decode("")
  let assert Ok(<<10>>) = bit_array.base16_decode("0a")
  let assert Ok(<<0, 255, 16>>) = bit_array.base16_decode("00ff10")
  let assert Ok(<<171, 205, 239>>) = bit_array.base16_decode("ABCDEF")
  let assert Error(Nil) = bit_array.base16_decode("a")
  let assert Error(Nil) = bit_array.base16_decode("0g")
  let assert Error(Nil) = bit_array.base16_decode(" 0")
  let assert Error(Nil) = bit_array.base16_decode("é0")
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

/// Integer segments of values: sizes that are not multiples of 8, at any
/// position, little endian, negative values and sizes larger than `Int`. The
/// results are those of the Erlang target.
#[test]
fn construction_ints() {
    let _ = run(r#"
import bit_array

fn id(x: a) -> a {
  x
}

pub fn main() {
  let assert "<<1>>" = bit_array.inspect(<<id(1)>>)
  let assert "<<1, 2>>" = bit_array.inspect(<<id(258):16>>)
  let assert "<<2, 1>>" = bit_array.inspect(<<id(258):16-little>>)
  let assert "<<255, 15:size(4)>>" = bit_array.inspect(<<id(-1):12>>)
  let assert "<<232, 3:size(4)>>" = bit_array.inspect(<<id(1000):12-little>>)
  let assert "<<222, 188, 10:size(4)>>" = bit_array.inspect(<<id(0xABCDE):20-little>>)
  let assert "<<255, 255, 255, 255, 255, 255, 255, 255, 63:size(6)>>" = bit_array.inspect(<<id(-1):70>>)
  let assert "<<254, 255, 255, 255, 255, 255, 255, 255, 63:size(6)>>" = bit_array.inspect(<<id(-2):70-little>>)
  let assert "<<161, 44>>" = bit_array.inspect(<<id(5):3, id(300):13>>)
  let assert "<<154, 9, 0:size(1)>>" = bit_array.inspect(<<id(1):1, id(0x1234):16-little>>)
  let assert "<<251>>" = bit_array.inspect(<<id(-5):8>>)
  let assert "<<18, 52, 86, 120>>" = bit_array.inspect(<<id(0x12345678):32-big>>)
  let assert "<<0, 1:size(2)>>" = bit_array.inspect(<<id(1):size(id(10))>>)
  let assert "<<0, 0, 1>>" = bit_array.inspect(<<id(1):size(id(3))-unit(8)>>)
  let assert "<<1>>" = bit_array.inspect(<<id(7):size(0), 1>>)
  Nil
}
"#);
}

/// Float segments of values, with 16 bits floats rounded half to even. The
/// results are those of the Erlang target.
#[test]
fn construction_floats() {
    let _ = run(r#"
import bit_array

fn id(x: a) -> a {
  x
}

pub fn main() {
  let assert "<<63, 248, 0, 0, 0, 0, 0, 0>>" = bit_array.inspect(<<id(1.5):float>>)
  let assert "<<63, 192, 0, 0>>" = bit_array.inspect(<<id(1.5):32-float>>)
  let assert "<<0, 0, 0, 0, 0, 0, 6, 192>>" = bit_array.inspect(<<id(-2.75):64-float-little>>)
  let assert "<<159, 252, 0, 0, 0, 0, 0, 0, 0:size(1)>>" = bit_array.inspect(<<id(1):1, id(1.5):float>>)
  let assert "<<63, 192, 0, 0>>" = bit_array.inspect(<<id(1.5):float-size(id(32))>>)
  let assert "<<62, 0>>" = bit_array.inspect(<<id(1.5):16-float>>)
  let assert "<<123, 255>>" = bit_array.inspect(<<id(65504.0):16-float>>)
  let assert "<<123, 255>>" = bit_array.inspect(<<id(65519.0):16-float>>)
  let assert "<<124, 0>>" = bit_array.inspect(<<id(65520.0):16-float>>)
  let assert "<<0, 1>>" = bit_array.inspect(<<id(5.960464477539063e-8):16-float>>)
  let assert "<<0, 0>>" = bit_array.inspect(<<id(2.9802322387695312e-8):16-float>>)
  let assert "<<0, 0>>" = bit_array.inspect(<<id(2.9802322387695315e-8):16-float>>)
  let assert "<<3, 255>>" = bit_array.inspect(<<id(6.097555160522461e-5):16-float>>)
  let assert "<<128, 0>>" = bit_array.inspect(<<id(-0.0):16-float>>)
  let assert "<<60, 1>>" = bit_array.inspect(<<id(1.0009765625):16-float>>)
  let assert "<<60, 0>>" = bit_array.inspect(<<id(1.00048828125):16-float>>)
  let assert "<<60, 2>>" = bit_array.inspect(<<id(1.00146484375):16-float>>)
  let assert "<<72, 66>>" = bit_array.inspect(<<id(3.14159):16-float-little>>)
  let assert "<<0, 0>>" = bit_array.inspect(<<id(1.0e-30):16-float>>)
  let assert "<<124, 0>>" = bit_array.inspect(<<id(1.0e30):16-float>>)
  Nil
}
"#);
}

/// String and code point segments of values, in UTF-8, UTF-16 and UTF-32. The
/// results are those of the Erlang target.
#[test]
fn construction_unicode() {
    let _ = run(r#"
import bit_array

fn id(x: a) -> a {
  x
}

@external(webassembly, "builtins", "_unsafe_int_to_utf_codepoint")
fn cp(value: Int) -> UtfCodepoint

pub fn main() {
  let assert "<<104, 195, 169, 108, 108, 111>>" = bit_array.inspect(<<id("héllo"):utf8>>)
  let assert "<<0, 97, 216, 61, 222, 0>>" = bit_array.inspect(<<id("a😀"):utf16>>)
  let assert "<<97, 0, 61, 216, 0, 222>>" = bit_array.inspect(<<id("a😀"):utf16-little>>)
  let assert "<<0, 0, 0, 97, 0, 0, 0, 233, 0, 1, 246, 0>>" = bit_array.inspect(<<id("aé😀"):utf32>>)
  let assert "<<97, 0, 0, 0, 233, 0, 0, 0, 0, 246, 1, 0>>" = bit_array.inspect(<<id("aé😀"):utf32-little>>)
  let assert "<<44, 44, 2:size(3)>>" = bit_array.inspect(<<id(1):3, id("ab"):utf8>>)
  let assert "<<32, 29, 1:size(3)>>" = bit_array.inspect(<<id(1):3, id("é"):utf16>>)
  let assert "<<97, 195, 169, 226, 130, 172, 240, 159, 152, 128>>" = bit_array.inspect(<<cp(0x61):utf8_codepoint, cp(0xE9):utf8_codepoint, cp(0x20AC):utf8_codepoint, cp(0x1F600):utf8_codepoint>>)
  let assert "<<0, 233, 61, 216, 0, 222>>" = bit_array.inspect(<<cp(0xE9):utf16_codepoint, cp(0x1F600):utf16_codepoint-little>>)
  let assert "<<0, 0, 0, 233, 0, 246, 1, 0>>" = bit_array.inspect(<<cp(0xE9):utf32_codepoint, cp(0x1F600):utf32_codepoint-little>>)
  let assert "<<120, 160, 171, 0:size(2)>>" = bit_array.inspect(<<id(1):2, cp(0x20AC):utf8_codepoint>>)
  let assert "<<120, 121, 5:size(3)>>" = bit_array.inspect(<<"x", id("y"):utf8, 5:3>>)
  Nil
}
"#);
}

/// Bit array segments of values, and literal segments among them. The
/// results are those of the Erlang target.
#[test]
fn construction_bits() {
    let _ = run(r#"
import bit_array

fn id(x: a) -> a {
  x
}

pub fn main() {
  let assert "<<1, 2, 3>>" = bit_array.inspect(<<id(<<1, 2, 3>>):bits>>)
  let assert "<<1, 2, 3, 4>>" = bit_array.inspect(<<id(<<1, 2, 3>>):bits, 4>>)
  let assert "<<1, 0:size(4)>>" = bit_array.inspect(<<id(<<1, 2, 3>>):bits-size(12)>>)
  let assert "<<1, 2>>" = bit_array.inspect(<<id(<<1, 2, 3>>):bits-size(16)>>)
  let assert "<<1, 2, 3:size(4)>>" = bit_array.inspect(<<id(<<1, 2>>):bits, id(<<3:4>>):bits>>)
  let assert "<<255, 128, 85, 1:size(2)>>" = bit_array.inspect(<<id(1):1, id(<<0xFF, 0x00, 0xAA>>):bits, id(1):1>>)
  let assert "<<248, 0:size(4)>>" = bit_array.inspect(<<id(<<1:1>>):bits, id(<<0xF0, 0x0F>>):bits-size(id(11))>>)
  let assert "<<1, 2, 3, 97, 98, 4:size(4)>>" = bit_array.inspect(<<1, 2, id(3), "ab", 4:4>>)
  // Bits that do not start at the start of their bytes.
  let assert <<_:3, u:bits>> = <<0:3, 1, 2, 3:2>>
  let assert "<<5, 1, 2, 3:size(2)>>" = bit_array.inspect(<<id(5), u:bits>>)
  let assert "<<1, 2, 192, 64, 11:size(4)>>" = bit_array.inspect(<<u:bits, u:bits>>)
  Nil
}
"#);
}

/// A bit array extended by the segments after it, in the same bytes when it
/// ends where they are written, changes no other bit array.
#[test]
fn construction_extend() {
    let _ = run(r#"
import bit_array

fn id(x: a) -> a {
  x
}

fn build(acc: BitArray, i: Int, n: Int, size: Int) -> BitArray {
  case i == n {
    True -> acc
    False -> build(<<acc:bits, i:size(size)>>, i + 1, n, size)
  }
}

fn sum(bits: BitArray, acc: Int) -> Int {
  case bits {
    <<byte, rest:bits>> -> sum(rest, acc + byte)
    _ -> acc
  }
}

pub fn main() {
  let bytes = build(<<>>, 0, 1000, 8)
  let assert 1000 = bit_array.byte_size(bytes)
  let assert 124_716 = sum(bytes, 0)
  let assert True = build(<<>>, 0, 8, 3) == <<0:3, 1:3, 2:3, 3:3, 4:3, 5:3, 6:3, 7:3>>

  // Two bit arrays of the same one.
  let base = <<id(<<1>>):bits, 2>>
  let x = <<base:bits, 3>>
  let y = <<base:bits, 4>>
  let assert True = base == <<1, 2>>
  let assert True = x == <<1, 2, 3>>
  let assert True = y == <<1, 2, 4>>
  let assert True = <<x:bits, 5>> == <<1, 2, 3, 5>>
  let assert True = <<y:bits, 6>> == <<1, 2, 4, 6>>
  let assert True = <<x:bits, 7>> == <<1, 2, 3, 7>>

  // Bits that are not whole bytes.
  let base = <<id(<<1:3>>):bits, 1:2>>
  let x = <<base:bits, 1:1>>
  let y = <<base:bits, 0:1>>
  let assert True = base == <<1:3, 1:2>>
  let assert True = x == <<1:3, 1:2, 1:1>>
  let assert True = y == <<1:3, 1:2, 0:1>>

  // Slices, the rest of a pattern and strings.
  let assert Ok(head) = bit_array.slice(x, 0, 0)
  let assert True = <<head:bits, 9>> == <<9>>
  let z = <<id(<<1>>):bits, 2, 3>>
  let assert Ok(middle) = bit_array.slice(z, 1, 1)
  let assert True = <<middle:bits, 9>> == <<2, 9>>
  let assert True = z == <<1, 2, 3>>
  let assert <<_, rest:bits>> = z
  let assert True = <<rest:bits, 4>> == <<2, 3, 4>>
  let assert True = <<rest:bits, 5>> == <<2, 3, 5>>
  let assert True = z == <<1, 2, 3>>
  let text = id("ab")
  let ab = bit_array.from_string(text)
  let assert True = <<ab:bits, 99>> == <<"abc">>
  let assert "ab" = text
  let t = <<id(<<97>>):bits, 98>>
  let assert Ok("ab") = bit_array.to_string(t)
  let assert Ok(string) = bit_array.to_string(t)
  let assert True = <<t:bits, 99>> == <<"abc">>
  let assert "ab" = string
  let assert True = <<<<1, 2>>:bits, id(3)>> == <<1, 2, 3>>
  Nil
}
"#);
}

/// Integer segments of values larger than 32 bits.
#[test]
fn construction_large_ints() {
    let result = run_wasm_with_config(
        r#"
import bit_array

fn id(x: a) -> a {
  x
}

pub fn main() {
  let assert "<<1, 35, 69, 103, 137, 171, 205, 239>>" = bit_array.inspect(<<id(0x123456789ABCDEF):64>>)
  let assert "<<128, 145, 162, 179, 196, 1:size(1)>>" = bit_array.inspect(<<id(1):1, id(0x123456789):40>>)
  Nil
}
"#,
        vec![(TEST_PACKAGE, "bit_array", BIT_ARRAY)],
        super::wasm_config(WasmInt::I64, WasmFloat::F64),
    );
    assert!(result.status.success(), "{}", result.stderr);
}

/// Float segments of values when `Float` has 32 bits.
#[test]
fn construction_floats_of_32_bits() {
    let result = run_wasm_with_config(
        r#"
import bit_array

fn id(x: a) -> a {
  x
}

pub fn main() {
  let assert "<<63, 248, 0, 0, 0, 0, 0, 0>>" = bit_array.inspect(<<id(1.5):float>>)
  let assert "<<0, 0, 48, 192>>" = bit_array.inspect(<<id(-2.75):32-float-little>>)
  let assert "<<123, 255>>" = bit_array.inspect(<<id(65504.0):16-float>>)
  // A literal is the same float as a value.
  let assert True = <<id(1.1):float>> == <<1.1:float>>
  let assert True = <<id(1.1):float-little>> == <<1.1:float-little>>
  let assert True = <<id(0.1):32-float>> == <<0.1:32-float>>
  let assert <<1.1:float>> = <<id(1.1):float>>
  let assert <<1.1:float>> = <<1.1:float>>
  // A float of 64 bits larger than any `Float` does not match.
  let assert True = case <<0x7E37E43C:32, 0x0800759C:32>> {
    <<_:float>> -> False
    _ -> True
  }
  Nil
}
"#,
        vec![(TEST_PACKAGE, "bit_array", BIT_ARRAY)],
        super::wasm_config(WasmInt::I32, WasmFloat::F32),
    );
    assert!(result.status.success(), "{}", result.stderr);
}

/// The values and the sizes of the segments are evaluated in order, before
/// the bit array is created, and an integer segment of a negative size is
/// empty.
#[test]
fn construction_evaluation() {
    let output = run(r#"
import bit_array

fn trace(x: a, name: String) -> a {
  echo name
  x
}

pub fn main() {
  let bits = <<trace(1, "a"):size(trace(4, "b")), trace(<<2>>, "c"):bits, trace(3, "d")>>
  let assert "<<16, 32, 3:size(4)>>" = bit_array.inspect(bits)
  let assert "<<1>>" = bit_array.inspect(<<trace(9, "e"):size(trace(-3, "f")), 1>>)
  Nil
}
"#);
    let names: Vec<_> = output
        .lines()
        .filter_map(|line| line.strip_prefix('"')?.strip_suffix('"'))
        .collect();
    assert_eq!(names, ["a", "b", "c", "d", "e", "f"]);
}

/// A segment that cannot be created fails when it is created.
#[test]
fn construction_errors() {
    for (segment, message) in [
        (
            "id(<<1>>):bits-size(id(9))",
            "Bit array segment size is larger than its value",
        ),
        (
            "<<1, 2>>:bits-size(24)",
            "Bit array segment size is larger than its value",
        ),
        (
            "id(1.5):float-size(id(8))",
            "Sized floats must be 16-bit, 32-bit or 64-bit",
        ),
        (
            "id(1.5):float-size(n24)",
            "Sized floats must be 16-bit, 32-bit or 64-bit",
        ),
        (
            "id(1.5):float-size(n0)",
            "Sized floats must be 16-bit, 32-bit or 64-bit",
        ),
        (
            "1.5:float-size(n24)",
            "Sized floats must be 16-bit, 32-bit or 64-bit",
        ),
    ] {
        let src = format!(
            r#"
const n24 = 24

const n0 = 0

fn id(x: a) -> a {{
  x
}}

pub fn main() {{
  <<{segment}>>
}}
"#
        );
        let result = run_wasm(&src, vec![]);
        assert!(!result.status.success(), "{segment} did not fail");
        assert!(
            result.stderr.contains(message),
            "{segment} failed with:\n{}",
            result.stderr
        );
    }
}

/// The externals of the runtime are checked with its types, so a module
/// that declares them, like `gleam/bit_array` in the tests of the standard
/// library, compiles on its own.
#[test]
fn runtime_externals_compile_on_their_own() {
    let _ = compile_wasm(BIT_ARRAY, vec![]);
}

/// The builtins of `Bytes` and of the bits of floats are emitted in place
/// of their calls, so the module has no function for them, except the loop
/// of `_bytes_copy_shifted`.
#[test]
fn bytes_builtins_are_inlined() {
    let wasm = compile_wasm(
        r#"
import bit_array

pub fn main() {
  let bits = bit_array.concat([<<1:4>>, bit_array.from_string("ab")])
  let _ = floats(1.5)
  let _ = read_floats(bits)
  bit_array.to_string(bits)
}

fn floats(x: Float) -> BitArray {
  <<x:float, x:32-float>>
}

fn read_floats(bits: BitArray) -> Float {
  let assert <<x:float, y:32-float>> = bits
  x +. y
}
"#,
        vec![(TEST_PACKAGE, "bit_array", BIT_ARRAY)],
    );
    let mut names = vec![];
    for_each_operator(&wasm, |function, _| names.push(function.to_string()));
    names.dedup();
    let builtins = names
        .iter()
        .filter(|name| {
            *name != "_bytes_copy_shifted"
                && [
                    "_bytes",
                    "_float64_byte",
                    "_float32_byte",
                    "_float64_from_bits",
                    "_float32_from_bits",
                ]
                .iter()
                .any(|builtin| name.starts_with(builtin))
        })
        .collect::<Vec<_>>();
    assert!(builtins.is_empty(), "{builtins:?}");
}

/// Integer segments of patterns. The results are those of the Erlang target.
#[test]
fn pattern_ints() {
    let _ = run(r##"
import bit_array.{show}

fn ints(b: BitArray) -> String {
  case b {
    <<>> -> "empty"
    <<a>> -> "one " <> show(a)
    <<a, b>> -> "two " <> show(#(a, b))
    <<a:3, b:13-signed, c:5-little, d:11-little-signed, rest:bytes>> ->
      "mixed " <> show(#(a, b, c, d, rest))
    <<a:16, 0:1, rest:bits>> -> "16+0 " <> show(#(a, rest))
    <<a:16-little, 1:1, rest:bits>> -> "16le+1 " <> show(#(a, rest))
    _ -> "other"
  }
}

fn signed(b: BitArray) -> String {
  case b {
    <<a:8-signed, b:16-signed-little, c:7-signed, d:9-signed, e:1-signed, f:24-little-signed>> ->
      show(#(a, b, c, d, e, f))
    _ -> "other"
  }
}

pub fn main() {
  let assert "empty" = ints(<<>>)
  let assert "one 1" = ints(<<1>>)
  let assert "two #(1, 2)" = ints(<<1, 2>>)
  let assert "16+0 #(258, <<3:size(7)>>)" = ints(<<1, 2, 3>>)
  let assert "16+0 #(258, <<5:size(3)>>)" = ints(<<1, 2, 0:1, 5:3>>)
  let assert "16le+1 #(1027, <<>>)" = ints(<<3, 4, 1:1>>)
  let assert "mixed #(5, 1370, 7, 920, <<15, 240>>)" = ints(<<0xA5, 0x5A, 0x3C, 0xC3, 0x0F, 0xF0>>)
  let assert "16le+1 #(59093, <<3:size(7)>>)" = ints(<<1:1, 0xABCD:16, 3:7>>)
  let assert "mixed #(5, -1, 31, -1, <<1>>)" = ints(<<0xBF, 0xFF, 0xFF, 0xFF, 1>>)
  let assert "16le+1 #(65471, <<255, 255>>)" = ints(<<0xBF, 0xFF, 0xFF, 0xFF, 1:1>>)
  let assert "#(-3, -300, -5, -200, -1, -98765)" = signed(<<-3:8, -300:16-little, -5:7, -200:9, -1:1, -98765:24-little>>)
  let assert "#(127, 32767, 63, 255, 0, 8388607)" = signed(<<127, 32767:16-little, 63:7, 255:9, 0:1, 8388607:24-little>>)
  Nil
}
"##);
}

/// Consecutive literal segments of patterns, compared with the bits of their
/// literal, and literals that are not their bits.
#[test]
fn pattern_literals() {
    let _ = run(r##"
import bit_array.{show}

fn literals(b: BitArray) -> String {
  case b {
    <<"GET ", rest:bytes>> -> "get " <> show(rest)
    <<0x89, "PNG", rest:bits>> -> "png " <> show(rest)
    <<1, 2, 3:4, 4:4-little, -1:8-signed, rest:bits>> -> "lits " <> show(rest)
    <<"héllo":utf16, rest:bits>> -> "utf16 " <> show(rest)
    <<"h€":utf32-little, rest:bits>> -> "utf32le " <> show(rest)
    <<"ab":utf8, 1:1, x:7>> -> "ab+bit " <> show(x)
    <<0x1234:16-little, x>> -> "le " <> show(x)
    <<1000:12, x:4>> -> "12 " <> show(x)
    <<"abc" as s, rest:bits>> -> "assign " <> s <> " " <> show(rest)
    <<7 as n, rest:bits>> -> "assign int " <> show(#(n, rest))
    <<-1, _:bits>> -> "never"
    <<256, _:bits>> -> "never"
    <<128:8-signed>> -> "never"
    <<129:8-signed>> -> "never"
    <<-129:8-signed>> -> "never"
    _ -> "other"
  }
}

pub fn main() {
  let assert "get <<47, 105>>" = literals(<<"GET /i">>)
  let assert "other" = literals(<<"GET">>)
  let assert "png <<1:size(3)>>" = literals(<<0x89, "PNG", 1:3>>)
  let assert "other" = literals(<<0x89, "PNx">>)
  let assert "lits <<9>>" = literals(<<1, 2, 3:4, 4:4-little, 255, 9>>)
  let assert "other" = literals(<<1, 2, 3:4, 4:4-little, 254>>)
  let assert "utf16 <<1>>" = literals(<<"héllo":utf16, 1>>)
  let assert "utf32le <<2:size(2)>>" = literals(<<"h€":utf32-little, 2:2>>)
  let assert "ab+bit 5" = literals(<<"ab", 1:1, 5:7>>)
  let assert "other" = literals(<<"ab", 0:1, 5:7>>)
  let assert "le 9" = literals(<<0x34, 0x12, 9>>)
  let assert "12 3" = literals(<<1000:12, 3:4>>)
  let assert "assign abc <<100>>" = literals(<<"abcd">>)
  let assert "assign int #(7, <<8, 9>>)" = literals(<<7, 8, 9>>)
  let assert "other" = literals(<<255>>)
  let assert "other" = literals(<<0, 1>>)
  let assert "other" = literals(<<127>>)
  let assert "other" = literals(<<128>>)
  let assert "other" = literals(<<129>>)
  Nil
}
"##);
}

/// Float segments of patterns, which are finite, and literal floats, which
/// are equal to the read floats when their bits are. The results are those
/// of the Erlang target.
#[test]
fn pattern_floats() {
    let _ = run(r##"
import bit_array.{show}

fn floats(b: BitArray) -> String {
  case b {
    <<1.5:float>> -> "lit 1.5"
    <<0.0:float>> -> "lit 0.0"
    <<x:float>> -> "f64 " <> show(x)
    <<x:16-float, _:bits>> -> "f16 first " <> show(x)
    <<_:float, _:bits>> -> "some float"
    _ -> "other"
  }
}

fn floats32(b: BitArray) -> String {
  case b {
    <<0.5:32-float, x:32-float>> -> "half " <> show(x)
    <<0.1:32-float, _:bits>> -> "never"
    <<x:32-float-little, y:32-float>> -> show(#(x, y))
    _ -> "other"
  }
}

fn floats16(b: BitArray) -> String {
  case b {
    <<x:16-float, y:16-float-little>> -> show(#(x, y))
    _ -> "other"
  }
}

fn sized(b: BitArray, n: Int) -> String {
  case b {
    <<x:float-size(n), rest:bits>> -> show(#(x, rest))
    _ -> "other"
  }
}

pub fn main() {
  let assert "lit 1.5" = floats(<<1.5:float>>)
  let assert "lit 0.0" = floats(<<0.0:float>>)
  let assert "f64 -0.0" = floats(<<-0.0:float>>)
  let assert "f64 2.5" = floats(<<2.5:float>>)
  let assert "f16 first 1.0" = floats(<<0x3C00:16, 1:3>>)
  let assert "other" = floats(<<0x7FF0:16, 0:48>>)
  let assert "f16 first -2.0" = floats(<<0xC000:16, 1:3>>)
  let assert "some float" = floats(<<1.0e300:float, 0x7FF0:16>>)
  let assert "other" = floats(<<0x7FF8:16, 0:48, 1>>)
  let assert "half 1.25" = floats32(<<0.5:32-float, 1.25:32-float>>)
  let assert "#(-429492128.0, 1.25)" = floats32(<<0.1:32-float, 1.25:32-float>>)
  let assert "#(2.0, -3.0)" = floats32(<<2.0:32-float-little, -3.0:32-float>>)
  let assert "other" = floats32(<<0x0000807F:32, 1.0:32-float>>)
  let assert "#(1.0, -2.0)" = floats16(<<0x3C00:16, 0x00C0:16>>)
  let assert "#(5.960464477539063e-8, 0.00006103515625)" = floats16(<<0x0001:16, 0x0004:16>>)
  let assert "#(-65504.0, 0.000022649765014648438)" = floats16(<<0xFBFF:16, 0x7C:8, 1>>)
  let assert "other" = floats16(<<0x7C00:16, 0, 0>>)
  let assert "#(2.015625, <<0, 0, 0, 0, 0, 0, 1:size(1)>>)" = sized(<<3.0:float, 1:1>>, 16)
  let assert "#(2.125, <<0, 0, 0, 0, 1:size(1)>>)" = sized(<<3.0:float, 1:1>>, 32)
  let assert "#(3.0, <<1:size(1)>>)" = sized(<<3.0:float, 1:1>>, 64)
  let assert "other" = sized(<<3.0:float, 1:1>>, 8)
  let assert "#(0.0, <<64, 8, 0, 0, 0, 0, 0, 0, 1:size(1)>>)" = sized(<<3.0:float, 1:1>>, 0)
  let assert "other" = sized(<<3.0:float, 1:1>>, -1)
  Nil
}
"##);
}

/// Bit array segments of patterns, and sizes of variables, of the previous
/// segments and of operators, whose division by zero is zero, and sizes
/// larger than any bit array. The results are those of the Erlang target,
/// which has no division in sizes.
#[test]
fn pattern_bits_and_sizes() {
    let _ = run(r##"
import bit_array.{show}

const four = 4

fn bits(b: BitArray) -> String {
  case b {
    <<x:bits-size(four), y:bytes-size(1), rest:bytes>> -> "4+8 " <> show(#(x, y, rest))
    <<len, payload:bytes-size(len), rest:bits>> -> "len " <> show(#(len, payload, rest))
    _ -> "other"
  }
}

fn sizes(b: BitArray, n: Int, z: Int) -> String {
  case b {
    <<a:size(n), b:size(a * 2 - 1), c:size(n * z), d:size({ a + n } - 4)-unit(2), rest:bits>> ->
      show(#(a, b, c, d, rest))
    _ -> "other"
  }
}

fn division(b: BitArray, n: Int) -> String {
  case b {
    <<a:size(8 / n), b:size(9 % n), rest:bits>> -> show(#(a, b, rest))
    _ -> "other"
  }
}

fn negative(b: BitArray, n: Int) -> String {
  case b {
    <<a:size(n), rest:bits>> -> show(#(a, rest))
    _ -> "other"
  }
}

fn huge(b: BitArray, n: Int) -> String {
  case b {
    <<_:4, a:size(n), _:bits>> -> show(a)
    <<_:size(4_611_686_018_427_387_904)-unit(8), _:bits>> -> "never"
    <<_:size({ -1 }), _:bits>> -> "never"
    _ -> "other"
  }
}

pub fn main() {
  let assert "4+8 #(<<10:size(4)>>, <<188>>, <<222, 242>>)" = bits(<<0xAB, 0xCD, 0xEF, 0x2:4>>)
  let assert "other" = bits(<<0xAB, 0xCD, 0xEF, 0x12>>)
  let assert "len #(2, <<7, 8>>, <<9>>)" = bits(<<2, 7, 8, 9>>)
  let assert "other" = bits(<<3, 1, 2>>)
  let assert "len #(0, <<>>, <<>>)" = bits(<<0>>)
  let assert "len #(0, <<>>, <<1:size(1)>>)" = bits(<<0, 1:1>>)
  let input = <<0xDE, 0xAD, 0xBE, 0xEF, 0x55>>
  let assert "other" = sizes(input, 1, 0)
  let assert "#(3, 15, 0, 1, <<91, 125, 222, 85:size(7)>>)" = sizes(input, 2, 0)
  let assert "#(3, 15, 1, 1, <<109, 247, 122, 21:size(5)>>)" = sizes(input, 2, 1)
  let assert "#(6, 1963, 0, 446, <<239, 85>>)" = sizes(input, 3, 0)
  let assert "#(6, 1963, 3, 503, <<122, 21:size(5)>>)" = sizes(input, 3, 1)
  let assert "other" = sizes(input, 4, 1)
  let assert "#(0, 0, <<222>>)" = division(<<0xDE>>, 0)
  let assert "#(13, 1, <<6:size(3)>>)" = division(<<0xDE>>, 2)
  let assert "#(222, 0, <<>>)" = division(<<0xDE>>, 1)
  let assert "other" = negative(<<0xF0>>, -1)
  let assert "#(0, <<240>>)" = negative(<<0xF0>>, 0)
  let assert "#(7, <<16:size(5)>>)" = negative(<<0xF0>>, 3)
  let assert "#(240, <<>>)" = negative(<<0xF0>>, 8)
  let assert "other" = negative(<<0xF0>>, 9)
  let assert "other" = huge(<<0xF0>>, 0x7FFFFFFF)
  let assert "other" = huge(<<0xF0>>, 0x7FFFFFFE)
  let assert "15" = huge(<<0xFF>>, 4)
  Nil
}
"##);
}

/// Code point segments of patterns, which are valid code points. The
/// results are those of the Erlang target.
#[test]
fn pattern_codepoints() {
    let _ = run(r##"
import bit_array.{show}

@external(webassembly, "builtins", "_utf_codepoint_to_int")
fn int(codepoint: UtfCodepoint) -> Int

fn codepoints(b: BitArray) -> String {
  case b {
    <<c:utf8_codepoint, d:utf16_codepoint, e:utf32_codepoint-little>> ->
      "all " <> show(#(int(c), int(d), int(e)))
    <<c:utf8_codepoint, rest:bits>> -> "utf8 " <> show(#(int(c), rest))
    _ -> "other"
  }
}

fn utf16(b: BitArray) -> String {
  case b {
    <<c:utf16_codepoint-little, rest:bits>> -> "utf16le " <> show(#(int(c), rest))
    _ -> "other"
  }
}

fn last(b: BitArray) -> String {
  case b {
    <<0, c:utf8_codepoint>> -> "last " <> show(int(c))
    <<1, c:utf32_codepoint>> -> "last32 " <> show(int(c))
    _ -> "other"
  }
}

pub fn main() {
  let assert "all #(233, 8364, 128512)" = codepoints(<<"é":utf8, "€":utf16, "😀":utf32-little>>)
  let assert "utf8 #(128512, <<1:size(1)>>)" = codepoints(<<"😀":utf8, 1:1>>)
  let assert "other" = codepoints(<<0xC0, 0x80>>)
  let assert "other" = codepoints(<<0xED, 0xA0, 0x80>>)
  let assert "other" = codepoints(<<0xF4, 0x90, 0x80, 0x80>>)
  let assert "other" = codepoints(<<0xE2, 0x82>>)
  let assert "utf8 #(61, <<216, 0, 222, 7>>)" = codepoints(<<0x3D, 0xD8, 0x00, 0xDE, 7>>)
  let assert "other" = codepoints(<<0xFF>>)
  let assert "utf16le #(128512, <<7>>)" = utf16(<<0x3D, 0xD8, 0x00, 0xDE, 7>>)
  let assert "other" = utf16(<<0x00, 0xDC>>)
  let assert "other" = utf16(<<0x3D, 0xD8, 0x00, 0x00>>)
  let assert "last 97" = last(<<0, "a">>)
  let assert "other" = last(<<0, "ab">>)
  let assert "last 8364" = last(<<0, "€">>)
  let assert "last32 128512" = last(<<1, 0x1F600:32>>)
  let assert "other" = last(<<1, 0xD800:32>>)
  let assert "other" = last(<<1, 0x110000:32>>)
  let assert "other" = last(<<1, 0x41:32, 0>>)
  Nil
}
"##);
}

/// Bit array patterns with guards, alternatives, assignments and nested
/// matches of slices that do not start at a whole byte.
#[test]
fn pattern_clauses() {
    let _ = run(r##"
import bit_array.{show}

fn guards(b: BitArray) -> String {
  case b {
    <<a, b>> if a > b -> "gt"
    <<a, _>> if a == 0 -> "zero"
    <<_, _>> | <<_, _, _>> -> "2 or 3"
    _ -> "other"
  }
}

fn lets(b: BitArray) -> String {
  let assert <<a:4, b:bits>> = b
  let <<c:bits>> = b
  show(#(a, b, c))
}

fn tail(b: BitArray) -> String {
  case b {
    <<_:3, rest:bits>> ->
      case rest {
        <<a:5, b:bytes>> -> "aligned " <> show(#(a, b))
        <<a, b:bits>> -> "unaligned " <> show(#(a, b))
        _ -> "other"
      }
    _ -> "other"
  }
}

pub fn main() {
  let assert "gt" = guards(<<2, 1>>)
  let assert "2 or 3" = guards(<<1, 2>>)
  let assert "zero" = guards(<<0, 3>>)
  let assert "2 or 3" = guards(<<5, 5, 5>>)
  let assert "other" = guards(<<1>>)
  let assert "#(10, <<11:size(4)>>, <<11:size(4)>>)" = lets(<<0xAB>>)
  let assert "#(1, <<35, 33:size(7)>>, <<35, 33:size(7)>>)" = lets(<<0x12, 0x34, 1:3>>)
  let assert "aligned #(31, <<255>>)" = tail(<<0xFF, 0xFF>>)
  let assert "unaligned #(86, <<103, 129>>)" = tail(<<0b10101010, 0b11001100, 0b11110000, 1:3>>)
  Nil
}
"##);
}

/// Integer segments of patterns of 64 bits and more, of which an `Int` has
/// the least significant bits.
#[test]
fn pattern_large_ints() {
    let result = run_wasm_with_config(
        r##"
import bit_array.{show}

fn large(b: BitArray) -> String {
  case b {
    <<e:64-signed, f:64-little-signed, g:32, h:32-little-signed, i:size(70), _:bits>> ->
      show(#(e, f, g, h, i))
    _ -> "other"
  }
}

pub fn main() {
  let assert "#(-1234567890123, -98765432109, 4294967295, -2, -1)" = large(<<-1234567890123:64, -98765432109:64-little, 0xFFFFFFFF:32, -2:32-little, -1:70>>)
  Nil
}
"##,
        vec![(TEST_PACKAGE, "bit_array", BIT_ARRAY)],
        super::wasm_config(WasmInt::I64, WasmFloat::F64),
    );
    assert!(result.status.success(), "{}", result.stderr);
}

/// Float segments of patterns when `Float` has 32 bits.
#[test]
fn pattern_floats_of_32_bits() {
    let result = run_wasm_with_config(
        r##"
import bit_array.{show}

fn floats(b: BitArray) -> String {
  case b {
    <<1.5:float, x:32-float-little, y:16-float>> -> show(#(x, y))
    _ -> "other"
  }
}

pub fn main() {
  let assert "#(-2.75, 65504.0)" = floats(<<1.5:float, -2.75:32-float-little, 65504.0:16-float>>)
  Nil
}
"##,
        vec![(TEST_PACKAGE, "bit_array", BIT_ARRAY)],
        super::wasm_config(WasmInt::I32, WasmFloat::F32),
    );
    assert!(result.status.success(), "{}", result.stderr);
}

/// A literal integer segment is written from its literal, which does not need
/// to fit in an `Int`.
#[test]
fn construction_literal_larger_than_int() {
    let _ = run(r#"
import bit_array

pub fn main() {
  let assert "<<255, 255, 255, 255, 255, 248, 0, 0, 0, 0:size(3)>>" =
    bit_array.inspect(<<9_444_732_965_739_289_353_650_176:75>>)
  Nil
}
"#);
}
