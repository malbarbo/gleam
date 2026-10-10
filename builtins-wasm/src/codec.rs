// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

use alloc::string::String;
use alloc::vec::Vec;
use base64::Engine;
use base64::alphabet;
use base64::engine::{DecodePaddingMode, GeneralPurpose, GeneralPurposeConfig};
use core::cmp::Ordering;

//////////////
// bit arrays

/// A bit array: `bit_len` bits stored most significant first in `bytes`;
/// the unused low bits of a partial last byte are ignored.
#[derive(Clone, Copy, Debug)]
pub struct Bits<'a> {
    pub bytes: &'a [u8],
    pub bit_len: usize,
}

impl<'a> Bits<'a> {
    pub fn new(bytes: &'a [u8], bit_len: usize) -> Bits<'a> {
        Bits {
            bytes: &bytes[..bit_len.div_ceil(8)],
            bit_len,
        }
    }

    pub fn bytes(bytes: &'a [u8]) -> Bits<'a> {
        Bits::new(bytes, bytes.len() * 8)
    }

    fn bit(&self, i: usize) -> bool {
        self.bytes[i / 8] & (0x80 >> (i % 8)) != 0
    }

    fn whole_bytes(&self) -> Option<&'a [u8]> {
        self.bit_len.is_multiple_of(8).then_some(self.bytes)
    }

    /// The bytes with the unused bits of the last byte set to zero.
    fn padded(&self) -> Vec<u8> {
        let mut v = Vec::from(self.bytes);
        let rem = self.bit_len % 8;
        if let (Some(last), true) = (v.last_mut(), rem != 0) {
            *last &= 0xFF << (8 - rem);
        }
        v
    }
}

pub fn bit_array_to_string(bits: Bits<'_>) -> Option<&str> {
    core::str::from_utf8(bits.whole_bytes()?).ok()
}

pub fn bit_array_is_utf8(bits: Bits) -> bool {
    bit_array_to_string(bits).is_some()
}

pub fn bit_array_inspect(bits: Bits) -> String {
    let mut out = String::from("<<");
    let rem = bits.bit_len % 8;
    for (i, &b) in bits.bytes.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        let mut n = itoa::Buffer::new();
        if i + 1 == bits.bytes.len() && rem != 0 {
            out.push_str(n.format(b >> (8 - rem)));
            out.push_str(":size(");
            out.push_str(n.format(rem));
            out.push(')');
        } else {
            out.push_str(n.format(b));
        }
    }
    out.push_str(">>");
    out
}

/// Unsigned big endian value of all bits (wrapping to 64 bits) and bit size.
pub fn bit_array_to_int_and_size(bits: Bits) -> (i64, i64) {
    let value = (0..bits.bit_len).fold(0u64, |acc, i| (acc << 1) | bits.bit(i) as u64);
    (value as i64, bits.bit_len as i64)
}

/// Same result as `bit_array.compare`: bytes are compared while both sides
/// have a whole byte; then, if both are non empty, the remaining bits are
/// compared as unsigned integers and, when equal, by bit size.
pub fn bit_array_compare(a: Bits, b: Bits) -> Ordering {
    let mut i = 0;
    while a.bit_len - i >= 8 && b.bit_len - i >= 8 {
        match a.bytes[i / 8].cmp(&b.bytes[i / 8]) {
            Ordering::Equal => i += 8,
            o => return o,
        }
    }
    match (a.bit_len - i, b.bit_len - i) {
        (0, 0) => Ordering::Equal,
        (_, 0) => Ordering::Greater,
        (0, _) => Ordering::Less,
        _ => compare_uint(a, b, i).then((a.bit_len - i).cmp(&(b.bit_len - i))),
    }
}

/// Compares the bits from `start` of `a` and `b` as unsigned integers.
fn compare_uint(a: Bits, b: Bits, start: usize) -> Ordering {
    let first_one = |x: Bits| (start..x.bit_len).find(|&i| x.bit(i)).unwrap_or(x.bit_len);
    let (fa, fb) = (first_one(a), first_one(b));
    let (wa, wb) = (a.bit_len - fa, b.bit_len - fb);
    wa.cmp(&wb).then_with(|| {
        (0..wa)
            .map(|k| a.bit(fa + k).cmp(&b.bit(fb + k)))
            .find(|o| o.is_ne())
            .unwrap_or(Ordering::Equal)
    })
}

//////////////
// base64 / base16

const DECODE: GeneralPurposeConfig = GeneralPurposeConfig::new()
    .with_decode_padding_mode(DecodePaddingMode::Indifferent)
    .with_decode_allow_trailing_bits(true);

const ENCODE_PAD: GeneralPurposeConfig = GeneralPurposeConfig::new().with_encode_padding(true);
const ENCODE_NO_PAD: GeneralPurposeConfig = GeneralPurposeConfig::new().with_encode_padding(false);

fn engine(url: bool, config: GeneralPurposeConfig) -> GeneralPurpose {
    let alphabet = if url {
        &alphabet::URL_SAFE
    } else {
        &alphabet::STANDARD
    };
    GeneralPurpose::new(alphabet, config)
}

pub fn base64_encode(bits: Bits, padding: bool, url: bool) -> String {
    let config = if padding { ENCODE_PAD } else { ENCODE_NO_PAD };
    engine(url, config).encode(bits.padded())
}

pub fn base64_decode(s: &str, url: bool) -> Option<Vec<u8>> {
    engine(url, DECODE).decode(s).ok()
}

pub fn base16_encode(bits: Bits) -> String {
    hex::encode_upper(bits.padded())
}

pub fn base16_decode(s: &str) -> Option<Vec<u8>> {
    hex::decode(s).ok()
}

//////////////
// uri

fn is_unescaped(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b"-._~!$'()*+".contains(&b)
}

pub fn percent_encode(s: &str) -> String {
    const HEX: &[u8; 16] = b"0123456789ABCDEF";
    let mut out = String::with_capacity(s.len());
    for b in s.bytes() {
        if is_unescaped(b) {
            out.push(b as char);
        } else {
            out.push('%');
            out.push(HEX[(b >> 4) as usize] as char);
            out.push(HEX[(b & 0xF) as usize] as char);
        }
    }
    out
}

fn hex_value(b: u8) -> Option<u8> {
    (b as char).to_digit(16).map(|d| d as u8)
}

/// Decodes `%XX` escapes (and `+` as space when `plus` is set). An invalid
/// escape is an error, but a `%` followed by fewer than two bytes at the end
/// is kept as is. The result must be valid UTF-8.
fn decode(s: &str, plus: bool) -> Option<String> {
    let s = s.as_bytes();
    let mut out = Vec::with_capacity(s.len());
    let mut i = 0;
    while i < s.len() {
        match s[i] {
            b'%' if i + 2 < s.len() => {
                let hi = hex_value(s[i + 1])?;
                let lo = hex_value(s[i + 2])?;
                out.push(hi << 4 | lo);
                i += 3;
                continue;
            }
            b'+' if plus => out.push(b' '),
            b => out.push(b),
        }
        i += 1;
    }
    String::from_utf8(out).ok()
}

pub fn percent_decode(s: &str) -> Option<String> {
    decode(s, false)
}

/// Like `uri_string:dissect_query` in Erlang: `&` separates the pairs (empty
/// segments included), the first `=` splits key and value (`""` when absent)
/// and `+` is a space.
pub fn parse_query(query: &str) -> Option<Vec<(String, String)>> {
    if query.is_empty() {
        return Some(Vec::new());
    }
    query
        .split('&')
        .map(|pair| {
            let (key, value) = pair.split_once('=').unwrap_or((pair, ""));
            Some((decode(key, true)?, decode(value, true)?))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    type CompareCase = (&'static [u8], usize, &'static [u8], usize, Ordering);
    type QueryCase = (
        &'static str,
        Option<&'static [(&'static str, &'static str)]>,
    );

    fn bits(bytes: &[u8], bit_len: usize) -> Bits<'_> {
        Bits::new(bytes, bit_len)
    }

    #[test]
    fn to_string() {
        assert_eq!(bit_array_to_string(Bits::bytes(b"abc")), Some("abc"));
        assert_eq!(bit_array_to_string(Bits::bytes(b"")), Some(""));
        assert_eq!(bit_array_to_string(Bits::bytes(&[0xFF])), None);
        assert_eq!(bit_array_to_string(Bits::bytes(&[0xC3])), None);
        assert_eq!(bit_array_to_string(bits(b"ab", 15)), None);
        assert!(bit_array_is_utf8(Bits::bytes("é".as_bytes())));
        assert!(!bit_array_is_utf8(bits(b"a", 7)));
    }

    #[test]
    fn inspect() {
        let cases: &[(&[u8], usize, &str)] = &[
            (&[], 0, "<<>>"),
            (&[1, 2, 3], 24, "<<1, 2, 3>>"),
            (&[255], 8, "<<255>>"),
            (&[0b1010_0000], 3, "<<5:size(3)>>"),
            (&[0b1010_0111], 3, "<<5:size(3)>>"),
            (&[1, 0b1100_0000], 10, "<<1, 3:size(2)>>"),
            (&[1, 0b1111_1110], 15, "<<1, 127:size(7)>>"),
            (&[0x80], 1, "<<1:size(1)>>"),
        ];
        for &(b, n, expected) in cases {
            assert_eq!(bit_array_inspect(bits(b, n)), expected);
        }
    }

    #[test]
    fn int_and_size() {
        assert_eq!(bit_array_to_int_and_size(bits(&[], 0)), (0, 0));
        assert_eq!(bit_array_to_int_and_size(bits(&[1, 2], 16)), (258, 16));
        assert_eq!(bit_array_to_int_and_size(bits(&[0b1010_0000], 3)), (5, 3));
        assert_eq!(
            bit_array_to_int_and_size(bits(&[1, 0b1100_0000], 10)),
            (7, 10)
        );
        assert_eq!(bit_array_to_int_and_size(bits(&[0xFF; 8], 64)), (-1, 64));
        // wraps past 64 bits
        assert_eq!(
            bit_array_to_int_and_size(bits(&[1, 0, 0, 0, 0, 0, 0, 0, 2], 72)),
            (2, 72)
        );
    }

    #[test]
    fn compare() {
        use Ordering::*;
        let cases: &[CompareCase] = &[
            (&[], 0, &[], 0, Equal),
            (&[1], 8, &[1], 8, Equal),
            (&[1], 8, &[2], 8, Less),
            (&[2], 8, &[1], 8, Greater),
            (&[1, 2], 16, &[1], 8, Greater),
            (&[1], 8, &[1, 2], 16, Less),
            (&[1], 8, &[], 0, Greater),
            (&[], 0, &[0x80], 1, Less),
            (&[0x80], 1, &[], 0, Greater),
            // partial bits: compared as integers, then by size
            (&[0b1000_0000], 1, &[0b0100_0000], 2, Equal.then(Less)),
            (&[0b1000_0000], 1, &[0b1000_0000], 2, Less),
            (&[0b1100_0000], 2, &[0b0110_0000], 3, Less),
            (&[0b0010_0000], 3, &[0b0100_0000], 2, Greater),
            (&[0b0000_0000], 3, &[0b0000_0000], 2, Greater),
            (&[0b0000_0000], 2, &[0b0000_0000], 2, Equal),
            (&[0b1110_0000], 3, &[0b1100_0000], 2, Greater),
            // one side partial, other with whole bytes
            (&[0b1110_0000], 3, &[0, 0], 16, Greater),
            (&[0b1110_0000], 3, &[0, 7], 16, Equal.then(Less)),
            (&[0b1110_0000], 3, &[0, 8], 16, Less),
            (&[0b0010_0000], 3, &[0], 8, Greater),
            (&[0b0000_0000], 3, &[0], 8, Less),
            (&[5, 0b1000_0000], 9, &[5, 0b1000_0000], 9, Equal),
            (&[5, 0b1000_0000], 9, &[5, 0b1000_0000], 10, Less),
            (&[4, 0b1000_0000], 9, &[5, 0b1000_0000], 9, Less),
            (&[5, 0xFF], 9, &[5, 0x80], 9, Equal),
        ];
        for &(a, na, b, nb, expected) in cases {
            assert_eq!(
                bit_array_compare(bits(a, na), bits(b, nb)),
                expected,
                "{a:?}:{na} {b:?}:{nb}"
            );
        }
    }

    /// Direct transcription of `bit_array.compare` (with big integers
    /// replaced by the bit lists they come from).
    fn reference_compare(a: &[bool], b: &[bool]) -> Ordering {
        if a.len() >= 8 && b.len() >= 8 {
            let (fa, fb) = (&a[..8], &b[..8]);
            return match fa.cmp(fb) {
                Ordering::Equal => reference_compare(&a[8..], &b[8..]),
                o => o,
            };
        }
        match (a.is_empty(), b.is_empty()) {
            (true, true) => Ordering::Equal,
            (false, true) => Ordering::Greater,
            (true, false) => Ordering::Less,
            _ => {
                let strip = |x: &[bool]| {
                    let i = x.iter().position(|&b| b).unwrap_or(x.len());
                    x[i..].to_vec()
                };
                let (va, vb) = (strip(a), strip(b));
                va.len()
                    .cmp(&vb.len())
                    .then(va.cmp(&vb))
                    .then(a.len().cmp(&b.len()))
            }
        }
    }

    #[test]
    fn compare_exhaustive_small() {
        // every bit array up to 10 bits against every other
        let mut all = Vec::new();
        for n in 0..=10usize {
            for v in 0..(1u32 << n) {
                let bools: Vec<bool> = (0..n).map(|i| v >> (n - 1 - i) & 1 == 1).collect();
                let mut bytes = vec![0u8; n.div_ceil(8)];
                for (i, &b) in bools.iter().enumerate() {
                    if b {
                        bytes[i / 8] |= 0x80 >> (i % 8);
                    }
                }
                all.push((bools, bytes, n));
            }
        }
        for (ba, ya, na) in &all {
            for (bb, yb, nb) in &all {
                assert_eq!(
                    bit_array_compare(bits(ya, *na), bits(yb, *nb)),
                    reference_compare(ba, bb),
                    "{ba:?} {bb:?}"
                );
            }
        }
    }

    #[test]
    fn base64() {
        let cases: &[(&[u8], usize, &str, &str)] = &[
            (b"", 0, "", ""),
            (b"f", 8, "Zg==", "Zg"),
            (b"fo", 16, "Zm8=", "Zm8"),
            (b"foo", 24, "Zm9v", "Zm9v"),
            (&[0xFB, 0xFF], 16, "+/8=", "+/8"),
            // trailing bits are padded with zeros
            (&[0xFF], 1, "gA==", "gA"),
            (&[0xFF, 0xFF], 12, "//A=", "//A"),
        ];
        for &(b, n, padded, unpadded) in cases {
            assert_eq!(base64_encode(bits(b, n), true, false), padded);
            assert_eq!(base64_encode(bits(b, n), false, false), unpadded);
        }
        assert_eq!(
            base64_encode(Bits::bytes(&[0xFB, 0xFF]), true, true),
            "-_8="
        );
        assert_eq!(
            base64_encode(Bits::bytes(&[0xFB, 0xFF]), false, true),
            "-_8"
        );

        assert_eq!(base64_decode("Zm9v", false), Some(b"foo".to_vec()));
        assert_eq!(base64_decode("Zg==", false), Some(b"f".to_vec()));
        assert_eq!(base64_decode("Zg", false), Some(b"f".to_vec()));
        assert_eq!(base64_decode("Zh==", false), Some(b"f".to_vec()));
        assert_eq!(base64_decode("", false), Some(vec![]));
        assert_eq!(base64_decode("+/8=", false), Some(vec![0xFB, 0xFF]));
        assert_eq!(base64_decode("-_8=", false), None);
        assert_eq!(base64_decode("-_8=", true), Some(vec![0xFB, 0xFF]));
        assert_eq!(base64_decode("+/8=", true), None);
        assert_eq!(base64_decode("Z", false), None);
        assert_eq!(base64_decode("Zm9v!", false), None);
        assert_eq!(base64_decode("Zg=", false), Some(b"f".to_vec()));
    }

    #[test]
    fn base16() {
        assert_eq!(base16_encode(Bits::bytes(&[])), "");
        assert_eq!(base16_encode(Bits::bytes(&[0x01, 0xAB, 0xFF])), "01ABFF");
        assert_eq!(base16_encode(bits(&[0xFF], 4)), "F0");
        assert_eq!(base16_decode("01abFF"), Some(vec![0x01, 0xAB, 0xFF]));
        assert_eq!(base16_decode(""), Some(vec![]));
        assert_eq!(base16_decode("abc"), None);
        assert_eq!(base16_decode("zz"), None);
    }

    #[test]
    fn percent_encoding() {
        assert_eq!(percent_encode("100% great"), "100%25%20great");
        assert_eq!(percent_encode("AZaz09-._~!$'()*+"), "AZaz09-._~!$'()*+");
        assert_eq!(
            percent_encode("&=/?#[]@,;:\"<>"),
            "%26%3D%2F%3F%23%5B%5D%40%2C%3B%3A%22%3C%3E"
        );
        assert_eq!(percent_encode("é"), "%C3%A9");
        assert_eq!(percent_encode(""), "");
    }

    #[test]
    fn percent_decoding() {
        let cases: &[(&str, Option<&str>)] = &[
            ("100%25%20great+fun", Some("100% great+fun")),
            ("%C3%A9", Some("é")),
            ("%c3%a9", Some("é")),
            ("%ZZ", None),
            ("%4G", None),
            ("%%41", None),
            ("%4%41", None),
            ("%4", Some("%4")),
            ("%", Some("%")),
            ("abc%", Some("abc%")),
            ("%41%", Some("A%")),
            ("%C3", None),
            ("%FF", None),
            ("", Some("")),
        ];
        for &(s, expected) in cases {
            assert_eq!(percent_decode(s).as_deref(), expected, "{s:?}");
        }
    }

    #[test]
    fn query() {
        // expected values from `uri_string:dissect_query` (OTP), with Gleam's
        // mapping of a key without `=` to an empty value
        let cases: &[QueryCase] = &[
            ("", Some(&[])),
            ("a=1&b=2", Some(&[("a", "1"), ("b", "2")])),
            ("a=1&&b", Some(&[("a", "1"), ("", ""), ("b", "")])),
            ("&", Some(&[("", ""), ("", "")])),
            ("a", Some(&[("a", "")])),
            ("a=", Some(&[("a", "")])),
            ("=1", Some(&[("", "1")])),
            ("=", Some(&[("", "")])),
            ("x=1=2", Some(&[("x", "1=2")])),
            ("a==", Some(&[("a", "=")])),
            ("a+b=c+d", Some(&[("a b", "c d")])),
            ("a=1;b=2", Some(&[("a", "1;b=2")])),
            ("a=%ZZ", None),
            ("a=%4G", None),
            ("a=%%41", None),
            ("a=%4%41", None),
            ("a=b=%ZZ", None),
            ("a=%41", Some(&[("a", "A")])),
            ("%41=%42", Some(&[("A", "B")])),
            ("a=%4", Some(&[("a", "%4")])),
            ("a=%", Some(&[("a", "%")])),
            ("a=%G", Some(&[("a", "%G")])),
            ("a=1%", Some(&[("a", "1%")])),
            ("a%=1", Some(&[("a%", "1")])),
            ("%", Some(&[("%", "")])),
            ("a=%4&b=1", Some(&[("a", "%4"), ("b", "1")])),
            ("a=%&b", Some(&[("a", "%"), ("b", "")])),
            ("a=%C3", None),
            ("a=%FF", None),
            ("a=%e9", None),
            ("a=%C3&b=%A9", None),
            ("a=%C3%A9", Some(&[("a", "é")])),
            ("k%3Dv=1", Some(&[("k=v", "1")])),
            ("a=%2B", Some(&[("a", "+")])),
            ("a=b%26c", Some(&[("a", "b&c")])),
            ("a b=c", Some(&[("a b", "c")])),
            ("a=%00", Some(&[("a", "\0")])),
            (
                "&&a=1&&",
                Some(&[("", ""), ("", ""), ("a", "1"), ("", ""), ("", "")]),
            ),
            ("a=1&=2&b", Some(&[("a", "1"), ("", "2"), ("b", "")])),
        ];
        for &(q, expected) in cases {
            let got = parse_query(q);
            let got: Option<Vec<(&str, &str)>> = got
                .as_ref()
                .map(|v| v.iter().map(|(k, v)| (k.as_str(), v.as_str())).collect());
            assert_eq!(got.as_deref(), expected, "{q:?}");
        }
    }
}
