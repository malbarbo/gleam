// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

use alloc::string::String;
use alloc::vec::Vec;
use base64::Engine;
use base64::alphabet;
use base64::engine::{DecodePaddingMode, GeneralPurpose, GeneralPurposeConfig};

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

    type QueryCase = (
        &'static str,
        Option<&'static [(&'static str, &'static str)]>,
    );

    fn bits(bytes: &[u8], bit_len: usize) -> Bits<'_> {
        Bits::new(bytes, bit_len)
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
