// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

use alloc::string::String;
use alloc::vec::Vec;
use core::str::FromStr;

//////////////
// int

fn fits(n: i64, int_bits: u32) -> bool {
    int_bits != 32 || i32::try_from(n).is_ok()
}

pub fn int_parse(s: &str, int_bits: u32) -> Option<i64> {
    i64::from_str(s).ok().filter(|&n| fits(n, int_bits))
}

pub fn int_base_parse(s: &str, base: i64, int_bits: u32) -> Option<i64> {
    if !(2..=36).contains(&base) {
        return None;
    }
    i64::from_str_radix(s, base as u32)
        .ok()
        .filter(|&n| fits(n, int_bits))
}

pub fn int_to_string(n: i64) -> String {
    String::from(itoa::Buffer::new().format(n))
}

pub fn int_to_base_string(n: i64, base: i64) -> String {
    assert!((2..=36).contains(&base), "invalid base");
    let base = base as u64;
    let mut m = n.unsigned_abs();
    let mut digits = Vec::new();
    loop {
        let d = (m % base) as u8;
        digits.push(if d < 10 { b'0' + d } else { b'A' + d - 10 });
        m /= base;
        if m == 0 {
            break;
        }
    }
    if n < 0 {
        digits.push(b'-');
    }
    digits.reverse();
    // Only ASCII digits and '-' were pushed.
    unsafe { String::from_utf8_unchecked(digits) }
}

//////////////
// float to string

/// Enough for the longest output, e.g. `-0.0000012345678901234567`.
pub const FLOAT_BUF_LEN: usize = 32;

/// Formats `x` like `float_to_string` in `gleam_stdlib.mjs`: the shortest
/// round-trip digits laid out as ECMAScript `Number#toString` does, with `.0`
/// added when there is no fractional part and without the `+` of the exponent.
/// `-0.0` gives `"-0.0"` (JavaScript gives `"0.0"`).
pub fn format_float(x: f64, f32mode: bool, buf: &mut [u8; FLOAT_BUF_LEN]) -> &str {
    let mut w = Writer { buf, len: 0 };
    if x.is_nan() {
        w.push_str("NaN.0");
    } else if x.is_infinite() {
        w.push_str(if x < 0.0 { "-Infinity.0" } else { "Infinity.0" });
    } else {
        let mut ryu = ryu::Buffer::new();
        let repr = if f32mode {
            ryu.format_finite(x as f32)
        } else {
            ryu.format_finite(x)
        };
        layout(repr, &mut w);
    }
    let len = w.len;
    // Only ASCII was written.
    unsafe { core::str::from_utf8_unchecked(&buf[..len]) }
}

pub fn float_to_string(x: f64, f32mode: bool) -> String {
    String::from(format_float(x, f32mode, &mut [0; FLOAT_BUF_LEN]))
}

struct Writer<'a> {
    buf: &'a mut [u8; FLOAT_BUF_LEN],
    len: usize,
}

impl Writer<'_> {
    fn push(&mut self, b: u8) {
        self.buf[self.len] = b;
        self.len += 1;
    }

    fn push_str(&mut self, s: &str) {
        s.bytes().for_each(|b| self.push(b));
    }

    fn push_zeros(&mut self, n: usize) {
        (0..n).for_each(|_| self.push(b'0'));
    }
}

/// Re-lays out a ryu representation (`[-]digits[.digits][e[-]digits]`).
fn layout(repr: &str, w: &mut Writer) {
    let (negative, repr) = match repr.strip_prefix('-') {
        Some(rest) => (true, rest),
        None => (false, repr),
    };
    let (mantissa, exp) = match repr.split_once(['e', 'E']) {
        Some((m, e)) => (m, i32::from_str(e).unwrap_or(0)),
        None => (repr, 0),
    };
    let (int, frac) = mantissa.split_once('.').unwrap_or((mantissa, ""));

    // Collect the significant digits; the value is 0.DIGITS * 10^point.
    let mut digits = [0u8; 20];
    let mut k = 0;
    let mut point = int.len() as i32 + exp;
    for b in int.bytes().chain(frac.bytes()) {
        if k == 0 && b == b'0' {
            point -= 1;
        } else {
            digits[k] = b;
            k += 1;
        }
    }
    while k > 0 && digits[k - 1] == b'0' {
        k -= 1;
    }

    if negative {
        w.push(b'-');
    }
    if k == 0 {
        w.push_str("0.0");
        return;
    }
    let digits = &digits[..k];
    let n = point;
    let k = k as i32;
    if k <= n && n <= 21 {
        digits.iter().for_each(|&d| w.push(d));
        w.push_zeros((n - k) as usize);
        w.push_str(".0");
    } else if 0 < n && n <= 21 {
        let (a, b) = digits.split_at(n as usize);
        a.iter().for_each(|&d| w.push(d));
        w.push(b'.');
        b.iter().for_each(|&d| w.push(d));
    } else if -6 < n && n <= 0 {
        w.push_str("0.");
        w.push_zeros((-n) as usize);
        digits.iter().for_each(|&d| w.push(d));
    } else {
        w.push(digits[0]);
        w.push(b'.');
        if k == 1 {
            w.push(b'0');
        } else {
            digits[1..].iter().for_each(|&d| w.push(d));
        }
        w.push(b'e');
        w.push_str(itoa::Buffer::new().format(n - 1));
    }
}

//////////////
// float parse

fn is_float_syntax(s: &[u8]) -> bool {
    fn digits(s: &[u8], i: &mut usize) -> bool {
        let start = *i;
        while *i < s.len() && s[*i].is_ascii_digit() {
            *i += 1;
        }
        *i > start
    }
    let mut i = 0;
    if matches!(s.first(), Some(b'+' | b'-')) {
        i += 1;
    }
    if !digits(s, &mut i) || s.get(i) != Some(&b'.') {
        return false;
    }
    i += 1;
    if !digits(s, &mut i) {
        return false;
    }
    if matches!(s.get(i), Some(b'e' | b'E')) {
        i += 1;
        if matches!(s.get(i), Some(b'+' | b'-')) {
            i += 1;
        }
        if !digits(s, &mut i) {
            return false;
        }
    }
    i == s.len()
}

pub fn float_parse(s: &str, f32mode: bool) -> Option<f64> {
    if !is_float_syntax(s.as_bytes()) {
        return None;
    }
    let x = if f32mode {
        fast_float2::parse::<f32, _>(s).ok()? as f64
    } else {
        fast_float2::parse::<f64, _>(s).ok()?
    };
    x.is_finite().then_some(x)
}

//////////////
// math

pub fn power(base: f64, exponent: f64) -> f64 {
    libm::pow(base, exponent)
}

pub fn logarithm(x: f64) -> f64 {
    libm::log(x)
}

pub fn exponential(x: f64) -> f64 {
    libm::exp(x)
}

//////////////
// random

/// xoshiro256++ (<https://prng.di.unimi.it/xoshiro256plusplus.c>).
pub struct Xoshiro256 {
    s: [u64; 4],
}

impl Xoshiro256 {
    pub const fn new(seed: [u64; 4]) -> Xoshiro256 {
        Xoshiro256 { s: seed }
    }

    pub fn next_u64(&mut self) -> u64 {
        let s = &mut self.s;
        let result = s[0].wrapping_add(s[3]).rotate_left(23).wrapping_add(s[0]);
        let t = s[1] << 17;
        s[2] ^= s[0];
        s[3] ^= s[1];
        s[1] ^= s[2];
        s[0] ^= s[3];
        s[2] ^= t;
        s[3] = s[3].rotate_left(45);
        result
    }

    /// Uniform in `[0, 1)` with 53 random bits.
    pub fn next_f64(&mut self) -> f64 {
        (self.next_u64() >> 11) as f64 * (1.0 / (1u64 << 53) as f64)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn int_parse_cases() {
        let cases: &[(&str, Option<i64>)] = &[
            ("0", Some(0)),
            ("123", Some(123)),
            ("+123", Some(123)),
            ("-123", Some(-123)),
            ("007", Some(7)),
            ("-0", Some(0)),
            ("9223372036854775807", Some(i64::MAX)),
            ("-9223372036854775808", Some(i64::MIN)),
            ("9223372036854775808", None),
            ("-9223372036854775809", None),
            ("", None),
            ("+", None),
            ("-", None),
            ("1_000", None),
            (" 1", None),
            ("1 ", None),
            ("1.0", None),
            ("0x10", None),
            ("--1", None),
            ("+-1", None),
            ("١", None),
        ];
        for &(s, expected) in cases {
            assert_eq!(int_parse(s, 64), expected, "{s:?}");
        }
    }

    #[test]
    fn int_parse_32() {
        assert_eq!(int_parse("2147483647", 32), Some(2147483647));
        assert_eq!(int_parse("-2147483648", 32), Some(-2147483648));
        assert_eq!(int_parse("2147483648", 32), None);
        assert_eq!(int_parse("-2147483649", 32), None);
        assert_eq!(int_base_parse("7FFFFFFF", 16, 32), Some(i32::MAX as i64));
        assert_eq!(int_base_parse("80000000", 16, 32), None);
        assert_eq!(int_base_parse("80000000", 16, 64), Some(0x80000000));
    }

    #[test]
    fn int_base_parse_cases() {
        let cases: &[(&str, i64, Option<i64>)] = &[
            ("101", 2, Some(5)),
            ("-101", 2, Some(-5)),
            ("+101", 2, Some(5)),
            ("102", 2, None),
            ("ff", 16, Some(255)),
            ("FF", 16, Some(255)),
            ("Ff", 16, Some(255)),
            ("zz", 36, Some(1295)),
            ("ZZ", 36, Some(1295)),
            ("10", 10, Some(10)),
            ("", 16, None),
            ("-", 16, None),
            ("0x1", 16, None),
            ("1_0", 10, None),
            ("7fffffffffffffff", 16, Some(i64::MAX)),
            ("-8000000000000000", 16, Some(i64::MIN)),
            ("8000000000000000", 16, None),
            ("1", 1, None),
            ("1", 37, None),
        ];
        for &(s, base, expected) in cases {
            assert_eq!(int_base_parse(s, base, 64), expected, "{s:?} {base}");
        }
    }

    #[test]
    fn int_to_string_cases() {
        assert_eq!(int_to_string(0), "0");
        assert_eq!(int_to_string(-42), "-42");
        assert_eq!(int_to_string(i64::MIN), "-9223372036854775808");
        assert_eq!(int_to_base_string(255, 16), "FF");
        assert_eq!(int_to_base_string(-255, 16), "-FF");
        assert_eq!(int_to_base_string(0, 2), "0");
        assert_eq!(int_to_base_string(5, 2), "101");
        assert_eq!(int_to_base_string(1295, 36), "ZZ");
        assert_eq!(int_to_base_string(i64::MAX, 16), "7FFFFFFFFFFFFFFF");
        assert_eq!(int_to_base_string(i64::MIN, 16), "-8000000000000000");
        assert_eq!(
            int_to_base_string(i64::MIN, 2),
            "-1000000000000000000000000000000000000000000000000000000000000000"
        );
    }

    #[test]
    fn float_to_string_cases() {
        let cases: &[(f64, &str)] = &[
            (0.0, "0.0"),
            (-0.0, "-0.0"),
            (1.0, "1.0"),
            (-1.0, "-1.0"),
            (0.1, "0.1"),
            (1.5, "1.5"),
            (100.0, "100.0"),
            (123.456, "123.456"),
            (0.000001, "0.000001"),
            (0.0000001, "1.0e-7"),
            (0.00000123, "0.00000123"),
            (1.23e-7, "1.23e-7"),
            (1e20, "100000000000000000000.0"),
            (1e21, "1.0e21"),
            (1.5e21, "1.5e21"),
            (123456789012345680000.0, "123456789012345680000.0"),
            (9007199254740993.0, "9007199254740992.0"),
            (f64::MAX, "1.7976931348623157e308"),
            (f64::MIN_POSITIVE, "2.2250738585072014e-308"),
            (5e-324, "5.0e-324"),
            (-5e-324, "-5.0e-324"),
            (1e100, "1.0e100"),
            (0.3, "0.3"),
            (0.1 + 0.2, "0.30000000000000004"),
            (f64::NAN, "NaN.0"),
            (f64::INFINITY, "Infinity.0"),
            (f64::NEG_INFINITY, "-Infinity.0"),
        ];
        for &(x, expected) in cases {
            assert_eq!(float_to_string(x, false), expected, "{x:e}");
        }
    }

    #[test]
    fn float_to_string_f32() {
        assert_eq!(float_to_string(0.1f32 as f64, true), "0.1");
        assert_eq!(float_to_string(0.1f32 as f64, false), "0.10000000149011612");
        assert_eq!(float_to_string(1e21f32 as f64, true), "1.0e21");
        assert_eq!(float_to_string(16777217.0, true), "16777216.0");
        assert_eq!(
            float_to_string(3.4028235e38f32 as f64, true),
            "3.4028235e38"
        );
        assert_eq!(float_to_string(-0.0, true), "-0.0");
    }

    #[test]
    fn float_buffer_is_large_enough() {
        let mut buf = [0; FLOAT_BUF_LEN];
        for x in [
            -1.2345678901234567e-6,
            -1.2345678901234567e20,
            -1.2345678901234567e-300,
            -f64::MAX,
            -f64::MIN_POSITIVE,
        ] {
            format_float(x, false, &mut buf);
        }
    }

    #[test]
    fn float_parse_cases() {
        let cases: &[(&str, Option<f64>)] = &[
            ("1.0", Some(1.0)),
            ("-1.0", Some(-1.0)),
            ("+1.0", Some(1.0)),
            ("0.5", Some(0.5)),
            ("1.5e3", Some(1500.0)),
            ("1.5E3", Some(1500.0)),
            ("1.5e+3", Some(1500.0)),
            ("1.5e-3", Some(0.0015)),
            ("00.10", Some(0.1)),
            ("1.0e400", None),
            ("-1.0e400", None),
            ("1.0e-400", Some(0.0)),
            ("1", None),
            ("1.", None),
            (".1", None),
            ("1.e3", None),
            ("1.0e", None),
            ("1.0e+", None),
            ("1e3", None),
            ("", None),
            ("-", None),
            ("1.0 ", None),
            (" 1.0", None),
            ("1_0.0", None),
            ("inf", None),
            ("NaN", None),
            ("1.0f", None),
        ];
        for &(s, expected) in cases {
            assert_eq!(float_parse(s, false), expected, "{s:?}");
        }
        assert_eq!(float_parse("1.0e39", true), None);
        assert_eq!(float_parse("1.0e38", true), Some(1e38f32 as f64));
        assert_eq!(float_parse("0.1", true), Some(0.1f32 as f64));
    }

    #[test]
    fn math() {
        assert_eq!(power(2.0, 10.0), 1024.0);
        assert_eq!(power(4.0, 0.5), 2.0);
        assert_eq!(logarithm(1.0), 0.0);
        assert_eq!(exponential(0.0), 1.0);
        assert!((logarithm(core::f64::consts::E) - 1.0).abs() < 1e-15);
    }

    fn float_samples() -> Vec<f64> {
        let mut r = Xoshiro256::new([0x1234, 0x5678, 0x9ABC, 0xDEF0]);
        let mut xs = Vec::new();
        let neighbours = |xs: &mut Vec<f64>, x: f64, n: i64| {
            for d in -n..=n {
                xs.push(f64::from_bits((x.to_bits() as i64 + d) as u64));
            }
        };
        // random bit patterns
        while xs.len() < 12000 {
            let x = f64::from_bits(r.next_u64());
            if x.is_finite() {
                xs.push(x);
            }
        }
        // powers of 10 and their neighbours
        for e in -323..=308 {
            let x: f64 = format!("1e{e}").parse().unwrap();
            neighbours(&mut xs, x, 2);
        }
        // integers near 2^53 and other powers of 2
        for p in [52, 53, 54, 63, 64, 69, 70] {
            let x = 2f64.powi(p);
            for d in -40..=40 {
                xs.push(x + d as f64);
            }
            neighbours(&mut xs, x, 5);
        }
        // the layout boundaries: 1e21, 1e-6, 1e-7
        for x in [1e21, 1e-6, 1e-7, 999999999999999900000.0, 0.000001234] {
            neighbours(&mut xs, x, 50);
            neighbours(&mut xs, -x, 3);
        }
        // subnormals
        for _ in 0..2000 {
            xs.push(f64::from_bits(r.next_u64() >> 12));
        }
        neighbours(&mut xs, f64::MIN_POSITIVE, 5);
        neighbours(&mut xs, 5e-324, 1);
        neighbours(&mut xs, f64::MAX, 3);
        // short decimals and plain integers
        for _ in 0..3000 {
            let digits = r.next_u64() % 10u64.pow(1 + (r.next_u64() % 17) as u32);
            let exp = (r.next_u64() % 60) as i32 - 30;
            xs.push(format!("{digits}e{exp}").parse().unwrap());
            xs.push((r.next_u64() >> (r.next_u64() % 64)) as f64);
        }
        xs.retain(|x| x.is_finite() && *x != 0.0);
        xs.extend([0.0, 1.0, -1.0, 0.1, 0.5, 100.0, 123.456]);
        xs
    }

    /// Compares `float_to_string` with the JavaScript implementation, using
    /// `tests-data/float_to_string.mjs`. Needs `node`; run with `--ignored`.
    #[test]
    #[ignore]
    fn float_to_string_matches_node() {
        use std::io::Write;
        use std::process::{Command, Stdio};
        let xs = float_samples();
        let mut child = Command::new("node")
            .arg(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/tests-data/float_to_string.mjs"
            ))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("node");
        let mut input = String::new();
        for x in &xs {
            input.push_str(&format!("{:016x}\n", x.to_bits()));
        }
        child
            .stdin
            .take()
            .unwrap()
            .write_all(input.as_bytes())
            .unwrap();
        let output = child.wait_with_output().unwrap();
        let expected = String::from_utf8(output.stdout).unwrap();
        let expected: Vec<&str> = expected.lines().collect();
        assert_eq!(expected.len(), xs.len());
        let mut divergences = 0;
        for (x, e) in xs.iter().zip(expected) {
            let got = float_to_string(*x, false);
            if got != e {
                divergences += 1;
                eprintln!("{:016x}: got {got}, node {e}", x.to_bits());
            }
        }
        eprintln!("{} values compared, {divergences} divergences", xs.len());
        assert_eq!(divergences, 0);
    }

    #[test]
    fn float_round_trips() {
        for x in float_samples() {
            assert_eq!(float_to_string(x, false).parse::<f64>().unwrap(), x);
            let y = x as f32;
            if y.is_finite() {
                let s = float_to_string(y as f64, true);
                assert_eq!(s.parse::<f32>().unwrap(), y, "{s}");
            }
        }
    }

    #[test]
    fn xoshiro_reference() {
        // First outputs of the reference implementation seeded with 1, 2, 3, 4.
        let mut r = Xoshiro256::new([1, 2, 3, 4]);
        let expected = [
            41943041,
            58720359,
            3588806011781223,
            3591011842654386,
            9228616714210784205,
        ];
        for e in expected {
            assert_eq!(r.next_u64(), e);
        }
        for _ in 0..10000 {
            let x = r.next_f64();
            assert!((0.0..1.0).contains(&x));
        }
    }
}
