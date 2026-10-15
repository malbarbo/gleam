// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

use alloc::string::String;
use alloc::vec::Vec;
use unicode_segmentation::UnicodeSegmentation;

pub fn length(s: &str) -> i64 {
    if s.is_ascii() {
        // Every ASCII character is a grapheme, except "\r\n", which is one.
        return (s.len() - s.matches("\r\n").count()) as i64;
    }
    s.graphemes(true).count() as i64
}

pub fn lowercase(s: &str) -> String {
    s.to_lowercase()
}

pub fn uppercase(s: &str) -> String {
    s.to_uppercase()
}

/// Pattern_White_Space.
fn is_whitespace(c: char) -> bool {
    matches!(
        c,
        '\u{0009}'
            ..='\u{000D}' | ' ' | '\u{0085}' | '\u{200E}' | '\u{200F}' | '\u{2028}' | '\u{2029}'
    )
}

pub fn trim(s: &str) -> &str {
    s.trim_matches(is_whitespace)
}

pub fn trim_start(s: &str) -> &str {
    s.trim_start_matches(is_whitespace)
}

pub fn trim_end(s: &str) -> &str {
    s.trim_end_matches(is_whitespace)
}

/// Byte offset of the first occurrence of `needle`.
fn find(haystack: &str, needle: &str) -> Option<usize> {
    memchr::memmem::find(haystack.as_bytes(), needle.as_bytes())
}

pub fn contains(haystack: &str, needle: &str) -> bool {
    find(haystack, needle).is_some()
}

pub fn crop<'a>(s: &'a str, prefix: &str) -> &'a str {
    match find(s, prefix) {
        Some(i) => &s[i..],
        None => s,
    }
}

pub fn split<'a>(s: &'a str, sep: &str) -> Vec<&'a str> {
    if sep.is_empty() {
        return to_graphemes(s);
    }
    let mut parts = Vec::new();
    let mut rest = s;
    while let Some(i) = find(rest, sep) {
        parts.push(&rest[..i]);
        rest = &rest[i + sep.len()..];
    }
    parts.push(rest);
    parts
}

pub fn split_once<'a>(s: &'a str, sep: &str) -> Option<(&'a str, &'a str)> {
    let i = find(s, sep)?;
    Some((&s[..i], &s[i + sep.len()..]))
}

pub fn replace(s: &str, pattern: &str, replacement: &str) -> String {
    if pattern.is_empty() {
        return String::from(s);
    }
    let mut out = String::with_capacity(s.len());
    let mut rest = s;
    while let Some(i) = find(rest, pattern) {
        out.push_str(&rest[..i]);
        out.push_str(replacement);
        rest = &rest[i + pattern.len()..];
    }
    out.push_str(rest);
    out
}

pub fn reverse(s: &str) -> String {
    s.graphemes(true).rev().collect()
}

pub fn to_graphemes(s: &str) -> Vec<&str> {
    s.graphemes(true).collect()
}

/// A non negative count as `usize`, saturating (`usize` is 32 bits on wasm).
fn count(n: i64) -> usize {
    usize::try_from(n).unwrap_or(usize::MAX)
}

/// Graphemes `[start, start + len)` with `start >= 0` and `len > 0`.
fn grapheme_slice(s: &str, start: i64, len: i64) -> &str {
    let mut it = s.grapheme_indices(true).skip(count(start));
    let Some((from, _)) = it.next() else {
        return "";
    };
    let to = it.nth(count(len - 1)).map_or(s.len(), |(i, _)| i);
    &s[from..to]
}

/// Like `string.slice`.
pub fn slice(s: &str, start: i64, len: i64) -> &str {
    if len <= 0 {
        return "";
    }
    let start = if start < 0 { length(s) + start } else { start };
    if start < 0 {
        return "";
    }
    grapheme_slice(s, start, len)
}

pub fn drop_start(s: &str, n: i64) -> &str {
    if n <= 0 {
        return s;
    }
    match s.grapheme_indices(true).nth(count(n)) {
        Some((i, _)) => &s[i..],
        None => "",
    }
}

pub fn drop_end(s: &str, n: i64) -> &str {
    if n <= 0 {
        return s;
    }
    match s.grapheme_indices(true).rev().nth(count(n - 1)) {
        Some((i, _)) => &s[..i],
        None => "",
    }
}

pub fn first(s: &str) -> Option<&str> {
    s.graphemes(true).next()
}

pub fn last(s: &str) -> Option<&str> {
    s.graphemes(true).next_back()
}

pub fn pop_grapheme(s: &str) -> Option<(&str, &str)> {
    let g = first(s)?;
    Some((g, &s[g.len()..]))
}

pub fn to_utf_codepoints(s: &str) -> Vec<u32> {
    s.chars().map(|c| c as u32).collect()
}

pub fn from_utf_codepoints(codepoints: impl IntoIterator<Item = u32>) -> String {
    codepoints
        .into_iter()
        // `panic!` with a literal, so the panic handler can print it without
        // formatting (`expect` passes the message as an argument).
        .map(|c| char::from_u32(c).unwrap_or_else(|| panic!("invalid code point")))
        .collect()
}

/// The Gleam repr of a string, as `string.inspect` on Erlang and JavaScript.
pub fn inspect(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{000C}' => out.push_str("\\f"),
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\u{0000}'..='\u{001F}' | '\u{007F}'..='\u{009F}' => {
                const HEX: &[u8; 16] = b"0123456789ABCDEF";
                out.push_str("\\u{00");
                out.push(HEX[(c as usize >> 4) & 0xF] as char);
                out.push(HEX[c as usize & 0xF] as char);
                out.push('}');
            }
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

/// Byte length of the first grapheme of `window`, the start of a string.
///
/// When `complete` is false, the window may be followed by more bytes, so `-1`
/// is returned when the grapheme reaches the end of the window (a following
/// code point could extend it), including when the window cuts a UTF-8
/// sequence. A complete window is the whole string; `""` gives 0.
pub fn grapheme_prefix_len(window: &[u8], complete: bool) -> i32 {
    let valid = match core::str::from_utf8(window) {
        Ok(s) => s,
        Err(e) => unsafe { core::str::from_utf8_unchecked(&window[..e.valid_up_to()]) },
    };
    let len = valid.graphemes(true).next().map_or(0, str::len);
    if !complete && len == valid.len() {
        -1
    } else {
        len as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn graphemes_and_length() {
        assert_eq!(length(""), 0);
        assert_eq!(length("abc"), 3);
        assert_eq!(length("e\u{301}"), 1);
        assert_eq!(length("🇧🇷🇺🇸"), 2);
        assert_eq!(length("👨‍👩‍👧"), 1);
        assert_eq!(length("\r\n"), 1);
        assert_eq!(to_graphemes("ae\u{301}🇧🇷"), ["a", "e\u{301}", "🇧🇷"]);
        assert_eq!(to_graphemes(""), [] as [&str; 0]);
        assert_eq!(reverse("ae\u{301}🇧🇷"), "🇧🇷e\u{301}a");
        assert_eq!(reverse(""), "");
    }

    #[test]
    fn case() {
        assert_eq!(lowercase("ΟΔΟΣ"), "οδος");
        assert_eq!(lowercase("AbC"), "abc");
        assert_eq!(uppercase("straße"), "STRASSE");
        assert_eq!(uppercase("abc"), "ABC");
        assert_eq!(lowercase("İ"), "i\u{307}");
    }

    #[test]
    fn trimming() {
        let ws = "\t\n\u{B}\u{C}\r \u{85}\u{200E}\u{200F}\u{2028}\u{2029}";
        let s = format!("{ws}a b{ws}");
        assert_eq!(trim(&s), "a b");
        assert_eq!(trim_start(&s), format!("a b{ws}"));
        assert_eq!(trim_end(&s), format!("{ws}a b"));
        assert_eq!(trim("\u{A0}a\u{3000}"), "\u{A0}a\u{3000}");
        assert_eq!(trim("\u{200B}a"), "\u{200B}a");
        assert_eq!(trim("   "), "");
        assert_eq!(trim(""), "");
    }

    #[test]
    fn searching() {
        assert!(contains("hello", "ll"));
        assert!(contains("hello", ""));
        assert!(contains("", ""));
        assert!(!contains("hello", "lo!"));
        assert!(!contains("", "a"));
        assert_eq!(crop("The Lone Gunmen", "Lone"), "Lone Gunmen");
        assert_eq!(crop("abc", "x"), "abc");
        assert_eq!(crop("abc", ""), "abc");
        assert_eq!(crop("abcbc", "bc"), "bcbc");
    }

    #[test]
    fn find_matches_std() {
        // every haystack of up to 7 letters from {a, b} against every needle
        // of up to 3 letters
        let words = |max: u32| {
            (0..=max).flat_map(|n| {
                (0..1u32 << n).map(move |v| {
                    (0..n)
                        .map(|i| if v >> i & 1 == 1 { 'a' } else { 'b' })
                        .collect::<String>()
                })
            })
        };
        for h in words(7) {
            for n in words(3) {
                assert_eq!(find(&h, &n), h.find(&n), "{h:?} {n:?}");
            }
        }
        assert_eq!(find("aé🎉é", "é"), Some(1));
        assert_eq!(find("aé🎉é", "🎉é"), Some(3));
    }

    #[test]
    fn splitting() {
        let cases: &[(&str, &str, &[&str])] = &[
            ("a,b,c", ",", &["a", "b", "c"]),
            ("a,,b", ",", &["a", "", "b"]),
            (",a,", ",", &["", "a", ""]),
            ("abc", ",", &["abc"]),
            ("", ",", &[""]),
            ("", "", &[]),
            ("ae\u{301}", "", &["a", "e\u{301}"]),
            ("aaa", "aa", &["", "a"]),
            ("a::b::", "::", &["a", "b", ""]),
            (
                "home/gleam/desktop/",
                "/",
                &["home", "gleam", "desktop", ""],
            ),
        ];
        for &(s, sep, expected) in cases {
            assert_eq!(split(s, sep), expected, "{s:?} {sep:?}");
        }
        assert_eq!(split_once("a=b=c", "="), Some(("a", "b=c")));
        assert_eq!(split_once("abc", "="), None);
        assert_eq!(split_once("abc", ""), Some(("", "abc")));
        assert_eq!(split_once("=", "="), Some(("", "")));
    }

    #[test]
    fn replacing() {
        assert_eq!(replace("www.example.com", ".", "-"), "www-example-com");
        assert_eq!(replace("aaa", "aa", "b"), "ba");
        assert_eq!(replace("abc", "", "x"), "abc");
        assert_eq!(replace("abc", "abc", ""), "");
        assert_eq!(replace("", "a", "b"), "");
        assert_eq!(replace("aXbXc", "X", "YY"), "aYYbYYc");
    }

    #[test]
    fn slicing() {
        let cases: &[(&str, i64, i64, &str)] = &[
            ("gleam", 1, 2, "le"),
            ("gleam", 1, 10, "leam"),
            ("gleam", 10, 3, ""),
            ("gleam", -2, 2, "am"),
            ("gleam", -12, 2, ""),
            ("gleam", 2, -3, ""),
            ("gleam", 0, 0, ""),
            ("gleam", -5, 1, "g"),
            ("gleam", -6, 1, ""),
            ("gleam", 5, 1, ""),
            ("gleam", 4, 1, "m"),
            ("gleam", 1, i64::MAX, "leam"),
            ("gleam", 1 << 40, 1, ""),
            ("gleam", i64::MIN, 1, ""),
            ("👶🏿", 0, 1, "👶🏿"),
            ("ae\u{301}o", 1, 1, "e\u{301}"),
            ("ae\u{301}o", -1, 1, "o"),
            ("", 0, 1, ""),
        ];
        for &(s, start, len, expected) in cases {
            assert_eq!(slice(s, start, len), expected, "{s:?} {start} {len}");
        }
    }

    #[test]
    fn dropping() {
        assert_eq!(drop_start("The Lone Gunmen", 2), "e Lone Gunmen");
        assert_eq!(drop_start("abc", 0), "abc");
        assert_eq!(drop_start("abc", -1), "abc");
        assert_eq!(drop_start("abc", 3), "");
        assert_eq!(drop_start("abc", 4), "");
        assert_eq!(drop_start("abc", 1 << 40), "");
        assert_eq!(drop_start("abc", i64::MAX), "");
        assert_eq!(drop_start("e\u{301}x", 1), "x");
        assert_eq!(drop_end("Cigarette Smoking Man", 2), "Cigarette Smoking M");
        assert_eq!(drop_end("abc", 0), "abc");
        assert_eq!(drop_end("abc", -1), "abc");
        assert_eq!(drop_end("abc", 3), "");
        assert_eq!(drop_end("abc", 4), "");
        assert_eq!(drop_end("abc", (1 << 32) + 1), "");
        assert_eq!(drop_end("abc", i64::MAX), "");
        assert_eq!(drop_end("xe\u{301}", 1), "x");
    }

    #[test]
    fn first_last_pop() {
        assert_eq!(first(""), None);
        assert_eq!(first("e\u{301}x"), Some("e\u{301}"));
        assert_eq!(last(""), None);
        assert_eq!(last("xe\u{301}"), Some("e\u{301}"));
        assert_eq!(last("a"), Some("a"));
        assert_eq!(pop_grapheme(""), None);
        assert_eq!(pop_grapheme("gleam"), Some(("g", "leam")));
        assert_eq!(pop_grapheme("\r\nx"), Some(("\r\n", "x")));
    }

    #[test]
    fn codepoints() {
        assert_eq!(to_utf_codepoints("aé🎉"), [0x61, 0xE9, 0x1F389]);
        assert_eq!(to_utf_codepoints(""), [] as [u32; 0]);
        assert_eq!(from_utf_codepoints([0x61, 0xE9, 0x1F389]), "aé🎉");
        assert_eq!(from_utf_codepoints([]), "");
    }

    #[test]
    #[should_panic(expected = "invalid code point")]
    fn invalid_codepoint() {
        from_utf_codepoints([0xD800]);
    }

    #[test]
    fn inspecting() {
        let cases: &[(&str, &str)] = &[
            ("", r#""""#),
            ("abc", r#""abc""#),
            ("a\"b", r#""a\"b""#),
            ("a\\b", r#""a\\b""#),
            ("\n\r\t\u{C}", r#""\n\r\t\f""#),
            ("\u{0}", r#""\u{0000}""#),
            ("\u{1B}[0m", r#""\u{001B}[0m""#),
            ("\u{1F}", r#""\u{001F}""#),
            ("\u{7F}", r#""\u{007F}""#),
            ("\u{9F}", r#""\u{009F}""#),
            ("\u{A0}", "\"\u{A0}\""),
            ("\u{200B}\u{2028}", "\"\u{200B}\u{2028}\""),
            ("~é🎉", "\"~é🎉\""),
            ("\u{B}", r#""\u{000B}""#),
        ];
        for &(s, expected) in cases {
            assert_eq!(inspect(s), expected, "{s:?}");
        }
    }

    #[test]
    fn grapheme_prefix() {
        let cases: &[(&[u8], bool, i32)] = &[
            (b"", true, 0),
            (b"", false, -1),
            (b"a", true, 1),
            (b"a", false, -1),
            (b"ab", false, 1),
            (b"ab", true, 1),
            ("e\u{301}".as_bytes(), false, -1),
            ("e\u{301}".as_bytes(), true, 3),
            ("e\u{301}x".as_bytes(), false, 3),
            (b"\r", false, -1),
            (b"\r\n", false, -1),
            (b"\r\nx", false, 2),
            (b"\rx", false, 1),
            ("🇧🇷🇺".as_bytes(), false, 8),
            ("🇧".as_bytes(), false, -1),
            // a window cutting a code point
            (&"é".as_bytes()[..1], false, -1),
            (&"aé".as_bytes()[..2], false, -1),
            (&"e\u{301}".as_bytes()[..2], false, -1),
            (&"ab\u{301}".as_bytes()[..3], false, 1),
            ("👨‍👩‍👧x".as_bytes(), false, 18),
            ("👨‍👩‍👧".as_bytes(), true, 18),
            (&"👨‍👩‍👧".as_bytes()[..10], false, -1),
        ];
        for &(w, complete, expected) in cases {
            assert_eq!(
                grapheme_prefix_len(w, complete),
                expected,
                "{w:?} {complete}"
            );
        }
    }
}
