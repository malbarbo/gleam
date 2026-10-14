// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 The Gleam contributors

//! Runtime of the WebAssembly backend. See `ABI.md` for the contract with the
//! code generator. The functions doing the actual work live in `num`, `text`
//! and `codec` and are plain Rust, tested natively; the exports below only
//! move values in and out of linear memory.

#![cfg_attr(all(target_arch = "wasm32", not(test)), no_std)]

extern crate alloc;

pub mod codec;
pub mod num;
pub mod text;

#[cfg(target_arch = "wasm32")]
mod abi;
#[cfg(target_arch = "wasm32")]
mod arena;
#[cfg(target_arch = "wasm32")]
mod io;

#[cfg(target_arch = "wasm32")]
mod exports {
    use crate::abi::{Out, bytes_arg, list_arg, str_arg};
    use crate::codec;
    use crate::io::{self, STDERR, STDOUT};
    use crate::num::{self, FLOAT_BUF_LEN, Xoshiro256};
    use crate::text;
    use core::ptr;

    #[cfg(not(test))]
    #[global_allocator]
    static ARENA: crate::arena::Arena = crate::arena::Arena;

    /// Only messages without arguments are printed (the location otherwise):
    /// formatting would bring `core::fmt` into every program that can panic.
    #[cfg(not(test))]
    #[panic_handler]
    fn panic(info: &core::panic::PanicInfo) -> ! {
        let out = |s: &str| io::write_all(STDERR, s.as_bytes());
        out("panic: ");
        match (info.message().as_str(), info.location()) {
            (Some(message), _) => out(message),
            (None, Some(location)) => {
                out(location.file());
                out(":");
                out(itoa::Buffer::new().format(location.line()));
            }
            (None, None) => out("unknown"),
        }
        out("\n");
        core::arch::wasm32::unreachable()
    }

    //////////////
    // configuration and memory

    // Zero initialized (no data segment); 64 bits until `_config` says otherwise.
    static mut INT32: bool = false;
    static mut FLOAT32: bool = false;

    fn int_bits() -> u32 {
        if unsafe { INT32 } { 32 } else { 64 }
    }

    fn f32mode() -> bool {
        unsafe { FLOAT32 }
    }

    #[unsafe(no_mangle)]
    pub extern "C" fn _config(int_bits: i32, float_bits: i32) {
        unsafe {
            INT32 = int_bits == 32;
            FLOAT32 = float_bits == 32;
        }
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn cabi_realloc(
        old_ptr: i32,
        old_size: i32,
        align: i32,
        new_size: i32,
    ) -> i32 {
        let ptr = unsafe {
            crate::arena::realloc(
                old_ptr as usize as *mut u8,
                old_size as usize,
                align as usize,
                new_size as usize,
            )
        };
        if ptr.is_null() {
            core::arch::wasm32::unreachable()
        }
        ptr as i32
    }

    #[unsafe(no_mangle)]
    pub extern "C" fn _arena_mark() -> i32 {
        crate::arena::top() as i32
    }

    #[unsafe(no_mangle)]
    pub extern "C" fn _arena_release(mark: i32) {
        crate::arena::release(mark as usize)
    }

    /// Scratch memory for the code generator: the top of the arena rounded up
    /// to 16. (Rounding also keeps LTO from merging it with `_arena_mark`; the
    /// code generator finds functions by name.)
    #[unsafe(no_mangle)]
    pub extern "C" fn _heap_base() -> u32 {
        (crate::arena::top() as u32 + 15) & !15
    }

    //////////////
    // older exports used directly by the code generator

    #[unsafe(no_mangle)]
    pub extern "C" fn _exit(code: i32) -> ! {
        unsafe { wasi::wasi_snapshot_preview1::proc_exit(code) }
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _print(fd: i32, ptr: *const u8, len: u32) -> i32 {
        let bytes = unsafe { bytes_arg(ptr as i32, len as i32) };
        io::write(fd, bytes).0
    }

    unsafe fn write_to(ptr: *mut u8, s: &str) -> u32 {
        unsafe { ptr::copy_nonoverlapping(s.as_ptr(), ptr, s.len()) };
        s.len() as u32
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _i32_to_str(n: i32, ptr: *mut u8) -> u32 {
        unsafe { write_to(ptr, itoa::Buffer::new().format(n)) }
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _i64_to_str(n: i64, ptr: *mut u8) -> u32 {
        unsafe { write_to(ptr, itoa::Buffer::new().format(n)) }
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _f32_to_str(f: f32, ptr: *mut u8) -> u32 {
        let mut buf = [0; FLOAT_BUF_LEN];
        unsafe { write_to(ptr, num::format_float(f as f64, true, &mut buf)) }
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _f64_to_str(f: f64, ptr: *mut u8) -> u32 {
        let mut buf = [0; FLOAT_BUF_LEN];
        unsafe { write_to(ptr, num::format_float(f, false, &mut buf)) }
    }

    /// Writes `//utfcodepoint(c)`, as on JavaScript, at most 20 bytes.
    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _utf_codepoint_repr(codepoint: u32, ptr: i32) -> i32 {
        let c = char::from_u32(codepoint).unwrap_or(char::REPLACEMENT_CHARACTER);
        let mut buf = [0; 4];
        let ptr = ptr as usize as *mut u8;
        let prefix = unsafe { write_to(ptr, "//utfcodepoint(") };
        let c = unsafe { write_to(ptr.add(prefix as usize), c.encode_utf8(&mut buf)) };
        let suffix = unsafe { write_to(ptr.add((prefix + c) as usize), ")") };
        (prefix + c + suffix) as i32
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _float_repr(f: f64, ptr: i32) -> i32 {
        let mut buf = [0; FLOAT_BUF_LEN];
        let s = num::format_float(f, f32mode(), &mut buf);
        unsafe { write_to(ptr as usize as *mut u8, s) as i32 }
    }

    unsafe fn parse_with<T: Default>(
        parsed: *mut bool,
        ptr: *const u8,
        len: u32,
        parse: impl FnOnce(&[u8]) -> Option<T>,
    ) -> T {
        let r = parse(unsafe { bytes_arg(ptr as i32, len as i32) });
        unsafe { *parsed = r.is_some() };
        r.unwrap_or_default()
    }

    fn parse_int<T: core::str::FromStr>(bytes: &[u8]) -> Option<T> {
        core::str::from_utf8(bytes).ok()?.parse().ok()
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _i32_parse(parsed: *mut bool, ptr: *const u8, len: u32) -> i32 {
        unsafe { parse_with(parsed, ptr, len, parse_int) }
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _i64_parse(parsed: *mut bool, ptr: *const u8, len: u32) -> i64 {
        unsafe { parse_with(parsed, ptr, len, parse_int) }
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _f32_parse(parsed: *mut bool, ptr: *const u8, len: u32) -> f32 {
        unsafe { parse_with(parsed, ptr, len, |b| fast_float2::parse(b).ok()) }
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _f64_parse(parsed: *mut bool, ptr: *const u8, len: u32) -> f64 {
        unsafe { parse_with(parsed, ptr, len, |b| fast_float2::parse(b).ok()) }
    }

    #[unsafe(no_mangle)]
    pub extern "C" fn _i32_is_codepoint(ch: i32) -> bool {
        ch.try_into().ok().and_then(char::from_u32).is_some()
    }

    #[unsafe(no_mangle)]
    pub extern "C" fn _i64_is_codepoint(ch: i64) -> bool {
        ch.try_into().ok().and_then(char::from_u32).is_some()
    }

    //////////////
    // raw exports used by compiler builtins

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn _grapheme_prefix_len(ptr: i32, len: i32, complete: i32) -> i32 {
        text::grapheme_prefix_len(unsafe { bytes_arg(ptr, len) }, complete != 0)
    }

    //////////////
    // glue exports: arguments flattened, non scalar results in `ret`

    fn str_out(out: Out, s: &str) {
        out.str(s)
    }

    fn pair_out(out: Out, (a, b): (&str, &str)) {
        out.str(a);
        out.at(8).str(b);
    }

    fn str_list_out(out: Out, items: alloc::vec::Vec<&str>) {
        out.list(items.into_iter(), 8, str_out)
    }

    /// A `String` result, borrowed from the arguments or owned.
    trait StrResult {
        fn write(self, out: Out);
    }

    impl StrResult for &str {
        fn write(self, out: Out) {
            out.str(self)
        }
    }

    impl StrResult for alloc::string::String {
        fn write(self, out: Out) {
            out.string(self)
        }
    }

    macro_rules! str_fn {
        ($name:ident, $f:expr) => {
            #[unsafe(no_mangle)]
            pub unsafe extern "C" fn $name(ptr: i32, len: i32, ret: i32) {
                let s = unsafe { str_arg(ptr, len) };
                $f(s).write(Out::new(ret))
            }
        };
    }

    // int

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn int_parse(ptr: i32, len: i32, ret: i32) {
        let s = unsafe { str_arg(ptr, len) };
        Out::new(ret).result(num::int_parse(s, int_bits()), Out::i64)
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn int_base_parse(ptr: i32, len: i32, base: i64, ret: i32) {
        let s = unsafe { str_arg(ptr, len) };
        Out::new(ret).result(num::int_base_parse(s, base, int_bits()), Out::i64)
    }

    #[unsafe(no_mangle)]
    pub extern "C" fn int_to_string(n: i64, ret: i32) {
        Out::new(ret).string(num::int_to_string(n))
    }

    #[unsafe(no_mangle)]
    pub extern "C" fn int_to_base_string(n: i64, base: i64, ret: i32) {
        Out::new(ret).string(num::int_to_base_string(n, base))
    }

    // float

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn float_parse(ptr: i32, len: i32, ret: i32) {
        let s = unsafe { str_arg(ptr, len) };
        Out::new(ret).result(num::float_parse(s, f32mode()), Out::f64)
    }

    #[unsafe(no_mangle)]
    pub extern "C" fn float_to_string(f: f64, ret: i32) {
        Out::new(ret).string(num::float_to_string(f, f32mode()))
    }

    #[unsafe(no_mangle)]
    pub extern "C" fn float_power(base: f64, exponent: f64) -> f64 {
        num::power(base, exponent)
    }

    #[unsafe(no_mangle)]
    pub extern "C" fn float_logarithm(x: f64) -> f64 {
        num::logarithm(x)
    }

    #[unsafe(no_mangle)]
    pub extern "C" fn float_exponential(x: f64) -> f64 {
        num::exponential(x)
    }

    static mut RNG: Option<Xoshiro256> = None;

    #[unsafe(no_mangle)]
    pub extern "C" fn float_random() -> f64 {
        let rng = &raw mut RNG;
        unsafe { (*rng).get_or_insert_with(|| Xoshiro256::new(io::random_seed())) }.next_f64()
    }

    // string

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_length(ptr: i32, len: i32) -> i64 {
        text::length(unsafe { str_arg(ptr, len) })
    }

    str_fn!(string_lowercase, text::lowercase);
    str_fn!(string_uppercase, text::uppercase);
    str_fn!(string_trim, text::trim);
    str_fn!(string_trim_start, text::trim_start);
    str_fn!(string_trim_end, text::trim_end);
    str_fn!(string_reverse, text::reverse);
    str_fn!(string_inspect, text::inspect);
    str_fn!(uri_percent_encode, codec::percent_encode);

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_contains(p1: i32, l1: i32, p2: i32, l2: i32) -> i32 {
        let (a, b) = unsafe { (str_arg(p1, l1), str_arg(p2, l2)) };
        text::contains(a, b) as i32
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_crop(p1: i32, l1: i32, p2: i32, l2: i32, ret: i32) {
        let (a, b) = unsafe { (str_arg(p1, l1), str_arg(p2, l2)) };
        Out::new(ret).str(text::crop(a, b))
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_split(p1: i32, l1: i32, p2: i32, l2: i32, ret: i32) {
        let (a, b) = unsafe { (str_arg(p1, l1), str_arg(p2, l2)) };
        str_list_out(Out::new(ret), text::split(a, b))
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_split_once(p1: i32, l1: i32, p2: i32, l2: i32, ret: i32) {
        let (a, b) = unsafe { (str_arg(p1, l1), str_arg(p2, l2)) };
        Out::new(ret).result(text::split_once(a, b), pair_out)
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_replace(
        p1: i32,
        l1: i32,
        p2: i32,
        l2: i32,
        p3: i32,
        l3: i32,
        ret: i32,
    ) {
        let (a, b, c) = unsafe { (str_arg(p1, l1), str_arg(p2, l2), str_arg(p3, l3)) };
        Out::new(ret).string(text::replace(a, b, c))
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_to_graphemes(ptr: i32, len: i32, ret: i32) {
        let s = unsafe { str_arg(ptr, len) };
        str_list_out(Out::new(ret), text::to_graphemes(s))
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_slice(ptr: i32, len: i32, start: i64, length: i64, ret: i32) {
        let s = unsafe { str_arg(ptr, len) };
        Out::new(ret).str(text::slice(s, start, length))
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_drop_start(ptr: i32, len: i32, n: i64, ret: i32) {
        let s = unsafe { str_arg(ptr, len) };
        Out::new(ret).str(text::drop_start(s, n))
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_drop_end(ptr: i32, len: i32, n: i64, ret: i32) {
        let s = unsafe { str_arg(ptr, len) };
        Out::new(ret).str(text::drop_end(s, n))
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_first(ptr: i32, len: i32, ret: i32) {
        let s = unsafe { str_arg(ptr, len) };
        Out::new(ret).result(text::first(s), str_out)
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_last(ptr: i32, len: i32, ret: i32) {
        let s = unsafe { str_arg(ptr, len) };
        Out::new(ret).result(text::last(s), str_out)
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_pop_grapheme(ptr: i32, len: i32, ret: i32) {
        let s = unsafe { str_arg(ptr, len) };
        Out::new(ret).result(text::pop_grapheme(s), pair_out)
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_to_utf_codepoints(ptr: i32, len: i32, ret: i32) {
        let s = unsafe { str_arg(ptr, len) };
        let codepoints = text::to_utf_codepoints(s);
        Out::new(ret).list(codepoints.into_iter(), 8, Out::u32)
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn string_from_utf_codepoints(ptr: i32, count: i32, ret: i32) {
        let codepoints = unsafe { list_arg(ptr, count, 8) }.map(|p| unsafe { *(p as *const u32) });
        Out::new(ret).string(text::from_utf_codepoints(codepoints))
    }

    // uri

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn uri_percent_decode(ptr: i32, len: i32, ret: i32) {
        let s = unsafe { str_arg(ptr, len) };
        Out::new(ret).result(codec::percent_decode(s), Out::string)
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn uri_parse_query(ptr: i32, len: i32, ret: i32) {
        let s = unsafe { str_arg(ptr, len) };
        Out::new(ret).result(codec::parse_query(s), |out, pairs| {
            out.list(pairs.into_iter(), 16, |out, (k, v)| {
                out.string(k);
                out.at(8).string(v);
            })
        })
    }

    // io

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn io_print(ptr: i32, len: i32) {
        io::write_all(STDOUT, unsafe { bytes_arg(ptr, len) })
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn io_println(ptr: i32, len: i32) {
        io::write_all(STDOUT, unsafe { bytes_arg(ptr, len) });
        io::write_all(STDOUT, b"\n")
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn io_print_error(ptr: i32, len: i32) {
        io::write_all(STDERR, unsafe { bytes_arg(ptr, len) })
    }

    #[unsafe(no_mangle)]
    pub unsafe extern "C" fn io_println_error(ptr: i32, len: i32) {
        io::write_all(STDERR, unsafe { bytes_arg(ptr, len) });
        io::write_all(STDERR, b"\n")
    }
}
