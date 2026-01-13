#![no_std]
#![no_main]
use core::{self, slice};
use dtoa;
use itoa;
use no_panic::no_panic;
use wasi;
use wasi::wasi_snapshot_preview1 as wasip1;

//////////////
// system

unsafe extern "C" {
    static __heap_base: u32;
}

#[unsafe(no_mangle)]
pub extern "C" fn _heap_base() -> u32 {
    unsafe { __heap_base }
}

#[unsafe(no_mangle)]
pub extern "C" fn _exit(code: i32) -> ! {
    unsafe { wasip1::proc_exit(code) }
}

#[unsafe(no_mangle)]
pub extern "C" fn _print(fd: i32, ptr: *const u8, len: u32) -> i32 {
    let mut written = 0i32;
    let iovec = wasi::Ciovec {
        buf: ptr,
        buf_len: len as usize,
    };
    unsafe {
        wasip1::fd_write(
            fd,
            &iovec as *const _ as i32,
            1,
            &mut written as *mut _ as i32,
        )
    }
}

//////////////
// to string

#[unsafe(no_mangle)]
pub extern "C" fn _i32_to_str(n: i32, ptr: *mut u8) -> u32 {
    unsafe {
        let mut buffer = itoa::Buffer::new();
        let s = buffer.format(n);
        core::ptr::copy_nonoverlapping(s.as_ptr(), ptr, s.len());
        s.len() as u32
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn _i64_to_str(n: i64, ptr: *mut u8) -> u32 {
    unsafe {
        let mut buffer = itoa::Buffer::new();
        let s = buffer.format(n);
        core::ptr::copy_nonoverlapping(s.as_ptr(), ptr, s.len());
        s.len() as u32
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn _f32_to_str(f: f32, ptr: *mut u8) -> u32 {
    unsafe {
        let mut buffer = dtoa::Buffer::new();
        let s = buffer.format(f);
        core::ptr::copy_nonoverlapping(s.as_ptr(), ptr, s.len());
        s.len() as u32
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn _f64_to_str(f: f64, ptr: *mut u8) -> u32 {
    unsafe {
        let mut buffer = dtoa::Buffer::new();
        let s = buffer.format(f);
        core::ptr::copy_nonoverlapping(s.as_ptr(), ptr, s.len());
        s.len() as u32
    }
}

//////////////
// parsing

#[unsafe(no_mangle)]
pub extern "C" fn _i32_parse(parsed: *mut bool, ptr: *const u8, len: u32) -> i32 {
    unsafe {
        if let Ok(r) = lexical_core::parse(bytes_from_ptr_len(ptr, len as usize)) {
            *parsed = true;
            r
        } else {
            *parsed = false;
            Default::default()
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn _i64_parse(parsed: *mut bool, ptr: *const u8, len: u32) -> i64 {
    unsafe {
        if let Ok(r) = lexical_core::parse(bytes_from_ptr_len(ptr, len as usize)) {
            *parsed = true;
            r
        } else {
            *parsed = false;
            Default::default()
        }
    }
}

#[unsafe(no_mangle)]
#[no_panic]
pub extern "C" fn _f32_parse(parsed: *mut bool, ptr: *const u8, len: u32) -> f32 {
    unsafe {
        if let Ok(r) = fast_float2::parse(bytes_from_ptr_len(ptr, len as usize)) {
            *parsed = true;
            r
        } else {
            *parsed = false;
            Default::default()
        }
    }
}

#[unsafe(no_mangle)]
#[no_panic]
pub extern "C" fn _f64_parse(parsed: *mut bool, ptr: *const u8, len: u32) -> f64 {
    unsafe {
        if let Ok(r) = fast_float2::parse(bytes_from_ptr_len(ptr, len as usize)) {
            *parsed = true;
            r
        } else {
            *parsed = false;
            Default::default()
        }
    }
}

unsafe fn bytes_from_ptr_len<'a>(ptr: *const u8, len: usize) -> &'a [u8] {
    unsafe { slice::from_raw_parts(ptr, len) }
}

// Strings
#[unsafe(no_mangle)]
pub extern "C" fn _i32_is_codepoint(ch: u32) -> bool {
    char::from_u32(ch).is_some()
}
