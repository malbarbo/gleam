#![no_std]
#![no_main]
use core;
use itoa;
use ryu;
use wasi;
use wasi::wasi_snapshot_preview1 as wasip1;

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
        let mut buffer = ryu::Buffer::new();
        let s = buffer.format(f);
        core::ptr::copy_nonoverlapping(s.as_ptr(), ptr, s.len());
        s.len() as u32
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn _f64_to_str(f: f64, ptr: *mut u8) -> u32 {
    unsafe {
        let mut buffer = ryu::Buffer::new();
        let s = buffer.format(f);
        core::ptr::copy_nonoverlapping(s.as_ptr(), ptr, s.len());
        s.len() as u32
    }
}
