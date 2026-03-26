#![no_std]
#![no_main]
use core::slice;

#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    loop {}
}

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

unsafe fn bytes_from_ptr_len<'a>(ptr: *const u8, len: usize) -> &'a [u8] {
    unsafe { slice::from_raw_parts(ptr, len) }
}
