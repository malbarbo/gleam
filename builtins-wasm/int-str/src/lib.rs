#![no_std]
#![no_main]
use itoa;

#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    loop {}
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
