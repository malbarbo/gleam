#![no_std]
#![no_main]
use dtoa;

#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    loop {}
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
