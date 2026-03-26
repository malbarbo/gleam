#![no_std]
#![no_main]

#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[unsafe(no_mangle)]
pub extern "C" fn _i32_is_codepoint(ch: i32) -> bool {
    ch.try_into().ok().and_then(char::from_u32).is_some()
}

#[unsafe(no_mangle)]
pub extern "C" fn _i64_is_codepoint(ch: i64) -> bool {
    ch.try_into().ok().and_then(char::from_u32).is_some()
}
