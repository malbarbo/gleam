
unsafe fn str_from_ptr_len<'a>(ptr: *const u8, len: usize) -> &'a str {
    unsafe { str::from_utf8_unchecked(bytes_from_ptr_len(ptr, len)) }
}

// Strings
#[unsafe(no_mangle)]
pub extern "C" fn _i32_is_codepoint(ch: u32) -> bool {
    char::from_u32(ch).is_some()
}

// Strings
#[unsafe(no_mangle)]
#[no_panic]
pub extern "C" fn _str_num_graphemes(ptr: *const u8, len: usize) -> usize {
    UnicodeSegmentation::graphemes(unsafe { str_from_ptr_len(ptr, len) }, true).count()
}

#[unsafe(no_mangle)]
#[no_panic]
pub extern "C" fn _str_graphemes_indices(ptr: *mut u8, len: usize) -> usize {
    unsafe {
        let mut count = 0;
        let mut dest = ptr.add(len) as *mut usize;
        for (_, s) in UnicodeSegmentation::grapheme_indices(str_from_ptr_len(ptr, len), true) {
            count += 1;
            dest.write(s.len());
            dest = dest.add(1);
        }
        count
    }
}

#[unsafe(no_mangle)]
#[no_panic]
pub extern "C" fn _str_compare(ptr: *const u8, len1: usize, len2: usize) -> i32 {
    let s1 = unsafe { str_from_ptr_len(ptr, len1) };
    let s2 = unsafe { str_from_ptr_len(ptr.add(len1), len2) };
    s1.cmp(s2) as i32
}

#[unsafe(no_mangle)]
#[no_panic]
pub extern "C" fn _str_lowercase(ptr: *mut u8, len: usize) -> usize {
    _str_case(ptr, len, char::to_lowercase)
}

#[unsafe(no_mangle)]
#[no_panic]
pub extern "C" fn _str_uppercase(ptr: *mut u8, len: usize) -> usize {
    _str_case(ptr, len, char::to_uppercase)
}

fn _str_case<T>(ptr: *mut u8, len: usize, f: impl Fn(char) -> T) -> usize
where
    T: IntoIterator<Item = char>,
{
    unsafe {
        let s = str_from_ptr_len(ptr, len);
        let mut dest = ptr.add(len);
        let mut len = 0;
        for ch in s.chars().flat_map(|ch| f(ch)) {
            encode_utf8_raw_unchecked(ch, dest);
            len += ch.len_utf8();
            dest = dest.add(ch.len_utf8());
        }
        len
    }
}

// Take from rust core/src/char/methods.rs
#[inline(never)]
pub const unsafe fn encode_utf8_raw_unchecked(ch: char, dst: *mut u8) {
    let len = ch.len_utf8();
    let code = ch as u32;
    // SAFETY: The caller must guarantee that the buffer pointed to by `dst`
    // is at least `len` bytes long.
    unsafe {
        match len {
            1 => {
                *dst = code as u8;
            }
            2 => {
                *dst = (code >> 6 & 0x1F) as u8 | TAG_TWO_B;
                *dst.add(1) = (code & 0x3F) as u8 | TAG_CONT;
            }
            3 => {
                *dst = (code >> 12 & 0x0F) as u8 | TAG_THREE_B;
                *dst.add(1) = (code >> 6 & 0x3F) as u8 | TAG_CONT;
                *dst.add(2) = (code & 0x3F) as u8 | TAG_CONT;
            }
            4 => {
                *dst = (code >> 18 & 0x07) as u8 | TAG_FOUR_B;
                *dst.add(1) = (code >> 12 & 0x3F) as u8 | TAG_CONT;
                *dst.add(2) = (code >> 6 & 0x3F) as u8 | TAG_CONT;
                *dst.add(3) = (code & 0x3F) as u8 | TAG_CONT;
            }
            // SAFETY: `char` always takes between 1 and 4 bytes to encode in UTF-8.
            _ => core::hint::unreachable_unchecked(),
        }
    }
}

const TAG_CONT: u8 = 0b1000_0000;
const TAG_TWO_B: u8 = 0b1100_0000;
const TAG_THREE_B: u8 = 0b1110_0000;
const TAG_FOUR_B: u8 = 0b1111_0000;
