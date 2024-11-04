use super::{
    encoder::WasmFunction,
    table::{self, SymbolTable},
};

pub fn emit_get_codepoint(sum: &table::Sum, table: &SymbolTable) -> WasmFunction {
    todo!()
    // emit_get_codepoint(string: array[i8], index: i32) -> i32, i32
    // Gets the unicode codepoint that starts at index in UTF-8 encoded string.
    // Returns the codepoint and the index of the next codepoint.
    // If the index is out of bounds, returns -1 and -1.

    // UTF-8 table:
    // First code point     Last code point 	Byte 1 	    Byte 2 	    Byte 3 	    Byte 4
    // U+0000 	            U+007F 	            0yyyzzzz
    // U+0080 	            U+07FF 	            110xxxyy 	10yyzzzz
    // U+0800 	            U+FFFF 	            1110wwww 	10xxxxyy 	10yyzzzz
    // U+010000 	        U+10FFFF 	        11110uvv 	10vvwwww 	10xxxxyy 	10yyzzzz
    // src: https://en.wikipedia.org/wiki/UTF-8

    /*
       -- 80 = 1000 0000
       -- C0 = 1100 0000
       -- E0 = 1110 0000
       -- F0 = 1111 0000
       -- F8 = 1111 1000

       -- 1F = 0001 1111
       -- 3F = 0011 1111

       first_byte = string[index]
       if (first_byte & 0x80) == 0 then
           -- first category
           return first_byte, index + 1
       else if (first_byte & 0xE0) == 0xC0 then
           -- second category
           second_byte = string[index + 1]
           if (second_byte & 0xC0) == 0x80 then
               return ((first_byte & 0x1F) << 6) | (second_byte & 0x3F), index + 2
           else
               return -1, -1
           end
       else if (first_byte & 0xF0) == 0xE0 then
           -- third category
           second_byte = string[index + 1]
           third_byte = string[index + 2]
           if (second_byte & 0xC0) == 0x80 and (third_byte & 0xC0) == 0x80 then
               return ((first_byte & 0x0F) << 12) | ((second_byte & 0x3F) << 6) | (third_byte & 0x3F), index + 3
           else
               return -1, -1
           end
       else if (first_byte & 0xF8) == 0xF0 then
           -- fourth category
           second_byte = string[index + 1]
           third_byte = string[index + 2]
           fourth_byte = string[index + 3]
           if (second_byte & 0xC0) == 0x80 and (third_byte & 0xC0) == 0x80 and (fourth_byte & 0xC0) == 0x80 then
               return ((first_byte & 0x07) << 18) | ((second_byte & 0x3F) << 12) | ((third_byte & 0x3F) << 6) | (fourth_byte & 0x3F), index + 4
           else
               return -1, -1
           end
       else
           return -1, -1
       end
    */
}
