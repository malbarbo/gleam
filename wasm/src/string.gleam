@external(webassembly, "builtins", "_string_get_byte")
fn get_byte(s: String, i: Int) -> Result(Int, Nil)

@external(webassembly, "builtins", "_string_num_bytes")
fn num_bytes(s: String) -> Int

@external(webassembly, "builtins", "_int_to_utf_codepoint")
fn utf_codepoint(a: Int) -> Result(UtfCodepoint, Nil)

pub fn main() {
  let s = "abc"
  let assert Ok(97) = echo get_byte(s, 0)
  let assert Ok(98) = echo get_byte(s, 1)
  let assert Ok(99) = echo get_byte(s, 2)
  let assert Error(_) = get_byte(s, 3)
  let assert Error(_) = get_byte(s, -1)
  assert num_bytes(s) == 3
}
