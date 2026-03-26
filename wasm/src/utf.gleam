@external(webassembly, "builtins", "_int_to_utf_codepoint")
fn utf_codepoint(a: Int) -> Result(UtfCodepoint, Nil)

//@external(webassembly, "builtins", "_utf_codepoints")
//fn utf_codepoints(s: String) -> List(UtfCodepoint)

pub fn main() {
  //case utf_codepoint(87), utf_codepoint(115) {
  //  Ok(a), Ok(s) -> {
  //    assert [a, s] == utf_codepoints("as")
  //    Nil
  //  }
  //   _, _ -> panic
  //}

  echo utf_codepoint(79)
  assert utf_codepoint(80) == utf_codepoint(80)
  let assert Ok(_) = utf_codepoint(87)
  let assert Error(_) = utf_codepoint(0x110000)
}
