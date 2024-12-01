pub fn main() {
  eq()
  ne()
  concat()
  escape()
}

fn eq() {
  assert_true("a" == "a")
  assert_false("a" == "b")
}

fn ne() {
  assert_true("a" != "b")
  assert_false("a" != "a")
}

fn concat() {
  assert_true("one" <> " " <> "word" == "one word")
  assert_false("one" <> " " <> "word" == "on word")
}

fn escape() {
  assert_true("\"" == "\u{22}")
  assert_true("\\" == "\u{5c}")
  assert_true("\f" == "\u{c}")
  assert_true("\n" == "\u{a}")
  assert_true("\r" == "\u{d}")
  assert_true("\t" == "\u{9}")
}

fn assert_true(a: Bool) {
  a || panic
}

fn assert_false(a: Bool) {
  !a || panic
}
