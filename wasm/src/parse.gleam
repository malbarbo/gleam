@external(webassembly, "builtins", "_parse_int")
fn parse_int(s: String) -> Result(Int, Nil)

@external(webassembly, "builtins", "_parse_float")
fn parse_float(s: String) -> Result(Float, Nil)

pub fn main() {
  assert parse_int("1234") == Ok(1234)
  assert parse_int("a123") == Error(Nil)

  assert parse_float("1234.567") == Ok(1234.567)
  assert parse_float("a123") == Error(Nil)
}
