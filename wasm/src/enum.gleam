pub type Enum {
  A
  Bb
  Ccc
}

const x = Ccc
pub const b = x
pub const c = b

pub fn main() {
  assert x == Ccc
  assert b == Ccc
  assert c == Ccc
  echo A
  echo Bb
  echo Ccc
}
