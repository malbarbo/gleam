pub type Option(a) {
  None
  Some(a)
}

pub const x = Some(10)
pub const y = Some("cas")

pub fn main() {
  let a = None
  let b = Some(10)

  assert a != b
  assert a == None
  assert b == Some(10)

  echo a
  echo b

  assert case a {
    None if Some(1) == None -> 0
    None -> 1
    Some(x) -> 2
  } == 1

  assert case b {
    None -> 0
    Some(x) -> x
  } == 10

  assert Some("a") != None
  echo Some(Some(#("a", 10)))
  echo x
  echo y
  0
}
