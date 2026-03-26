type A(a) {
  A(a, B(a))
}

type B(a) {
  None
  B(a, A(a))
}

pub fn main() {
  let b = B(1, A(2, None))
  let b = None
  let a = A(1, b)
  let a = A(2, B(3, a))
  echo a
  assert a != A(1, b)
  0
}
