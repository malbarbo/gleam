/// Devolve uma função que é semelhante a *pred*, mas que devolve a negação do
/// resultado de *pred*.
pub fn nega(pred: fn(a) -> Bool) -> fn(a) -> Bool {
  fn(x: a) -> Bool { !pred(x) }
}

pub fn nega_examples() {
  eq(nega(fn(x: Int) -> Bool { x > 0 })(3), False)
  eq(nega(fn(x: Int) -> Bool { x > 0 })(-3), True)
  eq(nega(fn(x: Int) -> Bool { x % 2 == 0 })(4), False)
  eq(nega(fn(x: Int) -> Bool { x % 2 == 0 })(3), True)
}

fn eq(a, b) {
  assert a == b
  0
}

pub fn main() {
  nega_examples()
}
