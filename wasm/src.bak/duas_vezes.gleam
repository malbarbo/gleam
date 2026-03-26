

/// Devolve uma função que aplica *f* ao argumento e *f* a esse resultado.
pub fn duas_vezes(f: fn(a) -> a) -> fn(a) -> a {
  fn(x) { f(f(x)) }
}

pub fn duas_vezes_examples() {
  eq(duas_vezes(fn(n) { n + 1 })(3), 5)
  eq(duas_vezes(fn(n) { n * 2 })(5), 20)
  eq(duas_vezes(duas_vezes(duas_vezes))(fn(n) { n + 1 })(5), 21)
}
