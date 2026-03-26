/// Devolve uma função que recebe um parâmetro *x* e faz a soma de *n* e *x*.
pub fn somador(n: Int) -> fn(Int) -> Int {
  fn(x) { n + x }
}

pub fn somador_examples() {
  eq(somador(4)(3), 7)
  eq(somador(-2)(8), 6)
}

fn eq(a, b) {
  assert a == b
  0
}

pub fn main() {
  somador_examples()
}
