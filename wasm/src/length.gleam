/// Devolve a quantidade de elementos de *lst*.
pub fn quantidade_elementos(lst: List(Int)) -> Int {
  case lst {
    [] -> 0
    [_, ..resto] -> 1 + quantidade_elementos(resto)
  }
}

pub fn quantidade_elementos_examples() {
  eq(quantidade_elementos([2]), 1)
}

fn eq(a, b) {
  assert a == b
  0
}

pub fn main() {
  quantidade_elementos_examples()
}
