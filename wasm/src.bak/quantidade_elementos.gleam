

/// Devolve a quantidade de elementos de *lst*.
pub fn quantidade_elementos(lst: List(Int)) -> Int {
  case lst {
    [] -> 0
    [_, ..resto] -> 1 + quantidade_elementos(resto)
  }
}

pub fn quantidade_elementos_examples() {
  eq(quantidade_elementos([]), 0)
  eq(quantidade_elementos([2]), 1)
  eq(quantidade_elementos([-3, 2]), 2)
  eq(quantidade_elementos([0, -3, 2]), 3)
}
