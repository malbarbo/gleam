/// Produz True se os elementos de *lst* estão em ordem não decrescente,
/// produz False caso contrário.
pub fn ordenado(lst: List(Int)) -> Bool {
  case lst {
    [] | [_] -> True
    [primeiro, segundo, ..resto] ->
      primeiro <= segundo && ordenado([segundo, ..resto])
  }
}

pub fn ordenado_examples() {
  eq(ordenado([]), True)
  eq(ordenado([4]), True)
  eq(ordenado([3, 4]), True)
  eq(ordenado([5, 4]), False)
  eq(ordenado([3, 3, 4]), True)
  eq(ordenado([3, 5, 4]), False)
}

fn eq(a, b) {
  assert a == b
  0
}

pub fn main() {
  ordenado_examples()
}
