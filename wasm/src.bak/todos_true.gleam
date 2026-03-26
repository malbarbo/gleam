

/// Produz True se todos os elementos de *lst* são True ou se *lst* é vazia,
/// produz False caso contrário.
pub fn todos_true(lst: List(Bool)) -> Bool {
  case lst {
    [] -> True
    [primeiro, ..resto] -> primeiro && todos_true(resto)
  }
}

pub fn todos_true_examples() {
  eq(todos_true([]), True)
  eq(todos_true([False]), False)
  eq(todos_true([True]), True)
  eq(todos_true([True, False]), False)
  eq(todos_true([True, True]), True)
  eq(todos_true([True, True, True]), True)
}
