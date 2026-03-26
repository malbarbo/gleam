

/// Devolve True se *n* está em *lst*, False caso contrário.
pub fn contem(lst: List(Int), n: Int) -> Bool {
  case lst {
    [] -> False
    [primeiro, ..resto] -> primeiro == n || contem(resto, n)
  }
}

pub fn contem_examples() {
  eq(contem([], 3), False)
  eq(contem([3], 3), True)
  eq(contem([5], 8), False)
  eq(contem([3, -4, 5, 9, 10], 5), True)
  eq(contem([2, -5, -2], 5), False)
}

/// Devolve True se 3 está em *lst*, False caso contrário.
pub fn contem_3(lst: List(Int)) -> Bool {
  contem(lst, 3)
}

pub fn contem_3_examples() {
  eq(contem_3([]), False)
  eq(contem_3([3]), True)
  eq(contem_3([5]), False)
  eq(contem_3([3, -4, 5, 9, 10]), True)
  eq(contem_3([2, -5, -2]), False)
}

/// Devolve True se 5 está em *lst*, False caso contrário.
pub fn contem_5(lst: List(Int)) -> Bool {
  contem(lst, 5)
}

pub fn contem_5_examples() {
  eq(contem_5([]), False)
  eq(contem_5([3]), False)
  eq(contem_5([5]), True)
  eq(contem_5([3, -4, 5, 9, 10]), True)
  eq(contem_5([2, -5, -2]), False)
}
