

/// Soma *x* a cada elemento de *lst*.
pub fn soma_x(lst: List(Int), x: Int) -> List(Int) {
  case lst {
    [] -> []
    [primeiro, ..resto] -> [primeiro + x, ..soma_x(resto, x)]
  }
}

pub fn soma_x_examples() {
  eq(soma_x([], 4), [])
  eq(soma_x([4, 2], 5), [9, 7])
  eq(soma_x([3, -1, 4], -2), [1, -3, 2])
}
