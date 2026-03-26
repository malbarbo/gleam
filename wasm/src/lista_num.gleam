/// Cria uma lista com os valores 1, 2, ..., n-1, n.
pub fn lista_num(n: Int) -> List(Int) {
  case n {
    _ if n <= 0 -> []
    _ -> adiciona_fim(lista_num(n - 1), n)
  }
}

pub fn lista_num_examples() {
  eq(lista_num(0), [])
  eq(lista_num(1), [1])
  eq(lista_num(2), [1, 2])
  eq(lista_num(3), [1, 2, 3])
}

/// Adiciona *n* ao final de *lst*.
pub fn adiciona_fim(lst: List(Int), n: Int) -> List(Int) {
  case lst {
    [] -> [n]
    [primeiro, ..resto] -> [primeiro, ..adiciona_fim(resto, n)]
  }
}

pub fn adiciona_fim_examples() {
  eq(adiciona_fim([], 3), [3])
  eq(adiciona_fim([3], 4), [3, 4])
  eq(adiciona_fim([3, 4], 1), [3, 4, 1])
}

pub fn main() {
  lista_num_examples()
  adiciona_fim_examples()
}

fn eq(a, b) {
  assert a == b
  0
}
