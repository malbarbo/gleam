/// Conta a quantidade de elementos de *lst*.
pub fn tamanho(lst: List(a)) -> Int {
  case lst {
    [] -> 0
    [_, ..resto] -> 1 + tamanho(resto)
  }
}

pub fn tamanho_examples() {
  eq(tamanho([]), 0)
  eq(tamanho([4]), 1)
  eq(tamanho([7, 1]), 2)
}

/// Conta a quantidade de elementos de *lst*.
pub fn tamanho2(lst: List(a)) -> Int {
  tamanho_loop(lst, 0)
}

pub fn tamanho_loop(lst: List(a), acc: Int) -> Int {
  // acc é a quantidade de elementos já processados
  case lst {
    [] -> acc
    [_, ..resto] -> tamanho_loop(resto, acc + 1)
  }
}

pub fn tamanho2_examples() {
  eq(tamanho2([]), 0)
  eq(tamanho2([4]), 1)
  eq(tamanho2([7, 1]), 2)
}

fn eq(a, b) {
  assert a == b
  0
}

pub fn main() {
  tamanho_examples()
  tamanho2_examples()
}
