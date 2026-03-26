/// Produz True se *lst* é palíndromo, isto é, tem os mesmos elementos quando
/// lida da direita para a esquerda e da esquerda para a direita. Produz False
/// caso contrário.
pub fn palindromo(lst: List(Int)) -> Bool {
  case lst {
    [] | [_] -> True
    [primeiro, ..] ->
      Ok(primeiro) == list_last(lst) && palindromo(sem_extremos(lst))
  }
}

pub fn list_last(lst: List(a)) -> Result(a, Nil) {
  case lst {
    [] -> Error(Nil)
    [last] -> Ok(last)
    [_, ..rest] -> list_last(rest)
  }
}

pub fn palindromo_examples() {
  eq(palindromo([]), True)
  eq(palindromo([2]), True)
  eq(palindromo([1, 2]), False)
  eq(palindromo([3, 3]), True)
  eq(palindromo([3, 7, 3]), True)
  eq(palindromo([3, 7, 3, 3]), False)
}

/// Remove o primeiro e o último de *lst*. Se *lst* é vazia ou só tem um
/// elemento, devolve vazia.
fn sem_extremos(lst: List(Int)) -> List(Int) {
  case lst {
    [] -> []
    [_, ..resto] -> sem_ultimo(resto)
  }
}

/// Remove o último elementod e *lst*. Se *lst* é vazia, devolve vazia.
fn sem_ultimo(lst: List(Int)) -> List(Int) {
  case lst {
    [] | [_] -> []
    [primeiro, ..resto] -> [primeiro, ..sem_ultimo(resto)]
  }
}

/// Produz True se *lst* é palíndromo, isto é, tem os mesmos elementos quando
/// lida da direita para a esquerda e da esquerda para a direita.  Produz False
/// caso contrário.
pub fn palindromo2(lst: List(Int)) -> Bool {
  lst == reverse(lst, [])
}

pub fn reverse(lst: List(a), r: List(a)) -> List(a) {
  case lst {
    [] -> r
    [first, ..rest] -> reverse(rest, [first, ..r])
  }
}

pub fn palindromo2_examples() {
  eq(palindromo2([]), True)
  eq(palindromo2([2]), True)
  eq(palindromo2([1, 2]), False)
  eq(palindromo2([3, 3]), True)
  eq(palindromo2([3, 7, 3]), True)
  eq(palindromo2([3, 7, 3, 3]), False)
}

pub fn main() {
  palindromo_examples()
  palindromo2_examples()
}

fn eq(a, b) {
  assert a == b
  0
}
