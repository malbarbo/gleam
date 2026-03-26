pub type Maioria {
  Positivo
  Negativo
  Nenhum
}

/// Determina se em *lst* existem mais números negativos ou positivos.
pub fn maioria(lst: List(Int)) -> Maioria {
  let npositivos = num_positivos(lst)
  let nnegativos = num_negativos(lst)
  case npositivos == nnegativos {
    True -> Nenhum
    False ->
      case npositivos > nnegativos {
        True -> Positivo
        False -> Negativo
      }
  }
}

/// Conta a quantidade de elementos positivos em *lst*.
fn num_positivos(lst: List(Int)) -> Int {
  case lst {
    [] -> 0
    [primeiro, ..resto] ->
      case primeiro > 0 {
        True -> 1 + num_positivos(resto)
        False -> num_positivos(resto)
      }
  }
}

/// Conta a quantidade de elementos negativos em *lst*.
fn num_negativos(lst: List(Int)) -> Int {
  case lst {
    [] -> 0
    [primeiro, ..resto] ->
      case primeiro < 0 {
        True -> 1 + num_negativos(resto)
        False -> num_negativos(resto)
      }
  }
}

pub fn maioria_examples() {
  eq(maioria([]), Nenhum)
  eq(maioria([0]), Nenhum)
  eq(maioria([10]), Positivo)
  eq(maioria([-2]), Negativo)
  eq(maioria([2, 10]), Positivo)
  eq(maioria([-1, 10]), Nenhum)
  eq(maioria([-1, -2]), Negativo)
  eq(maioria([10, -2]), Nenhum)
}

fn eq(a, b) {
  assert a == b
  0
}

pub fn main() {
  maioria_examples()
}
