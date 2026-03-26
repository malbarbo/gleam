pub type Result(ok, err) {
  Ok(ok)
  Error(err)
}
/// Devolve o valor máximo de *lst* ou Error(Nil) se *lst* é vazia.
pub fn maximo(lst: List(Int)) -> Result(Int, Nil) {
  case lst {
    [] -> Error(Nil)
    [primeiro, ..resto] ->
      case maximo(resto) {
        Error(Nil) -> Ok(primeiro)
        Ok(maximo_resto) -> Ok(max(primeiro, maximo_resto))
      }
  }
}

pub fn maximo_examples() {
  eq(maximo([]), Error(Nil))
  eq(maximo([2]), Ok(2))
  eq(maximo([2, 1]), Ok(2))
  eq(maximo([2, 1, 5]), Ok(5))
}

fn max(a, b) {
  case a > b {
    True -> a
    False -> b
  }
}

fn eq(a, b) {
  assert a == b
}

pub fn main() {
  maximo_examples()
}
