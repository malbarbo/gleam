/// Devolve True se *lsta* é prefixo de *lstb*, isto é, os elementos de *lsta*
/// aparecem no início de *lstb*. Devolve False, caso contrário.
pub fn prefixo(lsta: List(a), lstb: List(a)) -> Bool {
  case lsta, lstb {
    [], _ -> True
    _, [] -> False
    [a, ..restoa], [b, ..restob] -> a == b && prefixo(restoa, restob)
  }
}

pub fn prefixo_examples() {
  // [], []
  eq(prefixo([], []), True)
  // [], [_, ..]
  eq(prefixo([], [3, 4]), True)
  // [_, ..], []
  eq(prefixo([3, 4], []), False)
  // [_, ..], [_, ..]
  eq(prefixo([3, 4], [3, 4]), True)
  eq(prefixo([3, 4], [3, 4, 6, 8]), True)
  eq(prefixo([3, 4], [3, 5]), False)
  eq(prefixo([3, 4, 5], [3, 4]), False)
}

fn eq(a, b) {
  assert a == b
  0
}

pub fn main() {
  prefixo_examples()
}
