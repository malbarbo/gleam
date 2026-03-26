

/// Devolve o valor máximo entre *a*, *b* e *c*.
pub fn maximo3(a: Int, b: Int, c: Int) -> Int {
  case a >= b {
    True ->
      case a >= c {
        True -> a
        False -> c
      }
    False ->
      case b >= c {
        True -> b
        False -> c
      }
  }
}

pub fn maximo3_examples() {
  eq(maximo3(8, 5, 2), 8)
  eq(maximo3(4, 6, 1), 6)
  eq(maximo3(6, 6, 7), 7)
}
