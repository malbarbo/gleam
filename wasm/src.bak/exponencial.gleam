

/// Calcula o valor de *a* elevado a *n*. Se *a* e *n* forem zeros ou se *n*
/// for negativo, devolve Error(Nil).
pub fn exponencial(a: Float, n: Int) -> Result(Float, Nil) {
  case a == 0.0 && n == 0 || n < 0 {
    True -> Error(Nil)
    False -> Ok(exponencial_(a, n))
  }
}

// a deve ser diferente de 0 e n >= 0.
pub fn exponencial_(a: Float, n: Int) -> Float {
  case n {
    0 -> 1.0
    _ -> a *. exponencial_(a, n - 1)
  }
}

pub fn exponencial_examples() {
  eq(exponencial(0.0, -2), Error(Nil))
  eq(exponencial(0.0, 0), Error(Nil))
  eq(exponencial(0.0, 2), Ok(0.0))
  eq(exponencial(3.0, -2), Error(Nil))
  eq(exponencial(3.0, 0), Ok(1.0))
  eq(exponencial(3.0, 2), Ok(9.0))
}
