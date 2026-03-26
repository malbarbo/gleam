

/// Devolve True se *a* é par, False caso contrário.
pub fn par(a: Int) -> Bool {
  case a {
    _ if a < 0 -> impar(a + 1)
    _ if a == 0 -> True
    _ -> impar(a - 1)
  }
}

/// Devolve True se *a* é ímpar, False caso contrário.
pub fn impar(a: Int) -> Bool {
  case a {
    _ if a < 0 -> par(a + 1)
    _ if a == 0 -> False
    _ -> par(a - 1)
  }
}

pub fn par_examples() {
  eq(par(-3), False)
  eq(par(-2), True)
  eq(par(-1), False)
  eq(par(0), True)
  eq(par(1), False)
  eq(par(2), True)
  eq(par(3), False)
}

pub fn impar_examples() {
  eq(impar(-3), True)
  eq(impar(-2), False)
  eq(impar(-1), True)
  eq(impar(0), False)
  eq(impar(1), True)
  eq(impar(2), False)
  eq(impar(3), True)
}
