

/// Produz True se *n* é um número positivo de três dígitos.
pub fn tres_digitos(n: Int) -> Bool {
  100 <= n && n <= 999
}

pub fn tres_digitos_examples() {
  eq(tres_digitos(99), False)
  eq(tres_digitos(100), True)
  eq(tres_digitos(999), True)
  eq(tres_digitos(1000), False)
}
