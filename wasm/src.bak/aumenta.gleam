

/// Calcula um novo valor aplicando a *porcentagem* ao *valor*.
/// Requer que *valor* e *porcentagem* sejam não negativos.
pub fn aumenta(valor: Float, porcentagem: Float) -> Float {
  valor *. { 1.0 +. porcentagem /. 100.0 }
}

pub fn aumenta_examples() {
  eq(aumenta(100.0, 3.0), 103.0)
  eq(aumenta(20.0, 50.0), 30.0)
  eq(aumenta(10.0, 80.0), 18.0)
}
