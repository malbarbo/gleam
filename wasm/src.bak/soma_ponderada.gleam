

/// Calcula a soma ponderada dos valores de *lst* considerando que cada
/// elemento de *lst* tem como peso o elemento correspondente em *pesos*.
/// Devolve Error(Nil) se *lst* e *pesos* têm quantidades diferentes de
/// elementos.
pub fn soma_ponderada(
  lst: List(Float),
  pesos: List(Float),
) -> Result(Float, Nil) {
  case lst, pesos {
    [], [] -> Ok(0.0)
    [primeiro, ..resto], [peso, ..pesos] ->
      case soma_ponderada(resto, pesos) {
        Ok(s) -> Ok(s +. primeiro *. peso)
        _ -> Error(Nil)
      }
    _, _ -> Error(Nil)
  }
}

pub fn soma_ponderada_examples() {
  eq(soma_ponderada([], []), Ok(0.0))
  eq(soma_ponderada([], [1.0]), Error(Nil))
  eq(soma_ponderada([1.0], []), Error(Nil))
  eq(soma_ponderada([4.0], [2.0]), Ok(8.0))
  eq(soma_ponderada([3.0, 4.0], [5.0, 2.0]), Ok(23.0))
  eq(soma_ponderada([5.0, 3.0, 4.0], [1.0, 5.0, 2.0]), Ok(28.0))
}
