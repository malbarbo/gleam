

/// O preço do litro do combustível, deve ser um número positivo.
type Preco =
  Float

/// O tipo do combustível, deve ser "Álcool" ou "Gasolina".
type Combustivel =
  String

/// Encontra o combustível que deve ser utilizado no abastecimento. Produz
/// "Álcool" se *preco_alcool* for menor ou igual a 70% do *preco_gasolina*,
/// produz "Gasolina" caso contrário.
pub fn seleciona_combustivel(
  preco_alcool: Preco,
  preco_gasolina: Preco,
) -> Combustivel {
  case preco_alcool <=. 0.7 *. preco_gasolina {
    True -> "Álcool"
    False -> "Gasolina"
  }
}

pub fn seleciona_combustivel_examples() {
  eq(seleciona_combustivel(3.0, 4.0), "Gasolina")
  eq(seleciona_combustivel(2.9, 4.2), "Álcool")
  eq(seleciona_combustivel(3.5, 5.0), "Álcool")
}
