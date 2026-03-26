

/// Representa os pontos cardeais de acordo com o esboço a seguir
///
///       Norte
///         |
/// Oeste -   - Leste
///         |
///        Sul
pub type Direcao {
  Norte
  Leste
  Sul
  Oeste
}

/// Produz a direção oposta de *d*.
pub fn direcao_oposta(d: Direcao) -> Direcao {
  case d {
    Norte -> Sul
    Sul -> Norte
    Leste -> Oeste
    Oeste -> Leste
  }
}

pub fn direcao_oposta_examples() {
  eq(direcao_oposta(Norte), Sul)
  eq(direcao_oposta(Sul), Norte)
  eq(direcao_oposta(Leste), Oeste)
  eq(direcao_oposta(Oeste), Leste)
}

/// Devolve a direção que está a 90 graus no sentido horário de *d*.
pub fn direcao_90_horario(d: Direcao) -> Direcao {
  case d {
    Norte -> Leste
    Leste -> Sul
    Sul -> Oeste
    Oeste -> Norte
  }
}

pub fn direcao_90_horario_examples() {
  eq(direcao_90_horario(Norte), Leste)
  eq(direcao_90_horario(Leste), Sul)
  eq(direcao_90_horario(Sul), Oeste)
  eq(direcao_90_horario(Oeste), Norte)
}

/// Devolve a direção que está a 90 graus no sentido anti-horário de *d*.
pub fn direcao_90_anti_horario(d: Direcao) -> Direcao {
  direcao_90_horario(direcao_90_horario(direcao_90_horario(d)))
}

pub fn direcao_90_anti_horario_examples() {
  eq(direcao_90_anti_horario(Norte), Oeste)
  eq(direcao_90_anti_horario(Leste), Norte)
  eq(direcao_90_anti_horario(Sul), Leste)
  eq(direcao_90_anti_horario(Oeste), Sul)
}
