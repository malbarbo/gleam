

/// Produz True se uma pessoa com *idade* é supercentenária, isto é, tem 110
/// anos ou mais, produz False caso contrário.
pub fn supercentenario(idade: Int) -> Bool {
  idade >= 110
}

pub fn supercentenario_examples() {
  eq(supercentenario(101), False)
  eq(supercentenario(110), True)
  eq(supercentenario(112), True)
}
