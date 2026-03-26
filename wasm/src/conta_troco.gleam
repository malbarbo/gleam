/// Calcula a quantidade de formas diferentes de obter *valor* somando valores
/// em *moedas*.
pub fn conta_troco(valor: Int, moedas: List(Int)) -> Int {
  case valor, moedas {
    _, [] -> 0
    0, _ -> 1
    _, [primeira, ..resto] if valor < primeira -> conta_troco(valor, resto)
    _, [primeira, ..resto] -> {
      conta_troco(valor - primeira, moedas) + conta_troco(valor, resto)
    }
  }
}

pub fn conta_troco_examples() {
  eq(conta_troco(1, []), 0)
  eq(conta_troco(0, [1, 2]), 1)
  eq(conta_troco(3, [4]), 0)
  eq(conta_troco(4, [1, 2]), 3)
  eq(conta_troco(6, [2, 3]), 2)
  eq(conta_troco(10, [2, 3, 4]), 5)
}

fn eq(a, b) {
  assert a == b
  0
}

pub fn main() {
  conta_troco_examples()
}
