/// Produz True se *n* é um número primo, isto é, tem exatamente dois divisores
/// distintos (1 e *n*). Produz False caso contrário.
pub fn primo(n: Int) -> Bool {
  num_divisors(n, n) == 2
}

fn num_divisors(n: Int, a: Int) -> Int {
  case a {
    _ if a <= 0 -> 0
    _ if n % a == 0 -> 1 + num_divisors(n, a - 1)
    _ -> num_divisors(n, a - 1)
  }
}

pub fn primo_examples() {
  eq(primo(1), False)
  eq(primo(2), True)
  eq(primo(3), True)
  eq(primo(4), False)
  eq(primo(5), True)
  eq(primo(6), False)
  eq(primo(7), True)
  eq(primo(8), False)
}

fn eq(a, b) {
  assert a == b
  0
}

pub fn main() {
  primo_examples()
}
