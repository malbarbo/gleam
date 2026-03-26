/// Devolve a soma 1 + 2 + ... + n.
pub fn soma_nat(n: Int) -> Int {
  case n {
    _ if n <= 0 -> 0
    _ -> n + soma_nat(n - 1)
  }
}

pub fn soma_nat_examples() {
  eq(soma_nat(-1), 0)
  eq(soma_nat(0), 0)
  eq(soma_nat(1), 1)
  eq(soma_nat(3), 6)
  eq(soma_nat(4), 10)
}

fn eq(a, b) {
  assert a == b
  0
}

pub fn main() {
  soma_nat_examples()
}
