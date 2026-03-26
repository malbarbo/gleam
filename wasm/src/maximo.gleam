fn maximo(a, b) {
  case a > b {
    True -> a
    False -> b
  }
}

fn maximo_examples() {
  eq(maximo(1, 2), 2)
}

fn eq(a, b) {
  assert a == b
  0
}

pub fn main() {
  maximo_examples()
}
