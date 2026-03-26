pub fn main() {
  assert s(1, 2) == 3
  assert a(2, 3) == 5
}

const s = sum
const a = s

fn sum(a, b) {
  a + b
}
