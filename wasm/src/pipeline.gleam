pub fn mul(a, b) {
  a * b
}

pub fn pipeline_examples() {
  eq(10 |> mul(3) |> div(2) |> div(45, _) |> fn(x) { x + 1 } |> add2(), 6)
}

fn div(a, b) {
  a / b
}

fn add2() {
  fn(x) { x + 2 }
}

fn eq(a, b) {
  assert a == b
  0
}

pub fn main() {
  pipeline_examples()
}
