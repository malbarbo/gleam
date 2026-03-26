pub type Par(a, b) {
  Par(primeiro: a, segundo: b)
}

pub fn main() {
  assert case Par(1, 2) {
    p if p.primeiro == p.segundo -> False
    _ -> True
  }
}
