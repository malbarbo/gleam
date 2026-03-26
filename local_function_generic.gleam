fn create() -> fn(a) -> a {
  fn(a: a) { a }
}

pub fn main() {
  let f = fn() -> fn(a) -> a { fn(a: a) { a } }()
  echo f(10)
  echo f("10")

  let h = fn(a: a) { a }
  echo h(10)
  echo h("10")

  let g = create()
  echo g(10)
//  echo g("10")
}

