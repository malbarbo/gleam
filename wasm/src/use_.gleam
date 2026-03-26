pub fn use_examples() {
  eq(
    {
      use #(a, b) <- try_tuple(#(10, 20) )
      a + b
    },
    30,
  )
  eq(
    {
      use a <- try_non_zero(10)
      a + 2
    },
    12,
  )
  eq(
    {
      use a <- try_non_zero(0)
      a + 2
    },
    1,
  )
  eq(
    {
      use a, b <- try_non_eq(10, 20)
      a <> b
    },
    "ab",
  )
}

fn try_non_zero(a: Int, fun: fn(Int) -> Int) {
  case a {
    0 -> 1
    _ -> fun(a)
  }
}

fn try_non_eq(a, b, fun: fn(String, String) -> String) -> String {
  case a == b {
    True -> ""
    False -> fun("a", "b")
  }
}

fn try_tuple(a: #(Int, Int), fun: fn(#(Int, Int)) -> Int) {
  case a.0 == a.1 {
    True -> 0
    False -> fun(a)
  }
}

fn eq(a, b) {
  assert a == b
  0
}

pub fn main() {
  use_examples()
}
