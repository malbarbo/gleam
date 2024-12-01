pub fn main() {
  add()
  sub()
  div()
  mul()
  lt()
  le()
  eq()
  ne()
  gt()
  ge()
}

fn add() {
  assert_eq(0.0 +. 21.0, 21.0)
  assert_eq(8.0 +. 0.0, 8.0)

  assert_eq(8.0 +. 21.0, 29.0)
  assert_eq(8.0 +. -21.0, -13.0)
  assert_eq(-8.0 +. 21.0, 13.0)
  assert_eq(-8.0 +. -21.0, -29.0)
}

fn sub() {
  assert_eq(0.0 -. 21.0, -21.0)
  assert_eq(8.0 -. 0.0, 8.0)

  assert_eq(8.0 -. 21.0, -13.0)
  assert_eq(8.0 -. -21.0, 29.0)
  assert_eq(-8.0 -. 21.0, -29.0)
  assert_eq(-8.0 -. -21.0, 13.0)
}

fn mul() {
  assert_eq(0.0 *. 21.0, 0.0)
  assert_eq(8.0 *. 0.0, 0.0)
  assert_eq(1.0 *. 21.0, 21.0)
  assert_eq(8.0 *. 1.0, 8.0)

  assert_eq(8.0 *. 21.0, 168.0)
  assert_eq(8.0 *. -21.0, -168.0)
  assert_eq(-8.0 *. 21.0, -168.0)
  assert_eq(-8.0 *. -21.0, 168.0)
}

fn div() {
  assert_eq(0.0 /. 21.0, 0.0)
  assert_eq(8.0 /. 0.0, 0.0)
  assert_eq(10.0 /. 20.0, 0.5)
  assert_eq(8.0 /. 1.0, 8.0)

  assert_eq(21.0 /. 8.0, 2.625)
  assert_eq(21.0 /. -8.0, -2.625)
  assert_eq(-21.0 /. 8.0, -2.625)
  assert_eq(-21.0 /. -8.0, 2.625)
}

fn lt() {
  assert_false(13.0 <. 4.0)
  assert_false(13.0 <. -4.0)
  assert_true(-13.0 <. 4.0)
  assert_true(-13.0 <. -4.0)
}

fn le() {
  assert_false(25.0 <=. 24.0)
  assert_true(25.0 <=. 25.0)
  assert_true(25.0 <=. 26.0)
}

fn eq() {
  assert_true(67.0 == 67.0)
  assert_false(67.0 == 68.0)
}

fn ne() {
  assert_false(67.0 != 67.0)
  assert_true(67.0 != 68.0)
}

fn gt() {
  assert_true(13.0 >. 4.0)
  assert_true(13.0 >. -4.0)
  assert_false(-13.0 >. 4.0)
  assert_false(-13.0 >. -4.0)
}

fn ge() {
  assert_true(25.0 >=. 24.0)
  assert_true(25.0 >=. 25.0)
  assert_false(25.0 >=. 26.0)
}

fn assert_eq(a: Float, b: Float) {
  a == b || panic
}

fn assert_true(a: Bool) {
  a || panic
}

fn assert_false(a: Bool) {
  !a || panic
}
