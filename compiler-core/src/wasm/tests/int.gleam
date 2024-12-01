pub fn main() {
  add()
  sub()
  div()
  mul()
  rem()
  lt()
  le()
  eq()
  ne()
  gt()
  ge()
}

fn add() {
  assert_eq(0 + 21, 21)
  assert_eq(8 + 0, 8)

  assert_eq(8 + 21, 29)
  assert_eq(8 + -21, -13)
  assert_eq(-8 + 21, 13)
  assert_eq(-8 + -21, -29)
}

fn sub() {
  assert_eq(0 - 21, -21)
  assert_eq(8 - 0, 8)

  assert_eq(8 - 21, -13)
  assert_eq(8 - -21, 29)
  assert_eq(-8 - 21, -29)
  assert_eq(-8 - -21, 13)
}

fn mul() {
  assert_eq(0 * 21, 0)
  assert_eq(8 * 0, 0)
  assert_eq(1 * 21, 21)
  assert_eq(8 * 1, 8)

  assert_eq(8 * 21, 168)
  assert_eq(8 * -21, -168)
  assert_eq(-8 * 21, -168)
  assert_eq(-8 * -21, 168)
}

fn div() {
  assert_eq(0 / 21, 0)
  assert_eq(8 / 0, 0)
  assert_eq(1 / 21, 0)
  assert_eq(8 / 1, 8)

  assert_eq(21 / 8, 2)
  assert_eq(21 / -8, -2)
  assert_eq(-21 / 8, -2)
  assert_eq(-21 / -8, 2)
}

fn rem() {
  assert_eq(0 % 21, 0)
  assert_eq(8 % 0, 0)
  assert_eq(1 % 21, 1)
  assert_eq(8 % 1, 0)

  assert_eq(21 % 8, 5)
  assert_eq(21 % -8, 5)
  assert_eq(-21 % 8, -5)
  assert_eq(-21 % -8, -5)
}

fn lt() {
  assert_false(13 < 4)
  assert_false(13 < -4)
  assert_true(-13 < 4)
  assert_true(-13 < -4)
}

fn le() {
  assert_false(25 <= 24)
  assert_true(25 <= 25)
  assert_true(25 <= 26)
}

fn eq() {
  assert_true(67 == 67)
  assert_false(67 == 68)
}

fn ne() {
  assert_false(67 != 67)
  assert_true(67 != 68)
}

fn gt() {
  assert_true(13 > 4)
  assert_true(13 > -4)
  assert_false(-13 > 4)
  assert_false(-13 > -4)
}

fn ge() {
  assert_true(25 >= 24)
  assert_true(25 >= 25)
  assert_false(25 >= 26)
}

fn assert_eq(a: Int, b: Int) {
  a == b || panic
}

fn assert_true(a: Bool) {
  a || panic
}

fn assert_false(a: Bool) {
  !a || panic
}
