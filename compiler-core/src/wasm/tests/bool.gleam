fn main() {
  assert_true(True)
  assert_false(False)

  or()
  and()
  not()
}

fn or() {
  assert_false(False || False)
  assert_true(False || True)
  assert_true(True || False)
  assert_true(True || True)
}

fn and() {
  assert_false(False && False)
  assert_false(False && True)
  assert_false(True && False)
  assert_true(True && True)
}

fn not() {
  assert_true(!False)
  assert_false(!True)
}

fn assert_true(a: Bool) {
  a || panic
}

fn assert_false(a: Bool) {
  !a || panic
}
