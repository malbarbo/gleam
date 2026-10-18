// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

pub fn let_assert_ok_discard_test() {
  let result = {
    let x = ok_one()
    let assert Ok(_) = x
  }
  assert Ok(1) == result
}

pub fn let_assert_ok_bind_test() {
  let x = ok_one()
  let assert Ok(x) = x
  assert 1 == x
}

fn ok_one() -> Result(Int, a) {
  Ok(1)
}

fn assert_same(a: a, b: a) -> a {
  assert a == b as "not the same"
  a
}

pub fn generic_assert_with_message_test() {
  assert assert_same(1, 1) == 1
  assert assert_same("a", "a") == "a"
}
