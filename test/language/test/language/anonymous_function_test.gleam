// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

// https://github.com/gleam-lang/gleam/issues/1637
pub fn anonymous_function_test() {
  let f = fn(x) {
    let x = x
    x
  }
  assert 1 == f(1)
}

pub fn immutable_scope_test() {
  let x = 1
  let f = fn() { x }
  let x = 2
  assert f() == 1
  assert x == 2
}

type FnBox {
  FnBox(f: fn(Int) -> Int)
}

pub fn call_record_access_test() {
  let b = FnBox(f: fn(x) { x })
  assert b.f(5) == 5
}

pub fn call_tuple_access_test() {
  let t = #(fn(x) { x })
  assert t.0(5) == 5
}

fn apply(f, x) {
  f(x)
}

fn add_one(x) {
  x + 1
}

fn with(x, f) {
  f(x)
}

pub fn function_from_block_ending_in_use_test() {
  assert apply(
      {
        use n <- with(10)
        fn(x) { x + n }
      },
      2,
    )
    == 12
}

fn apply_second(_, f) {
  f(1)
}

pub fn discarded_parameter_test() {
  assert apply_second(0, add_one) == 2
}

fn make_adder_in_block(n) {
  {
    let f = fn(x) { x + n }
    f
  }
}

fn make_adder_with_use(n) {
  use m <- with(n)
  fn(x) { x + m }
}

fn make_adder_with_assert(n) {
  let f = fn(x) { x + n }
  assert n > 0
  f
}

pub fn returned_function_test() {
  assert apply(make_adder_in_block(1), 2) == 3
  assert apply(make_adder_with_use(1), 2) == 3
  assert apply(make_adder_with_assert(1), 2) == 3
}
