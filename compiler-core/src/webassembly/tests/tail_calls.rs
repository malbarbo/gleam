// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use super::{compile_wasm, for_each_operator, run_ok};

/// Returns the number of `return_call` and `return_call_ref` in the module
/// compiled from `src`, without the closure adapters.
fn tail_calls(src: &str) -> (usize, usize) {
    let mut counts = (0, 0);
    for_each_operator(&compile_wasm(src, vec![]), |function, op| {
        if function.ends_with("@closure") {
            return;
        }
        counts.0 += usize::from(matches!(op, wasmparser::Operator::ReturnCall { .. }));
        counts.1 += usize::from(matches!(op, wasmparser::Operator::ReturnCallRef { .. }));
    });
    counts
}

#[test]
fn deep_self_recursion() {
    run_ok(
        r#"
fn count(n: Int, acc: Int) -> Int {
  case n {
    0 -> acc
    _ -> count(n - 1, acc + 1)
  }
}

pub fn main() {
  assert count(1_000_000, 0) == 1_000_000
}
"#,
    );
}

#[test]
fn deep_mutual_recursion() {
    run_ok(
        r#"
fn is_even(n: Int) -> Bool {
  case n {
    0 -> True
    _ -> is_odd(n - 1)
  }
}

fn is_odd(n: Int) -> Bool {
  case n {
    0 -> False
    _ -> is_even(n - 1)
  }
}

pub fn main() {
  assert is_even(1_000_000)
  assert is_odd(1_000_001)
}
"#,
    );
}

#[test]
fn deep_recursion_through_case_and_block() {
    run_ok(
        r#"
fn count(n: Int, acc: Int) -> Int {
  case n {
    n if n <= 0 -> acc
    _ -> {
      let m = n - 1
      case m % 2 {
        0 -> count(m, acc + 1)
        _ -> {
          let acc = acc + 1
          count(m, acc)
        }
      }
    }
  }
}

pub fn main() {
  assert count(1_000_000, 0) == 1_000_000
}
"#,
    );
}

#[test]
fn deep_recursion_after_let_assert() {
    run_ok(
        r#"
fn count(n: Int, acc: Int) -> Int {
  case n {
    0 -> acc
    _ -> {
      let assert Ok(m) = Ok(n - 1)
      count(m, acc + 1)
    }
  }
}

pub fn main() {
  assert count(1_000_000, 0) == 1_000_000
}
"#,
    );
}

#[test]
fn deep_recursion_through_pipeline() {
    run_ok(
        r#"
fn count(n: Int, acc: Int) -> Int {
  case n {
    0 -> acc
    _ -> { n - 1 } |> count(acc + 1)
  }
}

pub fn main() {
  assert count(1_000_000, 0) == 1_000_000
}
"#,
    );
}

#[test]
fn deep_recursion_through_lambda() {
    run_ok(
        r#"
fn bounce(f: fn(Int, Int) -> Int, n: Int, acc: Int) -> Int {
  case n {
    0 -> acc
    _ -> f(n - 1, acc + 1)
  }
}

fn count(n: Int, acc: Int) -> Int {
  bounce(fn(n, acc) { count(n, acc) }, n, acc)
}

pub fn main() {
  assert count(1_000_000, 0) == 1_000_000
}
"#,
    );
}

#[test]
fn deep_recursion_through_local_function() {
    run_ok(
        r#"
fn count(n: Int, acc: Int) -> Int {
  let next = fn(n, acc) { count(n, acc) }
  case n {
    0 -> acc
    _ -> next(n - 1, acc + 1)
  }
}

pub fn main() {
  assert count(1_000_000, 0) == 1_000_000
}
"#,
    );
}

#[test]
fn deep_recursion_through_field_call() {
    run_ok(
        r#"
type Ops {
  Ops(step: fn(Ops, Int, Int) -> Int)
}

fn step(ops: Ops, n: Int, acc: Int) -> Int {
  case n {
    0 -> acc
    _ -> ops.step(ops, n - 1, acc + 1)
  }
}

pub fn main() {
  assert step(Ops(step), 1_000_000, 0) == 1_000_000
}
"#,
    );
}

#[test]
fn deep_recursion_with_use() {
    run_ok(
        r#"
fn with(n: Int, acc: Int, k: fn(Int, Int) -> Int) -> Int {
  k(n, acc)
}

fn count(n: Int, acc: Int) -> Int {
  case n {
    0 -> acc
    _ -> {
      use n, acc <- with(n - 1, acc + 1)
      count(n, acc)
    }
  }
}

pub fn main() {
  assert count(1_000_000, 0) == 1_000_000
}
"#,
    );
}

#[test]
fn deep_recursion_through_and_or() {
    run_ok(
        r#"
fn all_pos(n: Int) -> Bool {
  n == 0 || n > 0 && all_pos(n - 1)
}

pub fn main() {
  assert all_pos(1_000_000)
}
"#,
    );
}

#[test]
fn deep_recursion_over_lists() {
    run_ok(
        r#"
fn build(n: Int, acc: List(Int)) -> List(Int) {
  case n {
    0 -> acc
    _ -> build(n - 1, [n, ..acc])
  }
}

fn length(xs: List(a), acc: Int) -> Int {
  case xs {
    [] -> acc
    [_, ..rest] -> length(rest, acc + 1)
  }
}

fn reverse(xs: List(a), acc: List(a)) -> List(a) {
  case xs {
    [] -> acc
    [x, ..rest] -> reverse(rest, [x, ..acc])
  }
}

pub fn main() {
  let xs = build(1_000_000, [])
  assert length(xs, 0) == 1_000_000
  let assert [1_000_000, ..] = reverse(xs, [])
}
"#,
    );
}

#[test]
fn deep_recursion_returning_nil_and_string() {
    run_ok(
        r#"
fn loop(n: Int) -> Nil {
  case n {
    0 -> Nil
    _ -> loop(n - 1)
  }
}

fn repeat(n: Int, s: String) -> String {
  case n {
    0 -> s
    _ -> repeat(n - 1, s)
  }
}

pub fn main() {
  loop(1_000_000)
  assert repeat(1_000_000, "a") == "a"
}
"#,
    );
}

#[test]
fn non_tail_calls_are_plain() {
    let src = r#"
fn count(n: Int, acc: Int) -> Int {
  case n {
    0 -> acc
    _ -> count(n - 1, acc + 1)
  }
}

fn argument(n: Int) -> Result(Int, Nil) {
  Ok(count(n, 0))
}

fn subject(n: Int) -> Int {
  case count(n, 0) {
    0 -> 0
    m -> m
  }
}

fn operand(n: Int) -> Int {
  count(n, 0) + 1
}

fn let_value(n: Int) -> Int {
  let m = count(n, 0)
  m + 1
}

fn let_value_last(n: Int) -> Int {
  let m = count(n, 0)
}

fn not_last(n: Int) -> Int {
  count(n, 0)
  n
}

fn echoed(n: Int) -> Int {
  echo count(n, 0)
}

fn constructor(n: Int) -> Result(Int, Nil) {
  Ok(n)
}

fn and_left(n: Int) -> Bool {
  is_zero(n) && True
}

fn or_left(n: Int) -> Bool {
  is_zero(n) || False
}

fn is_zero(n: Int) -> Bool {
  count(n, 0) == 0
}

fn negated(n: Int) -> Bool {
  !is_zero(n)
}

fn let_assert_value(n: Int) -> Int {
  let assert Ok(m) = argument(n)
  m
}

fn asserted(n: Int) -> Int {
  assert is_zero(n)
  n
}

fn panicked(n: Int) -> Int {
  panic as text(n)
}

fn text(n: Int) -> String {
  case count(n, 0) {
    0 -> "zero"
    _ -> "other"
  }
}

fn elements(n: Int) -> #(List(Int), #(Int, Int)) {
  #([count(n, 0)], #(count(n, 0), n))
}

type Box {
  Box(x: Int, y: Int)
}

fn box(n: Int) -> Box {
  Box(x: n, y: n)
}

fn updated(n: Int) -> Box {
  Box(..box(n), x: count(n, 0))
}

fn field(n: Int) -> Int {
  box(n).x
}

pub fn main() {
  let _ = and_left(1)
  let _ = or_left(1)
  let _ = negated(1)
  let _ = let_assert_value(1)
  let _ = asserted(0)
  let _ = elements(1)
  let _ = updated(1)
  let _ = field(1)
  let _ = text(1)
  let _ = panicked
  let _ = argument(1)
  let _ = subject(1)
  let _ = operand(1)
  let _ = let_value(1)
  let _ = let_value_last(1)
  let _ = not_last(1)
  let _ = echoed(1)
  let _ = constructor(1)
  Nil
}
"#;
    // Only the recursive call in `count` is a tail call.
    assert_eq!(tail_calls(src), (1, 0));
}

#[test]
fn tail_calls_are_return_calls() {
    let cases = [
        // self recursion
        (
            r#"
fn count(n: Int) -> Int {
  case n {
    0 -> 0
    _ -> count(n - 1)
  }
}

pub fn main() {
  let _ = count(1)
  Nil
}
"#,
            (1, 0),
        ),
        // block and let assert
        (
            r#"
fn count(n: Int) -> Int {
  case n {
    0 -> 0
    _ -> {
      let assert Ok(m) = Ok(n - 1)
      { count(m) }
    }
  }
}

pub fn main() {
  let _ = count(1)
  Nil
}
"#,
            (1, 0),
        ),
        // pipeline
        (
            r#"
fn count(n: Int) -> Int {
  case n {
    0 -> 0
    _ -> n |> dec |> count
  }
}

fn dec(n: Int) -> Int {
  n - 1
}

pub fn main() {
  let _ = count(1)
  Nil
}
"#,
            (1, 0),
        ),
        // `&&` and `||`
        (
            r#"
fn all_pos(n: Int) -> Bool {
  n == 0 || n > 0 && all_pos(n - 1)
}

pub fn main() {
  let _ = all_pos(1)
  Nil
}
"#,
            (1, 0),
        ),
        // call through a parameter, a field and a lambda
        (
            r#"
type Ops {
  Ops(step: fn(Int) -> Int)
}

fn apply(f: fn(Int) -> Int, n: Int) -> Int {
  f(n)
}

fn field(ops: Ops, n: Int) -> Int {
  ops.step(n)
}

fn inc(n: Int) -> Int {
  n + 1
}

pub fn main() {
  // A function read from a list is not known where `apply` is called
  let assert [f] = [fn(n) { inc(n) }]
  let _ = apply(f, 1)
  let _ = field(Ops(inc), 1)
  Nil
}
"#,
            (1, 2),
        ),
        // use, `with` is specialized for the callback and calls it
        // directly
        (
            r#"
fn with(n: Int, k: fn(Int) -> Int) -> Int {
  k(n)
}

fn inc(n: Int) -> Int {
  use n <- with(n)
  n + 1
}

pub fn main() {
  let _ = inc(1)
  Nil
}
"#,
            (2, 0),
        ),
        // the last call of main
        (
            r#"
fn id(n: Int) -> Int {
  n
}

pub fn main() {
  id(1)
}
"#,
            (1, 0),
        ),
    ];
    for (src, expected) in cases {
        assert_eq!(tail_calls(src), expected, "{src}");
    }
}
