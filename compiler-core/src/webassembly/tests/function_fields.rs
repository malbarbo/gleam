// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

// Functions in records, tuples and lists

const STREAM: &str = r#"
pub type Stream(a) {
  Empty
  Cons(head: a, tail: fn() -> Stream(a))
}

fn from(n: Int) -> Stream(Int) {
  Cons(n, fn() { from(n + 1) })
}

fn map(s: Stream(a), f: fn(a) -> b) -> Stream(b) {
  case s {
    Empty -> Empty
    Cons(h, t) -> Cons(f(h), fn() { map(t(), f) })
  }
}

fn filter(s: Stream(a), p: fn(a) -> Bool) -> Stream(a) {
  case s {
    Empty -> Empty
    Cons(h, t) ->
      case p(h) {
        True -> Cons(h, fn() { filter(t(), p) })
        False -> filter(t(), p)
      }
  }
}

fn take(s: Stream(a), n: Int) -> List(a) {
  case n, s {
    0, _ -> []
    _, Empty -> []
    _, Cons(h, t) -> [h, ..take(t(), n - 1)]
  }
}

fn map_list(l: List(a), f: fn(a) -> b) -> List(b) {
  case l {
    [] -> []
    [x, ..xs] -> [f(x), ..map_list(xs, f)]
  }
}
"#;

#[track_caller]
fn run(src: &str) {
    super::run_ok(&format!("{STREAM}\n{src}"));
}

#[test]
fn streams() {
    run(r#"
pub fn main() {
  assert take(from(1), 3) == [1, 2, 3]
  assert take(from(1), 0) == []
  assert take(Empty, 2) == []
}
"#);
}

#[test]
fn stream_operations() {
    run(r#"
pub fn main() {
  let k = 3
  let even = fn(x) { x % 2 == 0 }
  let s = from(1) |> filter(even) |> map(fn(x) { x * k })
  assert take(s, 3) == [6, 12, 18]
}
"#);
}

#[test]
fn streams_of_two_types() {
    run(r#"
pub fn main() {
  assert take(map(from(1), fn(_) { "a" }), 2) == ["a", "a"]
  assert take(map(from(1), fn(x) { x > 1 }), 2) == [False, True]
}
"#);
}

#[test]
fn streams_of_functions() {
    run(r#"
pub fn main() {
  let k = 3
  let s = Cons(fn(x) { x + k }, fn() { Cons(fn(x) { x * k }, fn() { Empty }) })
  assert take(s, 2) |> map_list(fn(f) { f(2) }) == [5, 6]
}
"#);
}

#[test]
fn long_streams() {
    run(r#"
fn last(s: Stream(a), n: Int, default: a) -> a {
  case n, s {
    0, _ -> default
    _, Empty -> default
    _, Cons(h, t) -> last(t(), n - 1, h)
  }
}

pub fn main() {
  assert last(from(1), 100_000, 0) == 100_000
}
"#);
}

#[test]
fn records_with_functions() {
    run(r#"
pub type Operations {
  Operations(f: fn(Int) -> Int, g: fn(Int) -> Int)
}

pub fn main() {
  let k = 10
  let operations = Operations(fn(x) { x + k }, fn(x) { x * k })
  assert operations.f(1) == 11
  assert operations.g(2) == 20
  let Operations(f, g) = operations
  assert f(g(1)) == 20
  let updated = Operations(..operations, g: fn(x) { x - k })
  assert updated.g(updated.f(1)) == 1
}
"#);
}

#[test]
fn generic_records_with_functions() {
    run(r#"
pub type Pair(a, b) {
  Pair(value: a, function: fn(a) -> b)
}

fn apply(p: Pair(a, b)) -> b {
  p.function(p.value)
}

pub fn main() {
  let k = 1
  assert apply(Pair(1, fn(x) { x + k })) == 2
  assert apply(Pair("a", fn(x) { x == "a" })) == True
}
"#);
}

#[test]
fn records_with_functions_of_themselves() {
    run(r#"
pub type Node {
  Node(value: Int, next: fn(Node) -> Int)
}

pub fn main() {
  let k = 10
  let n = Node(3, fn(n) { n.value + k })
  assert n.next(n) == 13
}
"#);
}

#[test]
fn tuples_and_lists_with_functions() {
    run(r#"
pub fn main() {
  let k = 10
  let t = #(fn(x) { x + k }, 1)
  assert t.0(t.1) == 11
  let l = [fn(x) { x + k }, fn(x) { x - k }]
  assert map_list(l, fn(f) { f(5) }) == [15, -5]
}
"#);
}

#[test]
fn mutually_recursive_types_with_functions() {
    run(r#"
pub type A {
  A(f: fn(B) -> Int)
}

pub type B {
  B(g: fn(A) -> Int, n: Int)
}

pub fn main() {
  let a = A(fn(b) { b.n })
  let b = B(fn(a) { a.f(B(fn(_) { 0 }, 7)) }, 5)
  assert a.f(b) == 5
  assert b.g(a) == 7
  assert a == a
  assert a != A(fn(b) { b.n })
}
"#);
}

#[test]
fn echo_of_records_with_functions() {
    run(r#"
pub type Operations {
  Operations(f: fn(Int) -> Int)
}

pub fn main() {
  let k = 1
  let operations = echo Operations(fn(x) { x + k })
  let _ = echo Cons(1, fn() { Empty })
  assert operations.f(1) == 2
}
"#);
}
