// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

// Captures of local functions (`let f = fn ...`)

const MAP: &str = r#"
fn map(l: List(a), f: fn(a) -> b) -> List(b) {
  case l {
    [] -> []
    [x, ..xs] -> [f(x), ..map(xs, f)]
  }
}

fn id(x: a) -> a {
  x
}

fn add(a: Int, b: Int) -> Int {
  a + b
}
"#;

#[track_caller]
fn run(src: &str) {
    super::run_ok(&format!("{MAP}\n{src}"));
}

/// Asserts that the local variable `variable` is used with different types,
/// which the WebAssembly target does not support.
#[track_caller]
fn assert_used_with_different_types(src: &str, variable: &str) {
    let error = super::compile_wasm_error(&format!("{MAP}\n{src}"));
    assert!(
        matches!(
            error,
            crate::webassembly::Error::GenericLocalVariable { ref name, .. } if name == variable
        ),
        "{error:?}"
    );
}

#[test]
fn captures_unified_with_type_variables() {
    run(r#"
pub fn main() {
  let l = []
  let f = fn(x: a) { [x, ..l] }
  assert f("s") == ["s"]
}
"#);
}

#[test]
fn captured_functions_unified_with_type_variables() {
    run(r#"
pub fn main() {
  let k = id(fn(y) { y })
  let f = fn(x: a) { k(x) }
  assert f("s") == "s"
  assert map(["t"], f) == ["t"]
}
"#);
}

#[test]
fn captured_functions_unified_with_type_variables_in_local_functions() {
    run(r#"
pub fn main() {
  let n = 1
  let g = fn(z: Int) {
    let k = id(fn(y) { y })
    let f = fn(x: a) { #(k(x), z + n) }
    f("s")
  }
  assert g(2) == #("s", 3)
}
"#);
}

#[test]
fn captured_functions_unified_with_type_variables_in_generic_functions() {
    run(r#"
fn wrap(v: b) {
  let k = id(fn(y) { y })
  let f = fn(x: a) { #(k(x), v) }
  f(1.5)
}

pub fn main() {
  assert wrap(1) == #(1.5, 1)
  assert wrap("v") == #(1.5, "v")
}
"#);
}

/// Gleam does not generalise `let`, but a value whose type has a type
/// variable of an annotation is instantiated at each use, see
/// `Monomorphizer::conflicting_reference`.
#[test]
fn local_function_used_with_two_types() {
    assert_used_with_different_types(
        r#"
pub fn main() {
  let n = 10
  let f = fn(x: a) { #(x, n) }
  assert f(1) == #(1, 10)
  assert f("a") == #("a", 10)
}
"#,
        "f",
    );
}

#[test]
fn local_function_captured_used_with_two_types() {
    assert_used_with_different_types(
        r#"
pub fn main() {
  let k = id(fn(y) { y })
  let f = fn(x: a) { k(x) }
  assert f("s") == "s"
  assert f(1) == 1
}
"#,
        "f",
    );
}

#[test]
fn block_result_used_with_two_types() {
    assert_used_with_different_types(
        r#"
pub fn main() {
  let g = {
    let id = fn(x: a) -> a { x }
  }
  assert g(3) == 3
  assert g("s") == "s"
}
"#,
        "g",
    );
}

#[test]
fn call_result_used_with_two_types() {
    assert_used_with_different_types(
        r#"
pub fn main() {
  let g = id(fn(x: a) -> a { x })
  assert g(3) == 3
  assert g("s") == "s"
}
"#,
        "g",
    );
}

#[test]
fn pattern_variable_used_with_two_types() {
    assert_used_with_different_types(
        r#"
pub fn main() {
  let assert [f] = [fn(x: a) -> a { x }]
  assert f(3) == 3
  assert f("s") == "s"
}
"#,
        "f",
    );
}

#[test]
fn local_function_used_with_an_unresolved_type_and_another_type() {
    assert_used_with_different_types(
        r#"
pub fn main() {
  let id = fn(x: a) { x }
  let _ = id([])
  assert id(1) == 1
}
"#,
        "id",
    );
}

#[test]
fn captured_functions_unified_with_type_variables_of_inline_functions() {
    run(r#"
pub fn main() {
  let k = id(fn(y) { y })
  let f = fn(x: a) { x }
  let g = id(fn(x) { k(x) })
  assert f(g("s")) == "s"
}
"#);
}

#[test]
fn equality_in_guards() {
    run(r#"
pub fn main() {
  let n = 1
  let f = fn() { n }
  let r = case 1 {
    _ if f == f -> 1
    _ -> 2
  }
  assert r == 1
}
"#);
}

#[test]
fn equality_of_values_typed_in_specializations() {
    run(r#"
pub fn main() {
  let n = 3
  let f = fn(x: a) { #(x, n) }
  let g = fn(y: b) {
    let h: fn(b) -> #(b, Int) = f
    h
  }
  assert g(1) == g(2)
  let k = fn(z: c) { g(z) }
  assert k(1) == k(2)
  let l = fn(z: c) { g(z) == g(z) }
  assert l(1)
}
"#);
}

#[test]
fn equality_of_values_with_unbound_types() {
    run(r#"
pub fn main() {
  let n = 3
  let f = fn(x: a) { #(x, n) }
  let h = f
  assert h == f
  assert [f] == [f]
}
"#);
}

#[test]
fn equality_of_different_definitions_and_evaluations() {
    run(r#"
pub fn main() {
  let n = 3
  let f = fn(x) { x + n }
  let g = fn(x) { x + n }
  assert f != g
  let make = fn(m) {
    let f = fn(x) { x + m }
    #(f, f)
  }
  let #(a, b) = make(1)
  assert a == b
  assert a != make(1).0
  let last = fn(m) {
    let f = fn(x) { x + m }
    f
  }
  assert last(1) != last(1)
}
"#);
}

#[test]
fn equality_without_captures_in_guards() {
    run(r#"
pub fn main() {
  let f = fn() { 1 }
  let r = case 1 {
    _ if f == f -> 1
    _ -> 2
  }
  assert r == 1
}
"#);
}

#[test]
fn echo() {
    run(r#"
pub fn main() {
  let n = 1
  let f = fn() { n }
  echo f
  Nil
}
"#);
}

#[test]
fn captured_unions_unified_with_type_variables() {
    run(r#"
pub fn main() {
  let o = Error(Nil)
  let f = fn(x: a) { case o { Ok(y) -> y  Error(_) -> x } }
  assert f("s") == "s"
}
"#);
}

#[test]
fn polymorphic_in_polymorphic() {
    run(r#"
pub fn main() {
  let n = 1
  let f = fn(x: a) {
    let g = fn(y: b) { #(x, y, n) }
    #(g(1), g(2))
  }
  assert f(1.0) == #(#(1.0, 1, 1), #(1.0, 2, 1))
  assert map([2.0], f) == [#(#(2.0, 1, 1), #(2.0, 2, 1))]
}
"#);
}

#[test]
fn values_in_local_and_inline_functions() {
    run(r#"
pub fn main() {
  let m = 5
  let g = fn(x) { x + m }
  let f = fn(l) { map(l, g) }
  assert f([1]) == [6]
  assert map([[1]], fn(l) { map(l, g) }) == [[6]]
  assert map([1], fn(y) {
    let h = fn() { g(y) }
    h()
  }) == [6]
  assert map([1], fn(y) {
    let h = fn(z) { g(z) + y }
    map([y], h)
  }) == [[7]]
}
"#);
}

#[test]
fn local_functions_without_captures() {
    run(r#"
pub fn main() {
  let p = fn(x) { x * 2 }
  let n = 1
  let q = fn(x) { p(x) + n }
  assert map([1], q) == [3]
  assert map([1], fn(y) { p(y) + q(y) }) == [5]
}
"#);
}

#[test]
fn shadowed_local_functions() {
    run(r#"
pub fn main() {
  let n = 1
  let f = fn() { n }
  let n = 2
  let f = fn() { f() + n }
  assert f() == 3
  assert id(f)() == 3
}
"#);
}

#[test]
fn let_assert() {
    run(r#"
pub fn main() {
  let n = 1
  let assert f = fn() { n }
  assert f() == 1
}
"#);
}

#[test]
fn values_in_case_subjects_and_tuples() {
    run(r#"
pub fn main() {
  let n = 1
  let f = fn() { n }
  let g = case f {
    h -> h
  }
  assert g() == 1
  let r = #(f, f)
  assert { r.0 }() == 1
}
"#);
}

#[test]
fn used_by_returned_closures() {
    run(r#"
fn make(n: Int) -> fn(Int) -> Int {
  let f = fn(x) { x + n }
  fn(y) { f(f(y)) }
}

fn make2(n: Int) -> fn(Int) -> Int {
  let f = fn(x) { x + n }
  let g = fn(y) { f(f(y)) }
  g
}

pub fn main() {
  assert make(1)(1) == 3
  assert make2(2)(1) == 5
}
"#);
}

#[test]
fn tail_calls_from_local_functions() {
    run(r#"
fn count(n: Int, acc: Int) -> Int {
  let step = 1
  let next = fn(m, a) { count(m - step, a + step) }
  case n {
    0 -> acc
    _ -> next(n, acc)
  }
}

pub fn main() {
  assert count(1_000_000, 0) == 1_000_000
}
"#);
}

#[test]
fn string_prefix_alternatives() {
    run(r#"
fn go(s: String) -> String {
  case s {
    "a" <> rest | "b" <> rest -> {
      let f = fn() { rest }
      f()
    }
    _ -> ""
  }
}

pub fn main() {
  assert go("axy") == "xy"
  assert go("bz") == "z"
}
"#);
}

#[test]
fn pipelines_in_generic_functions() {
    run(r#"
fn go(x: a, l: List(b)) -> List(#(a, b)) {
  let f = fn(y) { #(x, y) }
  l |> map(f)
}

pub fn main() {
  assert go(1, ["a"]) == [#(1, "a")]
  assert go("s", [1.0]) == [#("s", 1.0)]
}
"#);
}

#[test]
fn monomorphic() {
    run(r#"
pub fn main() {
  let n = 10
  let f = fn(x) { x + n }
  assert f(1) == 11
}
"#);
}

#[test]
fn polymorphic() {
    run(r#"
pub fn main() {
  let n = 10
  let f = fn(x: a) { #(x, n) }
  assert f(1) == #(1, 10)
}
"#);
}

#[test]
fn shadowing() {
    run(r#"
pub fn main() {
  let n = 1
  let f = fn(x) { x + n }
  let n = 2
  assert f(0) + n == 3
  assert map([1, 2], fn(y) { let n = 3  f(y) + n }) == [5, 6]
  assert map([1, 2], fn(y) { let n = 3  y |> f }) == [2, 3]
}
"#);
}

#[test]
fn values() {
    run(r#"
pub fn main() {
  let n = 10
  let f = fn(x) { x + n }
  assert map([1, 2], f) == [11, 12]
  let g = fn(x: a) { #(x, n) }
  assert map([1, 2], g) == [#(1, 10), #(2, 10)]
}
"#);
}

#[test]
fn local_functions_calling_local_functions() {
    run(r#"
pub fn main() {
  let m = 100
  let g = fn(x) { x + m }
  let n = 10
  let f = fn(x) { g(x) + n }
  let m = 0
  assert f(1) == 111
  assert map([1], f) == [111]
  let h = fn(x) { f(x) + g(x) + m }
  assert h(1) == 212
  assert map([1], h) == [212]
  assert map([1], fn(x) { h(x) }) == [212]
}
"#);
}

#[test]
fn in_inline_functions() {
    run(r#"
pub fn main() {
  let n = 10
  let f = fn(x) { x + n }
  assert map([1], fn(y) { f(y) }) == [11]
  assert 1 |> f == 11
  let a = 5
  let k = fn(x, y) { x + y + n }
  assert 1 |> k(_, a) == 16
  assert map([1], fn(y) { map([y], f) }) == [[11]]
  assert fn(y) { map([y], f) }(2) == [12]
  assert 1 |> fn(y) { f(y) + add(y, a) } == 17
  assert map([1], k(_, a)) == [16]
}
"#);
}

#[test]
fn inline_functions_in_local_functions() {
    run(r#"
pub fn main() {
  let y = 1
  let g = fn(x) { fn() { #(x, y) } }
  assert g(2)() == #(2, 1)
}
"#);
}

#[test]
fn local_functions_in_local_functions() {
    run(r#"
pub fn main() {
  let a = 1
  let f = fn(x) {
    let b = 10
    let g = fn(y) { x + y + a + b }
    g(100) + g(1000)
  }
  assert f(5) == 1132
  assert map([5], f) == [1132]
  let h = fn(x) {
    let g = fn(y) { #(x, y, a) }
    g
  }
  assert h(1)(2) == #(1, 2, 1)
}
"#);
}

#[test]
fn in_generic_functions() {
    run(r#"
fn wrap(x: a) -> #(a, Int) {
  let n = 3
  let f = fn(k) { #(x, k + n) }
  f(1)
}

fn wrap2(x: a) -> fn(Int) -> #(a, Int) {
  let f = fn(k) { #(x, k) }
  f
}

fn wrap3(x: a) -> #(#(a, Int), #(a, Int)) {
  let f = fn(k: b) { #(x, k) }
  #(f(1), f(2))
}

pub fn main() {
  assert wrap(1) == #(1, 4)
  assert wrap("a") == #("a", 4)
  assert wrap2(1)(2) == #(1, 2)
  assert wrap2("b")(2) == #("b", 2)
  assert wrap3(1.0) == #(#(1.0, 1), #(1.0, 2))
}
"#);
}

#[test]
fn in_case_alternatives() {
    run(r#"
type T {
  A(Int)
  B(Int)
}

fn go(t: T) -> Int {
  case t {
    A(n) | B(n) -> {
      let f = fn() { n }
      f()
    }
  }
}

fn go2(t: T) -> Int {
  case t {
    A(n) | B(n) -> {
      let f = fn() { n }
      let g = fn() { f() + 1 }
      g()
    }
  }
}

fn go3(t: T) -> Int {
  let f = fn(x) { x }
  case t {
    A(n) | B(n) -> map([n], fn(y) { f(y) + n }) |> id |> fn(l) { case l { [x] -> x _ -> 0 } }
  }
}

pub fn main() {
  assert go(A(1)) == 1
  assert go(B(2)) == 2
  assert go2(B(2)) == 3
  assert go3(B(2)) == 4
}
"#);
}

#[test]
fn last_statements() {
    run(r#"
pub fn main() {
  let n = 3
  let g = {
    let f = fn(x) { x + n }
    f
  }
  assert g(1) == 4
}
"#);
}

#[test]
fn tail_calls() {
    run(r#"
fn loop(i: Int, step: Int) -> Int {
  let f = fn(i) { loop(i - step, step) }
  case i <= 0 {
    True -> 0
    False -> f(i)
  }
}

pub fn main() {
  assert loop(1_000_000, 1) == 0
}
"#);
}

#[test]
fn use_callbacks() {
    run(r#"
pub fn main() {
  let n = 3
  let with = fn(k) { k(n) }
  let r = {
    use x <- with
    x + 1
  }
  assert r == 4
  let pair = fn(k) { k(#(n, n)) }
  let s = {
    use #(a, b) <- pair
    a + b + n
  }
  assert s == 9
}
"#);
}

#[test]
fn equality() {
    run(r#"
pub fn main() {
  let n = 3
  let f = fn(x) { x + n }
  assert f == f
  let g = f
  assert g == f
  assert [f] == [g]
  let h = fn(x) { x + n }
  assert f != h
}
"#);
}

#[test]
fn equality_in_inline_functions() {
    run(r#"
pub fn main() {
  let n = 3
  let f = fn(x) { x + n }
  let g = fn() { f }
  let h = id(fn() { f })
  assert g() == f
  assert h() == f
  assert g() == h()
  assert map([1], f) == [4]
}
"#);
}

#[test]
fn equality_of_polymorphic_local_functions() {
    run(r#"
pub fn main() {
  let n = 3
  let f = fn(x: a) { #(x, n) }
  let i: fn(Int) -> #(Int, Int) = f
  assert i == f
  assert i(1) == #(1, 3)
}
"#);
}

#[test]
fn equality_in_case_alternatives() {
    run(r#"
type T {
  A(Int)
  B(Int)
}

pub fn main() {
  let r = case A(1) {
    A(n) | B(n) -> {
      let f = fn() { n }
      let g = f
      g == f
    }
  }
  assert r
}
"#);
}

#[test]
fn equality_in_local_functions() {
    run(r#"
pub fn main() {
  let n = 3
  let f = fn(x) { x + n }
  let g = fn(y: Int) { #(f, y) }
  let k = fn(y: a) { #(f, y) }
  assert g(1).0 == f
  assert k("s").0 == f
}
"#);
}

#[test]
fn captured_functions() {
    run(r#"
pub fn main() {
  let n = 1
  let inc = id(fn(x) { x + n })
  let f = fn(x) { inc(inc(x)) }
  assert f(1) == 3
  let p = fn(x) { x * 2 }
  let q = fn(x) { p(x) + 1 }
  assert q(3) == 7
  assert id(q)(3) == 7
  let r = fn(x) { p(x) + n }
  assert id(r)(3) == 7
}
"#);
}

#[test]
fn captured_empty_lists() {
    run(r#"
pub fn main() {
  let l = []
  let f = fn() { l }
  assert f() == []
  let g = fn(x) { #(x, l) }
  assert g(1).1 == []
}
"#);
}

#[test]
fn captures_bound_by_patterns() {
    run(r#"
type R {
  R(a: Int, b: Int)
}

pub fn main() {
  let assert "ab" <> rest = "abcd"
  let assert "x" as p <> _ = "xy"
  let f = fn() { rest <> p }
  assert f() == "cdx"
  let r = R(1, 2)
  let g = fn() { R(..r, a: 5) }
  assert g() == R(5, 2)
  let #(a, b) = #(1, 2)
  let h = fn() { a + b }
  assert h() == 3
  let s = 10 |> fn(x) { x + 1 }
  let k = fn() { s }
  assert k() == 11
  let t = R(..r, b: 7)
  let m = fn() { t.b }
  assert m() == 7
}
"#);
}

#[test]
fn guards_and_refinements() {
    run(r#"
type S {
  A(f: Int)
  B
}

pub fn main() {
  let s = A(4)
  let d = 2
  let f = case s {
    A(..) -> fn(x) { x + s.f / d }
    B -> fn(x) { x }
  }
  assert f(1) == 3
  let g = fn(x) {
    case x {
      y if y > d -> s
      _ -> B
    }
  }
  assert g(3) == A(4)
}
"#);
}

#[test]
fn direct_calls_do_not_allocate() {
    let wasm = super::compile_wasm(
        r#"
pub fn main() {
  let n = 1
  let f = fn(x) { x + n }
  f(2) + f(3)
}
"#,
        vec![],
    );
    assert_eq!(super::count_ref_calls(&wasm), 0);
    super::for_each_operator(&wasm, |function, op| {
        assert!(
            !matches!(
                op,
                wasmparser::Operator::StructNew { .. } | wasmparser::Operator::RefFunc { .. }
            ),
            "{function} allocates a closure"
        );
    });
}

// The definition allocates the closure, used by every use as a value, in the
// enclosing function. The adapter reads the environment.
#[test]
fn values_allocate_in_the_enclosing_function() {
    let wasm = super::compile_wasm(
        r#"
fn apply(f: fn(Int) -> Int) -> Int {
  f(1)
}

pub fn main() {
  let n = 1
  let f = fn(x) { x + n }
  apply(f) + apply(f)
}
"#,
        vec![],
    );
    let mut allocations = vec![];
    let mut casts = vec![];
    super::for_each_operator(&wasm, |function, op| {
        if let wasmparser::Operator::StructNew { .. } = op {
            allocations.push(function.to_string());
        } else if let wasmparser::Operator::RefCastNonNull { .. } = op {
            casts.push(function.to_string());
        }
    });
    assert_eq!(allocations, ["main"]);
    assert!(
        matches!(casts.as_slice(), [cast] if cast.ends_with("@closure")),
        "{casts:?}"
    );
}
