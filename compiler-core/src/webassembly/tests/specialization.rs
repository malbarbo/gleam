// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

// Specialization of the functions with function parameters by the code of
// their arguments

use super::{
    compile_wasm, count_ref_calls, for_each_operator, run_ok, run_wasm, wasm_function_names,
};

const FOLD: &str = r#"
fn fold(list: List(a), acc: b, f: fn(b, a) -> b) -> b {
  case list {
    [] -> acc
    [x, ..rest] -> fold(rest, f(acc, x), f)
  }
}

fn map(list: List(a), f: fn(a) -> b) -> List(b) {
  map_loop(list, f, [])
}

fn map_loop(list: List(a), f: fn(a) -> b, acc: List(b)) -> List(b) {
  case list {
    [] -> reverse(acc, [])
    [x, ..rest] -> map_loop(rest, f, [f(x), ..acc])
  }
}

fn reverse(list: List(a), acc: List(a)) -> List(a) {
  case list {
    [] -> acc
    [x, ..rest] -> reverse(rest, [x, ..acc])
  }
}
"#;

/// Runs `src` with `FOLD` and checks that the calls to the function
/// parameters are `ref_calls` calls through the closures.
#[track_caller]
fn run(src: &str, ref_calls: usize) {
    let src = format!("{FOLD}\n{src}");
    run_ok(&src);
    assert_eq!(count_ref_calls(&compile_wasm(&src, vec![])), ref_calls);
}

/// The names of the specializations of `function` in the module compiled
/// from `src` with `FOLD`.
fn specializations(src: &str, function: &str) -> Vec<String> {
    wasm_function_names(&compile_wasm(&format!("{FOLD}\n{src}"), vec![]))
        .into_iter()
        .filter(|name| name.starts_with(function) && name.contains('$'))
        .collect()
}

#[test]
fn function_literals() {
    run(
        r#"
pub fn main() {
  let k = 10
  assert fold([1, 2, 3], 0, fn(acc, x) { acc + x }) == 6
  assert fold([1, 2, 3], 0, fn(acc, x) { acc + x * k }) == 60
}
"#,
        0,
    );
}

#[test]
fn functions() {
    run(
        r#"
type Box {
  Box(Int)
}

const plus = add

fn add(a: Int, b: Int) -> Int {
  a + b
}

pub fn main() {
  assert fold([1, 2, 3], 0, add) == 6
  assert fold([1, 2, 3], 0, plus) == 6
  assert map([1, 2], Box) == [Box(1), Box(2)]
  assert map([1, 2], Ok) == [Ok(1), Ok(2)]
}
"#,
        0,
    );
}

#[test]
fn local_functions() {
    run(
        r#"
pub fn main() {
  let k = 10
  let add = fn(acc, x) { acc + x }
  let add_k = fn(acc, x) { acc + x * k }
  assert fold([1, 2, 3], 0, add) == 6
  assert fold([1, 2, 3], 0, add_k) == 60
  assert add_k == add_k
}
"#,
        0,
    );
}

#[test]
fn imported_functions() {
    let src = r#"
import thepackage/lists

fn add(a: Int, b: Int) -> Int {
  a + b
}

pub fn main() {
  let k = 10
  assert lists.fold([1, 2, 3], 0, add) == 6
  assert lists.fold([1, 2, 3], 0, fn(acc, x) { acc + x * k }) == 60
}
"#;
    let deps = vec![(
        "thepackage",
        "thepackage/lists",
        r#"
pub fn fold(list: List(a), acc: b, f: fn(b, a) -> b) -> b {
  case list {
    [] -> acc
    [x, ..rest] -> fold(rest, f(acc, x), f)
  }
}
"#,
    )];
    let result = run_wasm(src, deps.clone());
    assert!(result.status.success(), "{}", result.stderr);
    assert_eq!(count_ref_calls(&compile_wasm(src, deps)), 0);
}

#[test]
fn labelled_arguments() {
    run(
        r#"
fn apply(to x: Int, with f: fn(Int) -> Int, then g: fn(Int) -> Int) -> Int {
  g(f(x))
}

pub fn main() {
  assert apply(then: fn(x) { x * 2 }, to: 1, with: fn(x) { x + 1 }) == 4
}
"#,
        0,
    );
}

#[test]
fn use_callbacks() {
    run(
        r#"
fn with(x: Int, f: fn(Int) -> Int) -> Int {
  f(x)
}

pub fn main() {
  let k = 10
  let y = {
    use x <- with(1)
    x + k
  }
  assert y == 11
}
"#,
        0,
    );
}

#[test]
fn propagated_arguments() {
    run(
        r#"
fn swap(g: fn(Int) -> Int, h: fn(Int) -> Int, x: Int, n: Int) -> Int {
  case n {
    0 -> g(h(x))
    _ -> swap(h, g, x * 2, n - 1)
  }
}

pub fn main() {
  let k = 10
  assert map([1, 2, 3], fn(x) { x * k }) == [10, 20, 30]
  assert swap(fn(x) { x + 1 }, fn(x) { x * k }, 1, 1) == 30
}
"#,
        0,
    );
}

#[test]
fn aliases_of_known_arguments() {
    run(
        r#"
fn apply(f: fn(Int) -> Int, x: Int) -> Int {
  let g = f
  g(x)
}

pub fn main() {
  assert apply(fn(x) { x + 1 }, 1) == 2
}
"#,
        0,
    );
}

#[test]
fn nested_functions_call_known_arguments() {
    run(
        r#"
fn twice(g: fn(Int) -> Int) -> fn(Int) -> Int {
  fn(x) { g(g(x)) }
}

pub fn main() {
  let k = 10
  assert twice(fn(x) { x + k })(1) == 21
}
"#,
        // The closure that `twice` returns has the identity of its literal
        0,
    );
}

#[test]
fn known_arguments_as_values() {
    run(
        r#"
fn inc(x: Int) -> Int {
  x + 1
}

fn same(g: fn(Int) -> Int, h: fn(Int) -> Int) -> Bool {
  g(0) == h(0) && g == h
}

fn store(g: fn(Int) -> Int) -> List(fn(Int) -> Int) {
  let _ = g(0)
  [g, g]
}

fn call_all(gs: List(fn(Int) -> Int), x: Int) -> Int {
  case gs {
    [] -> x
    [g, ..rest] -> call_all(rest, g(x))
  }
}

pub fn main() {
  let k = 10
  let lit = fn(x) { x + k }
  assert same(inc, inc)
  assert same(lit, lit)
  assert !same(lit, inc)
  assert call_all(store(inc), 0) == 2
  assert call_all(store(fn(x) { x * k }), 1) == 100
}
"#,
        // `call_all` calls its argument dynamically
        1,
    );
}

#[test]
fn tail_calls() {
    run(
        r#"
type Box {
  Box(Int)
}

fn count(n: Int, f: fn(Int) -> Int) -> Int {
  case n {
    0 -> f(0)
    _ -> count(n - 1, f)
  }
}

fn box(n: Int, f: fn(Int) -> Box) -> Box {
  f(n)
}

pub fn main() {
  let k = 10
  assert count(1_000_000, fn(x) { x + k }) == 10
  assert box(1, Box) == Box(1)
}
"#,
        0,
    );
}

#[test]
fn new_codes_passed_to_the_same_function_terminate() {
    run(
        r#"
fn f(g: fn(Int) -> Int, n: Int) -> Int {
  case n {
    0 -> g(0)
    _ -> f(fn(x) { g(x) + 1 }, n - 1)
  }
}

fn h(g: fn(Int) -> Int, n: Int) -> Int {
  case n {
    0 -> g(0)
    _ -> i(g, n - 1)
  }
}

fn i(g: fn(Int) -> Int, n: Int) -> Int {
  h(fn(x) { g(x) + 1 }, n)
}

pub fn main() {
  assert f(fn(x) { x }, 10) == 10
  assert h(fn(x) { x }, 10) == 10
}
"#,
        // The new codes are dynamic in the functions being specialized
        4,
    );
}

#[test]
fn only_called_or_passed_parameters_are_specialized() {
    let src = r#"
fn store(g: fn(Int) -> Int) -> List(fn(Int) -> Int) {
  [g]
}

fn first(x: a, g: fn(a) -> a) -> a {
  g(x)
}

pub fn main() {
  let assert [g] = store(fn(x) { x + 1 })
  assert g(1) == 2
  assert first(fn(x) { x * 2 }, fn(h) { h })(2) == 4
}
"#;
    assert_eq!(specializations(src, "store"), Vec::<String>::new());
    assert_eq!(specializations(src, "first").len(), 1);
    run_ok(&format!("{FOLD}\n{src}"));
}

#[test]
fn exported_functions_are_not_specialized_by_name() {
    let src = r#"
pub fn apply(f: fn(Int) -> Int, x: Int) -> Int {
  f(x)
}

pub fn main() {
  assert apply(fn(x) { x + 1 }, 1) == 2
}
"#;
    let wasm = compile_wasm(&format!("{FOLD}\n{src}"), vec![]);
    let mut exports = vec![];
    for payload in wasmparser::Parser::new(0).parse_all(&wasm) {
        if let wasmparser::Payload::ExportSection(section) = payload.expect("valid wasm") {
            for export in section {
                exports.push(export.expect("valid export").name.to_string());
            }
        }
    }
    assert!(exports.contains(&"apply".to_string()), "{exports:?}");
    assert!(
        exports.iter().all(|name| !name.contains('$')),
        "{exports:?}"
    );
    assert_eq!(specializations(src, "apply").len(), 1);
}

#[test]
fn too_many_specializations_are_dynamic() {
    let calls = (0..40)
        .map(|i| format!("  assert apply(fn(x) {{ x + {i} }}, 1) == {}\n", i + 1))
        .collect::<String>();
    let src = format!(
        r#"
fn apply(f: fn(Int) -> Int, x: Int) -> Int {{
  f(x)
}}

pub fn main() {{
{calls}}}
"#
    );
    assert_eq!(specializations(&src, "apply").len(), 32);
    run(&src, 1);
}

#[test]
fn free_type_variables() {
    run(
        r#"
pub fn main() {
  assert map([], fn(x) { x }) == []
}
"#,
        0,
    );
}

#[test]
fn returned_function_literals() {
    run(
        r#"
fn adder(k: Int) -> fn(Int, Int) -> Int {
  fn(acc, x) { acc + x * k }
}

fn flip(f: fn(a, b) -> c) -> fn(b, a) -> c {
  fn(b, a) { f(a, b) }
}

pub fn main() {
  let k = 1
  assert fold([1, 2, 3], 0, adder(10)) == 60
  assert fold([1, 2, 3], 0, flip(fn(x, acc) { acc + x * k })) == 6
  let add_2 = adder(2)
  assert fold([1, 2, 3], 0, add_2) == 12
}
"#,
        0,
    );
}

#[test]
fn returned_function_literals_passed_to_the_same_function_terminate() {
    run_ok(&format!(
        "{FOLD}\n{}",
        r#"
fn inc(g: fn(Int) -> Int) -> fn(Int) -> Int {
  fn(x) { g(x) + 1 }
}

fn f(g: fn(Int) -> Int, n: Int) -> Int {
  case n {
    0 -> g(0)
    _ -> f(inc(g), n - 1)
  }
}

fn h(g: fn(Int) -> Int, n: Int) -> Int {
  case n {
    0 -> g(0)
    _ -> {
      let next = inc(g)
      h(next, n - 1)
    }
  }
}

pub fn main() {
  assert f(fn(x) { x }, 10) == 10
  assert h(fn(x) { x }, 10) == 10
}
"#
    ));
}

#[test]
fn pipes() {
    run(
        r#"
fn add(a: Int, b: Int) -> Int {
  a + b
}

fn apply(f: fn(Int) -> Int, x: Int) -> Int {
  f(x)
}

fn apply_piped(f: fn(Int) -> Int, x: Int) -> Int {
  f |> apply(x)
}

fn adder(k: Int) -> fn(Int) -> Int {
  fn(x) { x + k }
}

pub fn main() {
  let k = 10
  assert add |> fold([1, 2, 3], 0, _) == 6
  assert fn(x) { x + k } |> apply(1) == 11
  assert fn(x) { x * k } |> apply_piped(2) == 20
  assert 5 |> adder |> apply(1) == 6
}
"#,
        // A pipe into a function capture calls the capture literal, which is
        // not specialized
        1,
    );
}

#[test]
fn called_results_of_calls() {
    run(
        r#"
fn inc(x: Int) -> Int {
  x + 1
}

fn twice(g: fn(Int) -> Int) -> fn(Int) -> Int {
  fn(x) { g(g(x)) }
}

pub fn main() {
  let k = 10
  assert twice(inc)(1) == 3
  assert twice(fn(x) { x + k })(1) == 21
}
"#,
        0,
    );
}

#[test]
fn returned_parameters() {
    run(
        r#"
fn add(a: Int, b: Int) -> Int {
  a + b
}

fn id(f: fn(Int, Int) -> Int) -> fn(Int, Int) -> Int {
  f
}

pub fn main() {
  let k = 2
  assert fold([1, 2, 3], 0, id(add)) == 6
  assert fold([1, 2, 3], 0, id(fn(acc, x) { acc + x * k })) == 12
  assert id(add)(1, 2) == 3
}
"#,
        0,
    );
}

#[test]
fn returned_cases_with_the_same_identity() {
    run(
        r#"
fn add(a: Int, b: Int) -> Int {
  a + b
}

fn pick(b: Bool) -> fn(Int, Int) -> Int {
  case b {
    True -> add
    False -> add
  }
}

pub fn main() {
  assert fold([1, 2, 3], 0, pick(True)) == 6
  assert pick(False)(1, 2) == 3
}
"#,
        0,
    );
}

#[test]
fn returned_cases_with_different_identities_are_boxed() {
    run(
        r#"
fn add(a: Int, b: Int) -> Int {
  a + b
}

fn mul(a: Int, b: Int) -> Int {
  a * b
}

fn pick(b: Bool) -> fn(Int, Int) -> Int {
  case b {
    True -> add
    False -> mul
  }
}

pub fn main() {
  assert fold([1, 2, 3], 0, pick(True)) == 6
  assert fold([1, 2, 3], 1, pick(False)) == 6
  assert pick(False)(2, 3) == 6
  assert fold([1, 2, 3], 0, add) == 6
}
"#,
        // `fold` with a boxed function, and the call of `pick(False)`
        2,
    );
}

#[test]
fn returned_local_functions() {
    run(
        r#"
fn apply(f: fn(Int) -> Int, x: Int) -> Int {
  f(x)
}

fn adder(k: Int) -> fn(Int) -> Int {
  let h = fn(x) { x + k }
  h
}

fn doubler() -> fn(Int) -> Int {
  let h = fn(x) { x * 2 }
  h
}

pub fn main() {
  assert apply(adder(1), 2) == 3
  assert adder(2)(3) == 5
  assert apply(doubler(), 2) == 4
  assert doubler()(3) == 6
  let a = adder(1)
  assert a == a
}
"#,
        0,
    );
}

#[test]
fn let_cases() {
    run(
        r#"
fn add(a: Int, b: Int) -> Int {
  a + b
}

fn sum(b: Bool) -> Int {
  let f = case b {
    True -> add
    False -> add
  }
  fold([1, 2, 3], 0, f) + f(1, 2)
}

pub fn main() {
  assert sum(True) == 9
}
"#,
        0,
    );
}

#[test]
fn callees_with_effects_are_evaluated_once() {
    let src = r#"
fn inc(x: Int) -> Int {
  x + 1
}

fn make() -> fn(Int) -> Int {
  echo "one"
  inc
}

fn make_adder(k: Int) -> fn(Int) -> Int {
  echo "two"
  fn(x) { x + k }
}

pub fn main() {
  assert make()(1) == 2
  assert make_adder(1)(1) == 2
}
"#;
    let src = format!("{FOLD}\n{src}");
    let output = run_wasm(&src, vec![]);
    assert!(output.status.success(), "{}", output.stderr);
    assert_eq!(
        output.stderr.matches("\"one\"").count(),
        1,
        "{}",
        output.stderr
    );
    assert_eq!(
        output.stderr.matches("\"two\"").count(),
        1,
        "{}",
        output.stderr
    );
    assert_eq!(count_ref_calls(&compile_wasm(&src, vec![])), 0);
}

#[test]
fn constructors_as_callees_are_not_tail_calls() {
    let src = r#"
type Box {
  Box(Int)
}

fn wrap() -> fn(Int) -> Box {
  Box
}

fn boxed(x: Int) -> Box {
  wrap()(x)
}

pub fn main() {
  assert boxed(1) == Box(1)
}
"#;
    run(src, 0);
    let mut return_calls = 0;
    for_each_operator(
        &compile_wasm(&format!("{FOLD}\n{src}"), vec![]),
        |name, op| {
            if name == "boxed" && matches!(op, wasmparser::Operator::ReturnCall { .. }) {
                return_calls += 1;
            }
        },
    );
    assert_eq!(return_calls, 0);
}

#[test]
fn recursive_functions_returning_functions_are_boxed() {
    run(
        r#"
fn inc(x: Int) -> Int {
  x + 1
}

fn nth(n: Int) -> fn(Int) -> Int {
  case n {
    0 -> inc
    _ -> nth(n - 1)
  }
}

pub fn main() {
  assert nth(3)(1) == 2
}
"#,
        // `nth` has no identity while it is generated
        1,
    );
}

#[test]
fn returned_parameters_passed_to_the_same_function_terminate() {
    let src = r#"
fn id(f: fn(Int) -> Int) -> fn(Int) -> Int {
  f
}

fn f(g: fn(Int) -> Int, n: Int) -> Int {
  case n {
    0 -> g(0)
    _ -> f(id(g), n - 1)
  }
}

pub fn main() {
  assert f(fn(x) { x + 1 }, 10) == 1
}
"#;
    run(src, 0);
    assert_eq!(specializations(src, "f$").len(), 1);
}

#[test]
fn new_codes_through_intermediate_functions_terminate() {
    let src = r#"
fn k(g: fn(Int) -> Int) -> fn(Int) -> Int {
  fn(x) { g(x) + 1 }
}

fn h(g: fn(Int) -> Int, n: Int) -> Int {
  f(g, n)
}

fn f(g: fn(Int) -> Int, n: Int) -> Int {
  case n {
    0 -> g(0)
    _ -> h(k(g), n - 1)
  }
}

pub fn main() {
  assert f(fn(x) { x }, 10) == 10
}
"#;
    // The code that `k` returns is not a parameter of `f`, so `f` is not
    // specialized by it
    run(src, 2);
    assert_eq!(specializations(src, "f$").len(), 1);
    assert_eq!(specializations(src, "k$").len(), 1);
}

#[test]
fn blocks_bind_their_variables() {
    run(
        r#"
fn add(a: Int, b: Int) -> Int {
  a + b
}

fn inc(x: Int) -> Int {
  x + 1
}

fn apply(f: fn(Int) -> Int, x: Int) -> Int {
  f(x)
}

pub fn main() {
  assert fold([1, 2, 3], 0, { let f = add f }) == 6
  assert apply({ let h = inc fn(x) { h(x) } }, 1) == 2
}
"#,
        0,
    );
}

#[test]
fn piped_returned_parameters() {
    run(
        r#"
fn add(a: Int, b: Int) -> Int {
  a + b
}

fn id(f: fn(Int, Int) -> Int) -> fn(Int, Int) -> Int {
  f
}

pub fn main() {
  assert fold([1, 2, 3], 0, add |> id) == 6
}
"#,
        0,
    );
}

#[test]
fn calls_of_known_parameters_returning_functions() {
    run(
        r#"
fn adder(k: Int) -> fn(Int) -> Int {
  fn(x) { x + k }
}

fn call(make: fn(Int) -> fn(Int) -> Int, k: Int) -> Int {
  make(k)(1)
}

pub fn main() {
  assert call(adder, 10) == 11
}
"#,
        0,
    );
}

#[test]
fn curried_functions() {
    run(
        r#"
fn add(a: Int) -> fn(Int) -> fn(Int) -> Int {
  fn(b) { fn(c) { a + b + c } }
}

pub fn main() {
  assert add(1)(2)(3) == 6
}
"#,
        0,
    );
}

#[test]
fn clauses_that_do_not_return_are_neutral() {
    run(
        r#"
fn add(a: Int, b: Int) -> Int {
  a + b
}

fn pick(b: Bool) -> fn(Int, Int) -> Int {
  case b {
    True -> add
    False -> panic
  }
}

pub fn main() {
  assert pick(True)(1, 2) == 3
}
"#,
        0,
    );
}

#[test]
fn local_functions_passed_to_the_same_function_terminate() {
    let src = r#"
fn f(g: fn(Int) -> Int, n: Int) -> Int {
  let inc = fn(x) { g(x) + 1 }
  case n {
    0 -> g(0)
    _ -> f(inc, n - 1)
  }
}

pub fn main() {
  assert f(fn(x) { x }, 10) == 10
}
"#;
    // `inc` is a new code, so the recursive call uses the generic `f`, which
    // calls `g` through the closure, in its body and in `inc`
    run(src, 2);
    assert_eq!(specializations(src, "f$").len(), 1);
}
