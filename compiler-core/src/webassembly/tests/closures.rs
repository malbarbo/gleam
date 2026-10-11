// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use std::{collections::HashMap, sync::Arc};

use src_span::{LineNumbers, SrcSpan};
use walrus::{FunctionBuilder, FunctionId, HeapType, RefType, ValType};

use crate::type_::{Type, bool, float, int};
use crate::webassembly::{ClosureType, Generator, captures::Capture};

/// Runs `test` with a generator for an empty module, and validates the
/// module it generates.
fn with_generator(test: impl FnOnce(&mut Generator<'_>)) {
    let src = "pub fn main() { 0 }";
    let (module, _) = super::compile(src, vec![]);
    let line_numbers = LineNumbers::new(src);
    let all_modules = HashMap::from([(module.name.clone(), &module)]);
    let all_line_numbers = HashMap::from([(module.name.clone(), line_numbers.clone())]);
    let mut generator = Generator::new(&module, &line_numbers, &all_modules, &all_line_numbers);
    generator.types_prelude();
    test(&mut generator);
    let wasm = generator.wasm_module.emit_wasm();
    let _ = wasmparser::Validator::new()
        .validate_all(&wasm)
        .expect("valid module");
}

fn capture(name: &str, type_: Arc<Type>) -> Capture {
    Capture {
        name: name.into(),
        type_,
        definition: SrcSpan::default(),
    }
}

fn ref_(id: walrus::TypeId) -> ValType {
    ValType::Ref(RefType {
        heap_type: HeapType::Concrete(id),
        nullable: false,
    })
}

/// Adds a code function of `closure` with these params and results.
fn add_code_function(
    generator: &mut Generator<'_>,
    closure: ClosureType,
    params: &[ValType],
    results: &[ValType],
) -> FunctionId {
    let module = &mut generator.wasm_module;
    let params: Vec<_> = std::iter::once(ref_(closure.type_index))
        .chain(params.iter().copied())
        .collect();
    let mut builder = FunctionBuilder::new(&mut module.types, &params, results);
    let _ = builder.func_body().unreachable();
    let args = params
        .iter()
        .map(|param| module.locals.add(*param))
        .collect();
    builder.finish(args, &mut module.funcs)
}

#[test]
fn closure_type_validates() {
    with_generator(|generator| {
        let closure = generator.closure_type([int(), float()], int());
        let code = add_code_function(
            generator,
            closure,
            &[ValType::I32, ValType::F64],
            &[ValType::I32],
        );
        let module = &mut generator.wasm_module;
        let c = module.locals.add(ref_(closure.type_index));
        let mut builder = FunctionBuilder::new(&mut module.types, &[], &[ValType::I32]);
        let _ = builder
            .func_body()
            .ref_func(code)
            .struct_new(closure.type_index)
            .local_set(c)
            .local_get(c)
            .i32_const(1)
            .f64_const(2.0)
            .local_get(c)
            .struct_get(closure.type_index, 0)
            .call_ref(closure.code_type_index);
        let _ = builder.finish(vec![], &mut module.funcs);
    });
}

#[test]
fn closure_type_is_cached() {
    with_generator(|generator| {
        let int_int = generator.closure_type([int()], int());
        assert_eq!(generator.closure_type([int()], int()), int_int);
        assert_eq!(generator.closure_type([bool()], int()), int_int);
        assert_ne!(generator.closure_type([float()], int()), int_int);
        assert_ne!(generator.closure_type([int()], float()), int_int);
    });
}

#[test]
fn closure_code_matches_function_builder() {
    with_generator(|generator| {
        let closure = generator.closure_type([int(), float()], float());
        let code = add_code_function(
            generator,
            closure,
            &[ValType::I32, ValType::F64],
            &[ValType::F64],
        );
        assert_eq!(
            generator.wasm_module.funcs.get(code).ty(),
            closure.code_type_index
        );
    });
}

#[test]
fn closure_environment_type_validates() {
    with_generator(|generator| {
        let closure = generator.closure_type([int()], int());
        let captures = [capture("a", int()), capture("b", float())];
        let environment = generator.closure_environment_type(&closure, &captures);
        assert_eq!(
            generator.closure_environment_type(&closure, &captures),
            environment
        );
        let code = add_code_function(generator, closure, &[ValType::I32], &[ValType::I32]);
        let module = &mut generator.wasm_module;
        let c = module.locals.add(ref_(closure.type_index));
        let mut builder = FunctionBuilder::new(&mut module.types, &[], &[ValType::I32]);
        let _ = builder
            .func_body()
            .ref_func(code)
            .i32_const(7)
            .f64_const(8.0)
            .struct_new(environment)
            .local_set(c)
            .local_get(c)
            .instr(walrus::ir::RefCast {
                nullable: false,
                heap_type: HeapType::Concrete(environment),
            })
            .struct_get(environment, 1);
        let _ = builder.finish(vec![], &mut module.funcs);
    });
}

#[test]
fn closure_type_moved_to_rec_group() {
    with_generator(|generator| {
        let old = generator.closure_type([int()], int());
        let type_index = generator.rec_group_from(old.type_index, old.type_index);
        let closure = generator.closure_type([int()], int());
        assert_eq!(closure.type_index, type_index);
        assert_ne!(closure, old);
        let code = add_code_function(generator, closure, &[ValType::I32], &[ValType::I32]);
        assert_eq!(
            generator.wasm_module.funcs.get(code).ty(),
            closure.code_type_index
        );
    });
}

#[test]
fn function_values() {
    let result = super::run_wasm(
        r#"
import thepackage/helpers

type I32 {}

@external(webassembly, "builtins", "_int_to_i32")
fn i32(value: Int) -> I32

@external(webassembly, "builtins", "_i32_to_int")
fn int(value: I32) -> Int

@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

type Box {
  Box(value: Int)
}

type Ops {
  Ops(step: fn(Int) -> Int)
}

const inc_const = inc

const inc_alias = inc_const

const steps = [inc, inc_alias]

fn apply(f: fn(a) -> b, x: a) -> b {
  f(x)
}

fn inc(n: Int) -> Int {
  n + 1
}

fn add_three() -> fn(Int) -> Int {
  let g = fn(n) { n + 3 }
  g
}

fn add_four() -> fn(Int) -> Int {
  let g = fn(n) { n + 4 }
}

pub fn main() {
  assert apply(inc, 1) == 2
  assert apply(helpers.double, 2) == 4
  assert apply(Box, 3) == Box(3)
  assert apply(int, apply(i32, 4)) == 4
  assert apply(inspect, 5) == "5"
  let dec = fn(n) { n - 1 }
  assert apply(dec, 6) == 5
  assert apply(fn(n) { n * 2 }, 7) == 14
  assert apply(inc_const, 8) == 9
  assert apply(inc_alias, 9) == 10
  let assert [first, second] = steps
  assert first(second(10)) == 12
  assert Ops(inc).step(11) == 12
  assert add_three()(12) == 15
  assert add_four()(12) == 16
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub fn double(n: Int) -> Int {
  n * 2
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn function_equality() {
    let result = super::run_wasm(
        r#"
import thepackage/helpers

type Ops {
  Ops(step: fn(Int) -> Int)
}

type Step {
  Step(step: fn(Int) -> Int)
  Stop
}

const inc_const = inc

fn id(value: a) -> a {
  value
}

fn inc(n: Int) -> Int {
  n + 1
}

fn dec(n: Int) -> Int {
  n - 1
}

pub fn main() {
  assert inc == inc
  assert inc != dec
  assert id(inc) == inc
  assert inc_const == inc
  assert helpers.double == helpers.double
  assert helpers.double != inc
  let f = fn(n) { n * 3 }
  assert f == f
  assert id(f) == f
  assert f != inc
  assert Ops(inc) == Ops(inc)
  assert Ops(inc) != Ops(dec)
  assert Step(inc) == Step(id(inc))
  assert Step(inc) != Stop
  assert #(1, inc) == #(1, inc)
  assert #(1, inc) != #(1, dec)
  assert [inc, dec] == [inc, dec]
  assert [inc, dec] != [dec, inc]
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub fn double(n: Int) -> Int {
  n * 2
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn function_values_are_shared_singletons() {
    let wasm = super::compile_wasm(
        r#"
fn apply(f: fn(Int) -> Int, x: Int) -> Int {
  f(x)
}

fn inc(n: Int) -> Int {
  n + 1
}

fn one() -> Int {
  apply(inc, 1)
}

fn two() -> Int {
  apply(inc, 2) + apply(fn(n) { n }, 2)
}

pub fn main() {
  one() + two()
}
"#,
        vec![],
    );
    let adapters = super::wasm_function_names(&wasm)
        .into_iter()
        .filter(|name| name.ends_with("@closure"))
        .collect::<Vec<_>>();
    assert_eq!(adapters, ["inc@closure"]);
    let closures = super::wasm_global_names(&wasm)
        .into_iter()
        .filter(|name| name.contains("@closure") || name.starts_with("anonymous@"))
        .count();
    assert_eq!(closures, 2);
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

#[test]
fn captures() {
    super::run_ok(
        r#"
pub type Box {
  Box(value: Int, name: String)
}

pub type Maybe {
  Just(Int)
  Nothing
}

fn adder(n: Int) -> fn(Int) -> Int {
  fn(x) { x + n }
}

fn compose(f: fn(a) -> b, g: fn(b) -> c) -> fn(a) -> c {
  fn(x) { g(f(x)) }
}

fn values(i: Int, f: Float, s: String, t: #(Int, Int), b: Box, l: List(Int), m: Maybe) {
  fn(x: Int) { #(x + i, f, s, t, b, l, m) }
}

pub fn main() {
  assert adder(1)(2) == 3
  assert adder(10)(2) == 12
  let inc_double = compose(adder(1), fn(x) { x * 2 })
  assert inc_double(3) == 8
  let box = Box(1, "a")
  assert values(1, 2.0, "s", #(3, 4), box, [5], Nothing)(1)
    == #(2, 2.0, "s", #(3, 4), box, [5], Nothing)
  assert values(1, 2.0, "s", #(3, 4), box, [5], Just(6))(0).6 == Just(6)
}
"#,
    );
}

#[test]
fn nested_captures() {
    super::run_ok(
        r#"
fn id(x: a) -> a {
  x
}

pub fn main() {
  let a = 1
  let f = id(fn() { fn() { a } })
  assert f()() == 1
  let g = id(fn(x) { fn() { x + a } })
  assert g(2)() == 3
  let h = id(fn(x) {
    let y = a
    let a = 2
    y + a + x
  })
  assert h(3) == 6
}
"#,
    );
}

#[test]
fn captures_in_guards_divisions_and_patterns() {
    super::run_ok(
        r#"
fn id(x: a) -> a {
  x
}

pub type S {
  A(f: Int)
  B
}

pub type R {
  R(a: Int, b: Int)
}

pub fn main() {
  let n = 2
  let d = 0
  let above = id(fn(x) {
    case x {
      y if y > n -> True
      _ -> False
    }
  })
  assert above(3)
  assert !above(1)
  let divide = id(fn(x) { x / n + x / d })
  assert divide(6) == 3
  let s = A(5)
  let field = case s {
    A(..) -> fn() { s.f }
    B -> fn() { 0 }
  }
  assert field() == 5
  let guarded = case s {
    A(..) -> fn(x) {
      case x {
        y if y == s.f -> True
        _ -> False
      }
    }
    B -> fn(_) { False }
  }
  assert guarded(5)
  let r = R(1, 2)
  let update = id(fn() { R(..r, a: 3) })
  assert update() == R(3, 2)
  let assert "a" as p <> rest = "abc"
  let prefix = id(fn() { p <> rest })
  assert prefix() == "abc"
  let l = []
  let empty = id(fn() { l })
  assert empty() == []
  let e = 7
  let echoed = id(fn() { echo e })
  assert echoed() == 7
}
"#,
    );
}

#[test]
fn captures_in_case_alternatives() {
    super::run_ok(
        r#"
pub type T {
  A(Int)
  B(Int)
}

fn add(a: Int, b: Int) -> Int {
  a + b
}

fn make(t: T) -> fn() -> Int {
  case t {
    A(n) | B(n) -> fn() { n }
  }
}

fn piped(t: T) -> Int {
  case t {
    A(n) | B(n) -> 1 |> add(n, _)
  }
}

pub fn main() {
  assert make(A(1))() == 1
  assert make(B(2))() == 2
  assert piped(A(1)) == 2
  assert piped(B(2)) == 3
}
"#,
    );
}

#[test]
fn captures_in_use_callbacks() {
    super::run_ok(
        r#"
fn id(x: a) -> a {
  x
}

fn with(a: Int, f: fn(Int) -> Int) -> Int {
  f(a)
}

fn pair(f: fn(#(Int, Int)) -> Int) -> Int {
  f(#(1, 2))
}

pub fn main() {
  let a = 1
  let b = 10
  let f = id(fn() {
    use x <- with(a)
    use #(m, n) <- pair
    x + b + m + n
  })
  assert f() == 14
  let g = {
    use x <- fn(k) { k(a) }
    x + b
  }
  assert g == 11
}
"#,
    );
}

#[test]
fn captures_in_generic_functions() {
    super::run_ok(
        r#"
fn wrap(x: a) -> fn() -> a {
  fn() { x }
}

fn apply_to(x: a) -> a {
  1 |> fn(_) { x }
}

pub fn main() {
  assert wrap(1)() == 1
  assert wrap("a")() == "a"
  assert apply_to(2) == 2
  assert apply_to("b") == "b"
}
"#,
    );
}

#[test]
fn twice_example() {
    super::run_ok(
        r#"
fn twice(f: fn(a) -> a) -> fn(a) -> a {
  fn(x) { f(f(x)) }
}

pub fn main() {
  assert twice(fn(n) { n + 1 })(3) == 5
  assert twice(fn(n) { n * 2 })(5) == 20
  assert twice(twice(twice))(fn(n) { n + 1 })(5) == 21
}
"#,
    );
}

#[test]
fn captures_and_local_functions() {
    super::run_ok(
        r#"
fn id(x: a) -> a {
  x
}

pub fn main() {
  let f = fn(x) { x + 1 }
  let a = 10
  let g = id(fn(y) { f(y) + a })
  assert g(1) == 12
  let h = fn(x) { fn() { x } }
  assert h(3)() == 3
}
"#,
    );
}

#[test]
fn captures_equality() {
    super::run_ok(
        r#"
fn make(n: Int) -> fn() -> Int {
  fn() { n }
}

pub fn main() {
  assert make(1) != make(1)
  let f = make(1)
  assert f == f
}
"#,
    );
}

#[test]
fn captures_in_tail_calls() {
    super::run_ok(
        r#"
fn count(n: Int, step: Int) -> fn(Int) -> Int {
  fn(acc) {
    case n {
      0 -> acc
      _ -> count(n - 1, step)(acc + step)
    }
  }
}

pub fn main() {
  assert count(100_000, 2)(0) == 200_000
}
"#,
    );
}

#[test]
fn captures_in_applied_functions() {
    super::run_ok(
        r#"
fn id(x: a) -> a {
  x
}

fn add(a: Int, b: Int) -> Int {
  a + b
}

fn sub(a a: Int, b b: Int) -> Int {
  a - b
}

pub fn main() {
  let a = 1
  assert 2 |> add(a, _) == 3
  assert 2 |> fn(y) { y + a } == 3
  assert fn(x) { x + a }(4) == 5
  let x = 10
  assert 3 |> sub(b: x, a: _) == -7
  let f = id(add(a, _))
  assert f(5) == 6
}
"#,
    );
}

#[test]
fn captures_of_recursive_types() {
    super::run_ok(
        r#"
fn id(x: a) -> a {
  x
}

pub type S {
  S(f: fn(S) -> Int)
}

pub fn main() {
  let s = S(fn(_) { 1 })
  let g = id(fn(x: S) { x.f(s) + s.f(x) })
  assert g(s) == 2
}
"#,
    );
}

#[test]
fn closures_with_captures_allocate_their_environment() {
    let wasm = super::compile_wasm(
        r#"
fn adder(n: Int) -> fn(Int) -> Int {
  fn(x) { x + n }
}

fn subtracter(n: Int) -> fn(Int) -> Int {
  fn(x) { x - n }
}

pub fn main() {
  adder(1)(2) + subtracter(1)(2)
}
"#,
        vec![],
    );
    let mut allocations = vec![];
    let mut casts = vec![];
    super::for_each_operator(&wasm, |function, op| {
        if let wasmparser::Operator::StructNew { .. } | wasmparser::Operator::RefFunc { .. } = op {
            allocations.push(function.to_string());
        } else if let wasmparser::Operator::RefCastNonNull { .. } = op {
            casts.push(function.to_string());
        }
    });
    assert_eq!(allocations, ["adder", "adder", "subtracter", "subtracter"]);
    assert_eq!(casts.len(), 2);
    assert!(casts.iter().all(|name| name.starts_with("anonymous@")));
    let environments = super::wasm_type_names(&wasm)
        .into_iter()
        .filter(|name| name.contains("::environment"))
        .collect::<Vec<_>>();
    assert_eq!(environments, ["fn(Int)->Int::environment(Int)"]);
}

#[test]
fn applied_functions_are_called_directly() {
    let wasm = super::compile_wasm(
        r#"
fn add(a: Int, b: Int) -> Int {
  a + b
}

pub fn main() {
  let a = 1
  let b = 2 |> add(a, _)
  let c = 3 |> fn(x) { x }
  b + c
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
    let direct = super::wasm_function_names(&wasm)
        .into_iter()
        .filter(|name| name.ends_with("@direct"))
        .count();
    assert_eq!(direct, 2);
}

#[test]
fn negations_in_generic_functions() {
    super::run_ok(
        r#"
fn negate(pred: fn(a) -> Bool) -> fn(a) -> Bool {
  fn(x: a) -> Bool { !pred(x) }
}

fn negative(f: fn(a) -> Int, x: a) -> Int {
  -f(x)
}

pub fn main() {
  assert negate(fn(x: Int) -> Bool { x > 0 })(-3)
  assert !negate(fn(x: String) -> Bool { x == "" })("")
  assert negative(fn(x: Int) { x + 1 }, 1) == -2
}
"#,
    );
}
