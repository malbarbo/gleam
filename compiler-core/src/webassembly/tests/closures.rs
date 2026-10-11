// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use std::collections::HashMap;

use src_span::LineNumbers;
use walrus::{FunctionBuilder, FunctionId, HeapType, RefType, ValType};

use crate::type_::{bool, float, int};
use crate::webassembly::{ClosureType, Generator};

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
        let environment =
            generator.closure_environment_type(&closure, vec![ValType::I32, ValType::I64]);
        assert_eq!(
            generator.closure_environment_type(&closure, vec![ValType::I32, ValType::I64]),
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
            .i64_const(8)
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
