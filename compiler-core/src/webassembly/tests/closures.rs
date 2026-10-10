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
    let params: Vec<_> = std::iter::once(ref_(closure.clo))
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
        let c = module.locals.add(ref_(closure.clo));
        let mut builder = FunctionBuilder::new(&mut module.types, &[], &[ValType::I32]);
        let _ = builder
            .func_body()
            .ref_func(code)
            .struct_new(closure.clo)
            .local_set(c)
            .local_get(c)
            .i32_const(1)
            .f64_const(2.0)
            .local_get(c)
            .struct_get(closure.clo, 0)
            .call_ref(closure.code);
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
        assert_eq!(generator.wasm_module.funcs.get(code).ty(), closure.code);
    });
}

#[test]
fn closure_env_type_validates() {
    with_generator(|generator| {
        let closure = generator.closure_type([int()], int());
        let env = generator.closure_env_type(&closure, vec![ValType::I32, ValType::I64]);
        assert_eq!(
            generator.closure_env_type(&closure, vec![ValType::I32, ValType::I64]),
            env
        );
        let code = add_code_function(generator, closure, &[ValType::I32], &[ValType::I32]);
        let module = &mut generator.wasm_module;
        let c = module.locals.add(ref_(closure.clo));
        let mut builder = FunctionBuilder::new(&mut module.types, &[], &[ValType::I32]);
        let _ = builder
            .func_body()
            .ref_func(code)
            .i32_const(7)
            .i64_const(8)
            .struct_new(env)
            .local_set(c)
            .local_get(c)
            .instr(walrus::ir::RefCast {
                nullable: false,
                heap_type: HeapType::Concrete(env),
            })
            .struct_get(env, 1);
        let _ = builder.finish(vec![], &mut module.funcs);
    });
}

#[test]
fn closure_type_moved_to_rec_group() {
    with_generator(|generator| {
        let old = generator.closure_type([int()], int());
        let clo = generator.rec_group_from(old.clo, old.clo);
        let closure = generator.closure_type([int()], int());
        assert_eq!(closure.clo, clo);
        assert_ne!(closure, old);
        let code = add_code_function(generator, closure, &[ValType::I32], &[ValType::I32]);
        assert_eq!(generator.wasm_module.funcs.get(code).ty(), closure.code);
    });
}
