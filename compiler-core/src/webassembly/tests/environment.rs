// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use crate::config::{WasmFloat, WasmInt};
use crate::webassembly::Error;

/// The function imports of `wasm`, as `(module, name, params, results)`.
fn imports(wasm: &[u8]) -> Vec<(String, String, String, String)> {
    let mut types = vec![];
    let mut imports = vec![];
    for payload in wasmparser::Parser::new(0).parse_all(wasm) {
        match payload.expect("a valid module") {
            wasmparser::Payload::TypeSection(reader) => {
                for group in reader {
                    for sub_type in group.expect("a type").into_types() {
                        types.push(sub_type);
                    }
                }
            }
            wasmparser::Payload::ImportSection(reader) => {
                for import in reader.into_imports() {
                    let import = import.expect("an import");
                    if let wasmparser::TypeRef::Func(index) = import.ty {
                        let func = types[index as usize].unwrap_func();
                        imports.push((
                            import.module.to_string(),
                            import.name.to_string(),
                            format!("{:?}", func.params()),
                            format!("{:?}", func.results()),
                        ));
                    }
                }
            }
            _ => {}
        }
    }
    imports
}

fn environment_imports(src: &str, int: WasmInt) -> Vec<(String, String, String, String)> {
    let wasm = super::compile_wasm_in(
        "thepackage",
        src,
        vec![],
        super::wasm_config(int, WasmFloat::F64),
    );
    imports(&wasm)
        .into_iter()
        .filter(|(module, ..)| module == "env")
        .collect()
}

fn import(
    module: &str,
    name: &str,
    params: &str,
    results: &str,
) -> (String, String, String, String) {
    (module.into(), name.into(), params.into(), results.into())
}

/// A function of a module of a plain name is imported from the environment,
/// with the wasm types of its Gleam types.
#[test]
fn numbers_cross_as_wasm_values() {
    let imports = environment_imports(
        r#"
pub type I32
pub type F32

@external(webassembly, "env", "add")
fn add(a: Int, b: Float, c: I32) -> F32

@external(webassembly, "env", "zero")
fn zero() -> I32

pub fn main() { add(1, 2.0, zero()) }
"#,
        WasmInt::I64,
    );
    assert_eq!(
        imports,
        vec![
            import("env", "add", "[I64, F64, I32]", "[F32]"),
            import("env", "zero", "[]", "[I32]"),
        ]
    );
}

/// An `Int` is an `i32` with `int_bits = 32`.
#[test]
fn int_follows_the_configuration() {
    let imports = environment_imports(
        r#"
@external(webassembly, "env", "next")
fn next(n: Int) -> Int

pub fn main() { next(1) }
"#,
        WasmInt::I32,
    );
    assert_eq!(imports, vec![import("env", "next", "[I32]", "[I32]")]);
}

/// A `Bool` is an `i32`, and a `Nil` result is no result.
#[test]
fn bool_and_nil() {
    let imports = environment_imports(
        r#"
@external(webassembly, "env", "is_even")
fn is_even(n: Int) -> Bool

@external(webassembly, "env", "log")
fn log(b: Bool) -> Nil

pub fn main() { log(is_even(2)) }
"#,
        WasmInt::I64,
    );
    assert_eq!(
        imports,
        vec![
            import("env", "log", "[I32]", "[]"),
            import("env", "is_even", "[I64]", "[I32]"),
        ]
    );
}

/// A function used twice, and declared by two modules, is imported once.
#[test]
fn imported_once() {
    let wasm = super::compile_wasm_in(
        "thepackage",
        r#"
import other

@external(webassembly, "env", "tick")
fn tick() -> Int

pub fn main() { tick() + tick() + other.tick() }
"#,
        vec![(
            "thepackage",
            "other",
            r#"
@external(webassembly, "env", "tick")
pub fn tick() -> Int
"#,
        )],
        super::wasm_config(WasmInt::I64, WasmFloat::F64),
    );
    let names: Vec<_> = imports(&wasm)
        .into_iter()
        .filter(|(module, ..)| module == "env")
        .map(|(_, name, ..)| name)
        .collect();
    assert_eq!(names, vec!["tick"]);
}

/// A program that imports from the environment does not run in a
/// JavaScript host, and exports its memory, which the environment reads and
/// writes.
#[test]
fn no_javascript_host_and_memory_exported() {
    let wasm = super::compile_wasm_in(
        "thepackage",
        r#"
@external(webassembly, "env", "tick")
fn tick() -> Int

pub fn main() { tick() }
"#,
        vec![],
        super::wasm_config(WasmInt::I64, WasmFloat::F64),
    );
    let modules: Vec<_> = imports(&wasm)
        .into_iter()
        .map(|(module, ..)| module)
        .filter(|module| module != "env" && module != "wasi_snapshot_preview1")
        .collect();
    assert_eq!(modules, Vec::<String>::new());
    let mut memory = false;
    for payload in wasmparser::Parser::new(0).parse_all(&wasm) {
        if let Ok(wasmparser::Payload::ExportSection(reader)) = payload {
            for export in reader {
                let export = export.expect("an export");
                memory |=
                    export.name == "memory" && export.kind == wasmparser::ExternalKind::Memory;
            }
        }
    }
    assert!(memory);
}

/// A function of WASI runs in wasmtime.
#[test]
fn wasi_function() {
    let output = super::run_wasm_with_config(
        r#"
@external(webassembly, "wasi_snapshot_preview1", "proc_exit")
fn exit(code: Int) -> Nil

pub fn main() { exit(3) }
"#,
        vec![],
        super::wasm_config(WasmInt::I32, WasmFloat::F64),
    );
    assert_eq!(output.status.code(), Some(3), "{}", output.stderr);
}

/// A function of WASI that the runtime imports has to have the same
/// signature.
#[test]
fn wasi_function_with_another_signature() {
    let error = super::compile_wasm_error_in(
        "thepackage",
        r#"
@external(webassembly, "wasi_snapshot_preview1", "proc_exit")
fn exit(code: Float) -> Nil

pub fn main() { exit(3.0) }
"#,
    );
    assert!(
        matches!(&error, Error::WrongWasiFunctionSignature { name, .. } if name == "proc_exit"),
        "{error:?}"
    );
}

/// A `String` is the address and the size of a copy of its bytes.
#[test]
fn string_is_an_address_and_a_size() {
    let imports = environment_imports(
        r#"
@external(webassembly, "env", "print")
fn print(text: String, times: Int) -> Nil

pub fn main() { print("hello", 2) }
"#,
        WasmInt::I64,
    );
    assert_eq!(
        imports,
        vec![import("env", "print", "[I32, I32, I64]", "[]")]
    );
}
