// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use crate::config::{WasmFloat, WasmInt};

/// The function imports of `wasm`, as `(module, name)`.
fn function_imports(wasm: &[u8]) -> Vec<(String, String)> {
    let mut imports = vec![];
    for payload in wasmparser::Parser::new(0).parse_all(wasm) {
        if let Ok(wasmparser::Payload::ImportSection(reader)) = payload {
            for import in reader.into_imports() {
                let import = import.expect("an import");
                if matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                    imports.push((import.module.to_string(), import.name.to_string()));
                }
            }
        }
    }
    imports
}

fn host_imports(src: &str, deps: Vec<(&str, &str, &str)>) -> Vec<(String, String)> {
    let wasm = super::compile_wasm_in(
        "thepackage",
        src,
        deps,
        super::wasm_config(WasmInt::I32, WasmFloat::F64),
    );
    function_imports(&wasm)
        .into_iter()
        .filter(|(module, _)| module != "wasi_snapshot_preview1")
        .collect()
}

fn import(module: &str, name: &str) -> (String, String) {
    (module.into(), name.into())
}

/// An external of a module other than "builtins" is a function of the host,
/// which a program outside the standard library can declare.
#[test]
fn external_of_the_host() {
    let imports = host_imports(
        r#"
@external(webassembly, "env", "add")
fn add(a: Int, b: Int) -> Int

pub fn main() { add(1, 2) }
"#,
        vec![],
    );
    assert_eq!(imports, vec![import("env", "add")]);
}

/// A relative path is relative to the module that declares it, like the path
/// of a JavaScript import, so it is made relative to the program.
#[test]
fn external_of_the_host_relative_to_the_module() {
    let imports = host_imports(
        r#"
@external(webassembly, "./ffi.mjs", "run")
fn run() -> Nil

pub fn main() { run() }
"#,
        vec![],
    );
    assert_eq!(imports, vec![import("./ffi.mjs", "run")]);
}

#[test]
fn external_of_the_host_in_another_package() {
    let imports = host_imports(
        r#"
import a/b

pub fn main() { b.run() }
"#,
        vec![(
            "other",
            "a/b",
            r#"
@external(webassembly, "../ffi.mjs", "run")
pub fn run() -> Nil
"#,
        )],
    );
    assert_eq!(imports, vec![import("../../other/ffi.mjs", "run")]);
}

/// Each function of the host is imported once.
#[test]
fn external_of_the_host_used_twice() {
    let imports = host_imports(
        r#"
@external(webassembly, "env", "tick")
fn tick() -> Int

pub fn main() { tick() + tick() }
"#,
        vec![],
    );
    assert_eq!(imports, vec![import("env", "tick")]);
}

/// A generic function of the host is imported with each type it is used
/// with.
#[test]
fn generic_external_of_the_host() {
    let imports = host_imports(
        r#"
@external(webassembly, "env", "id")
fn id(a: a) -> a

pub fn main() {
  let _ = id(1.0)
  id(1)
}
"#,
        vec![],
    );
    assert_eq!(imports, vec![import("env", "id"), import("env", "id")]);
}
