// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! The functions of builtins-wasm linked into the generated module.

use super::{compile_wasm, for_each_operator, run_ok};

const BUILTINS: &str = r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

@external(webassembly, "builtins", "string_lowercase")
fn lowercase(string: String) -> String

pub fn main() {
    assert inspect(1.5) == "1.5"
    assert lowercase("ABC") == "abc"
}
"#;

fn validate(wasm: &[u8]) {
    let _ = wasmparser::Validator::new_with_features(wasmparser::WasmFeatures::all())
        .validate_all(wasm)
        .expect("valid wasm");
}

/// Whether `wasm` has a section with the given id.
fn has_section(wasm: &[u8], id: u8) -> bool {
    wasmparser::Parser::new(0).parse_all(wasm).any(|payload| {
        payload
            .expect("valid wasm")
            .as_section()
            .is_some_and(|(section, _)| section == id)
    })
}

#[test]
fn builtins_run() {
    run_ok(BUILTINS);
}

#[test]
fn builtins_have_their_code() {
    let wasm = compile_wasm(BUILTINS, vec![]);
    validate(&wasm);
    let mut operators = 0;
    for_each_operator(&wasm, |name, _| {
        if name == "string_lowercase" {
            operators += 1;
        }
    });
    // More than the `unreachable` and `end` of its stub.
    assert!(operators > 2, "{operators} operators");
}

#[test]
fn builtins_are_the_same_in_every_compilation() {
    assert_eq!(
        compile_wasm(BUILTINS, vec![]),
        compile_wasm(BUILTINS, vec![])
    );
}

#[test]
fn module_without_builtins_has_none_of_their_items() {
    if super::super::link::walrus() {
        // The table of builtins-wasm is kept with walrus.
        return;
    }
    let wasm = compile_wasm("pub fn main() { 1 + 2 }", vec![]);
    validate(&wasm);
    for (id, section) in [(2, "import"), (4, "table")] {
        assert!(!has_section(&wasm, id), "{section} section");
    }
}

/// The section that records where the items of builtins-wasm are is not in
/// the emitted module.
#[test]
fn builtins_leave_no_custom_section_of_their_own() {
    let wasm = compile_wasm(BUILTINS, vec![]);
    let mut customs = vec![];
    for payload in wasmparser::Parser::new(0).parse_all(&wasm) {
        if let wasmparser::Payload::CustomSection(section) = payload.expect("valid wasm") {
            customs.push(section.name().to_string());
        }
    }
    customs.sort();
    assert_eq!(customs, ["name", "producers", "target_features"]);
}
