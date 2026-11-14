// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `call_lines = true`, see `lines`.

use std::collections::HashSet;

use super::{TEST_PACKAGE, compile_wasm_in};
use crate::config::{WasmOptLevel, WebAssemblyConfig};
use crate::webassembly::{Lines, LinesFunction};

const SRC: &str = "pub fn main() {
  f(1) + 0
}

fn f(n: Int) -> Int {
  1 + g(n)
}

fn g(n: Int) -> Int {
  let h = fn(x) {
    f(x) + 0
  }
  1 + h(n)
}
";

fn lines(src: &str, config: WebAssemblyConfig) -> Option<Lines> {
    Lines::of(&compile_wasm_in(TEST_PACKAGE, src, vec![], config))
}

fn with_lines() -> WebAssemblyConfig {
    WebAssemblyConfig {
        call_lines: true,
        ..WebAssemblyConfig::default()
    }
}

/// The calls of the function named `name`, by their lines.
fn call_lines_of(lines: &Lines, name: &str) -> Vec<u32> {
    let indexes: HashSet<u32> = lines
        .functions
        .iter()
        .filter(|(_, function)| function.name == name)
        .map(|(index, _)| *index)
        .collect();
    let mut found: Vec<u32> = lines
        .calls
        .iter()
        .filter(|((index, _), _)| indexes.contains(index))
        .map(|(_, line)| *line)
        .collect();
    found.sort_unstable();
    found.dedup();
    found
}

#[test]
fn no_section_by_default() {
    assert_eq!(lines(SRC, WebAssemblyConfig::default()), None);
}

/// `wasm-opt` moves the code, so the offsets would be wrong.
#[test]
fn no_section_with_opt_level() {
    let config = WebAssemblyConfig {
        opt_level: WasmOptLevel::O2,
        ..with_lines()
    };
    assert_eq!(lines(SRC, config), None);
}

#[test]
fn functions_and_calls() {
    let lines = lines(SRC, with_lines()).expect("section");
    let file = "src/my/mod.gleam".to_string();
    for name in ["main", "f", "g", "<anonymous>"] {
        assert!(
            lines.functions.values().any(|function| *function
                == LinesFunction {
                    name: name.into(),
                    file: file.clone()
                }),
            "{name} in {:?}",
            lines.functions
        );
    }
    assert_eq!(call_lines_of(&lines, "main"), vec![2]);
    assert_eq!(call_lines_of(&lines, "f"), vec![6]);
    assert_eq!(call_lines_of(&lines, "g"), vec![13]);
    assert_eq!(call_lines_of(&lines, "<anonymous>"), vec![11]);
}

/// Each call of the section is a `call` or a `call_ref` at its offset in
/// the body of its function, which counts from the declarations of the
/// locals.
#[test]
fn offsets_are_calls() {
    let wasm = compile_wasm_in(TEST_PACKAGE, SRC, vec![], with_lines());
    let lines = Lines::of(&wasm).expect("section");
    let mut calls = HashSet::new();
    let mut index = 0;
    for payload in wasmparser::Parser::new(0).parse_all(&wasm) {
        let payload = payload.expect("valid wasm");
        if let wasmparser::Payload::ImportSection(imports) = payload {
            index += imports
                .into_imports()
                .filter(|import| {
                    matches!(
                        import.as_ref().expect("valid wasm").ty,
                        wasmparser::TypeRef::Func(_)
                    )
                })
                .count() as u32;
        } else if let wasmparser::Payload::CodeSectionEntry(body) = payload {
            let start = body.range().start;
            let mut reader = body.get_operators_reader().expect("valid wasm");
            while !reader.eof() {
                let (operator, offset) = reader.read_with_offset().expect("valid wasm");
                if matches!(
                    operator,
                    wasmparser::Operator::Call { .. } | wasmparser::Operator::CallRef { .. }
                ) {
                    let _ = calls.insert((index, (offset - start) as u32));
                }
            }
            index += 1;
        }
    }
    assert!(!lines.calls.is_empty());
    for call in lines.calls.keys() {
        assert!(calls.contains(call), "{call:?} is not a call");
    }
}
