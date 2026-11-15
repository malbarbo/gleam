// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `_try`, see `panic`.

use super::{TEST_PACKAGE, WasmOutput, compile_wasm_in, for_each_operator, unique_name};
use crate::config::{WasmFailure, WasmStackOverflow, WebAssemblyConfig};

/// A host with WASI and `env.wasm_try`, which returns 1 when a stack
/// overflow ended the call.
const RUNNER: &str = r#"
import { readFileSync } from "node:fs";
import { WASI } from "node:wasi";
const wasi = new WASI({ version: "preview1", returnOnExit: true });
const env = {
  wasm_try(f, x) {
    try {
      f(x);
      return 0;
    } catch (error) {
      if (error instanceof RangeError) return 1;
      throw error;
    }
  },
};
const module = await WebAssembly.compile(readFileSync(process.argv[2]));
const instance = await WebAssembly.instantiate(module, {
  wasi_snapshot_preview1: wasi.getImportObject().wasi_snapshot_preview1,
  env,
});
process.exitCode = wasi.start(instance);
"#;

const TRY: &str = r#"
@external(webassembly, "builtins", "_try")
fn try_with(run: fn() -> a, ok: fn(a) -> b, overflow: fn() -> b) -> b

fn try_(run: fn() -> a) -> Result(a, Nil) {
  try_with(run, Ok, fn() { Error(Nil) })
}

fn deep(n: Int) -> Int {
  1 + deep(n + 1)
}
"#;

fn run(src: &str, config: WebAssemblyConfig) -> WasmOutput {
    let wasm = compile_wasm_in(TEST_PACKAGE, &format!("{TRY}{src}"), vec![], config);
    let dir = std::env::temp_dir().join(unique_name("gleam_wasm_try_test"));
    std::fs::create_dir_all(&dir).expect("failed to create the test directory");
    std::fs::write(dir.join("main.wasm"), wasm).expect("failed to write the module");
    std::fs::write(dir.join("run.mjs"), RUNNER).expect("failed to write the runner");
    let output = std::process::Command::new("node")
        .arg("--no-warnings")
        .arg(dir.join("run.mjs"))
        .arg(dir.join("main.wasm"))
        .output()
        .expect("node not found");
    if output.status.success() {
        let _ = std::fs::remove_dir_all(&dir);
    }
    WasmOutput {
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        status: output.status,
    }
}

#[track_caller]
fn runs(src: &str, config: WebAssemblyConfig) {
    let result = run(src, config);
    assert!(result.status.success(), "{}", result.stderr);
}

#[test]
fn try_gives_the_value() {
    runs(
        r#"
pub fn main() {
  assert try_(fn() { 1 + 2 }) == Ok(3)
  assert try_(fn() { 1.5 }) == Ok(1.5)
  assert try_(fn() { "a" }) == Ok("a")
  assert try_(fn() { [1, 2] }) == Ok([1, 2])
  assert try_(fn() { try_(fn() { Nil }) }) == Ok(Ok(Nil))
}
"#,
        WebAssemblyConfig::default(),
    );
}

/// The program goes on after a stack overflow, as many times as it has one.
#[test]
fn try_catches_a_stack_overflow() {
    runs(
        r#"
pub fn main() {
  assert try_(fn() { deep(0) }) == Error(Nil)
  assert try_(fn() { try_(fn() { deep(0) }) }) == Ok(Error(Nil))
  repeat(20)
  assert try_(fn() { 1 }) == Ok(1)
}

fn repeat(n: Int) -> Nil {
  case n {
    0 -> Nil
    _ -> {
      let assert Error(Nil) = try_(fn() { deep(0) })
      repeat(n - 1)
    }
  }
}
"#,
        WebAssemblyConfig::default(),
    );
}

/// A failure is an exception, which goes through the host to the `_catch`
/// outside.
#[test]
fn a_failure_goes_through_try() {
    let result = run(
        r#"
pub fn main() {
  try_(fn() { panic as "boom" })
}
"#,
        WebAssemblyConfig {
            failure: WasmFailure::Exception,
            ..WebAssemblyConfig::default()
        },
    );
    assert!(!result.status.success());
    assert!(result.stderr.contains("boom"), "{}", result.stderr);
}

/// With `stack_overflow = "fail"`, the depth of the calls goes back after a
/// trap, so the limit stays where it was.
#[test]
fn the_depth_goes_back_after_a_trap() {
    runs(
        r#"
pub fn main() {
  repeat(300)
  assert sum(5000) == 12_502_500
}

fn repeat(n: Int) -> Nil {
  case n {
    0 -> Nil
    _ -> {
      let assert Error(Nil) = try_(fn() { deep(0) })
      repeat(n - 1)
    }
  }
}

fn sum(n: Int) -> Int {
  case n {
    0 -> 0
    _ -> n + sum(n - 1)
  }
}
"#,
        WebAssemblyConfig {
            stack_overflow: WasmStackOverflow::Fail,
            stack_limit: 1_000_000,
            ..WebAssemblyConfig::default()
        },
    );
}

#[test]
fn try_imports_wasm_try() {
    let wasm = compile_wasm_in(
        TEST_PACKAGE,
        &format!("{TRY}pub fn main() {{ try_(fn() {{ 1 }}) }}"),
        vec![],
        WebAssemblyConfig::default(),
    );
    let imports: Vec<_> = wasmparser::Parser::new(0)
        .parse_all(&wasm)
        .filter_map(|payload| match payload {
            Ok(wasmparser::Payload::ImportSection(reader)) => Some(reader),
            _ => None,
        })
        .flat_map(|reader| reader.into_imports().flatten())
        .map(|import| format!("{}.{}", import.module, import.name))
        .collect();
    assert!(imports.contains(&"env.wasm_try".to_string()), "{imports:?}");
}

/// `_try` sets the stack pointer of builtins-wasm back after a trap, when a
/// builtin uses it.
#[test]
fn try_resets_the_stack_pointer() {
    let global_sets = |src: &str| {
        let wasm = compile_wasm_in(
            TEST_PACKAGE,
            &format!("{TRY}{src}"),
            vec![],
            WebAssemblyConfig::default(),
        );
        let mut count = 0;
        for_each_operator(&wasm, |name, operator| {
            if name == "_reset_stack_pointer"
                && matches!(operator, wasmparser::Operator::GlobalSet { .. })
            {
                count += 1;
            }
        });
        count
    };
    assert_eq!(
        global_sets(
            r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(a: a) -> String

pub fn main() { try_(fn() { inspect(1.5) }) }
"#
        ),
        1
    );
    assert_eq!(global_sets("pub fn main() { try_(fn() { 1 }) }"), 0);
}

#[test]
fn wrong_signature_of_try() {
    let error = super::compile_wasm_error(
        r#"
@external(webassembly, "builtins", "_try")
fn try_(run: fn() -> a) -> Result(a, Nil)
"#,
    );
    assert!(
        matches!(
            error,
            crate::webassembly::Error::WrongBuiltinFunctionSignature { ref name, .. }
                if name == "_try"
        ),
        "{error:?}"
    );
}
