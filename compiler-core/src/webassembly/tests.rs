// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::{
    analyse::TargetSupport,
    ast::TypedModule,
    build::{Origin, Target},
    config::{PackageConfig, WasmFloat, WasmInt, WebAssemblyConfig},
    type_::PRELUDE_MODULE_NAME,
    uid::UniqueIdGenerator,
    warning::{TypeWarningEmitter, WarningEmitter},
};
use camino::Utf8PathBuf;
use ecow::EcoString;
use src_span::LineNumbers;

static TEST_COUNTER: AtomicU64 = AtomicU64::new(0);

/// The package of the module of a test: the standard library, which reaches
/// the builtins, so the tests can declare them.
pub const TEST_PACKAGE: &str = crate::STDLIB_PACKAGE_NAME;

/// The configuration with `int` and `float`, the defaults otherwise.
pub fn wasm_config(int: WasmInt, float: WasmFloat) -> WebAssemblyConfig {
    WebAssemblyConfig {
        int,
        float,
        ..WebAssemblyConfig::default()
    }
}

pub fn compile(
    src: &str,
    deps: Vec<(&str, &str, &str)>,
) -> (TypedModule, HashMap<EcoString, TypedModule>) {
    compile_in(TEST_PACKAGE, src, deps)
}

/// Like `compile`, with the module in `package`.
pub fn compile_in(
    package: &str,
    src: &str,
    deps: Vec<(&str, &str, &str)>,
) -> (TypedModule, HashMap<EcoString, TypedModule>) {
    let mut modules = imbl::HashMap::new();
    let ids = UniqueIdGenerator::new();
    let _ = modules.insert(
        PRELUDE_MODULE_NAME.into(),
        crate::type_::build_prelude(&ids),
    );
    let mut direct_dependencies = HashMap::from_iter(vec![]);
    let mut dep_modules = HashMap::new();

    deps.iter().for_each(|(dep_package, dep_name, dep_src)| {
        let dep_config = PackageConfig {
            name: (*dep_package).into(),
            ..Default::default()
        };
        let parsed = crate::parse::parse_module(
            Utf8PathBuf::from("test/path"),
            dep_src,
            &WarningEmitter::null(),
        )
        .expect("dep syntax error");
        let mut ast = parsed.module;
        ast.name = (*dep_name).into();
        let line_numbers = LineNumbers::new(dep_src);

        let dep = crate::analyse::ModuleAnalyzerConstructor::<()> {
            target: Target::WebAssembly,
            ids: &ids,
            origin: Origin::Src,
            importable_modules: &modules,
            warnings: &TypeWarningEmitter::null(),
            direct_dependencies: &HashMap::new(),
            dev_dependencies: &std::collections::HashSet::new(),
            target_support: TargetSupport::Enforced,
            package_config: &dep_config,
        }
        .infer_module(ast, line_numbers, "".into())
        .expect("should successfully infer");
        let _ = modules.insert((*dep_name).into(), dep.type_info.clone());
        let _ = direct_dependencies.insert((*dep_package).into(), ());
        let _ = dep_modules.insert(EcoString::from(*dep_name), dep);
    });

    let parsed =
        crate::parse::parse_module(Utf8PathBuf::from("test/path"), src, &WarningEmitter::null())
            .expect("syntax error");
    let mut ast = parsed.module;
    ast.name = "my/mod".into();
    let line_numbers = LineNumbers::new(src);
    let config = PackageConfig {
        name: package.into(),
        ..Default::default()
    };

    let module = crate::analyse::ModuleAnalyzerConstructor::<()> {
        target: Target::WebAssembly,
        ids: &ids,
        origin: Origin::Src,
        importable_modules: &modules,
        warnings: &TypeWarningEmitter::null(),
        direct_dependencies: &direct_dependencies,
        dev_dependencies: &std::collections::HashSet::new(),
        target_support: TargetSupport::NotEnforced,
        package_config: &config,
    }
    .infer_module(ast, line_numbers, "src/module.gleam".into())
    .expect("should successfully infer");

    (module, dep_modules)
}

pub fn compile_wasm(src: &str, deps: Vec<(&str, &str, &str)>) -> Vec<u8> {
    compile_wasm_with_config(src, deps, WebAssemblyConfig::default())
}

pub fn compile_wasm_with_config(
    src: &str,
    deps: Vec<(&str, &str, &str)>,
    config: WebAssemblyConfig,
) -> Vec<u8> {
    compile_wasm_in(TEST_PACKAGE, src, deps, config)
}

/// Like `compile_wasm_with_config`, with the module in `package`.
pub fn compile_wasm_in(
    package: &str,
    src: &str,
    deps: Vec<(&str, &str, &str)>,
    config: WebAssemblyConfig,
) -> Vec<u8> {
    codegen_in(package, src, deps, config).expect("wasm codegen failed")
}

fn codegen_in(
    package: &str,
    src: &str,
    deps: Vec<(&str, &str, &str)>,
    config: WebAssemblyConfig,
) -> Result<Vec<u8>, crate::webassembly::Error> {
    let mut all_line_numbers: HashMap<EcoString, LineNumbers> = deps
        .iter()
        .map(|(_, name, dep_src)| (EcoString::from(*name), LineNumbers::new(dep_src)))
        .collect();
    let (main_module, dep_modules) = compile_in(package, src, deps);
    let line_numbers = LineNumbers::new(src);
    let _ = all_line_numbers.insert(main_module.name.clone(), line_numbers.clone());
    let mut all_modules: HashMap<_, _> = dep_modules
        .iter()
        .map(|(name, m)| (name.clone(), m))
        .collect();
    let _ = all_modules.insert(main_module.name.clone(), &main_module);
    crate::webassembly::module(
        &main_module,
        &line_numbers,
        &all_modules,
        &all_line_numbers,
        &crate::webassembly::runtime_modules(),
        config,
    )
}

/// Extract type names from the wasm binary's name section.
pub fn wasm_type_names(wasm_bytes: &[u8]) -> Vec<String> {
    let mut names = vec![];
    for payload in wasmparser::Parser::new(0).parse_all(wasm_bytes) {
        if let wasmparser::Payload::CustomSection(section) = payload.expect("valid wasm")
            && let wasmparser::KnownCustom::Name(name_section) = section.as_known()
        {
            for subsection in name_section {
                if let wasmparser::Name::Type(type_names) = subsection.expect("valid wasm") {
                    for naming in type_names {
                        let naming = naming.expect("valid wasm");
                        names.push(naming.name.to_string());
                    }
                }
            }
        }
    }
    names
}

/// The number of fields of each named struct type of the wasm binary.
pub fn wasm_struct_fields(wasm_bytes: &[u8]) -> HashMap<String, usize> {
    let mut types = vec![];
    let mut names = vec![];
    for payload in wasmparser::Parser::new(0).parse_all(wasm_bytes) {
        let payload = payload.expect("valid wasm");
        if let wasmparser::Payload::TypeSection(reader) = &payload {
            for group in reader.clone() {
                for type_ in group.expect("valid wasm").types() {
                    let fields = match &type_.composite_type.inner {
                        wasmparser::CompositeInnerType::Struct(struct_) => {
                            Some(struct_.fields.len())
                        }
                        wasmparser::CompositeInnerType::Func(_)
                        | wasmparser::CompositeInnerType::Array(_)
                        | wasmparser::CompositeInnerType::Cont(_) => None,
                    };
                    types.push(fields);
                }
            }
        }
        if let wasmparser::Payload::CustomSection(section) = &payload
            && let wasmparser::KnownCustom::Name(name_section) = section.as_known()
        {
            for subsection in name_section {
                if let wasmparser::Name::Type(type_names) = subsection.expect("valid wasm") {
                    for naming in type_names {
                        let naming = naming.expect("valid wasm");
                        names.push((naming.index, naming.name.to_string()));
                    }
                }
            }
        }
    }
    names
        .into_iter()
        .filter_map(|(index, name)| Some((name, (*types.get(index as usize)?)?)))
        .collect()
}

/// Extract function names from the wasm binary's name section.
pub fn wasm_function_names(wasm_bytes: &[u8]) -> Vec<String> {
    let mut names = vec![];
    for payload in wasmparser::Parser::new(0).parse_all(wasm_bytes) {
        if let wasmparser::Payload::CustomSection(section) = payload.expect("valid wasm")
            && let wasmparser::KnownCustom::Name(name_section) = section.as_known()
        {
            for subsection in name_section {
                if let wasmparser::Name::Function(function_names) = subsection.expect("valid wasm")
                {
                    for naming in function_names {
                        names.push(naming.expect("valid wasm").name.to_string());
                    }
                }
            }
        }
    }
    names
}

/// Extract global names from the wasm binary's name section.
pub fn wasm_global_names(wasm_bytes: &[u8]) -> Vec<String> {
    let mut names = vec![];
    for payload in wasmparser::Parser::new(0).parse_all(wasm_bytes) {
        if let wasmparser::Payload::CustomSection(section) = payload.expect("valid wasm")
            && let wasmparser::KnownCustom::Name(name_section) = section.as_known()
        {
            for subsection in name_section {
                if let wasmparser::Name::Global(global_names) = subsection.expect("valid wasm") {
                    for naming in global_names {
                        names.push(naming.expect("valid wasm").name.to_string());
                    }
                }
            }
        }
    }
    names
}

/// Count the calls through function references (`call_ref` and
/// `return_call_ref`) in the code of the wasm binary.
pub fn count_ref_calls(wasm_bytes: &[u8]) -> usize {
    let mut count = 0;
    for_each_operator(wasm_bytes, |_, op| {
        if let wasmparser::Operator::CallRef { .. } | wasmparser::Operator::ReturnCallRef { .. } =
            op
        {
            count += 1;
        }
    });
    count
}

/// Calls `f` with each operator in the code of the wasm binary and the name
/// of the function that contains it.
pub fn for_each_operator(wasm_bytes: &[u8], mut f: impl FnMut(&str, wasmparser::Operator<'_>)) {
    let mut names = HashMap::new();
    for payload in wasmparser::Parser::new(0).parse_all(wasm_bytes) {
        if let wasmparser::Payload::CustomSection(section) = payload.expect("valid wasm")
            && let wasmparser::KnownCustom::Name(name_section) = section.as_known()
        {
            for subsection in name_section {
                if let wasmparser::Name::Function(function_names) = subsection.expect("valid wasm")
                {
                    for naming in function_names {
                        let naming = naming.expect("valid wasm");
                        let _ = names.insert(naming.index, naming.name.to_string());
                    }
                }
            }
        }
    }
    let mut index = 0;
    for payload in wasmparser::Parser::new(0).parse_all(wasm_bytes) {
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
            let name = names.get(&index).map_or("", String::as_str);
            let mut reader = body.get_operators_reader().expect("valid wasm");
            while !reader.eof() {
                f(name, reader.read().expect("valid wasm"));
            }
            index += 1;
        }
    }
}

pub fn compile_wasm_error(src: &str) -> crate::webassembly::Error {
    compile_wasm_error_with(src, vec![])
}

/// Like `compile_wasm_error`, with the modules `deps`.
pub fn compile_wasm_error_with(
    src: &str,
    deps: Vec<(&str, &str, &str)>,
) -> crate::webassembly::Error {
    codegen_in(TEST_PACKAGE, src, deps, WebAssemblyConfig::default())
        .expect_err("expected codegen error")
}

/// Like `compile_wasm_error`, with the module in `package`.
pub fn compile_wasm_error_in(package: &str, src: &str) -> crate::webassembly::Error {
    codegen_in(package, src, vec![], WebAssemblyConfig::default())
        .expect_err("expected codegen error")
}

pub struct WasmOutput {
    #[allow(unused)]
    pub stdout: String,
    pub stderr: String,
    pub status: std::process::ExitStatus,
}

pub fn run_wasm(src: &str, deps: Vec<(&str, &str, &str)>) -> WasmOutput {
    run_wasm_with_config(src, deps, WebAssemblyConfig::default())
}

pub fn run_wasm_with_config(
    src: &str,
    deps: Vec<(&str, &str, &str)>,
    config: WebAssemblyConfig,
) -> WasmOutput {
    run_wasm_in(TEST_PACKAGE, src, deps, config)
}

/// Like `run_wasm_with_config`, with the module in `package`.
pub fn run_wasm_in(
    package: &str,
    src: &str,
    deps: Vec<(&str, &str, &str)>,
    config: WebAssemblyConfig,
) -> WasmOutput {
    let wasm_bytes = compile_wasm_in(package, src, deps, config);
    let id = TEST_COUNTER.fetch_add(1, Ordering::Relaxed);
    let tmp = std::env::temp_dir().join(format!("gleam_wasm_test_{id}.wasm"));
    std::fs::write(&tmp, &wasm_bytes).expect("failed to write temp wasm file");
    let output = std::process::Command::new("wasmtime")
        .args(["-W", "function-references=y", "-W", "gc=y"])
        .arg(&tmp)
        .output()
        .expect("wasmtime not found — install it to run WASM tests");
    if output.status.success() {
        let _ = std::fs::remove_file(&tmp);
    }
    WasmOutput {
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        status: output.status,
    }
}

/// Like `run_wasm`, with `string = "JSString"`, in Node.js with the host of
/// `HOST`, next to the JavaScript modules `files` (as `(name, source)`).
pub fn run_wasm_js(src: &str, files: &[(&str, &str)]) -> WasmOutput {
    let config = WebAssemblyConfig {
        string: crate::config::WasmString::JsString,
        ..WebAssemblyConfig::default()
    };
    let wasm_bytes = compile_wasm_in(TEST_PACKAGE, src, vec![], config);
    let id = TEST_COUNTER.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("gleam_wasm_js_test_{id}"));
    std::fs::create_dir_all(&dir).expect("failed to create the test directory");
    let run = r#"import { run } from "./host.mjs";
await run(new URL("./main.wasm", import.meta.url));
"#;
    let files = [("host.mjs", crate::webassembly::HOST), ("run.mjs", run)]
        .into_iter()
        .chain(files.iter().copied());
    for (name, source) in files {
        std::fs::write(dir.join(name), source).expect("failed to write a test file");
    }
    std::fs::write(dir.join("main.wasm"), &wasm_bytes).expect("failed to write the wasm file");
    let output = std::process::Command::new("node")
        .arg(dir.join("run.mjs"))
        .output()
        .expect("node not found — install it to run the WASM tests of JavaScript strings");
    if output.status.success() {
        let _ = std::fs::remove_dir_all(&dir);
    }
    WasmOutput {
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        status: output.status,
    }
}

/// Compile and run WASM, assert it succeeds.
#[track_caller]
pub fn run_ok(src: &str) {
    let result = run_wasm(src, vec![]);
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

/// Compile and run WASM with I64 int type, assert it succeeds.
#[track_caller]
pub fn run_ok_i64(src: &str) {
    let result = run_wasm_with_config(src, vec![], wasm_config(WasmInt::I64, WasmFloat::F64));
    assert!(
        result.status.success(),
        "WASM (I64) execution failed:\n{}",
        result.stderr
    );
}

/// Compile and run WASM, assert it fails.
#[track_caller]
pub fn run_fail(src: &str) {
    let result = run_wasm(src, vec![]);
    assert!(
        !result.status.success(),
        "Expected WASM to fail but it succeeded"
    );
}

/// Compile and run WASM, assert it fails and stderr contains expected string.
#[track_caller]
pub fn run_fail_stderr(src: &str, expected: &str) {
    let result = run_wasm(src, vec![]);
    assert!(
        !result.status.success(),
        "Expected WASM to fail but it succeeded"
    );
    assert!(
        result.stderr.contains(expected),
        "Expected stderr to contain {:?}, got:\n{}",
        expected,
        result.stderr
    );
}

macro_rules! assert_wasm_echo {
    ($src:expr $(,)?) => {{
        let result = super::run_wasm($src, vec![]);
        assert!(
            result.status.success(),
            "WASM execution failed:\n{}",
            result.stderr
        );
        let output = format!(
            "----- SOURCE CODE\n{}\n\n----- STDERR\n{}",
            $src, result.stderr
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};
}

macro_rules! assert_wasm_error {
    ($src:expr $(,)?) => {{
        let error = super::compile_wasm_error($src);
        let output = format!("----- SOURCE CODE\n{}\n\n----- ERROR\n{:?}", $src, error);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};
}

pub fn compile_validate_error(src: &str, config: WebAssemblyConfig) -> crate::webassembly::Error {
    let (main_module, _) = compile(src, vec![]);
    let main_ref: &TypedModule = &main_module;
    let all_modules = HashMap::from([(main_module.name.clone(), main_ref)]);
    crate::webassembly::validate_module(&main_module, &all_modules, config)
        .expect_err("expected validation error")
}

macro_rules! assert_wasm_validate_error {
    ($src:expr, $int:expr, $float:expr $(,)?) => {{
        let int = $int;
        let float = $float;
        let error = super::compile_validate_error($src, super::wasm_config(int, float));
        let output = format!(
            "----- SOURCE CODE\n{}\n\n----- CONFIG\nint={:?} float={:?}\n\n----- ERROR\n{:?}",
            $src, int, float, error
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};
}

mod arrays;
mod bit_arrays;
mod bools;
mod captures;
mod case_;
mod closures;
mod consts;
mod custom_types;
mod echo;
mod errors;
mod fail;
mod function_fields;
mod functions;
mod glue;
mod host;
mod js_strings;
mod link;
mod lists;
mod local_closures;
mod memory;
mod numbers;
mod runtime;
mod specialization;
mod stdlib;
mod strings;
mod tail_calls;
mod tuples;
mod units;
