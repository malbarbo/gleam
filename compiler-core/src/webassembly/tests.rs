use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::{
    analyse::TargetSupport,
    ast::TypedModule,
    build::{Origin, Target},
    config::PackageConfig,
    line_numbers::LineNumbers,
    type_::PRELUDE_MODULE_NAME,
    uid::UniqueIdGenerator,
    warning::{TypeWarningEmitter, WarningEmitter},
};
use camino::Utf8PathBuf;
use ecow::EcoString;

static TEST_COUNTER: AtomicU64 = AtomicU64::new(0);

pub fn compile(
    src: &str,
    deps: Vec<(&str, &str, &str)>,
) -> (TypedModule, HashMap<EcoString, TypedModule>) {
    let mut modules = im::HashMap::new();
    let ids = UniqueIdGenerator::new();
    let _ = modules.insert(
        PRELUDE_MODULE_NAME.into(),
        crate::type_::build_prelude(&ids),
    );
    let mut direct_dependencies = HashMap::from_iter(vec![]);
    let mut dep_modules = HashMap::new();

    deps.iter().for_each(|(dep_package, dep_name, dep_src)| {
        let mut dep_config = PackageConfig::default();
        dep_config.name = (*dep_package).into();
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
    let mut config = PackageConfig::default();
    config.name = "thepackage".into();

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
    let mut all_line_numbers: HashMap<EcoString, LineNumbers> = deps
        .iter()
        .map(|(_, name, dep_src)| (EcoString::from(*name), LineNumbers::new(dep_src)))
        .collect();
    let (main_module, dep_modules) = compile(src, deps);
    let line_numbers = LineNumbers::new(src);
    let _ = all_line_numbers.insert(main_module.name.clone(), line_numbers.clone());
    let mut all_modules: HashMap<_, _> = dep_modules
        .iter()
        .map(|(name, m)| (name.clone(), m))
        .collect();
    let _ = all_modules.insert(main_module.name.clone(), &main_module);
    crate::webassembly::module(&main_module, &line_numbers, &all_modules, &all_line_numbers)
        .expect("wasm codegen failed")
}

/// Extract type names from the wasm binary's name section.
pub fn wasm_type_names(wasm_bytes: &[u8]) -> Vec<String> {
    let mut names = vec![];
    for payload in wasmparser::Parser::new(0).parse_all(wasm_bytes) {
        if let wasmparser::Payload::CustomSection(section) = payload.unwrap()
            && let wasmparser::KnownCustom::Name(name_section) = section.as_known()
        {
            for subsection in name_section {
                if let wasmparser::Name::Type(type_names) = subsection.unwrap() {
                    for naming in type_names {
                        let naming = naming.unwrap();
                        names.push(naming.name.to_string());
                    }
                }
            }
        }
    }
    names
}

pub fn compile_wasm_error(src: &str) -> crate::webassembly::Error {
    let (main_module, _) = compile(src, vec![]);
    let line_numbers = LineNumbers::new(src);
    let main_ref: &TypedModule = &main_module;
    let all_modules = HashMap::from([(main_module.name.clone(), main_ref)]);
    let all_line_numbers = HashMap::from([(main_module.name.clone(), line_numbers.clone())]);
    crate::webassembly::module(&main_module, &line_numbers, &all_modules, &all_line_numbers)
        .expect_err("expected codegen error")
}

pub struct WasmOutput {
    #[allow(unused)]
    pub stdout: String,
    pub stderr: String,
    pub status: std::process::ExitStatus,
}

pub fn run_wasm(src: &str, deps: Vec<(&str, &str, &str)>) -> WasmOutput {
    let wasm_bytes = compile_wasm(src, deps);
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

pub fn compile_validate_error(src: &str, int: &str, float: &str) -> crate::webassembly::Error {
    let (main_module, _) = compile(src, vec![]);
    let main_ref: &TypedModule = &main_module;
    let all_modules = HashMap::from([(main_module.name.clone(), main_ref)]);
    crate::webassembly::validate_module(&main_module, &all_modules, int, float)
        .expect_err("expected validation error")
}

macro_rules! assert_wasm_validate_error {
    ($src:expr, $int:expr, $float:expr $(,)?) => {{
        let error = super::compile_validate_error($src, $int, $float);
        let output = format!(
            "----- SOURCE CODE\n{}\n\n----- CONFIG\nint={} float={}\n\n----- ERROR\n{:?}",
            $src, $int, $float, error
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};
}

mod bools;
mod case_;
mod consts;
mod custom_types;
mod echo;
mod errors;
mod fail;
mod functions;
mod lists;
mod memory;
mod numbers;
mod strings;
mod tuples;
