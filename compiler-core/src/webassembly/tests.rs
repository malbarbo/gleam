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

static TEST_COUNTER: AtomicU64 = AtomicU64::new(0);

pub fn compile(src: &str, deps: Vec<(&str, &str, &str)>) -> TypedModule {
    let mut modules = im::HashMap::new();
    let ids = UniqueIdGenerator::new();
    let _ = modules.insert(
        PRELUDE_MODULE_NAME.into(),
        crate::type_::build_prelude(&ids),
    );
    let mut direct_dependencies = HashMap::from_iter(vec![]);

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
        let _ = modules.insert((*dep_name).into(), dep.type_info);
        let _ = direct_dependencies.insert((*dep_package).into(), ());
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

    module
}

pub fn compile_wasm(src: &str, deps: Vec<(&str, &str, &str)>) -> Vec<u8> {
    let ast = compile(src, deps);
    let line_numbers = LineNumbers::new(src);
    crate::webassembly::module(&ast, &line_numbers)
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
    let _ = std::fs::remove_file(&tmp);
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

mod bools;
mod case_;
mod consts;
mod custom_types;
mod echo;
mod fail;
mod functions;
mod lists;
mod numbers;
mod strings;
mod tuples;
