// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! The part of the WebAssembly target written in Gleam: the types that wasm
//! does not have (like a map that holds Gleam values) and their functions.
//! It plays the role of `prelude.mjs` on JavaScript. Programs reach it through
//! the `"builtins"` module, like the other builtins:
//!
//! ```gleam
//! @external(webassembly, "builtins", "Dict")
//! pub type Dict(key, value)
//!
//! @external(webassembly, "builtins", "dict_size")
//! pub fn size(dict: Dict(k, v)) -> Int
//! ```
//!
//! The external type is represented by the public type of the runtime with
//! the same name, and the external function is the public function of the
//! runtime with the same name. The names of the public types and functions
//! are unique in the runtime, and its functions don't share a name with the
//! Rust builtins.
//!
//! The modules of the runtime are analysed when a program is generated. They
//! import only the prelude and each other, and programs can't import them.

use std::collections::HashMap;

use camino::Utf8PathBuf;
use ecow::EcoString;
use src_span::LineNumbers;

use crate::{
    analyse::{ModuleAnalyzerConstructor, TargetSupport},
    ast::TypedModule,
    build::{Origin, Target},
    config::PackageConfig,
    type_::{self, PRELUDE_MODULE_NAME},
    uid::UniqueIdGenerator,
    warning::{TypeWarningEmitter, WarningEmitter},
};

/// The package of the runtime modules.
pub(super) const RUNTIME_PACKAGE: &str = "gleam_wasm_runtime";

/// The modules of the runtime, in an order where a module comes after the
/// modules it imports.
const SOURCES: &[(&str, &str)] = &[
    ("builtins/base", include_str!("runtime/base.gleam")),
    ("builtins/array", include_str!("runtime/array.gleam")),
    ("builtins/dict", include_str!("runtime/dict.gleam")),
    (
        "builtins/string_tree",
        include_str!("runtime/string_tree.gleam"),
    ),
    ("builtins/dynamic", include_str!("runtime/dynamic.gleam")),
    (
        "builtins/bit_array",
        include_str!("runtime/bit_array.gleam"),
    ),
];

/// The functions of the runtime that give `==`, `_hash` and the text of
/// `string.inspect` of one of its types, when they are not structural.
pub(super) struct Hooks {
    /// `fn(T, T) -> Bool`.
    pub(super) equal: &'static str,
    /// `fn(T) -> Int`, consistent with `equal`.
    pub(super) hash: &'static str,
    /// `fn(T) -> String`.
    pub(super) inspect: &'static str,
}

/// The hooks of the types of the runtime, by module and type name.
const HOOKS: &[(&str, &str, Hooks)] = &[
    (
        "builtins/dict",
        "Dict",
        Hooks {
            equal: "dict_equal",
            hash: "dict_hash",
            inspect: "dict_inspect",
        },
    ),
    (
        "builtins/string_tree",
        "StringTree",
        Hooks {
            equal: "string_tree_is_equal",
            hash: "string_tree_hash",
            inspect: "string_tree_inspect",
        },
    ),
    (
        "builtins/bit_array",
        "Bits",
        Hooks {
            equal: "bit_array_equal",
            hash: "bit_array_hash",
            inspect: "bit_array_inspect",
        },
    ),
    (
        "builtins/dynamic",
        "Dynamic",
        Hooks {
            equal: "dynamic_equal",
            hash: "dynamic_hash",
            inspect: "dynamic_inspect",
        },
    ),
];

/// The hooks of the type `name` of the module `module`.
pub(super) fn hooks(module: &str, name: &str) -> Option<&'static Hooks> {
    HOOKS
        .iter()
        .find(|(m, n, _)| *m == module && *n == name)
        .map(|(_, _, hooks)| hooks)
}

/// A module of the runtime, analysed.
#[derive(Debug)]
pub struct RuntimeModule {
    pub(super) module: TypedModule,
    pub(super) line_numbers: LineNumbers,
}

/// Analyses the modules of the runtime, once for all the modules compiled
/// with them, see `module`.
///
/// Panics when they don't type check, which is a bug of the compiler.
pub fn modules() -> Vec<RuntimeModule> {
    let ids = UniqueIdGenerator::new();
    let mut importable = imbl::HashMap::new();
    let _ = importable.insert(PRELUDE_MODULE_NAME.into(), type_::build_prelude(&ids));
    let config = PackageConfig {
        name: RUNTIME_PACKAGE.into(),
        ..Default::default()
    };
    let mut modules = vec![];
    for (name, source) in SOURCES {
        let path = Utf8PathBuf::from(format!("{name}.gleam"));
        let parsed = crate::parse::parse_module(path.clone(), source, &WarningEmitter::null())
            .unwrap_or_else(|error| panic!("runtime module {name} to parse: {error:?}"));
        let mut ast = parsed.module;
        ast.name = (*name).into();
        let line_numbers = LineNumbers::new(source);
        let module = ModuleAnalyzerConstructor::<()> {
            target: Target::WebAssembly,
            ids: &ids,
            origin: Origin::Src,
            importable_modules: &importable,
            warnings: &TypeWarningEmitter::null(),
            direct_dependencies: &HashMap::new(),
            dev_dependencies: &std::collections::HashSet::new(),
            target_support: TargetSupport::NotEnforced,
            package_config: &config,
        }
        .infer_module(ast, line_numbers.clone(), path)
        .into_result()
        .unwrap_or_else(|errors| panic!("runtime module {name} to type check: {errors:?}"));
        let _ = importable.insert(EcoString::from(*name), module.type_info.clone());
        modules.push(RuntimeModule {
            module,
            line_numbers,
        });
    }
    modules
}
