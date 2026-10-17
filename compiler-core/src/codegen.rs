// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2020 The Gleam contributors

use crate::{
    Result,
    ast::TypedModule,
    build::{
        ErlangAppCodegenConfiguration, ErlangOutput, Module, module_erlang_name,
        package_compiler::StdlibPackage,
    },
    config::PackageConfig,
    erlang,
    io::FileSystemWriter,
    javascript::{self, ModuleConfig},
    webassembly,
};
use ecow::EcoString;
use erlang::escape_atom_string;
use erlang_generation::{ErlangBinaryBuilder, ErlangSourceBuilder};
use itertools::Itertools;
use src_span::LineNumbers;
use std::collections::HashMap;
use std::fmt::Debug;

use camino::Utf8Path;

/// A code generator that creates a .erl Erlang module and record header files
/// for each Gleam module in the package.
#[derive(Debug)]
pub struct Erlang<'a> {
    build_directory: &'a Utf8Path,
    include_directory: &'a Utf8Path,
}

impl<'a> Erlang<'a> {
    pub fn new(build_directory: &'a Utf8Path, include_directory: &'a Utf8Path) -> Self {
        Self {
            build_directory,
            include_directory,
        }
    }

    pub fn render<Writer: FileSystemWriter>(
        &self,
        output: ErlangOutput,
        writer: Writer,
        modules: &[Module],
        root: &Utf8Path,
    ) -> Result<()> {
        for module in modules {
            let erl_name = module.erlang_name();
            self.erlang_module(output, &writer, module, &erl_name, root)?;
            self.erlang_record_headers(&writer, module, &erl_name)?;
        }
        Ok(())
    }

    fn erlang_module<Writer: FileSystemWriter>(
        &self,
        output: ErlangOutput,
        writer: &Writer,
        module: &Module,
        erl_name: &str,
        root: &Utf8Path,
    ) -> Result<()> {
        let line_numbers = LineNumbers::new(&module.code);

        match output {
            ErlangOutput::Binary => {
                let name = format!("{erl_name}.abstr");
                let path = self.build_directory.join(&name);

                let builder = ErlangBinaryBuilder::new(Some(&line_numbers));
                let output = erlang::module(builder, &module.ast, &line_numbers, root);
                tracing::debug!(name = ?name, "Generated Erlang module");
                writer.write_bytes(&path, &output)
            }
            ErlangOutput::Textual => {
                let name = format!("{erl_name}.erl");
                let path = self.build_directory.join(&name);

                let builder = ErlangSourceBuilder::default();
                let output = erlang::module(builder, &module.ast, &line_numbers, root);
                tracing::debug!(name = ?name, "Generated Erlang module");
                writer.write(&path, &output)
            }
        }
    }

    fn erlang_record_headers<Writer: FileSystemWriter>(
        &self,
        writer: &Writer,
        module: &Module,
        erl_name: &str,
    ) -> Result<()> {
        for (name, text) in erlang::records(&module.ast) {
            let name = format!("{erl_name}_{name}.hrl");
            tracing::debug!(name = ?name, "Generated Erlang header");
            writer.write(&self.include_directory.join(name), &text)?;
        }
        Ok(())
    }
}

/// A code generator that creates a .app Erlang application file for the package
#[derive(Debug)]
pub struct ErlangApp<'a> {
    output_directory: &'a Utf8Path,
    config: &'a ErlangAppCodegenConfiguration,
}

impl<'a> ErlangApp<'a> {
    pub fn new(output_directory: &'a Utf8Path, config: &'a ErlangAppCodegenConfiguration) -> Self {
        Self {
            output_directory,
            config,
        }
    }

    pub fn render<Writer: FileSystemWriter>(
        &self,
        writer: Writer,
        config: &PackageConfig,
        compiled_modules: Vec<EcoString>,
    ) -> Result<()> {
        fn tuple(key: &str, value: &str) -> String {
            format!("    {{{key}, {value}}},\n")
        }

        let path = self.output_directory.join(format!("{}.app", config.name));

        let start_module = match config.erlang.application_start_module.as_ref() {
            None => "".into(),
            Some(module) => {
                let module = module_erlang_name(module);
                let argument = match config.erlang.application_start_argument.as_ref() {
                    Some(argument) => argument.as_str(),
                    None => "[]",
                };
                tuple("mod", &format!("{{'{module}', {argument}}}"))
            }
        };

        let modules = compiled_modules
            .into_iter()
            .sorted()
            .map(escape_atom_string)
            .join(",\n               ");

        // TODO: When precompiling for production (i.e. as a precompiled hex
        // package) we will need to exclude the dev deps.
        let applications = config
            .dependencies
            .keys()
            .chain(
                config
                    .dev_dependencies
                    .keys()
                    .take_while(|_| self.config.include_dev_deps),
            )
            // TODO: test this!
            .map(|name| self.config.package_name_overrides.get(name).unwrap_or(name))
            .chain(config.erlang.extra_applications.iter())
            .sorted()
            .join(",\n                    ");

        let text = format!(
            r#"{{application, {package}, [
{start_module}    {{vsn, "{version}"}},
    {{applications, [{applications}]}},
    {{description, "{description}"}},
    {{modules, [{modules}]}},
    {{registered, []}}
]}}.
"#,
            applications = applications,
            description = config.description,
            modules = modules,
            package = config.name,
            start_module = start_module,
            version = config.version,
        );

        writer.write(&path, &text)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeScriptDeclarations {
    None,
    Emit,
}

#[derive(Debug)]
pub struct JavaScript<'a> {
    output_directory: &'a Utf8Path,
    prelude_location: &'a Utf8Path,
    project_root: &'a Utf8Path,
    typescript: TypeScriptDeclarations,
    source_map: bool,
}

impl<'a> JavaScript<'a> {
    pub fn new(
        output_directory: &'a Utf8Path,
        typescript: TypeScriptDeclarations,
        source_map: bool,
        prelude_location: &'a Utf8Path,
        project_root: &'a Utf8Path,
    ) -> Self {
        Self {
            prelude_location,
            output_directory,
            project_root,
            typescript,
            source_map,
        }
    }

    pub fn render(
        &self,
        writer: &impl FileSystemWriter,
        modules: &[Module],
        stdlib_package: StdlibPackage,
    ) -> Result<()> {
        for module in modules {
            let js_name = module.name.clone();
            if self.typescript == TypeScriptDeclarations::Emit {
                self.ts_declaration(writer, module, &js_name)?;
            }
            self.js_module(writer, module, &js_name, stdlib_package)?;
        }
        self.write_prelude(writer)?;
        Ok(())
    }

    fn write_prelude(&self, writer: &impl FileSystemWriter) -> Result<()> {
        let rexport = format!("export * from \"{}\";\n", self.prelude_location);
        let prelude_path = &self.output_directory.join("gleam.mjs");

        // This check skips unnecessary `gleam.mjs` writes which confuse
        // watchers and HMR build tools
        if !writer.exists(prelude_path) {
            writer.write(prelude_path, &rexport)?;
        }

        if self.typescript == TypeScriptDeclarations::Emit {
            let rexport = format!(
                "export * from \"{}\";\nexport type * from \"{}\";\n",
                self.prelude_location,
                self.prelude_location.as_str().replace(".mjs", ".d.mts")
            );
            let prelude_declaration_path = &self.output_directory.join("gleam.d.mts");

            // Type declaration may trigger badly configured watchers
            if !writer.exists(prelude_declaration_path) {
                writer.write(prelude_declaration_path, &rexport)?;
            }
        }

        Ok(())
    }

    fn ts_declaration(
        &self,
        writer: &impl FileSystemWriter,
        module: &Module,
        js_name: &str,
    ) -> Result<()> {
        let name = format!("{js_name}.d.mts");
        let path = self.output_directory.join(name);
        let output = javascript::ts_declaration(&module.ast);
        tracing::debug!(name = ?js_name, "Generated TS declaration");
        writer.write(&path, &output)
    }

    fn js_module(
        &self,
        writer: &impl FileSystemWriter,
        module: &Module,
        js_name: &str,
        stdlib_package: StdlibPackage,
    ) -> Result<()> {
        let name = format!("{js_name}.mjs");
        let path = self.output_directory.join(name);
        let line_numbers = LineNumbers::new(&module.code);
        let (output, source_map) = javascript::module(ModuleConfig {
            module: &module.ast,
            line_numbers: &line_numbers,
            path: &module.input_path,
            project_root: self.project_root,
            src: &module.code,
            typescript: self.typescript,
            source_map: self.source_map,
            stdlib_package,
        });
        tracing::debug!(name = ?js_name, "Generated js module");
        writer.write(&path, &output)?;

        if let Some(source_map) = source_map {
            let mut output = Vec::new();
            // We first write to a vector then build a string, hoping that
            // the `sourcemap` crate generated a valid sourcemap. If it
            // did not, it is a bug that should be reported.
            //
            // SourceMap currently does not support being written directly
            // to a string.
            source_map
                .to_writer(&mut output)
                .expect("Failed to write sourcemap to memory.");
            let content =
                String::from_utf8(output).expect("Sourcemap did not generate valid UTF-8.");
            let source_map_path = self.output_directory.join(format!("{js_name}.mjs.map"));
            tracing::debug!(path = ?source_map_path, name = ?js_name, "Emitting sourcemap for module");
            writer.write(&source_map_path, &content)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::{FileSystemReader, memory::InMemoryFileSystem};
    use std::collections::HashMap;

    #[test]
    fn app_file_includes_cached_modules() {
        // A warm rebuild recompiles nothing, so every module comes from the
        // `ebin` scan rather than being freshly compiled; they must still be
        // listed in the .app (https://github.com/gleam-lang/gleam/issues/5834).
        let fs = InMemoryFileSystem::new();
        let mut config = PackageConfig::default();
        config.name = "my_app".into();
        let codegen_config = ErlangAppCodegenConfiguration {
            include_dev_deps: false,
            package_name_overrides: HashMap::new(),
        };

        // Module names here are already in Erlang atom form (`@`, not `/`),
        // matching what a real `ebin` scan of `.beam` filenames would yield.
        ErlangApp::new(Utf8Path::new("/ebin"), &codegen_config)
            .render(
                fs.clone(),
                &config,
                vec!["my_app@one".into(), "my_app@two".into()],
            )
            .expect("render .app");

        let app = fs
            .read(Utf8Path::new("/ebin/my_app.app"))
            .expect("read .app");
        insta::assert_snapshot!(app);
    }
}

#[derive(Debug)]
pub struct WebAssembly<'a> {
    output_directory: &'a Utf8Path,
    config: crate::config::WebAssemblyConfig,
}

impl<'a> WebAssembly<'a> {
    pub fn new(output_directory: &'a Utf8Path, config: crate::config::WebAssemblyConfig) -> Self {
        Self {
            output_directory,
            config,
        }
    }

    /// Generates a WebAssembly module for each of `modules`. Each one is a
    /// whole program, so the modules it imports from other packages must be
    /// in `dependencies`.
    pub fn render(
        &self,
        writer: &impl FileSystemWriter,
        modules: &[Module],
        dependencies: &[Module],
    ) -> Result<()> {
        let all_modules: HashMap<_, _> = dependencies
            .iter()
            .chain(modules)
            .map(|m| (m.name.clone(), &m.ast))
            .collect();
        let all_line_numbers: HashMap<_, _> = dependencies
            .iter()
            .chain(modules)
            .map(|m| (m.name.clone(), LineNumbers::new(&m.code)))
            .collect();
        let sources: HashMap<_, _> = dependencies
            .iter()
            .chain(modules)
            .map(|m| (m.name.clone(), m))
            .collect();
        let runtime = webassembly::runtime_modules();
        for module in modules {
            self.types_module(writer, module)?;
            self.wasm_module(
                writer,
                module,
                &all_modules,
                &all_line_numbers,
                &runtime,
                &sources,
            )?
        }
        self.write_prelude(writer)
    }

    /// Writes the JavaScript module of the custom types of `module`, whose
    /// classes are the values of the custom types that the functions of the
    /// host receive and return.
    fn types_module(&self, writer: &impl FileSystemWriter, module: &Module) -> Result<()> {
        let path = self.output_directory.join(format!("{}.mjs", module.name));
        let line_numbers = LineNumbers::new(&module.code);
        writer.write(&path, &javascript::types_module(&module.ast, &line_numbers))
    }

    /// Writes the `gleam.mjs` of the package, which gives its FFI modules the
    /// JavaScript prelude, with the classes of the values they receive and
    /// return, and the host.
    fn write_prelude(&self, writer: &impl FileSystemWriter) -> Result<()> {
        let path = self.output_directory.join("gleam.mjs");
        if !writer.exists(&path) {
            writer.write(&path, webassembly::PACKAGE_PRELUDE)?;
        }
        Ok(())
    }

    fn wasm_module<Writer: FileSystemWriter>(
        &self,
        writer: &Writer,
        module: &Module,
        all_modules: &HashMap<EcoString, &TypedModule>,
        all_line_numbers: &HashMap<EcoString, LineNumbers>,
        runtime: &[webassembly::RuntimeModule],
        sources: &HashMap<EcoString, &Module>,
    ) -> Result<()> {
        let name = format!("{}.wasm", module.name);
        println!("Generating {name}");
        let path = self.output_directory.join(&name);
        let line_numbers = LineNumbers::new(&module.code);
        let output = webassembly::module(
            &module.ast,
            &line_numbers,
            all_modules,
            all_line_numbers,
            runtime,
            self.config,
        )
        .map_err(|error| {
            // The location of the error may be in another module.
            let module = error
                .module()
                .and_then(|name| sources.get(name).copied())
                .unwrap_or(module);
            crate::Error::WebAssembly {
                path: module.input_path.clone(),
                src: module.code.clone(),
                error,
            }
        })?;
        tracing::debug!(name = ?name, "Generated WebAssembly module");
        writer.write_bytes(&path, &output)?;

        if self.config.opt_level.is_enabled() {
            self.run_wasm_opt(&path)?;
        }

        Ok(())
    }

    fn run_wasm_opt(&self, path: &Utf8Path) -> Result<()> {
        let flag = self.config.opt_level.as_flag();
        let result = std::process::Command::new("wasm-opt")
            .args([
                flag,
                "--enable-gc",
                "--enable-reference-types",
                "--enable-tail-call",
                "-o",
            ])
            .arg(path.as_str())
            .arg(path.as_str())
            .status();

        match result {
            Ok(status) if status.success() => Ok(()),
            Ok(_) => Err(crate::Error::ShellCommand {
                program: "wasm-opt".into(),
                reason: crate::error::ShellCommandFailureReason::Unknown,
            }),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                Err(crate::Error::ShellProgramNotFound {
                    program: "wasm-opt".into(),
                    os: crate::error::OS::Other,
                })
            }
            Err(error) => Err(crate::Error::ShellCommand {
                program: "wasm-opt".into(),
                reason: crate::error::ShellCommandFailureReason::IoError(error.kind()),
            }),
        }
    }
}
