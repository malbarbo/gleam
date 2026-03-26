# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is `gleam-core`, the core library of the Gleam compiler. It handles parsing, type-checking, and code generation for multiple targets (Erlang, JavaScript, WebAssembly). It lives in a Cargo workspace rooted at `../` alongside `gleam-bin` (CLI entry point), `compiler-cli`, `compiler-wasm` (browser-targeted wrapper), and `builtins-wasm` (low-level WASM runtime).

## Build Commands

All commands run from the workspace root (`../`):

```bash
cargo build --release              # Build the compiler
cargo test --quiet                 # Run unit tests (all crates)
cargo test --quiet -p gleam-core   # Run only compiler-core tests
cargo clippy                       # Lint
make install                       # Build and install gleam binary to PATH
make test                          # Full test suite (unit + integration + language)
make language-test                 # Language integration tests only
```

Run a single test:
```bash
cargo test --quiet -p gleam-core test_name
```

Update snapshots (uses `insta` crate):
```bash
cargo insta review
```

Watch mode:
```bash
make test-watch                    # Re-run unit tests on file changes
```

## Build Prerequisites

The build script (`build.rs`) does two things automatically during `cargo build`:
1. Compiles `schema.capnp` via Cap'n Proto (`capnpc`) into `generated/schema_capnp.rs`
2. Builds `builtins-wasm` for `wasm32-unknown-unknown` target — requires the `wasm32-unknown-unknown` rustup target installed

The compiled builtins binary is embedded into `webassembly.rs` via `include_bytes!`.

## Architecture

### Code Generation Pipeline

Gleam source → `parse` → AST → `analyse` (type-checking) → `TypedModule` → code generators:
- `erlang.rs` → `.erl` + `.hrl` files
- `javascript.rs` → `.mjs` + `.d.mts` files
- `webassembly.rs` → `.wasm` binary

`codegen.rs` orchestrates all three generators. Each has a `render()` method that takes `&[Module]` and writes output files.

### WebAssembly Code Generation (`webassembly.rs`)

The largest module (~6300 lines). Key design:

- **Entry point**: `pub fn module()` creates a `Generator`, calls `generate()`, then assembles WASM sections
- **Generator struct**: holds type maps, function sets, string interning tables, scope chains, and section builders
- **Type mapping**: Gleam types → `CustomType` enum (ExternalI32/Enum/Struct/Union) → `WasmType` (Array/Function/List/Struct/Union) → WASM binary types via `wasm-encoder`
- **Configurable numeric types**: `IntType` (I32/I64) and `FloatType` (F32/F64) with macro-dispatched operations (`int_op!`)
- **Builtins**: Runtime functions from `builtins-wasm` crate are linked via the `BUILTINS_WASM` constant and parsed with `walrus`/`wasmparser`
- **Scope management**: Linked-list `Scope` enum for lexical variable lookup; `Locals` for WASM local index allocation

### Key Crate Dependencies

- `wasm-encoder` / `wasmparser` / `walrus` — WASM binary generation and manipulation
- `insta` — snapshot testing (test outputs in `snapshots/` dirs)
- `ecow::EcoString` — used throughout instead of `String` for clone-on-write efficiency
- `im` — immutable data structures (HashMap, etc.) used in type system
- `capnp` — binary metadata serialization format

### Test Organization

Tests use `insta` snapshot testing. Each codegen target has test modules under `src/{erlang,javascript}/tests/` with `snapshots/` directories containing expected outputs. Type system tests are in `src/type_/tests/`.
