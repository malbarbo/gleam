// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2018 The Gleam contributors

use camino::Utf8Path;
use std::process::Command;

const BUILTINS: &str = "builtins-wasm";

fn main() {
    println!("cargo:rerun-if-changed=../{BUILTINS}/src");
    println!("cargo:rerun-if-changed=../{BUILTINS}/Cargo.toml");

    assert!(
        Command::new("cargo")
            .args(["build", "--release", "--target", "wasm32-unknown-unknown"])
            .env("RUSTFLAGS", "-C link-arg=--no-merge-data-segments")
            // The flags and the target directory of the outer build (for
            // example, the instrumentation of a coverage build) are not for
            // the builtins, which are included from their own target.
            .env_remove("CARGO_ENCODED_RUSTFLAGS")
            .env_remove("CARGO_TARGET_DIR")
            .current_dir(Utf8Path::new("..").join(BUILTINS))
            .status()
            .unwrap_or_else(|_| panic!("Building {BUILTINS}"))
            .success()
    );
}
