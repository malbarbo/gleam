use std::{path::Path, process::Command};

const BUILTINS: &str = "builtins-wasm";

fn main() {
    println!("cargo:rerun-if-changed=../{BUILTINS}/src/lib.rs");

    capnpc::CompilerCommand::new()
        .file("schema.capnp")
        .output_path("generated/")
        .run()
        .expect("compiling schema.capnp");

    Command::new("cargo")
        .args(["build", "--release", "--target", "wasm32-unknown-unknown"])
        .current_dir(Path::new("..").join(BUILTINS))
        .status()
        .expect(&format!("Building {BUILTINS}"));
}
