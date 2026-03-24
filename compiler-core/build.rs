use camino::Utf8Path;
use std::process::Command;

const BUILTINS: &str = "builtins-wasm";

fn main() {
    println!("cargo:rerun-if-changed=../{BUILTINS}/src/lib.rs");

    capnpc::CompilerCommand::new()
        .file("schema.capnp")
        .output_path("generated/")
        .run()
        .expect("compiling schema.capnp");

    assert!(
        Command::new("cargo")
            .args(["build", "--release", "--target", "wasm32-unknown-unknown"])
            .env("RUSTFLAGS", "-C link-arg=--no-merge-data-segments")
            .current_dir(Utf8Path::new("..").join(BUILTINS))
            .status()
            .unwrap_or_else(|_| panic!("Building {BUILTINS}"))
            .success()
    );
}
