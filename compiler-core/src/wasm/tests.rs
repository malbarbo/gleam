#[test]
fn string_unescape() {
    use crate::wasm::string;
    assert_eq!(
        string::unescape(r#"sure \\ \n \"its\" \t works! \u{263A}, \r or \f not..."#),
        "sure \\ \n \"its\" \t works! ☺, \r or \x0C not..."
    );
}

#[test]
fn int_parse() {
    use crate::wasm::integer;
    assert_eq!(integer::parse("0b1_0_1"), 5);
    assert_eq!(integer::parse("0o7_654_321_0"), 16434824);
    assert_eq!(integer::parse("0xAbc_DEF_1"), 180150001);
    assert_eq!(integer::parse("759_012"), 759_012);
}

#[test]
fn bool() {
    test_file("bool.gleam");
}

#[test]
fn int() {
    test_file("int.gleam");
}

#[test]
fn float() {
    test_file("float.gleam");
}

#[test]
fn string() {
    test_file("string.gleam");
}

#[cfg(test)]
fn test_file(file: &str) {
    let mut case = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    case.extend(["src", "wasm", "tests", file].iter());
    let code = std::fs::read_to_string(case).unwrap();
    let _ = build_and_run(&code).success();
}

#[cfg(test)]
fn build_and_run(code: &str) -> assert_cmd::assert::Assert {
    use assert_cmd::prelude::*;
    use std::{self, fs, process::Command};
    use tempdir::TempDir;

    let dir = TempDir::new("gleam").unwrap();

    fs::write(dir.path().join("gleam.toml"), "name=\"a\"").unwrap();

    let src = dir.path().join("src");
    fs::create_dir(&src).unwrap();
    fs::write(src.join("a.gleam"), code).unwrap();

    assert!(Command::cargo_bin("gleam")
        .unwrap()
        .args(["build", "--target", "wasm"])
        .current_dir(&dir)
        .status()
        .unwrap()
        .success());

    let wasm = dir.path().join("out.wasm");

    Command::new("wasmtime")
        .args(["-W", "all-proposals=y", wasm.to_str().unwrap()])
        .assert()
}
