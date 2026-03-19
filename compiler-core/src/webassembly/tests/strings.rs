use super::run_ok;

#[test]
fn string_const() {
    run_ok(
        r#"
const mystring = "string"

pub fn main() {
    let assert "string" = mystring
}
"#,
    );
}

#[test]
fn string_concat() {
    run_ok(
        r#"
const mystring = "string"

pub fn main() {
    let assert "ab cd" = "ab" <> " " <> "cd"
    let assert "string" = mystring <> ""
    let assert "string" = "" <> mystring
    let assert "string-string" = mystring <> "-" <> "string"
}
"#,
    );
}

#[test]
fn string_comparison() {
    run_ok(
        r#"
pub fn main() {
    let assert True = "abc" == "ab" <> "c"
    let assert False = "ab" == "cd" <> "a"
    let assert True = "ab" != "cd" <> "a"
    let assert False = "ab" != "a" <> "b"
}
"#,
    );
}

#[test]
fn string_escape() {
    assert_wasm_echo!(
        r#"
pub fn main() {
    echo "\" \n \r \t \f \\ "
}
"#,
    );
}

#[test]
fn string_global_const() {
    run_ok(
        r#"
pub const s = "a"

pub fn main() {
    let assert "a" = s
}
"#,
    );
}

#[test]
fn string_prefix_basic() {
    run_ok(
        r#"
pub fn main() {
    assert case "Hello, World" {
        "Hello, " <> name -> name == "World"
        _ -> False
    }
}
"#,
    );
}

#[test]
fn string_prefix_empty() {
    run_ok(
        r#"
pub fn main() {
    assert case "anything" {
        "" <> rest -> rest == "anything"
        _ -> False
    }
}
"#,
    );
}

#[test]
fn string_prefix_with_assignment() {
    run_ok(
        r#"
pub fn main() {
    assert case "Hello, World" {
        "Hello, " as greeting <> name -> greeting == "Hello, " && name == "World"
        _ -> False
    }
}
"#,
    );
}

#[test]
fn string_prefix_no_match() {
    run_ok(
        r#"
pub fn main() {
    assert case "Goodbye" {
        "Hello, " <> _ -> False
        _ -> True
    }
}
"#,
    );
}
