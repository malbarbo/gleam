use super::run_ok;

#[test]
fn large_inspect() {
    run_ok(
        r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

fn repeat(s: String, n: Int) -> String {
    case n {
        0 -> s
        _ -> repeat(s <> s, n - 1)
    }
}

pub fn main() {
    // Creates a string of 2^14 = 16384 'a' characters
    // inspect wraps it in quotes, writing ~16386 bytes to memory
    let big = repeat("a", 14)
    let s = inspect(big)
    assert s == "\"" <> big <> "\""
}
"#,
    );
}

#[test]
fn very_large_inspect() {
    run_ok(
        r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

fn repeat(s: String, n: Int) -> String {
    case n {
        0 -> s
        _ -> repeat(s <> s, n - 1)
    }
}

pub fn main() {
    // Creates a string of 2^21 = 2097152 'a' characters (~2MB)
    // inspect needs ~2MB which exceeds the 1.1MB memory
    let big = repeat("a", 21)
    let s = inspect(big)
    assert s == "\"" <> big <> "\""
}
"#,
    );
}
