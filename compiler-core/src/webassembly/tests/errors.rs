#[test]
fn unknown_external_type() {
    assert_wasm_error!(
        r#"
@external(webassembly, "builtins", "F32")
type MyFloat
"#,
    );
}

#[test]
fn unknown_external_module() {
    assert_wasm_error!(
        r#"
@external(webassembly, "other", "I32")
type Foo
"#,
    );
}

#[test]
fn unknown_builtin_function() {
    assert_wasm_error!(
        r#"
@external(webassembly, "builtins", "_unknown_func")
fn unknown(a: Int) -> Int

pub fn main() {
    unknown(1)
}
"#,
    );
}

#[test]
fn wrong_inspect_signature() {
    assert_wasm_error!(
        r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(a: Int, b: Int) -> String

pub fn main() {
    inspect(1, 2)
}
"#,
    );
}

#[test]
fn wrong_external_function_type() {
    assert_wasm_error!(
        r#"
type I32 {}

@external(webassembly, "builtins", "_i32_to_str")
fn to_str(value: Int, ptr: I32) -> I32
"#,
    );
}

#[test]
fn wrong_external_function_arity() {
    assert_wasm_error!(
        r#"
type I32 {}

@external(webassembly, "builtins", "_i32_to_str")
fn to_str(value: I32) -> I32
"#,
    );
}

// Module imports are not yet supported but the test infrastructure
// doesn't support multi-module compilation, so we can't test this
// error here yet.
