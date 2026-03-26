#[test]
fn unknown_external_type() {
    assert_wasm_error!(
        r#"
@external(webassembly, "builtins", "F128")
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

// is_external_type correctly rejects non-external types like Bool.
// This is tested indirectly by wrong_external_function_type which
// uses Int (a non-external type) in a function signature.

#[test]
fn int_literal_overflow_i32() {
    assert_wasm_error!(
        r#"
pub fn main() {
    2147483648
}
"#,
    );
}

#[test]
fn int_literal_underflow_i32() {
    assert_wasm_error!(
        r#"
pub fn main() {
    -2147483649
}
"#,
    );
}

#[test]
fn int_literal_overflow_i64() {
    assert_wasm_validate_error!(
        r#"
pub fn main() {
    9223372036854775808
}
"#,
        crate::config::WasmInt::I64,
        crate::config::WasmFloat::F64,
    );
}

#[test]
fn int_literal_underflow_i64() {
    assert_wasm_validate_error!(
        r#"
pub fn main() {
    -9223372036854775809
}
"#,
        crate::config::WasmInt::I64,
        crate::config::WasmFloat::F64,
    );
}

#[test]
fn float_literal_overflow_f32() {
    assert_wasm_validate_error!(
        r#"
pub fn main() {
    3.5e38
}
"#,
        crate::config::WasmInt::I32,
        crate::config::WasmFloat::F32,
    );
}

// Module imports are not yet supported but the test infrastructure
// doesn't support multi-module compilation, so we can't test this
// error here yet.
