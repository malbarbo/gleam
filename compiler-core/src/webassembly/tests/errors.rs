// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

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
fn unknown_glued_builtin_function() {
    assert_wasm_error!(
        r#"
@external(webassembly, "builtins", "unknown_func")
fn unknown(a: String) -> String

pub fn main() {
    unknown("a")
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
fn wrong_builtin_function_type() {
    assert_wasm_error!(
        r#"
@external(webassembly, "builtins", "_string_concat")
fn concat(a: String, b: Int) -> String
"#,
    );
}

#[test]
fn wrong_builtin_function_external_type() {
    assert_wasm_error!(
        r#"
type I64 {}

@external(webassembly, "builtins", "_int_to_i32")
fn to_i32(value: Int) -> I64
"#,
    );
}

#[test]
fn external_type_without_representation() {
    assert_wasm_error!(
        r#"
pub type Foo

pub fn same(x: Foo, y: Foo) -> Bool {
    x == y
}

pub fn main() {
    echo same
}
"#,
    );
}

/// A value of a type without representation of another module is reported,
/// also when its equality is generated.
#[test]
fn external_type_without_representation_of_other_module() {
    let error = super::compile_wasm_error_with(
        r#"
import other.{type Foo, foo}

pub fn main() {
  foo() == foo()
}
"#,
        vec![(
            super::TEST_PACKAGE,
            "other",
            r#"
pub type Foo

pub fn foo() -> Foo {
  panic
}
"#,
        )],
    );
    insta::assert_debug_snapshot!(error);
}

/// `AnyRef` is only for the runtime.
#[test]
fn external_type_anyref() {
    assert_wasm_error!(
        r#"
pub type AnyRef

pub fn same(x: AnyRef, y: AnyRef) -> Bool {
    x == y
}

pub fn main() {
    echo same
}
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

#[test]
fn type_too_large_polymorphic_recursion() {
    assert_wasm_error!(
        r#"
pub type N(a) {
  Flat(a)
  Nest(N(#(a, a)))
}

pub fn build(n: Int, x: a) -> N(a) {
  case n {
    0 -> Flat(x)
    _ -> Nest(helper(n, x))
  }
}

fn helper(n: Int, x: b) -> N(#(b, b)) {
  build(n - 1, #(x, x))
}

pub fn main() {
  build(3, 1)
}
"#,
    );
}

#[test]
fn type_too_large_polymorphic_recursion_by_one_level() {
    assert_wasm_error!(
        r#"
pub type N(a) {
  Flat(a)
  Nest(N(List(a)))
}

pub fn build(n: Int, x: a) -> N(a) {
  case n {
    0 -> Flat(x)
    _ -> Nest(helper(n, x))
  }
}

fn helper(n: Int, x: b) -> N(List(b)) {
  build(n - 1, [x])
}

pub fn main() {
  build(3, 1)
}
"#,
    );
}

#[test]
fn type_too_large_function_field() {
    assert_wasm_error!(
        r#"
pub type F(a) {
  F(a, fn() -> F(#(a, a)))
}

pub fn main() {
  F(1, fn() { panic })
}
"#,
    );
}

#[test]
fn type_too_large_function_field_by_one_level() {
    assert_wasm_error!(
        r#"
pub type F(a) {
  F(a, fn() -> F(List(a)))
}

pub fn main() {
  F(1, fn() { panic })
}
"#,
    );
}

// Module imports are not yet supported but the test infrastructure
// doesn't support multi-module compilation, so we can't test this
// error here yet.

/// Outside the standard library, a program can't declare the builtins.
#[test]
fn external_function_outside_the_stdlib() {
    let error = super::compile_wasm_error_in(
        "thepackage",
        r#"
@external(webassembly, "builtins", "_int_bitwise_and")
fn and(a: Int, b: Int) -> Int

pub fn main() { and(1, 2) }
"#,
    );
    assert!(
        matches!(
            error,
            crate::webassembly::Error::ExternalOutsideStdlib { .. }
        ),
        "{error:?}"
    );
}

#[test]
fn external_type_outside_the_stdlib() {
    let error = super::compile_wasm_error_in(
        "thepackage",
        r#"
@external(webassembly, "builtins", "Dict")
pub type Dict(k, v)

pub fn main() { Nil }
"#,
    );
    assert!(
        matches!(
            error,
            crate::webassembly::Error::ExternalOutsideStdlib { .. }
        ),
        "{error:?}"
    );
}
