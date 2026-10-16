// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! The external type `Array` and its builtins, which only the runtime
//! reaches, so the modules of these tests are in its package.

use super::{compile_wasm_error_in, run_wasm_in};
use crate::config::{WasmFloat, WasmInt};
use crate::webassembly::runtime::RUNTIME_PACKAGE;

const ARRAY: &str = r#"
@external(webassembly, "builtins", "Array")
pub type Array(a)

@external(webassembly, "builtins", "_array_size")
pub fn size(array: Array(a)) -> Int

@external(webassembly, "builtins", "_array_get")
pub fn get(array: Array(a), index: Int) -> a

@external(webassembly, "builtins", "_array_set")
pub fn set(array: Array(a), index: Int, value: a) -> Array(a)

@external(webassembly, "builtins", "_array_insert")
pub fn insert(array: Array(a), index: Int, value: a) -> Array(a)

@external(webassembly, "builtins", "_array_remove")
pub fn remove(array: Array(a), index: Int) -> Array(a)

@external(webassembly, "builtins", "_array_from_list")
pub fn from_list(list: List(a)) -> Array(a)

@external(webassembly, "builtins", "_hash")
pub fn hash(value: a) -> Int

@external(webassembly, "builtins", "_inspect")
pub fn inspect(value: a) -> String

pub fn to_list(array: Array(a)) -> List(a) {
  to_list_loop(array, size(array) - 1, [])
}

fn to_list_loop(array: Array(a), i: Int, acc: List(a)) -> List(a) {
  case i < 0 {
    True -> acc
    False -> {
      to_list_loop(array, i - 1, [get(array, i), ..acc])
    }
  }
}
"#;

#[track_caller]
fn run_array(src: &str) -> String {
    let result = run_wasm_in(
        RUNTIME_PACKAGE,
        src,
        vec![(RUNTIME_PACKAGE, "array", ARRAY)],
        super::wasm_config(WasmInt::I32, WasmFloat::F64),
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
    result.stderr
}

#[track_caller]
fn run_array_i64(src: &str) {
    let result = run_wasm_in(
        RUNTIME_PACKAGE,
        src,
        vec![(RUNTIME_PACKAGE, "array", ARRAY)],
        super::wasm_config(WasmInt::I64, WasmFloat::F64),
    );
    assert!(
        result.status.success(),
        "WASM (I64) execution failed:\n{}",
        result.stderr
    );
}

const FUNCTIONS: &str = r#"
import array

pub fn main() {
  let a = array.from_list([10, 20, 30])
  let assert 3 = array.size(a)
  let assert 20 = array.get(a, 1)
  let assert [10, 25, 30] = array.to_list(array.set(a, 1, 25))
  let assert [5, 10, 20, 30] = array.to_list(array.insert(a, 0, 5))
  let assert [10, 15, 20, 30] = array.to_list(array.insert(a, 1, 15))
  let assert [10, 20, 30, 35] = array.to_list(array.insert(a, 3, 35))
  let assert [20, 30] = array.to_list(array.remove(a, 0))
  let assert [10, 30] = array.to_list(array.remove(a, 1))
  let assert [10, 20] = array.to_list(array.remove(a, 2))
  let assert [] = array.to_list(array.remove(array.from_list([1]), 0))
  let assert 0 = array.size(array.from_list([]))
  let assert [1] = array.to_list(array.insert(array.from_list([]), 0, 1))
  // The functions return copies.
  let assert [10, 20, 30] = array.to_list(a)
}
"#;

#[test]
fn array_functions() {
    let _ = run_array(FUNCTIONS);
}

#[test]
fn array_functions_i64() {
    run_array_i64(FUNCTIONS);
}

#[test]
fn arrays_of_references_and_numbers() {
    let _ = run_array(
        r#"
import array

type Point {
  Point(x: Int, y: Int)
}

pub fn main() {
  let strings = array.from_list(["a", "b"]) |> array.insert(2, "c")
  let assert ["a", "b", "c"] = array.to_list(strings)
  let floats = array.from_list([1.5, 2.5]) |> array.set(0, 0.5)
  let assert [0.5, 2.5] = array.to_list(floats)
  let points = array.from_list([Point(1, 2)]) |> array.insert(0, Point(0, 0))
  let assert Point(1, 2) = array.get(points, 1)
  let bools = array.from_list([True, False])
  let assert False = array.get(bools, 1)
  let nested = array.from_list([array.from_list([1]), array.from_list([2, 3])])
  let assert 3 = array.get(array.get(nested, 1), 1)
  let assert [Nil] = array.to_list(array.from_list([Nil]))
}
"#,
    );
}

#[test]
fn recursive_type_with_an_array() {
    let _ = run_array(
        r#"
import array.{type Array}

type Tree {
  Leaf(Int)
  Branch(children: Array(Tree))
}

fn sum(tree: Tree) -> Int {
  case tree {
    Leaf(n) -> n
    Branch(children) -> sum_loop(children, 0, 0)
  }
}

fn sum_loop(children: Array(Tree), i: Int, acc: Int) -> Int {
  case i < array.size(children) {
    True -> {
      sum_loop(children, i + 1, acc + sum(array.get(children, i)))
    }
    False -> acc
  }
}

pub fn main() {
  let tree =
    Branch(array.from_list([Leaf(1), Branch(array.from_list([Leaf(2), Leaf(3)]))]))
  let assert 6 = sum(tree)
  let assert True = tree == Branch(array.from_list([Leaf(1), Branch(array.from_list([Leaf(2), Leaf(3)]))]))
}
"#,
    );
}

#[test]
fn array_equality_hash_and_inspect() {
    let _ = run_array(
        r##"
import array

pub fn main() {
  let a = array.from_list([1, 2, 3])
  let b = array.from_list([1, 2]) |> array.insert(2, 3)
  let assert True = a == b
  let assert False = a == array.set(b, 0, 0)
  let assert False = a == array.remove(b, 2)
  let assert True = array.hash(a) == array.hash(b)
  let assert True = array.from_list([]) == array.remove(array.from_list([1]), 0)
  let assert "#(1, 2, 3)" = array.inspect(a)
  let assert "#()" = array.inspect(array.from_list([]))
  let assert "#(\"a\", #(1.5))" = array.inspect(#("a", array.from_list([1.5])))
  let assert "[#(\"x\")]" = array.inspect([array.from_list(["x"])])
}
"##,
    );
}

const OUT_OF_RANGE: &str = r#"
import array

pub fn main() {
  let a = array.from_list([1, 2])
  let assert [1, 2] = array.to_list(array.set(a, 2, 0))
  let assert [1, 2] = array.to_list(array.set(a, -1, 0))
  let assert [1, 2] = array.to_list(array.insert(a, 3, 0))
  let assert [1, 2] = array.to_list(array.insert(a, -1, 0))
  let assert [1, 2] = array.to_list(array.remove(a, 2))
  let assert [1, 2] = array.to_list(array.remove(a, -1))
  let assert [] = array.to_list(array.remove(array.remove(array.from_list([1]), 0), 0))
}
"#;

/// Changing an index out of range gives the same array.
#[test]
fn array_index_out_of_range() {
    let _ = run_array(OUT_OF_RANGE);
}

#[test]
fn array_index_out_of_range_i64() {
    run_array_i64(OUT_OF_RANGE);
}

/// An index that doesn't fit in 32 bits is out of range, it doesn't wrap.
#[test]
fn array_index_wider_than_32_bits() {
    run_array_i64(
        r#"
import array

pub fn main() {
  let a = array.from_list([1, 2])
  let assert [1, 2] = array.to_list(array.set(a, 4_294_967_296, 0))
  let assert [1, 2] = array.to_list(array.insert(a, 4_294_967_296, 0))
  let assert [1, 2] = array.to_list(array.remove(a, 4_294_967_297))
}
"#,
    );
}

/// Reading an index out of range is a bug of the runtime, and traps.
#[track_caller]
fn assert_get_traps(index: &str, int: WasmInt) {
    let result = run_wasm_in(
        RUNTIME_PACKAGE,
        &format!(
            r#"
import array

pub fn main() {{
  array.get(array.from_list([1, 2]), {index})
}}
"#
        ),
        vec![(RUNTIME_PACKAGE, "array", ARRAY)],
        super::wasm_config(int, WasmFloat::F64),
    );
    assert!(!result.status.success(), "{}", result.stderr);
    assert!(result.stderr.contains("unreachable"), "{}", result.stderr);
}

#[test]
fn array_get_out_of_range_traps() {
    assert_get_traps("2", WasmInt::I32);
    assert_get_traps("-1", WasmInt::I32);
}

#[test]
fn array_get_wider_than_32_bits_traps() {
    assert_get_traps("4_294_967_296", WasmInt::I64);
}

/// Outside the runtime, `Array` is not an external type, not even in the
/// standard library.
#[test]
fn array_outside_the_runtime() {
    let error = super::compile_wasm_error(
        r#"
@external(webassembly, "builtins", "Array")
type Array(a)

pub fn main() { Nil }
"#,
    );
    assert!(
        matches!(
            error,
            crate::webassembly::Error::UnknownExternalType { ref name, .. } if name == "Array"
        ),
        "{error:?}"
    );
}

/// Outside the runtime, the functions of `Array` are not builtins.
#[test]
fn array_function_outside_the_runtime() {
    let error = super::compile_wasm_error(
        r#"
@external(webassembly, "builtins", "_array_size")
fn size(array: a) -> Int

pub fn main() { Nil }
"#,
    );
    assert!(
        matches!(
            error,
            crate::webassembly::Error::UnknownBuiltinFunction { ref name, .. }
                if name == "_array_size"
        ),
        "{error:?}"
    );
}

#[test]
fn array_with_a_wrong_arity() {
    let error = compile_wasm_error_in(
        RUNTIME_PACKAGE,
        r#"
@external(webassembly, "builtins", "Array")
type Array

pub fn main() { Nil }
"#,
    );
    assert!(
        matches!(
            error,
            crate::webassembly::Error::WrongExternalTypeArity { ref name, expected: 1, got: 0, .. }
                if name == "Array"
        ),
        "{error:?}"
    );
}

#[test]
fn array_function_with_a_wrong_signature() {
    let error = compile_wasm_error_in(
        RUNTIME_PACKAGE,
        r#"
@external(webassembly, "builtins", "Array")
type Array(a)

@external(webassembly, "builtins", "_array_get")
fn get(array: Array(a), index: Int) -> b

pub fn main() { Nil }
"#,
    );
    assert!(
        matches!(
            error,
            crate::webassembly::Error::WrongBuiltinFunctionSignature { ref name, .. }
                if name == "_array_get"
        ),
        "{error:?}"
    );
}

#[test]
fn array_function_with_a_specific_signature() {
    let _ = run_array(
        r#"
@external(webassembly, "builtins", "Array")
type Ints(a)

@external(webassembly, "builtins", "_array_from_list")
fn from_list(list: List(Int)) -> Ints(Int)

@external(webassembly, "builtins", "_array_size")
fn size(array: Ints(Int)) -> Int

pub fn main() {
  let assert 2 = size(from_list([1, 2]))
}
"#,
    );
}

/// Two types declared as `Array` are different types: a function of `Array`
/// doesn't turn one into the other.
#[test]
fn array_function_with_two_array_types() {
    let error = compile_wasm_error_in(
        RUNTIME_PACKAGE,
        r#"
@external(webassembly, "builtins", "Array")
type Array(a)

@external(webassembly, "builtins", "Array")
type Other(a)

@external(webassembly, "builtins", "_array_set")
fn set(array: Array(a), index: Int, value: a) -> Other(a)

pub fn main() { Nil }
"#,
    );
    assert!(
        matches!(
            error,
            crate::webassembly::Error::WrongBuiltinFunctionSignature { ref name, .. }
                if name == "_array_set"
        ),
        "{error:?}"
    );
}
