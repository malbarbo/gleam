// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use super::{
    compile_wasm, count_ref_calls, run_ok, run_ok_i64, run_wasm, wasm_function_names,
    wasm_local_names, wasm_type_names,
};

#[test]
fn const_function_assignment() {
    run_ok(
        r#"
const s = sum
const a = s

fn sum(a, b) {
    a + b
}

pub fn main() {
    assert s(1, 2) == 3
    assert a(2, 3) == 5
}
"#,
    );
}

#[test]
fn pipeline() {
    run_ok(
        r#"
pub fn mul(a, b) {
    a * b
}

fn div(a, b) {
    a / b
}

fn add2() {
    fn(x) { x + 2 }
}

pub fn main() {
    assert 10 |> mul(3) |> div(2) |> div(45, _) |> fn(x) { x + 1 } |> add2() == 6
}
"#,
    );
}

#[test]
fn use_expression() {
    run_ok(
        r#"
fn try_non_zero(a: Int, fun: fn(Int) -> Int) {
    case a {
        0 -> 1
        _ -> fun(a)
    }
}

fn try_non_eq(a, b, fun: fn(String, String) -> String) -> String {
    case a == b {
        True -> ""
        False -> fun("a", "b")
    }
}

fn try_tuple(a: #(Int, Int), fun: fn(#(Int, Int)) -> Int) {
    case a.0 == a.1 {
        True -> 0
        False -> fun(a)
    }
}

pub fn main() {
    assert {
        use #(a, b) <- try_tuple(#(10, 20))
        a + b
    } == 30

    assert {
        use a <- try_non_zero(10)
        a + 2
    } == 12

    assert {
        use a <- try_non_zero(0)
        a + 2
    } == 1

    assert {
        use a, b <- try_non_eq(10, 20)
        a <> b
    } == "ab"
}
"#,
    );
}

#[test]
fn variable_shadowing() {
    run_ok(
        r#"
pub fn main() {
    let a = 10
    let a = a + 1
    assert a == 11
}
"#,
    );
}

#[test]
fn scope() {
    run_ok(
        r#"
pub fn main() {
    let a = 5
    let assert 5 = a
    let b = {
        let a = 2
        let assert 5 = 10 / a
        a + 1
    }
    let assert 5 = a
    let assert 3 = b
}
"#,
    );
}

#[test]
fn higher_order_call() {
    run_ok(
        r#"
fn call(f: fn(Int) -> Int, a: Int) -> Int {
    f(a)
}

fn inc(a) {
    a + 1
}

pub fn main() {
    let assert 11 = call(inc, 10)
}
"#,
    );
}

#[test]
fn local_function() {
    run_ok(
        r#"
fn local_function() -> fn(Int) -> Int {
    let f = fn(c) { c + 1 }
    f
}

pub fn main() {
    let assert 2 = local_function()(1)
}
"#,
    );
}

#[test]
fn mixed_params() {
    run_ok(
        r#"
pub fn main() {
    let a = 10
    let b = 2.0
    let c = "c"

    let assert 10 = a
    let assert 4 = {
        let a = a / 4
        let b = 1
        let c = 1
        a + b + c
    }
    let assert 10 = a

    let assert 2.0 = b
    let assert 8.0 = {
        let b = b *. 3.0
        let a = 1.0
        let c = 1.0
        a +. b +. c
    }
    let assert 2.0 = b

    let assert "c" = c
    let assert "-a-b-c-" = {
        let c = "-" <> c <> "-"
        let a = "-a"
        let b = "-b"
        a <> b <> c
    }
    let assert "c" = c
    True
}
"#,
    );
}

#[test]
fn import_function() {
    let result = run_wasm(
        r#"
import thepackage/helpers

pub fn main() {
    assert helpers.add(1, 2) == 3
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub fn add(a: Int, b: Int) -> Int {
    a + b
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_generic_function() {
    let result = run_wasm(
        r#"
import thepackage/helpers

pub fn main() {
    assert helpers.identity(42) == 42
    assert helpers.identity("hello") == "hello"
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub fn identity(x: a) -> a {
    x
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_constant() {
    let result = run_wasm(
        r#"
import thepackage/helpers

pub fn main() {
    assert helpers.magic == 42
    assert helpers.greeting == "hi"
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub const magic = 42
pub const greeting = "hi"
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_type_and_constructor() {
    let result = run_wasm(
        r#"
import thepackage/helpers

pub fn main() {
    let p = helpers.Pair(1, 2)
    assert p.first == 1
    assert p.second == 2
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub type Pair(a, b) {
    Pair(first: a, second: b)
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_unqualified() {
    let result = run_wasm(
        r#"
import thepackage/helpers.{add, magic}

pub fn main() {
    assert add(magic, 8) == 50
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub const magic = 42

pub fn add(a: Int, b: Int) -> Int {
    a + b
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_generic_type_with_pattern() {
    let result = run_wasm(
        r#"
import thepackage/helpers.{Box, unbox}

pub fn main() {
    let b = Box(42)
    assert unbox(b) == 42
    let s = Box("hello")
    assert unbox(s) == "hello"
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub type Box(a) {
    Box(value: a)
}

pub fn unbox(b: Box(a)) -> a {
    let Box(v) = b
    v
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_module_select_guard() {
    let result = run_wasm(
        r#"
import thepackage/helpers

pub fn main() {
    let x = 42
    assert case x {
        n if n == helpers.magic -> True
        _ -> False
    }
    let y = 10
    assert case y {
        n if n == helpers.magic -> False
        _ -> True
    }
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub const magic = 42
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_union_type_with_pattern() {
    let result = run_wasm(
        r#"
import thepackage/helpers.{type Shape, Circle, Rectangle}

pub fn area(shape: Shape) -> Int {
    case shape {
        Circle(r) -> r * r * 3
        Rectangle(w, h) -> w * h
    }
}

pub fn main() {
    assert area(Circle(10)) == 300
    assert area(Rectangle(3, 4)) == 12
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub type Shape {
    Circle(radius: Int)
    Rectangle(width: Int, height: Int)
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_multiple_modules() {
    let result = run_wasm(
        r#"
import thepackage/math
import thepackage/strings

pub fn main() {
    assert math.add(1, 2) == 3
    assert strings.greeting == "hello"
    0
}
"#,
        vec![
            (
                "thepackage",
                "thepackage/math",
                r#"
pub fn add(a: Int, b: Int) -> Int {
    a + b
}
"#,
            ),
            (
                "thepackage",
                "thepackage/strings",
                r#"
pub const greeting = "hello"
"#,
            ),
        ],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_aliased() {
    let result = run_wasm(
        r#"
import thepackage/helpers as h

pub fn main() {
    assert h.add(1, 2) == 3
    assert h.magic == 42
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub const magic = 42

pub fn add(a: Int, b: Int) -> Int {
    a + b
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_function_as_value() {
    let result = run_wasm(
        r#"
import thepackage/helpers

fn apply(f: fn(Int) -> Int, x: Int) -> Int {
    f(x)
}

pub fn main() {
    assert apply(helpers.double, 5) == 10
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub fn double(x: Int) -> Int {
    x * 2
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_generic_function_calls_internal() {
    let result = run_wasm(
        r#"
import thepackage/helpers

pub fn main() {
    assert helpers.wrap(42) == #(42, 1)
    assert helpers.wrap("hi") == #("hi", 1)
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
fn tag(x: a) -> #(a, Int) {
    #(x, 1)
}

pub fn wrap(x: a) -> #(a, Int) {
    tag(x)
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_generic_recursive_function() {
    let result = run_wasm(
        r#"
import thepackage/helpers

pub fn main() {
    assert helpers.len([1, 2, 3]) == 3
    assert helpers.len(["a", "b"]) == 2
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub fn len(lst: List(a)) -> Int {
    case lst {
        [] -> 0
        [_, ..rest] -> 1 + len(rest)
    }
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_generic_function_with_imported_type() {
    let result = run_wasm(
        r#"
import thepackage/helpers.{type Box, Box, map_box}

pub fn main() {
    let b = Box(21)
    let c = map_box(b, fn(x) { x * 2 })
    assert c == Box(42)
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub type Box(a) {
    Box(value: a)
}

pub fn map_box(b: Box(a), f: fn(a) -> b) -> Box(b) {
    let Box(v) = b
    Box(f(v))
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_generic_union_pattern_match() {
    let result = run_wasm(
        r#"
import thepackage/helpers.{type Maybe, Just, Nothing, unwrap}

pub fn main() {
    assert unwrap(Just(42), 0) == 42
    assert unwrap(Nothing, 0) == 0
    assert unwrap(Just("hi"), "default") == "hi"
    assert unwrap(Nothing, "default") == "default"
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub type Maybe(a) {
    Just(a)
    Nothing
}

pub fn unwrap(m: Maybe(a), default: a) -> a {
    case m {
        Just(v) -> v
        Nothing -> default
    }
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_generic_function_multiple_type_params() {
    let result = run_wasm(
        r#"
import thepackage/helpers

pub fn main() {
    assert helpers.swap(#(1, "hello")) == #("hello", 1)
    assert helpers.swap(#("a", 2)) == #(2, "a")
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub fn swap(pair: #(a, b)) -> #(b, a) {
    let #(x, y) = pair
    #(y, x)
}
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_generic_transitive_dependency() {
    let result = run_wasm(
        r#"
import thepackage/helpers

pub fn main() {
    let w = helpers.wrap(42)
    assert helpers.get(w) == 42
    let w2 = helpers.wrap("hi")
    assert helpers.get(w2) == "hi"
    0
}
"#,
        vec![
            (
                "thepackage",
                "thepackage/types",
                r#"
pub type Wrapper(a) {
    Wrapper(value: a)
}
"#,
            ),
            (
                "thepackage",
                "thepackage/helpers",
                r#"
import thepackage/types.{type Wrapper, Wrapper}

pub fn wrap(x: a) -> Wrapper(a) {
    Wrapper(x)
}

pub fn get(w: Wrapper(a)) -> a {
    w.value
}
"#,
            ),
        ],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function() {
    let result = run_wasm(
        r#"
pub fn main() {
    let id = fn(a: a) { a }
    assert id(42) == 42
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_from_case() {
    let result = run_wasm(
        r#"
pub fn main() {
    let a = fn(a: a) {
        echo 1
        a
    }
    let b = fn(a: a) {
        echo 2
        a
    }
    let c = fn(a: a) {
        echo 3
        a
    }
    let f1 = case 1 {
        1 -> a
        2 -> b
        _ -> c
    }
    let f2 = case 2 {
        1 -> a
        2 -> b
        _ -> c
    }
    let f3 = case 3 {
        1 -> a
        2 -> b
        _ -> c
    }
    assert f1(42) == 42
    assert f2(43) == 43
    assert f3(99) == 99
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
    assert_eq!(
        result.stderr_without_colour(),
        "src/my/mod.gleam:4\n1\nsrc/my/mod.gleam:8\n2\nsrc/my/mod.gleam:12\n3\n"
    );
}

#[test]
fn mono_local_generic_function_name_collision() {
    let result = run_wasm(
        r#"
fn bar() -> Int {
    let id = fn(a: a) {
        echo 2
        a
    }
    assert id(42) == 42
    assert id(43) == 43
    0
}

pub fn main() {
    let id = fn(a: a) {
        echo 1
        a
    }
    assert id(42) == 42
    bar()
    assert id(44) == 44
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
    assert_eq!(
        result.stderr_without_colour(),
        "src/my/mod.gleam:14\n1\nsrc/my/mod.gleam:4\n2\nsrc/my/mod.gleam:4\n2\nsrc/my/mod.gleam:14\n1\n"
    );
}

#[test]
fn mono_local_generic_function_multiple_type_params() {
    let result = run_wasm(
        r#"
pub fn main() {
    let swap = fn(a: a, b: b) { #(b, a) }
    assert swap(1, "hi") == #("hi", 1)
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_as_argument() {
    let result = run_wasm(
        r#"
fn apply(f: fn(a) -> a, x: a) -> a {
    f(x)
}

pub fn main() {
    let id = fn(a: a) { a }
    assert apply(id, 42) == 42
    assert apply(id, 43) == 43
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_complex_return() {
    let result = run_wasm(
        r#"
pub fn main() {
    let wrap = fn(a: a) { [a] }
    assert wrap(42) == [42]
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_nested() {
    let result = run_wasm(
        r#"
pub fn main() {
    let outer = fn(a: a) {
        let inner = fn(b: b) { b }
        inner(a)
    }
    assert outer(42) == 42
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_nested_transitive() {
    let result = run_wasm(
        r#"
pub fn main() {
    let outer = fn(a: a) {
        let middle = fn(b: b) { b }
        let inner = fn(c: c) { c }
        inner(middle(a))
    }
    assert outer(42) == 42
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_passed_to_different_callers() {
    let result = run_wasm(
        r#"
fn use_int(f: fn(Int) -> Int) -> Int {
    f(42)
}

fn use_string(f: fn(String) -> String) -> String {
    f("hello")
}

pub fn main() {
    let id = fn(a: a) { a }
    assert use_int(id) == 42
    assert use_int(id) == 42
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_lambda_name_collision_across_modules() {
    // Both modules have a lambda at the same byte offset with the same type
    // but different bodies (x*2 vs x*3, same length so same SrcSpan).
    let result = run_wasm(
        r#"
import thepackage/mod_a
import thepackage/mod_b

pub fn main() {
    assert mod_a.apply(10) == 20
    assert mod_b.apply(10) == 30
    0
}
"#,
        vec![
            (
                "thepackage",
                "thepackage/mod_a",
                "\npub fn apply(x: Int) -> Int {\n  let f = fn(y: Int) { y * 2 }\n  f(x)\n}\n",
            ),
            (
                "thepackage",
                "thepackage/mod_b",
                "\npub fn apply(x: Int) -> Int {\n  let f = fn(y: Int) { y * 3 }\n  f(x)\n}\n",
            ),
        ],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_nested_mixed_params() {
    let result = run_wasm(
        r#"
pub fn main() {
    let outer = fn(a: a) {
        let inner = fn(x: a, y: b) { #(x, y) }
        #(inner(a, "hello"), inner(a, "bye"))
    }
    assert outer(42) == #(#(42, "hello"), #(42, "bye"))
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_return_only_type_var() {
    // make() returns a type var that only appears in the return type (via panic).
    // The WASM must compile correctly even though the type var is not in the params.
    let result = run_wasm(
        r#"
pub fn main() {
    let outer = fn(a: a) {
        let make = fn() -> b { panic }
        #(a, make())
    }
    outer(42)
    0
}
"#,
        vec![],
    );
    // outer(42) calls make() which panics at runtime, but WASM compilation must succeed.
    assert!(
        !result.status.success(),
        "Expected runtime panic but got success"
    );
    assert!(
        result.stderr.contains("panic"),
        "Expected panic message, got: {}",
        result.stderr
    );
}

#[test]
fn mono_local_function_used_with_a_type_var_bound_by_nothing() {
    // The type of the use of `outer` has the type variable of the result of
    // `make()`, which nothing binds, but `a` is still `Int`.
    run_ok_i64(
        r#"
pub fn main() {
    let outer = fn(a: a, fail: Bool) {
        let make = fn() -> b { panic }
        case fail {
            True -> Error(make())
            False -> Ok(a)
        }
    }
    assert outer(42, False) == Ok(42)
}
"#,
    );
}

#[test]
fn mono_local_generic_function_echo_param() {
    let result = run_wasm(
        r#"
pub fn main() {
    let show = fn(a: a) { echo a }
    show(42)
    show(43)
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
    assert_eq!(
        result.stderr_without_colour(),
        "src/my/mod.gleam:3\n42\nsrc/my/mod.gleam:3\n43\n"
    );
}

#[test]
fn mono_local_generic_function_shadowed_by_let() {
    let result = run_wasm(
        r#"
pub fn main() {
    let f = fn(a: a) { a }
    assert f(1) == 1
    let f = fn(x: Int) { x + 100 }
    assert f(1) == 101
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_shadowed_by_param() {
    let result = run_wasm(
        r#"
pub fn main() {
    let f = fn(a: a) { a }
    let h = fn(f: fn(Int) -> Int) { f(1) }
    assert h(fn(x) { x + 100 }) == 101
    assert f(1) == 1
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_shadowed_by_case_pattern() {
    let result = run_wasm(
        r#"
pub fn main() {
    let f = fn(a: a) { a }
    let r = case fn(x: Int) { x + 100 } {
      f -> f(1)
    }
    assert r == 101
    assert f(1) == 1
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_function_in_sibling_scope() {
    let result = run_wasm(
        r#"
pub fn main() {
    let r = {
      let f = fn(x: Int) { x + 100 }
      f(1)
    }
    let g = fn(f: fn(Int) -> Int) { f(2) }
    assert r == 101
    assert g(fn(x) { x * 10 }) == 20
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_as_case_subject() {
    let result = run_wasm(
        r#"
pub fn main() {
    let id = fn(a: a) { a }
    let r = case id {
      f -> f(7)
    }
    assert r == 7
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_with_nested_lambdas() {
    let result = run_wasm(
        r#"
pub fn main() {
    let id = fn(a: a) {
      let g = fn(x) { x }
      let h = fn(y) { g(y) }
      h(a)
    }
    assert id(42) == 42
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_in_generic_function() {
    let result = run_wasm(
        r#"
fn wrap(x: t) -> #(t, Int) {
    let pair = fn(a: a, b: b) { #(a, b) }
    let #(p, _) = pair(x, 1)
    let #(_, q) = pair(x, 7)
    #(p, q)
}
pub fn main() {
    assert wrap(1) == #(1, 7)
    assert wrap("z") == #("z", 7)
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_passed_to_function() {
    let result = run_wasm(
        r#"
fn apply(f: fn(Int) -> Int, x: Int) -> Int { f(x) }
fn apply_s(f: fn(String) -> String, x: String) -> String { f(x) }
pub fn main() {
    let id = fn(a: a) { a }
    assert apply(id, 3) == 3
    assert apply(id, 4) == 4
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn mono_local_generic_function_calls_generic() {
    let result = run_wasm(
        r#"
pub fn main() {
    let id = fn(a: a) { a }
    let twice = fn(b: b) { id(id(b)) }
    assert twice(5) == 5
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

fn assert_runs_without_call_ref(src: &str, deps: Vec<(&str, &str, &str)>) {
    assert_eq!(count_ref_calls(&compile_wasm(src, deps.clone())), 0);
    let result = run_wasm(src, deps);
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn direct_call_module_functions() {
    assert_runs_without_call_ref(
        r#"
fn inc(x: Int) -> Int { x + 1 }
fn id(x: a) -> a { x }
pub fn main() {
    assert inc(1) == 2
    assert id(3) == 3
    assert id("a") == "a"
    0
}
"#,
        vec![],
    );
}

#[test]
fn direct_call_constructors_and_record_update() {
    assert_runs_without_call_ref(
        r#"
type Point { Point(x: Int, y: Int) }
type Shape { Circle(r: Int) Square(side: Int) }
pub fn main() {
    let p = Point(1, 2)
    let q = Point(..p, y: 3)
    assert q.x == 1
    assert q.y == 3
    assert Circle(2) != Square(2)
    0
}
"#,
        vec![],
    );
}

#[test]
fn direct_call_local_functions() {
    assert_runs_without_call_ref(
        r#"
pub fn main() {
    let inc = fn(x) { x + 1 }
    let id = fn(a: a) { a }
    assert inc(1) == 2
    assert id(4) == 4
    0
}
"#,
        vec![],
    );
}

#[test]
fn direct_call_imported_functions() {
    assert_runs_without_call_ref(
        r#"
import thepackage/helpers.{Box, unbox}

pub fn main() {
    assert helpers.double(2) == 4
    assert unbox(Box(5)) == 5
    assert helpers.unbox(helpers.Box("c")) == "c"
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub type Box(a) {
    Box(value: a)
}

pub fn unbox(b: Box(a)) -> a {
    let Box(v) = b
    v
}

pub fn double(x: Int) -> Int { x * 2 }
"#,
        )],
    );
}

#[test]
fn indirect_call_through_function_value() {
    let src = r#"
fn apply(f: fn(Int) -> Int, x: Int) -> Int { f(x) }
fn inc(x: Int) -> Int { x + 1 }
pub fn main() {
    // A function read from a list is not known where `apply` is called
    let assert [f] = [inc]
    assert apply(f, 1) == 2
    0
}
"#;
    assert_eq!(count_ref_calls(&compile_wasm(src, vec![])), 1);
    run_ok(src);
}

fn sorted_with_prefix(names: Vec<String>, prefix: &str) -> Vec<String> {
    let mut names: Vec<_> = names
        .into_iter()
        .filter(|name| name.starts_with(prefix))
        .collect();
    names.sort();
    names
}

#[test]
fn local_function_uses_enclosing_specialization() {
    // The type `a` of the enclosing function must not be resolved to Nil.
    let src = r#"
fn g(x: a) -> Int {
    let f = fn(y: c) -> Int { let z: List(a) = []  echo z  1 }
    f(1)
}
pub fn main() {
    assert g(1) + g("s") == 2
    0
}
"#;
    let wasm = compile_wasm(src, vec![]);
    assert_eq!(
        sorted_with_prefix(wasm_type_names(&wasm), "List("),
        ["List(Int).Cons", "List(String).Cons"]
    );
    assert_eq!(
        sorted_with_prefix(wasm_function_names(&wasm), "anonymous@").len(),
        2
    );
    run_ok(src);
}

#[test]
fn nested_local_function_uses_enclosing_specialization() {
    let src = r#"
pub fn main() {
    let outer = fn(x: a) -> Int {
        let inner = fn(y: b) -> Int { let z: List(a) = []  echo z  1 }
        inner(1)
    }
    assert outer("s") == 1
    0
}
"#;
    let wasm = compile_wasm(src, vec![]);
    assert_eq!(
        sorted_with_prefix(wasm_type_names(&wasm), "List("),
        ["List(String).Cons"]
    );
    // One `outer` and one `inner`.
    assert_eq!(
        sorted_with_prefix(wasm_function_names(&wasm), "anonymous@").len(),
        2
    );
    run_ok(src);
}

#[test]
fn anonymous_function_per_enclosing_specialization() {
    let src = r#"
fn wrap(x: a) -> fn() -> Int { fn() { let z: List(a) = []  echo z  1 } }
pub fn main() {
    assert wrap(1)() + wrap("s")() == 2
    0
}
"#;
    let wasm = compile_wasm(src, vec![]);
    assert_eq!(
        sorted_with_prefix(wasm_type_names(&wasm), "List("),
        ["List(Int).Cons", "List(String).Cons"]
    );
    assert_eq!(
        sorted_with_prefix(wasm_function_names(&wasm), "anonymous@").len(),
        2
    );
    run_ok(src);
}

#[test]
fn local_function_definition_generates_no_code() {
    let src = r#"
pub fn main() {
    let f = fn(x: Int) -> Int { x }
    assert f(1) == 1
    0
}
"#;
    let wasm = compile_wasm(src, vec![]);
    assert_eq!(
        sorted_with_prefix(wasm_function_names(&wasm), "anonymous@").len(),
        1
    );
    run_ok(src);
}

/// The type of `g` is generic, its variable is bound to the type of its uses.
/// With `I64` the default type of an unbound variable, `Nil`, has a different
/// representation than `Int`.
#[test]
fn local_function_definition_as_block_result() {
    run_ok_i64(
        r#"
fn make() -> fn(Int) -> Int {
    let f = fn(x: Int) -> Int { x + 1 }
}
pub fn main() {
    let g = {
        let id = fn(x: a) -> a { x }
    }
    assert make()(1) == 2
    assert g(3) == 3
    0
}
"#,
    );
}

#[test]
fn local_function_used_in_anonymous_and_nested_functions() {
    run_ok(
        r#"
fn apply(f: fn(Int) -> Int, x: Int) -> Int { f(x) }
pub fn main() {
    let id = fn(x: a) -> a { x }
    let assert inc = fn(x: Int) -> Int { x + 1 }
    let twice = fn(x: b) -> #(b, b) {
        let pair = fn(y: c) -> #(c, c) { #(id(y), id(y)) }
        pair(x)
    }
    assert apply(fn(x) { inc(id(x)) }, 1) == 2
    assert twice(1) == #(1, 1)
    0
}
"#,
    );
}

#[test]
fn specializations_with_same_type_name() {
    let result = run_wasm(
        r#"
import thepackage/a/y as ay
import thepackage/b/y as by

fn id(x: t) -> t { x }

fn apply(x: t) -> t {
    let f = fn(y) { id(y) }
    f(x)
}

pub fn main() {
    assert ay.value(id(ay.new())) == 3
    assert by.value(id(by.new())) == 4.5
    assert ay.value(apply(ay.new())) == 3
    assert by.value(apply(by.new())) == 4.5
    0
}
"#,
        vec![
            (
                super::TEST_PACKAGE,
                "thepackage/a/x",
                "pub type T { T(Int) }",
            ),
            (
                "thepackage",
                "thepackage/a/y",
                "import thepackage/a/x
pub fn new() { x.T(3) }
pub fn value(t: x.T) { let x.T(v) = t v }",
            ),
            (
                super::TEST_PACKAGE,
                "thepackage/b/x",
                "pub type T { T(Float) }",
            ),
            (
                "thepackage",
                "thepackage/b/y",
                "import thepackage/b/x
pub fn new() { x.T(4.5) }
pub fn value(t: x.T) { let x.T(v) = t v }",
            ),
        ],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn import_function_constants() {
    let result = run_wasm(
        r#"
import thepackage/helpers.{step}

const local_step = helpers.step

const steps = [helpers.step, helpers.inc, step]

const local_magic = helpers.magic

const local_greeting = helpers.greeting

const local_list = helpers.list

fn apply(f: fn(Int) -> Int, x: Int) -> Int {
  f(x)
}

fn apply_all(fs: List(fn(Int) -> Int), x: Int) -> Int {
  case fs {
    [] -> x
    [f, ..fs] -> apply_all(fs, f(x))
  }
}

pub fn main() {
    assert helpers.step(1) == 2
    assert step(1) == 2
    assert apply(helpers.step, 1) == 2
    assert local_step(1) == 2
    assert apply_all(steps, 0) == 3
    assert helpers.step == helpers.inc
    assert local_step == step
    assert apply_all(helpers.steps, 0) == 2
    assert local_magic == 42
    assert local_greeting == "hi"
    assert local_list == [42, 2]
    assert helpers.list == [42, 2]
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub fn inc(x: Int) -> Int {
  x + 1
}

pub const step = inc

pub const steps = [step, inc]

pub const magic = 42

pub const greeting = "hi"

pub const list = [magic, 2]
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn builtins_as_values() {
    run_ok(
        r#"
@external(webassembly, "builtins", "_string_concat")
fn concat(a: String, b: String) -> String

@external(webassembly, "builtins", "_string_num_bytes")
fn num_bytes(a: String) -> Int

@external(webassembly, "builtins", "_string_get_byte")
fn get_byte(a: String, i: Int) -> Result(Int, Nil)

@external(webassembly, "builtins", "_int_to_utf_codepoint")
fn to_codepoint(i: Int) -> Result(UtfCodepoint, Nil)

@external(webassembly, "builtins", "int_parse")
fn parse_int(s: String) -> Result(Int, Nil)

fn apply2(f, a, b) { f(a, b) }
fn apply(f, a) { f(a) }
fn tail(s) { concat(s, "!") }
fn tail_bytes(s) { num_bytes(s) }

pub fn main() {
    let c = concat
    assert c("a", "b") == "ab"
    assert apply2(concat, "x", "y") == "xy"
    assert apply(num_bytes, "abc") == 3
    assert apply2(get_byte, "a", 0) == Ok(97)
    assert apply(to_codepoint, 97) == to_codepoint(97)
    assert apply(parse_int, "42") == Ok(42)
    assert tail("a") == "a!"
    assert tail_bytes("ab") == 2
    assert c == concat
}
"#,
    );
}

/// `main` is called by the `_start` export, not by the start function, so a
/// host that calls `main` after instantiating the module runs it once.
#[test]
fn main_runs_once_when_called_by_the_host() {
    let wasm_bytes = compile_wasm(
        r#"
pub fn main() {
    echo "main"
}
"#,
        vec![],
    );
    let path = std::env::temp_dir().join(format!("{}.wasm", super::unique_name("gleam_wasm_test")));
    std::fs::write(&path, &wasm_bytes).expect("failed to write temp wasm file");
    let output = std::process::Command::new("wasmtime")
        .args(["run", "-W", "function-references=y", "-W", "gc=y"])
        .args(["--invoke", "main"])
        .arg(&path)
        .output()
        .expect("wasmtime not found — install it to run WASM tests");
    let _ = std::fs::remove_file(&path);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "{stderr}");
    assert_eq!(stderr.matches("\"main\"").count(), 1, "{stderr}");
}

/// A public generic function without arguments is exported with its type
/// variables as `Nil`, also when they are inside of a function result.
#[test]
fn public_generic_function_without_arguments_returning_a_constructor() {
    let result = run_wasm(
        r#"
pub type Effect(r) {
    Done(r)
    Now(resume: fn(Int) -> Effect(r))
}

pub fn now() {
    Now
}

pub fn main() {
    let assert Now(resume) = now()(Done)
    assert resume(1) == Done(1)
    0
}
"#,
        vec![],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

/// Locals with the same name and location are told apart by primes.
#[test]
fn names_of_locals_at_the_same_location() {
    let src = r#"
fn id(x: Int) -> Int {
  x
}

pub fn divide(a: Int, b: Int, c: Int) -> Int {
  id(a) / b / c / id(b)
}

pub fn main() {
  assert divide(12, 1, 2) == 6
}
"#;
    run_ok(src);
    let names = wasm_local_names(&compile_wasm(src, vec![]));
    assert_eq!(
        names.get("divide").map_or(&[][..], Vec::as_slice),
        [
            "a",
            "b",
            "c",
            "div_left@7:2",
            "div_right@7:18",
            "div_left@7:2'",
            "div_left@7:2''"
        ]
    );
}
