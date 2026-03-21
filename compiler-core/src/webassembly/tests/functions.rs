use super::{run_ok, run_wasm};

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
