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

#[test]
fn mono_local_generic_function() {
    let result = run_wasm(
        r#"
pub fn main() {
    let id = fn(a: a) { a }
    assert id(42) == 42
    assert id("hello") == "hello"
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
    assert f2("hello") == "hello"
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
        result.stderr,
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
    assert id("bar") == "bar"
    0
}

pub fn main() {
    let id = fn(a: a) {
        echo 1
        a
    }
    assert id(42) == 42
    bar()
    assert id("main") == "main"
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
        result.stderr,
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
    assert swap("hi", 1) == #(1, "hi")
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
    assert apply(id, "hello") == "hello"
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
    assert wrap("hi") == ["hi"]
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
    assert outer("hello") == "hello"
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
    assert outer("hello") == "hello"
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
    assert use_string(id) == "hello"
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
        #(inner(a, "hello"), inner(a, 1))
    }
    assert outer(42) == #(#(42, "hello"), #(42, 1))
    assert outer("world") == #(#("world", "hello"), #("world", 1))
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
fn mono_local_generic_function_echo_param() {
    let result = run_wasm(
        r#"
pub fn main() {
    let show = fn(a: a) { echo a }
    show(42)
    show("hello")
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
        result.stderr,
        "src/my/mod.gleam:3\n42\nsrc/my/mod.gleam:3\n\"hello\"\n"
    );
}
