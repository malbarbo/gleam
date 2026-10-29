// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use super::run_ok;

#[test]
fn const_int() {
    run_ok(
        r#"
const i1 = 10
const i2 = i1

pub fn main() {
    assert i1 == i2
}
"#,
    );
}

#[test]
fn const_float() {
    run_ok(
        r#"
const f1 = 20.0
const f2 = f1

pub fn main() {
    assert f1 == f2
}
"#,
    );
}

#[test]
fn const_bool() {
    run_ok(
        r#"
const b1 = True
const b2 = b1

pub fn main() {
    assert b1 == b2
}
"#,
    );
}

#[test]
fn const_string() {
    run_ok(
        r#"
const s1 = "abc"
const s2 = s1

pub fn main() {
    assert s1 == s2
}
"#,
    );
}

#[test]
fn const_list() {
    run_ok(
        r#"
const b1 = True
const l1 = [b1]
const l2 = l1

pub fn main() {
    assert l1 == l2
}
"#,
    );
}

#[test]
fn const_ok() {
    run_ok(
        r#"
const s1 = "abc"
const o1 = Ok(s1)
const o2 = o1

pub fn main() {
    assert o1 == o2
}
"#,
    );
}

#[test]
fn const_error() {
    run_ok(
        r#"
const f1 = 20.0
const e1 = Error(f1)
const e2 = e1

pub fn main() {
    assert e1 == e2
}
"#,
    );
}

#[test]
fn const_record_update() {
    run_ok(
        r#"
pub type Person {
    Person(name: String, age: Int)
}

const alice = Person("Alice", 30)
const bob = Person(..alice, name: "Bob")

pub fn main() {
    assert alice.name == "Alice"
    assert alice.age == 30
    assert bob.name == "Bob"
    assert bob.age == 30
}
"#,
    );
}

#[test]
fn const_string_concatenation() {
    run_ok(
        r#"
const greeting = "Hello, " <> "World!"
const prefix = "ab"
const full = prefix <> "cd"

pub fn main() {
    assert greeting == "Hello, World!"
    assert full == "abcd"
}
"#,
    );
}

#[test]
fn const_generic_function() {
    run_ok(
        r#"
fn identity(x: a) -> a {
  x
}

fn apply(f: fn(a) -> b, x: a) -> b {
  f(x)
}

const f = identity

const g = f

const h: fn(Int) -> Int = f

pub fn main() {
    assert f(1) == 1
    assert f("a") == "a"
    assert g(1.5) == 1.5
    assert h(2) == 2
    assert apply(f, 3) == 3
    assert apply(g, "b") == "b"
    let i: fn(Int) -> Int = f
    assert i == h
    assert i == identity
}
"#,
    );
}

#[test]
fn const_generic_constructor() {
    run_ok(
        r#"
type Box(a) {
  Box(value: a)
}

const make = Box

pub fn main() {
    assert make(1) == Box(1)
    assert make("a").value == "a"
}
"#,
    );
}

#[test]
fn const_generic_values() {
    run_ok(
        r#"
const empty = []

const nothing = Error(Nil)

pub fn main() {
    assert [1, ..empty] == [1]
    assert ["a", ..empty] == ["a"]
    let r: Result(Int, Nil) = nothing
    assert r == Error(Nil)
}
"#,
    );
}

#[test]
fn const_imported_generic_function() {
    let result = super::run_wasm(
        r#"
import thepackage/helpers.{f}

const g = helpers.f

pub fn main() {
    assert helpers.f(1) == 1
    assert f("a") == "a"
    assert g(1.5) == 1.5
    assert helpers.g(2) == 2
    assert helpers.f == f
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

pub const f = identity

pub const g = f
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
fn const_typed_alias_of_generic_const() {
    run_ok(
        r#"
const empty = []
const pair = #([], Error(Nil))
const xs: List(Int) = empty
pub const ys: List(String) = empty
const p: #(List(Float), Result(Int, Nil)) = pair

pub fn main() {
    assert xs == []
    assert ys == []
    assert [1, ..xs] == [1]
    assert ["a", ..ys] == ["a"]
    assert p == #([], Error(Nil))
}
"#,
    );
}

#[test]
fn const_typed_alias_of_imported_generic_const() {
    let result = super::run_wasm(
        r#"
import thepackage/helpers

const xs: List(Int) = helpers.empty
const ys: List(String) = helpers.alias

pub fn main() {
    assert xs == []
    assert ys == []
    assert [1, ..xs] == [1]
    assert ["a", ..ys] == ["a"]
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub const empty = []

pub const alias = empty
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
fn const_constructors() {
    run_ok(
        r#"
type Point {
  Point(x: Int, y: Int)
}

const make = Point

const make_again = make

pub fn main() {
    assert make(1, 2) == Point(1, 2)
    assert make_again(1, 2).y == 2
    assert make == Point
}
"#,
    );
}

#[test]
fn const_nested_generic_values() {
    run_ok(
        r#"
fn identity(x: a) -> a {
  x
}

const empty = []

const pair = #(empty, 1)

const functions = [identity, identity]

pub fn main() {
    let #(l, n) = pair
    assert ["a", ..l] == ["a"]
    assert n == 1
    let #(m, _) = pair
    assert [1.5, ..m] == [1.5]
    let assert [f, ..] = functions
    assert f(2) == 2
    let assert [_, g] = functions
    assert g("b") == "b"
}
"#,
    );
}

#[test]
fn const_imported_generic_values() {
    let result = super::run_wasm(
        r#"
import thepackage/helpers.{empty}

const local = helpers.pair

pub fn main() {
    assert [1, ..empty] == [1]
    assert ["a", ..helpers.empty] == ["a"]
    let #(l, n) = local
    assert [1.5, ..l] == [1.5]
    assert n == 2
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub const empty = []

pub const pair = #(empty, 2)
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
fn aliased_imported_constructors() {
    let result = super::run_wasm(
        r#"
import thepackage/helpers.{type Wrapper, Empty as Nothing, Wrap as Box}

const make = Box

const boxed = Box(3)

const nothing = Nothing

fn unwrap(wrapper: Wrapper) -> Int {
    case wrapper {
        Box(value) -> value
        Nothing -> 0
    }
}

pub fn main() {
    let h = Box
    assert unwrap(Box(1)) == 1
    assert unwrap(h(2)) == 2
    assert unwrap(make(4)) == 4
    assert unwrap(boxed) == 3
    assert unwrap(nothing) == 0
    assert unwrap(Nothing) == 0
    assert h == make
    assert unwrap(helpers.make(5)) == 5
    assert unwrap(helpers.empty) == 0
    assert helpers.make == make
    0
}
"#,
        vec![(
            "thepackage",
            "thepackage/helpers",
            r#"
pub type Wrapper {
  Empty
  Wrap(Int)
}

pub const make = Wrap

pub const empty = Empty
"#,
        )],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

/// The value of `x` is only constructed by its constant, which must record
/// that the program constructs values of `B` for its inspect (see
/// `constructed`).
#[test]
fn const_variant_only_in_constants() {
    run_ok(
        r#"
type T {
    A(Int)
    B(Float)
}

const x = B(1.0)
const w = [x]

@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

pub fn main() {
    assert inspect(w) == "[B(1.0)]"
}
"#,
    );
}
