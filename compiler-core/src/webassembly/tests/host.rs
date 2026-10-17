// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use crate::config::{WasmFloat, WasmInt, WasmString, WebAssemblyConfig};

/// The function imports of `wasm`, as `(module, name)`.
fn function_imports(wasm: &[u8]) -> Vec<(String, String)> {
    let mut imports = vec![];
    for payload in wasmparser::Parser::new(0).parse_all(wasm) {
        if let Ok(wasmparser::Payload::ImportSection(reader)) = payload {
            for import in reader.into_imports() {
                let import = import.expect("an import");
                if matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                    imports.push((import.module.to_string(), import.name.to_string()));
                }
            }
        }
    }
    imports
}

fn host_imports(src: &str, deps: Vec<(&str, &str, &str)>) -> Vec<(String, String)> {
    let wasm = super::compile_wasm_in(
        "thepackage",
        src,
        deps,
        super::wasm_config(WasmInt::I32, WasmFloat::F64),
    );
    function_imports(&wasm)
        .into_iter()
        .filter(|(module, _)| module != "wasi_snapshot_preview1" && module != "gleam:js")
        .collect()
}

fn import(module: &str, name: &str) -> (String, String) {
    (module.into(), name.into())
}

/// An external of a module other than "builtins" is a function of the host,
/// which a program outside the standard library can declare.
#[test]
fn external_of_the_host() {
    let imports = host_imports(
        r#"
@external(webassembly, "env", "add")
fn add(a: Int, b: Int) -> Int

pub fn main() { add(1, 2) }
"#,
        vec![],
    );
    assert_eq!(imports, vec![import("env", "add")]);
}

/// A relative path is relative to the module that declares it, like the path
/// of a JavaScript import, so it is made relative to the program.
#[test]
fn external_of_the_host_relative_to_the_module() {
    let imports = host_imports(
        r#"
@external(webassembly, "./ffi.mjs", "run")
fn run() -> Nil

pub fn main() { run() }
"#,
        vec![],
    );
    assert_eq!(imports, vec![import("./ffi.mjs", "run")]);
}

#[test]
fn external_of_the_host_in_another_package() {
    let imports = host_imports(
        r#"
import a/b

pub fn main() { b.run() }
"#,
        vec![(
            "other",
            "a/b",
            r#"
@external(webassembly, "../ffi.mjs", "run")
pub fn run() -> Nil
"#,
        )],
    );
    assert_eq!(imports, vec![import("../../other/ffi.mjs", "run")]);
}

/// Each function of the host is imported once.
#[test]
fn external_of_the_host_used_twice() {
    let imports = host_imports(
        r#"
@external(webassembly, "env", "tick")
fn tick() -> Int

pub fn main() { tick() + tick() }
"#,
        vec![],
    );
    assert_eq!(imports, vec![import("env", "tick")]);
}

/// A generic function of the host is imported with each type it is used
/// with.
#[test]
fn generic_external_of_the_host() {
    let imports = host_imports(
        r#"
@external(webassembly, "env", "id")
fn id(a: a) -> a

pub fn main() {
  let _ = id(1.0)
  id(1)
}
"#,
        vec![],
    );
    assert_eq!(imports, vec![import("env", "id"), import("env", "id")]);
}

/// A public external function is exported, like the other public functions,
/// also when the module does not use it.
#[test]
fn public_external_is_exported() {
    let wasm = super::compile_wasm_in(
        "thepackage",
        r#"
@external(webassembly, "env", "add")
pub fn add(a: Int, b: Int) -> Int

@external(webassembly, "env", "nothing")
pub fn nothing_test() -> a

@external(webassembly, "env", "id")
pub fn id(a: a) -> a
"#,
        vec![],
        super::wasm_config(WasmInt::I32, WasmFloat::F64),
    );
    let mut exports = vec![];
    for payload in wasmparser::Parser::new(0).parse_all(&wasm) {
        if let Ok(wasmparser::Payload::ExportSection(reader)) = payload {
            for export in reader {
                let export = export.expect("an export");
                if export.kind == wasmparser::ExternalKind::Func {
                    exports.push(export.name.to_string());
                }
            }
        }
    }
    assert_eq!(exports, vec!["add", "nothing_test"]);
}

/// Runs `src` in Node.js next to the module `ffi.mjs` with the source `ffi`,
/// with UTF-8 and JavaScript strings, and 64 and 32-bit ints.
#[track_caller]
fn run_with_host(src: &str, ffi: &str) {
    run_with_host_in(super::TEST_PACKAGE, src, vec![], ffi);
}

/// Like `run_with_host`, with the module in `package` and the modules
/// `deps`.
#[track_caller]
fn run_with_host_in(package: &str, src: &str, deps: Vec<(&str, &str, &str)>, ffi: &str) {
    run_with_host_files(package, src, deps, &[("ffi.mjs", ffi)]);
}

/// Like `run_with_host_in`, with the files `files` next to the module.
#[track_caller]
fn run_with_host_files(
    package: &str,
    src: &str,
    deps: Vec<(&str, &str, &str)>,
    files: &[(&str, &str)],
) {
    for string in [WasmString::Utf8, WasmString::JsString] {
        for int in [WasmInt::I64, WasmInt::I32] {
            let config = WebAssemblyConfig {
                string,
                ..super::wasm_config(int, WasmFloat::F64)
            };
            let result = super::run_wasm_node_in(package, src, deps.clone(), config, files);
            assert!(
                result.status.success(),
                "{string:?} {int:?}:\n{}",
                result.stderr
            );
        }
    }
}

/// An `Int` or a `Float` is a number, a `Bool` a boolean, `Nil` is
/// `undefined` and a `UtfCodepoint` a `UtfCodepoint`, as on JavaScript.
#[test]
fn host_scalars() {
    run_with_host(
        r#"
@external(webassembly, "./ffi.mjs", "add")
fn add(a: Int, b: Int) -> Int
@external(webassembly, "./ffi.mjs", "half")
fn half(a: Float) -> Float
@external(webassembly, "./ffi.mjs", "type_of")
fn type_of_int(a: Int) -> String
@external(webassembly, "./ffi.mjs", "type_of")
fn type_of_bool(a: Bool) -> String
@external(webassembly, "./ffi.mjs", "type_of")
fn type_of_nil(a: Nil) -> String
@external(webassembly, "./ffi.mjs", "not")
fn not(a: Bool) -> Bool
@external(webassembly, "./ffi.mjs", "nothing")
fn nothing() -> Nil
@external(webassembly, "./ffi.mjs", "next")
fn next(a: UtfCodepoint) -> UtfCodepoint
@external(webassembly, "./ffi.mjs", "nothing")
fn undefined_int() -> Int

pub fn main() {
    let assert 5 = add(2, 3)
    let assert -1 = add(2, -3)
    let assert 1.25 = half(2.5)
    let assert "number" = type_of_int(1)
    let assert "boolean" = type_of_bool(True)
    let assert "undefined" = type_of_nil(Nil)
    let assert False = not(True)
    let assert True = not(False)
    let assert Nil = nothing()
    let assert Ok(a) = utf_codepoint(97)
    let assert Ok(b) = utf_codepoint(98)
    let assert True = next(a) == b
    let assert 0 = undefined_int()
}

@external(webassembly, "builtins", "_int_to_utf_codepoint")
fn utf_codepoint(a: Int) -> Result(UtfCodepoint, Nil)
"#,
        r#"
import { UtfCodepoint } from "../gleam.mjs";
export const add = (a, b) => a + b;
export const half = (a) => a / 2;
export const type_of = (a) => typeof a;
export const not = (a) => !a;
export const nothing = () => undefined;
export const next = (a) => new UtfCodepoint(a.value + 1);
"#,
    );
}

/// A function of a module of the runtime, like `node:path`, is imported by
/// its name, as on JavaScript.
#[test]
fn host_runtime_module() {
    run_with_host(
        r#"
@external(webassembly, "node:path", "basename")
fn basename(path: String) -> String

pub fn main() {
    let assert "c.txt" = basename("/a/b/c.txt")
}
"#,
        "",
    );
}

#[test]
fn host_strings() {
    run_with_host(
        r#"
@external(webassembly, "./ffi.mjs", "shout")
fn shout(a: String) -> String
@external(webassembly, "./ffi.mjs", "length")
fn length(a: String) -> Int
@external(webassembly, "./ffi.mjs", "nothing")
fn nothing() -> String
@external(webassembly, "./ffi.mjs", "number")
fn number() -> String

pub fn main() {
    let assert "OLÁ 😀!" = shout("olá 😀")
    let assert "!" = shout("")
    let assert 3 = length("a😀")
    // A value that is not a string becomes one, as on JavaScript.
    let assert "undefined" = nothing()
    let assert "42" = number()
}
"#,
        r#"
export const shout = (a) => a.toUpperCase() + "!";
export const length = (a) => a.length;
export const nothing = () => undefined;
export const number = () => 42;
"#,
    );
}

/// A list is a `List` of the prelude, a `Result` an `Ok` or an `Error`, and
/// a tuple an array, with their items converted.
#[test]
fn host_lists_results_and_tuples() {
    run_with_host(
        r#"
@external(webassembly, "./ffi.mjs", "reverse")
fn reverse(a: List(Int)) -> List(Int)
@external(webassembly, "./ffi.mjs", "parse")
fn parse(a: String) -> Result(Int, String)
@external(webassembly, "./ffi.mjs", "is_ok")
fn is_ok(a: Result(Float, Nil)) -> Bool
@external(webassembly, "./ffi.mjs", "swap")
fn swap(a: #(Int, String, Nil)) -> #(Nil, String, Int)
@external(webassembly, "./ffi.mjs", "nested")
fn nested() -> List(Result(List(String), #(Bool, Int)))

pub fn main() {
    let assert [3, 2, 1] = reverse([1, 2, 3])
    let assert [] = reverse([])
    let assert Ok(12) = parse("12")
    let assert Error("Not a number: x") = parse("x")
    let assert True = is_ok(Ok(1.5))
    let assert False = is_ok(Error(Nil))
    let assert #(Nil, "a", 1) = swap(#(1, "a", Nil))
    let assert [Ok(["a", "b"]), Error(#(True, 2)), Ok([])] = nested()
}
"#,
        r#"
import { Ok, Error, List, toList } from "../gleam.mjs";
export const reverse = (a) => List.fromArray(a.toArray().reverse());
export function parse(a) {
  const n = Number(a);
  return Number.isNaN(n) ? new Error(`Not a number: ${a}`) : new Ok(n);
}
export const is_ok = (a) => a instanceof Ok && a[0] === 1.5;
export const swap = ([a, b, c]) => [c, b, a];
export const nested = () =>
  toList([new Ok(toList(["a", "b"])), new Error([true, 2]), new Ok(toList([]))]);
"#,
    );
}

/// A bit array is a `BitArray` of the prelude, with the offset of its first
/// bit in its first byte.
#[test]
fn host_bit_arrays() {
    run_with_host(
        r#"
@external(webassembly, "./ffi.mjs", "reverse")
fn reverse(a: BitArray) -> BitArray
@external(webassembly, "./ffi.mjs", "describe")
fn describe(a: BitArray) -> #(Int, Int, List(Int))
@external(webassembly, "./ffi.mjs", "same")
fn same(a: BitArray) -> BitArray

pub fn main() {
    let assert <<3, 2, 1>> = reverse(<<1, 2, 3>>)
    let assert <<>> = reverse(<<>>)
    let assert <<_:4, rest:bits>> = <<0xAB, 0xCD>>
    let assert #(12, 4, [0xAB, 0xCD]) = describe(rest)
    let assert True = same(rest) == rest
    let assert <<0xB:size(4), 0xCD>> = <<same(rest):bits>>
    let assert #(16, 0, [0xAB, 0xCD]) = describe(<<0xAB, 0xCD>>)
}
"#,
        r#"
import { BitArray, toList } from "../gleam.mjs";
export const reverse = (a) => new BitArray(a.rawBuffer.slice().reverse());
export const describe = (a) => [a.bitSize, a.bitOffset, toList([...a.rawBuffer])];
export const same = (a) => a;
"#,
    );
}

/// A value whose type is a type variable of the declaration is converted by
/// the type it is used with, and the host can give it back.
#[test]
fn host_generic_values() {
    run_with_host(
        r#"
@external(webassembly, "./ffi.mjs", "id")
fn id(a: a) -> a
@external(webassembly, "./ffi.mjs", "type_of")
fn type_of(a: a) -> String
@external(webassembly, "./ffi.mjs", "first")
fn first(a: List(a)) -> Result(a, Nil)
@external(webassembly, "./ffi.mjs", "is_array")
fn is_array(a: a) -> Bool
@external(webassembly, "./ffi.mjs", "is_point")
fn is_point(a: a) -> Bool

pub type Point {
    Point(x: Int, y: Int)
}

pub fn main() {
    let assert 1 = id(1)
    let assert 1.5 = id(1.5)
    let assert True = id(True)
    let assert "a" = id("a")
    let assert [1, 2] = id([1, 2])
    let assert Point(1, 2) = id(Point(1, 2))
    let f = id(fn(x) { x + 1 })
    let assert 2 = f(1)
    let assert "object" = type_of([1])
    let assert "number" = type_of(1)
    let assert "string" = type_of("a")
    let assert "function" = type_of(fn(x) { x })
    let assert True = is_array(#(1, "a"))
    let assert True = is_point(Point(1, 2))
    let assert Ok(Point(3, 4)) = first([Point(3, 4)])
    let assert Error(Nil) = first([])
}
"#,
        r#"
import { Ok, Error } from "../gleam.mjs";
import { Point } from "./mod.mjs";
export const id = (a) => a;
export const type_of = (a) => typeof a;
export const first = (a) => (a.head === undefined ? new Error(undefined) : new Ok(a.head));
export const is_array = (a) => Array.isArray(a) && a[0] === 1 && a[1] === "a";
export const is_point = (a) => a instanceof Point && a.x === 1 && a.y === 2;
"#,
    );
}

/// Outside the standard library, an external type without
/// `@external(webassembly, ...)` is a type of values of the host, which are
/// compared, hashed and shown by the host, as on JavaScript.
#[test]
fn host_external_types() {
    run_with_host_in(
        "app",
        r#"
import builtins.{cast, hash, inspect}
import my/regex.{type Regex}

@external(webassembly, "./ffi.mjs", "id")
fn id(a: a) -> a

type Holder {
    Holder(regex: Regex, name: String)
}

pub fn main() {
    let a = regex.new("a+")
    let assert True = regex.matches(a, "caab")
    let assert False = regex.matches(regex.new("b"), "caa")
    let assert True = a == regex.new("a+")
    let assert False = a == regex.new("b")
    let assert True = Holder(a, "a") == Holder(regex.new("a+"), "a")
    let assert True = hash(a) == hash(regex.new("a+"))
    let assert "//js(/a+/)" = inspect(a)
    let assert "Holder(//js(/a+/), \"a\")" = inspect(Holder(a, "a"))
    let assert True = regex.matches(id(a), "a")
    let assert [True, False] = regex.all([a, regex.new("x")], "a")
    let assert True = cast(a) == cast(regex.new("a+"))
    let assert False = cast(a) == cast(regex.new("b"))
    let assert "//js(/a+/)" = inspect(cast(a))
}
"#,
        vec![
            (
                super::TEST_PACKAGE,
                "builtins",
                r#"
@external(webassembly, "builtins", "_inspect")
pub fn inspect(a: a) -> String
@external(webassembly, "builtins", "_hash")
pub fn hash(a: a) -> Int
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic
@external(webassembly, "builtins", "_dynamic_cast")
pub fn cast(a: a) -> Dynamic
"#,
            ),
            (
                "app",
                "my/regex",
                r#"
pub type Regex

@external(webassembly, "./ffi.mjs", "new_regex")
pub fn new(source: String) -> Regex

@external(webassembly, "./ffi.mjs", "matches")
pub fn matches(regex: Regex, text: String) -> Bool

@external(webassembly, "./ffi.mjs", "all")
pub fn all(regexes: List(Regex), text: String) -> List(Bool)
"#,
            ),
        ],
        r#"
import { toList } from "../gleam.mjs";
export const id = (a) => a;
export const new_regex = (source) => new RegExp(source);
export const matches = (regex, text) => regex.test(text);
export const all = (regexes, text) => toList(regexes.toArray().map((r) => r.test(text)));
"#,
    );
}

/// A value of a custom type is an instance of the class of its variant, as on
/// JavaScript, from the module of the custom types of its module.
#[test]
fn host_custom_types() {
    run_with_host(
        r#"
pub type Point {
    Point(x: Int, y: Int)
}

pub type Shape {
    Circle(center: Point, radius: Float)
    Polygon(List(Point))
    Empty
}

pub type Color {
    Red
    Green
    Blue
}

pub type Option(a) {
    Some(a)
    None
}

type Secret {
    Secret(String)
}

@external(webassembly, "./ffi.mjs", "describe")
fn describe(shape: Shape) -> String
@external(webassembly, "./ffi.mjs", "id")
fn shape(shape: Shape) -> Shape
@external(webassembly, "./ffi.mjs", "id")
fn color(color: Color) -> Color
@external(webassembly, "./ffi.mjs", "next")
fn next(color: Color) -> Color
@external(webassembly, "./ffi.mjs", "id")
fn option(option: Option(Int)) -> Option(Int)
@external(webassembly, "./ffi.mjs", "first")
fn first(list: List(a)) -> Option(a)
@external(webassembly, "./ffi.mjs", "reveal")
fn reveal(secret: Secret) -> String
@external(webassembly, "./ffi.mjs", "hide")
fn hide(text: String) -> Secret
@external(webassembly, "./ffi.mjs", "origin")
fn origin() -> Point

pub fn main() {
    let assert "circle 1,2 3.5" = describe(Circle(Point(1, 2), 3.5))
    let assert "polygon 2" = describe(Polygon([Point(0, 0), Point(1, 1)]))
    let assert "empty" = describe(Empty)
    let assert Circle(Point(1, 2), 3.5) = shape(Circle(Point(1, 2), 3.5))
    let assert Polygon([Point(0, 0)]) = shape(Polygon([Point(0, 0)]))
    let assert Empty = shape(Empty)
    let assert Green = color(Green)
    let assert Blue = next(Green)
    let assert Red = next(Blue)
    let assert Some(1) = option(Some(1))
    let assert None = option(None)
    let assert Some(Point(1, 2)) = first([Point(1, 2)])
    let assert None = first([])
    let assert "abc" = reveal(Secret("abc"))
    let assert Secret("xyz") = hide("xyz")
    let assert Point(0, 0) = origin()
}
"#,
        r#"
import { Circle, Polygon, Empty, Shape$isCircle, Shape$isPolygon, Red, Green, Blue, Some, None, Secret } from "./mod.mjs";
export const id = (a) => a;
export function describe(shape) {
  if (shape instanceof Circle) return `circle ${shape.center.x},${shape.center.y} ${shape.radius}`;
  if (Shape$isPolygon(shape)) return `polygon ${shape[0].toArray().length}`;
  if (shape instanceof Empty) return "empty";
  return "?";
}
export function next(color) {
  if (color instanceof Red) return new Green();
  if (color instanceof Green) return new Blue();
  return new Red();
}
export const first = (list) => (list.head === undefined ? new None() : new Some(list.head));
export const reveal = (secret) => secret[0];
export const hide = (text) => new Secret(text);
// A record of a type with a single variant is read by its fields.
export const origin = () => ({ x: 0, y: 0 });
"#,
    );
}

/// A function is a JavaScript function, which converts its arguments and its
/// result, and a JavaScript function is a function.
#[test]
fn host_functions() {
    run_with_host(
        r#"
pub type Point {
    Point(x: Int, y: Int)
}

@external(webassembly, "./ffi.mjs", "apply")
fn apply(f: fn(Int, String) -> String, a: Int, b: String) -> String
@external(webassembly, "./ffi.mjs", "map")
fn map(list: List(a), f: fn(a) -> b) -> List(b)
@external(webassembly, "./ffi.mjs", "adder")
fn adder(n: Int) -> fn(Int) -> Int
@external(webassembly, "./ffi.mjs", "id")
fn id(f: fn(Point) -> Point) -> fn(Point) -> Point
@external(webassembly, "./ffi.mjs", "twice")
fn twice(f: fn() -> Nil) -> Nil
@external(webassembly, "./ffi.mjs", "id")
fn same_function(f: fn(Int) -> Int) -> fn(Int) -> Int
@external(webassembly, "./ffi.mjs", "same")
fn same(f: fn(Int) -> Int, g: fn(Int) -> Int) -> Bool

pub fn main() {
    let assert "3 a" = apply(fn(a, b) { to_string(a + 1) <> " " <> b }, 2, "a")
    let assert [2, 4, 6] = map([1, 2, 3], fn(x) { x * 2 })
    let assert [#(1, "a")] = map([1], fn(x) { #(x, "a") })
    let add = adder(10)
    let assert 15 = add(5)
    let assert 20 = adder(10)(10)
    let move = id(fn(p: Point) { Point(..p, x: p.x + 1) })
    let assert Point(2, 1) = move(Point(1, 1))
    twice(fn() { Nil })
    // A function crosses as the same value every time.
    let double = fn(x) { x * 2 }
    let assert True = same_function(double) == double
    let assert True = same(double, double)
    let assert True = same_function(add) == add
    let assert True = same(add, add)
    let assert False = same(add, double)
}

fn to_string(n: Int) -> String {
    case n {
        3 -> "3"
        _ -> "?"
    }
}
"#,
        r#"
import { toList } from "../gleam.mjs";
export const apply = (f, a, b) => f(a, b);
export const map = (list, f) => toList(list.toArray().map(f));
export const adder = (n) => (m) => n + m;
export const id = (f) => f;
export const same = (f, g) => f === g;
export function twice(f) {
  f();
  f();
}
"#,
    );
}

/// A value of the host is a `Dynamic` of a value of the host, which is
/// compared and shown as on JavaScript.
#[test]
fn host_dynamic_values() {
    run_with_host(
        r#"
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic
@external(webassembly, "builtins", "dynamic_classify")
fn classify(data: Dynamic) -> String
@external(webassembly, "builtins", "_inspect")
fn inspect(a: a) -> String
@external(webassembly, "./ffi.mjs", "object")
fn object(a: Int) -> Dynamic
@external(webassembly, "./ffi.mjs", "thrown")
fn thrown(a: Int) -> Exception

pub type Exception {
    Thrown(Dynamic)
}

pub fn main() {
    let assert Thrown(value) = thrown(1)
    let assert True = value == object(1)
    let assert "Object" = classify(object(1))
    let assert "//js({ \"a\": 1 })" = inspect(object(1))
    let assert True = object(1) == object(1)
    let assert False = object(1) == object(2)
}
"#,
        r#"
import { Thrown } from "./mod.mjs";
export const object = (a) => ({ a });
export const thrown = (a) => new Thrown({ a });
"#,
    );
}

/// A plain object of the host is read by `decode.field` and `decode.dict` as
/// the dict of its properties, as on JavaScript, and is equal to that dict,
/// though it is the same object when it goes back to the host. `null` is
/// `Nil`.
#[test]
fn host_plain_objects() {
    run_with_host(
        r#"
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic
@external(webassembly, "builtins", "Dict")
pub type Dict(k, v)
@external(webassembly, "builtins", "dict_new")
fn dict_new() -> Dict(k, v)
@external(webassembly, "builtins", "dict_insert")
fn dict_insert(dict: Dict(k, v), key: k, value: v) -> Dict(k, v)
@external(webassembly, "builtins", "dynamic_dict")
fn dynamic_dict(dict: Dict(Dynamic, Dynamic)) -> Dynamic
@external(webassembly, "builtins", "dynamic_string")
fn string(a: String) -> Dynamic
@external(webassembly, "builtins", "dynamic_int")
fn int(a: Int) -> Dynamic
@external(webassembly, "builtins", "dynamic_index")
fn index(data: Dynamic, key: Dynamic) -> Result(Result(Dynamic, Nil), String)
@external(webassembly, "builtins", "dynamic_is_null")
fn is_null(a: Dynamic) -> Bool
@external(webassembly, "./ffi.mjs", "object")
fn object(a: Int) -> Dynamic
@external(webassembly, "./ffi.mjs", "nested")
fn nested() -> Dynamic
@external(webassembly, "./ffi.mjs", "null")
fn null() -> Dynamic
@external(webassembly, "./ffi.mjs", "same")
fn same(a: Dynamic, b: Dynamic) -> Bool

pub fn main() {
    let a = object(1)
    let assert Ok(Ok(value)) = index(a, string("a"))
    let assert True = value == int(1)
    let assert Ok(Error(Nil)) = index(a, string("b"))
    let assert True = a == dynamic_dict(dict_insert(dict_new(), string("a"), int(1)))
    let assert True = same(a, a)
    let assert Ok(Ok(inner)) = index(nested(), string("inner"))
    let assert Ok(Ok(value)) = index(inner, string("a"))
    let assert True = value == int(2)
    let assert True = is_null(null())
}
"#,
        r#"
export const object = (a) => ({ a });
const inner = { a: 2 };
export const nested = () => ({ inner });
const null_ = () => null;
export { null_ as null };
export function same(a, b) {
  return a === b && a.a === 1;
}
"#,
    );
}

/// A value of a type variable bound to nothing is a value of the host, as on
/// JavaScript.
#[test]
fn host_unbound_type_variable() {
    run_with_host(
        r#"
@external(webassembly, "./ffi.mjs", "parse")
fn parse(a: String) -> Result(a, Nil)
@external(webassembly, "./ffi.mjs", "stringify")
fn stringify(a: a) -> String
@external(webassembly, "./ffi.mjs", "call")
fn call(f: fn(a) -> a) -> String

pub fn main() {
    let assert Ok(value) = parse("[1,{\"a\":2}]")
    let assert "[1,{\"a\":2}]" = stringify(value)
    let assert "x" = call(fn(x) { x })
}
"#,
        r#"
import { Ok } from "../gleam.mjs";
export const parse = (a) => new Ok(JSON.parse(a));
export const stringify = (a) => JSON.stringify(a);
export const call = (f) => f("x");
"#,
    );
}

/// A module of the host can export a function `then`, and the strings
/// that cross keep a byte order mark at their start.
#[test]
fn host_module_with_then() {
    run_with_host(
        r#"
@external(webassembly, "./ffi.mjs", "then")
fn then(a: Int) -> Int
@external(webassembly, "./ffi.mjs", "bom")
fn bom() -> String

pub fn main() {
    let assert 2 = then(1)
    let assert "\u{FEFF}a" = bom()
}
"#,
        r#"
export const then = (a) => a + 1;
export const bom = () => "﻿a";
"#,
    );
}

/// The output of the program is written to `process.stdout` and
/// `process.stderr`, as on JavaScript.
#[test]
fn host_output_streams() {
    run_with_host(
        r#"
@external(webassembly, "./ffi.mjs", "capture")
fn capture(f: fn() -> Nil) -> String

pub fn main() {
    let assert "src/my/mod.gleam:7\n1\n" = capture(fn() {
      echo 1
      Nil
    })
}
"#,
        r#"
export function capture(f) {
  const chunks = [];
  const write = process.stderr.write;
  process.stderr.write = (chunk) => chunks.push(chunk);
  try {
    f();
  } finally {
    process.stderr.write = write;
  }
  return chunks.join("");
}
"#,
    );
}

/// A value that crosses to the host as an opaque value crosses back as the
/// same value, also through a `Dynamic`, like in the JavaScript target.
#[test]
fn host_values_back() {
    run_with_host(
        r#"
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic
@external(webassembly, "./ffi.mjs", "put")
fn put(a: a) -> Nil
@external(webassembly, "./ffi.mjs", "get")
fn get() -> Dynamic
@external(webassembly, "./ffi.mjs", "identity")
fn to_list(a: Dynamic) -> List(String)
@external(webassembly, "./ffi.mjs", "identity")
fn to_color(a: Dynamic) -> Color
@external(webassembly, "./ffi.mjs", "identity")
fn to_function(a: Dynamic) -> fn(Int) -> Int
@external(webassembly, "./ffi.mjs", "identity")
fn to_dynamic(a: Dynamic) -> Dynamic
@external(webassembly, "./ffi.mjs", "identity")
fn to_int(a: Dynamic) -> Int

pub type Color {
    Red
    Rgb(Int, Int, Int)
}

pub fn main() {
    put(["a", "b"])
    let assert ["a", "b"] = to_list(get())
    put(10)
    let assert 10 = to_int(get())
    put(Rgb(1, 2, 3))
    let assert Rgb(1, 2, 3) = to_color(get())
    let add = fn(a) { a + 1 }
    put(add)
    let assert 2 = to_function(get())(1)
    let assert True = to_dynamic(get()) == get()
}
"#,
        r#"
let value;
export const put = (a) => { value = a; };
export const get = () => value;
export const identity = (a) => a;
"#,
    );
}

/// A value of a custom type that crosses back as a `Dynamic` is a value of the
/// host, though its structure is the one of a `Dynamic`, and a `Dynamic`
/// crosses back as itself.
#[test]
fn host_values_back_as_dynamic() {
    run_with_host(
        r#"
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic
@external(webassembly, "builtins", "dynamic_classify")
fn classify(data: Dynamic) -> String
@external(webassembly, "builtins", "dynamic_int")
fn int(a: Int) -> Dynamic
@external(webassembly, "./ffi.mjs", "identity")
fn from(a: a) -> Dynamic
@external(webassembly, "./ffi.mjs", "identity")
fn to_dynamic(a: Dynamic) -> Dynamic

pub type Shape {
    Circle(Int)
    Name(String)
}

pub fn main() {
    let assert "Object" = classify(from(Circle(1)))
    let assert "Object" = classify(from(Name("a")))
    let assert "Int" = classify(to_dynamic(int(1)))
    let assert True = to_dynamic(int(1)) == int(1)
}
"#,
        r#"export const identity = (a) => a;"#,
    );
}

/// A value of the host is a `Dynamic` read as on JavaScript, and a `Dynamic`
/// crosses as the value it holds.
#[test]
fn host_values_as_dynamic() {
    run_with_host(
        r#"
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic
@external(webassembly, "builtins", "_dynamic_cast")
fn cast(a: a) -> Dynamic
@external(webassembly, "./ffi.mjs", "identity")
fn from(a: a) -> Dynamic
@external(webassembly, "./ffi.mjs", "identity")
fn to_strings(a: Dynamic) -> List(String)
@external(webassembly, "./ffi.mjs", "identity")
fn to_pair(a: Dynamic) -> #(Int, Bool)
@external(webassembly, "./ffi.mjs", "type_of")
fn type_of(a: Dynamic) -> String

pub fn main() {
    let assert True = from(1) == cast(1)
    let assert True = from(1.5) == cast(1.5)
    let assert True = from(2.0) == cast(2)
    let assert True = from("a") == cast("a")
    let assert True = from(True) == cast(True)
    let assert True = from(Nil) == cast(Nil)
    let assert True = from(<<1, 2>>) == cast(<<1, 2>>)
    let assert True = from([[1], [2, 3]]) == cast([[1], [2, 3]])
    let assert True = from(#(1, "a", [True])) == cast(#(1, "a", [True]))
    let assert ["a", "b"] = to_strings(cast(["a", "b"]))
    let assert #(1, False) = to_pair(cast(#(1, False)))
    let assert "number" = type_of(cast(1))
}
"#,
        r#"
export const identity = (a) => a;
export const type_of = (a) => typeof a;
"#,
    );
}

/// A dict is a `Dict` of the module `dict.mjs` of the stdlib, as on
/// JavaScript, also as a `Dynamic`.
#[test]
fn host_dicts() {
    run_with_host_files(
        "app",
        r#"
import builtins.{type Dynamic, cast}
import gleam/dict.{type Dict}

@external(webassembly, "./ffi.mjs", "id")
fn id(a: a) -> a
@external(webassembly, "./ffi.mjs", "id")
fn from(a: a) -> Dynamic
@external(webassembly, "./ffi.mjs", "size")
fn size(a: Dict(String, Int)) -> Int
@external(webassembly, "./ffi.mjs", "make")
fn make() -> Dict(String, List(Int))
@external(webassembly, "./ffi.mjs", "class_name")
fn class_name(a: Dynamic) -> String

pub fn main() {
    let a = dict.from_list([#("a", 1), #("b", 2)])
    let assert 2 = size(a)
    let assert True = id(a) == a
    let assert [#("x", [1, 2])] = dict.to_list(make())
    let assert True = from(a) == cast(a)
    let assert True = from(make()) == cast(make())
    let assert "Dict" = class_name(cast(a))
    let assert True = id(cast(a)) == cast(a)
}
"#,
        vec![
            (
                super::TEST_PACKAGE,
                "builtins",
                r#"
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic
@external(webassembly, "builtins", "_dynamic_cast")
pub fn cast(a: a) -> Dynamic
"#,
            ),
            (
                "gleam_stdlib",
                "gleam/dict",
                r#"
@external(webassembly, "builtins", "Dict")
pub type Dict(k, v)
@external(webassembly, "builtins", "dict_from_list")
pub fn from_list(list: List(#(k, v))) -> Dict(k, v)
@external(webassembly, "builtins", "dict_to_list")
pub fn to_list(dict: Dict(k, v)) -> List(#(k, v))
"#,
            ),
        ],
        &[
            (
                "ffi.mjs",
                r#"
import Dict, { from } from "../../gleam_stdlib/dict.mjs";
import { toList } from "../gleam.mjs";
export const id = (a) => a;
export const size = (dict) => (dict instanceof Dict ? dict.entries.length : -1);
export const make = () => from([["x", toList([1, 2])]]);
export const class_name = (a) => a.constructor.name;
"#,
            ),
            (
                "../../gleam_stdlib/dict.mjs",
                r#"
export default class Dict {
  constructor(entries) {
    this.entries = entries;
  }
}
export const from = (entries) => new Dict([...entries]);
export const fold = (dict, state, fun) =>
  dict.entries.reduce((state, [key, value]) => fun(state, key, value), state);
"#,
            ),
        ],
    );
}

/// A `panic`, `todo`, `let assert` or `assert` throws the `Error` of the
/// JavaScript target, which the host can catch.
#[test]
fn host_catches_errors() {
    run_with_host(
        r#"
@external(webassembly, "./ffi.mjs", "rescue")
fn rescue(f: fn() -> a) -> String

fn fails(n: Int) -> Int {
    case n {
        0 -> panic as "boom"
        1 -> todo
        2 -> {
            let assert Ok(m) = Error(n) as "not ok"
            m
        }
        _ -> {
            assert n == 4
            n
        }
    }
}

pub fn main() {
    let assert "panic src/my/mod.gleam my/mod fails 7 boom" = rescue(fn() { fails(0) })
    let assert "todo src/my/mod.gleam my/mod fails 8 `todo` expression evaluated. This code has not yet been implemented." = rescue(fn() { fails(1) })
    let assert "let_assert src/my/mod.gleam my/mod fails 10 not ok" = rescue(fn() { fails(2) })
    let assert "assert src/my/mod.gleam my/mod fails 14 Assertion failed." = rescue(fn() { fails(3) })
    let assert "" = rescue(fn() { fails(4) })
}
"#,
        r#"
export function rescue(f) {
  try {
    f();
    return "";
  } catch (e) {
    return [e.gleam_error, e.file, e.module, e.function, e.line, e.message].join(" ");
  }
}
"#,
    );
}

/// The error of a `let assert` or an `assert` has the fields of the error of
/// the JavaScript target: the positions in the source and the values of the
/// operands.
#[test]
fn host_error_details() {
    let fails = r#"
pub fn let_assert(n: Int) -> Int {
  let assert 1 = n
  n
}

pub fn binary_operator(a: Int, b: Int) -> Nil {
  assert a + 1 == b
}

pub fn function_call(a: Int) -> Nil {
  assert is_one(a, 2)
}

fn is_one(a: Int, _b: Int) -> Bool {
  a == 1
}

pub fn and(a: Bool, b: Bool) -> Nil {
  assert a && b
}

pub fn or(a: Bool, b: Bool) -> Nil {
  assert a || b
}

pub fn expression(a: Bool) -> Nil {
  assert !a
}
"#;
    // The start and end of `part` in the first line with `line`, after its
    // `assert`.
    let span = |line: &str, part: &str| {
        let after = line
            .find("assert ")
            .map_or(0, |start| start + "assert ".len());
        let start = fails.find(line).expect("the line")
            + line[after..].find(part).map_or(0, |start| after + start);
        (start, start + part.len())
    };
    let expression = |kind: &str, value: Option<&str>, (start, end): (usize, usize)| match value {
        Some(value) => {
            format!(r#"{{"kind":"{kind}","value":{value},"start":{start},"end":{end}}}"#)
        }
        None => format!(r#"{{"kind":"{kind}","start":{start},"end":{end}}}"#),
    };
    let assert = |line: &str, value: &str, fields: String| {
        let (start, _) = span(line, line);
        let (expression_start, end) = span(line, value);
        format!(r#"{{{fields},"start":{start},"end":{end},"expression_start":{expression_start}}}"#)
    };
    let binary_operator = |line: &str, value: &str, operator: &str, left: String, right: String| {
        assert(
            line,
            value,
            format!(
                r#""kind":"binary_operator","operator":"{operator}","left":{left},"right":{right}"#
            ),
        )
    };
    let (start, _) = span("let assert 1 = n", "let");
    let (pattern_start, pattern_end) = span("let assert 1 = n", "1");
    let (_, end) = span("let assert 1 = n", "n");
    let cases = [
        (
            "let_assert(7)",
            format!(
                r#"{{"value":7,"start":{start},"end":{end},"pattern_start":{pattern_start},"pattern_end":{pattern_end}}}"#
            ),
        ),
        (
            "binary_operator(2, 4)",
            binary_operator(
                "assert a + 1 == b",
                "a + 1 == b",
                "==",
                expression("expression", Some("3"), span("assert a + 1 == b", "a + 1")),
                expression("expression", Some("4"), span("assert a + 1 == b", "b")),
            ),
        ),
        (
            "function_call(5)",
            assert(
                "assert is_one(a, 2)",
                "is_one(a, 2)",
                format!(
                    r#""kind":"function_call","arguments":[{},{}]"#,
                    expression("expression", Some("5"), span("assert is_one(a, 2)", "a")),
                    expression("literal", Some("2"), span("assert is_one(a, 2)", "2")),
                ),
            ),
        ),
        (
            "and(True, False)",
            binary_operator(
                "assert a && b",
                "a && b",
                "&&",
                expression("expression", Some("true"), span("assert a && b", "a")),
                expression("expression", Some("false"), span("assert a && b", "b")),
            ),
        ),
        (
            "and(False, True)",
            binary_operator(
                "assert a && b",
                "a && b",
                "&&",
                expression("expression", Some("false"), span("assert a && b", "a")),
                expression("unevaluated", None, span("assert a && b", "b")),
            ),
        ),
        (
            "or(False, False)",
            binary_operator(
                "assert a || b",
                "a || b",
                "||",
                expression("expression", Some("false"), span("assert a || b", "a")),
                expression("expression", Some("false"), span("assert a || b", "b")),
            ),
        ),
        (
            "expression(True)",
            assert(
                "assert !a",
                "!a",
                format!(
                    r#""kind":"expression","expression":{}"#,
                    expression("expression", Some("false"), span("assert !a", "!a")),
                ),
            ),
        ),
    ];
    let mut src = String::from(
        r#"
import my/fails

@external(webassembly, "./ffi.mjs", "details")
fn details(f: fn() -> a) -> String

pub fn main() {
"#,
    );
    for (call, expected) in cases {
        let expected = expected.replace('"', "\\\"");
        src.push_str(&format!(
            "    let assert \"{expected}\" = details(fn() {{ fails.{call} }})\n"
        ));
    }
    src.push_str("}\n");
    run_with_host_in(
        super::TEST_PACKAGE,
        &src,
        vec![(super::TEST_PACKAGE, "my/fails", fails)],
        r#"
const fields = ["kind", "operator", "left", "right", "arguments", "expression", "value",
  "start", "end", "expression_start", "pattern_start", "pattern_end"];

export function details(f) {
  try {
    f();
    return "";
  } catch (e) {
    return JSON.stringify(e, fields);
  }
}
"#,
    );
}

/// A value that the host returns as a `List`, a `Result`, a `BitArray` or a
/// custom type with variants is checked to be one.
#[test]
fn host_wrong_values() {
    let ffi = r#"export const wrong = () => 42;"#;
    for (type_, expected) in [
        ("List(Int)", "Not a List: 42"),
        ("Result(Int, Nil)", "Not a Result: 42"),
        ("BitArray", "Not a BitArray: 42"),
        ("Color", "Not a variant of the custom type: 42"),
    ] {
        for string in [WasmString::Utf8, WasmString::JsString] {
            let src = format!(
                r#"
pub type Color {{
    Red
    Green
}}

@external(webassembly, "./ffi.mjs", "wrong")
fn wrong() -> {type_}

pub fn main() {{
    wrong()
}}
"#
            );
            let config = WebAssemblyConfig {
                string,
                ..WebAssemblyConfig::default()
            };
            let result = super::run_wasm_node(&src, vec![], config, &[("ffi.mjs", ffi)]);
            assert!(!result.status.success());
            assert!(result.stderr.contains(expected), "{}", result.stderr);
        }
    }
}
