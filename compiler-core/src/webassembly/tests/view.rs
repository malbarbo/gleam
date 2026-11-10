// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `dynamic_view`, which opens one level of a `Dynamic`.

use super::{run_wasm, runtime::DICT};

/// `show` writes a value with `dynamic_view`, one level at a time.
const VIEW: &str = r##"
@external(webassembly, "builtins", "Dynamic")
pub type Dynamic

@external(webassembly, "builtins", "_dynamic_cast")
pub fn cast(a: anything) -> Dynamic

@external(webassembly, "builtins", "dynamic_inspect")
fn inspect(data: Dynamic) -> String

@external(webassembly, "builtins", "dynamic_view")
fn view(
  data: Dynamic,
  int: fn(Int) -> v,
  float: fn(Float) -> v,
  string: fn(String) -> v,
  bool: fn(Bool) -> v,
  nil: v,
  bit_array: fn(BitArray) -> v,
  utf_codepoint: fn(UtfCodepoint) -> v,
  tuple: fn(List(Dynamic)) -> v,
  empty: v,
  cons: fn(Dynamic, Dynamic) -> v,
  empty_dict: v,
  entry: fn(Dynamic, Dynamic, Dynamic) -> v,
  record: fn(String, String, String, List(String), List(Dynamic), Bool) -> v,
  function: fn(Int) -> v,
  external: fn(String, String) -> v,
) -> v

pub fn show(data: Dynamic) -> String {
  view(
    data,
    fn(_) { "int " <> inspect(data) },
    fn(_) { "float " <> inspect(data) },
    fn(_) { "string " <> inspect(data) },
    fn(_) { "bool " <> inspect(data) },
    "nil",
    fn(_) { "bit_array " <> inspect(data) },
    fn(_) { "utf_codepoint " <> inspect(data) },
    fn(items) { "#(" <> join(items, []) <> ")" },
    "[]",
    fn(first, rest) { show(first) <> " :: " <> show(rest) },
    "{}",
    fn(key, value, rest) { show(key) <> " => " <> show(value) <> "; " <> show(rest) },
    fn(module, name, constructor, labels, fields, is_opaque) {
      let prefix = case is_opaque {
        True -> "opaque "
        False -> ""
      }
      prefix <> module <> "." <> name <> "." <> constructor
        <> "(" <> join(fields, labels) <> ")"
    },
    fn(arity) { "fn/" <> inspect(cast(arity)) },
    fn(module, name) { "external " <> module <> "." <> name },
  )
}

fn join(items: List(Dynamic), labels: List(String)) -> String {
  let #(label, labels) = case labels {
    [] | ["", ..] -> #("", drop(labels))
    [label, ..rest] -> #(label <> ": ", rest)
  }
  case items {
    [] -> ""
    [item] -> label <> show(item)
    [item, ..rest] -> label <> show(item) <> ", " <> join(rest, labels)
  }
}

fn drop(labels: List(String)) -> List(String) {
  case labels {
    [] -> []
    [_, ..rest] -> rest
  }
}
"##;

#[track_caller]
fn assert_views(src: &str) {
    let result = run_wasm(
        src,
        vec![
            (super::TEST_PACKAGE, "dict", DICT),
            (super::TEST_PACKAGE, "view", VIEW),
        ],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn view_of_the_values_with_variants() {
    assert_views(
        r#"
import view.{cast, show}

@external(webassembly, "builtins", "_unsafe_int_to_utf_codepoint")
fn codepoint(n: Int) -> UtfCodepoint

pub fn main() {
  assert show(cast(1)) == "int 1"
  assert show(cast(1.5)) == "float 1.5"
  assert show(cast("a")) == "string \"a\""
  assert show(cast(True)) == "bool True"
  assert show(cast(Nil)) == "nil"
  assert show(cast(<<1>>)) == "bit_array <<1>>"
  assert show(cast(codepoint(97))) == "utf_codepoint //utfcodepoint(a)"
}
"#,
    );
}

#[test]
fn view_of_lists_and_tuples() {
    assert_views(
        r##"
import view.{cast, show}

pub fn main() {
  assert show(cast([1, 2])) == "int 1 :: int 2 :: []"
  assert show(cast(#(1, "a"))) == "#(int 1, string \"a\")"
}
"##,
    );
}

/// A dict gives its entries in the order of the hashes, one at a time.
#[test]
fn view_of_dicts() {
    assert_views(
        r#"
import dict
import view.{cast, show}

pub fn main() {
  assert show(cast(dict.new())) == "{}"
  assert show(cast(dict.from_list([#(1, "a")]))) == "int 1 => string \"a\"; {}"
  let shown = show(cast(dict.from_list([#(1, 10), #(2, 20)])))
  assert shown == "int 1 => int 10; int 2 => int 20; {}"
    || shown == "int 2 => int 20; int 1 => int 10; {}"
}
"#,
    );
}

/// `delete` does not find a key that is not equal to itself, and the view
/// still gives each entry once.
#[test]
fn view_of_a_dict_with_nan_keys() {
    assert_views(
        r#"
import dict
import view.{cast, show}

pub fn main() {
  let infinity = 1.0e308 *. 10.0
  let nan = infinity -. infinity
  let shown = show(cast(dict.from_list([#(nan, 1), #(nan, 2)])))
  assert shown == "float NaN.0 => int 1; float NaN.0 => int 2; {}"
    || shown == "float NaN.0 => int 2; float NaN.0 => int 1; {}"
}
"#,
    );
}

#[test]
fn view_of_records() {
    assert_views(
        r#"
import view.{cast, show}

type Point {
  Point(Int, y: Int)
}

type Shape {
  Circle(radius: Float)
  Square(Point, Point)
  Empty
}

type Color {
  Red
  Green
}

pub opaque type Secret {
  Secret(Int)
}

pub fn main() {
  assert show(cast(Point(1, 2))) == "my/mod.Point.Point(int 1, y: int 2)"
  assert show(cast(Circle(1.0))) == "my/mod.Shape.Circle(radius: float 1.0)"
  assert show(cast(Empty)) == "my/mod.Shape.Empty()"
  assert show(cast(Square(Point(1, 2), Point(3, 4))))
    == "my/mod.Shape.Square(my/mod.Point.Point(int 1, y: int 2), my/mod.Point.Point(int 3, y: int 4))"
  assert show(cast(Green)) == "my/mod.Color.Green()"
  assert show(cast(Secret(1))) == "opaque my/mod.Secret.Secret(int 1)"
  assert show(cast(Ok(1))) == "gleam.Result.Ok(int 1)"
  assert show(cast([Red])) == "my/mod.Color.Red() :: []"
}
"#,
    );
}

/// A variant without fields of a type with others is null, see `UnionLayout`.
#[test]
fn view_of_a_null_variant() {
    assert_views(
        r#"
import view.{cast, show}

type Option {
  None
  Some(Int)
}

pub fn main() {
  assert show(cast(None)) == "my/mod.Option.None()"
  assert show(cast(Some(1))) == "my/mod.Option.Some(int 1)"
}
"#,
    );
}

#[test]
fn view_of_a_recursive_type() {
    assert_views(
        r#"
import view.{cast, show}

type Tree {
  Leaf
  Node(Tree, Int, Tree)
}

pub fn main() {
  assert show(cast(Node(Leaf, 1, Node(Leaf, 2, Leaf))))
    == "my/mod.Tree.Node(my/mod.Tree.Leaf(), int 1, my/mod.Tree.Node(my/mod.Tree.Leaf(), int 2, my/mod.Tree.Leaf()))"
}
"#,
    );
}

/// The view of `T(Int)` needs the view of `T(#(Int, Int))` only when the
/// program builds one, see `get_function_constructor`.
#[test]
fn view_of_a_type_with_polymorphic_recursion() {
    assert_views(
        r#"
import view.{cast, show}

type T(a) {
  T(a, List(T(#(a, a))))
}

pub fn main() {
  assert show(cast(T(1, []))) == "my/mod.T.T(int 1, [])"
}
"#,
    );
}

#[test]
fn view_of_functions_and_externals() {
    assert_views(
        r#"
import view.{cast, show}

type I32

@external(webassembly, "builtins", "_int_to_i32")
fn i32(n: Int) -> I32

pub fn main() {
  assert show(cast(fn(a, b) { a + b })) == "fn/2"
  assert show(cast(i32(1))) == "external my/mod.I32"
}
"#,
    );
}
