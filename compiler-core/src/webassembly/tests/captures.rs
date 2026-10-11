use std::sync::Arc;

use src_span::SrcSpan;
use vec1::Vec1;

use crate::{
    ast::{
        FunctionLiteralKind, TypeAst, TypedArg, TypedStatement,
        visit::{Visit, visit_typed_expr_fn},
    },
    type_::Type,
    webassembly::captures::captures,
};

#[derive(Default)]
struct FunctionsVisit<'ast> {
    functions: Vec<(SrcSpan, &'ast [TypedArg], &'ast [TypedStatement])>,
}

impl<'ast> Visit<'ast> for FunctionsVisit<'ast> {
    fn visit_typed_expr_fn(
        &mut self,
        location: &'ast SrcSpan,
        type_: &'ast Arc<Type>,
        kind: &'ast FunctionLiteralKind,
        arguments: &'ast [TypedArg],
        body: &'ast Vec1<TypedStatement>,
        return_annotation: &'ast Option<TypeAst>,
    ) {
        self.functions.push((*location, arguments, body));
        visit_typed_expr_fn(
            self,
            location,
            type_,
            kind,
            arguments,
            body,
            return_annotation,
        );
    }
}

/// Asserts the names captured by each function literal of `main`, in
/// preorder.
#[track_caller]
fn assert_captures(src: &str, expected: &[&[&str]]) {
    let (module, _) = super::compile(src, vec![]);
    let main = module
        .definitions
        .functions
        .iter()
        .find(|function| {
            function
                .name
                .as_ref()
                .is_some_and(|(_, name)| name == "main")
        })
        .expect("main function");
    let mut visit = FunctionsVisit::default();
    for statement in &main.body {
        visit.visit_typed_statement(statement);
    }
    let actual: Vec<Vec<String>> = visit
        .functions
        .into_iter()
        .map(|(location, arguments, body)| {
            captures(location, arguments, body)
                .into_iter()
                .map(|capture| capture.name.to_string())
                .collect()
        })
        .collect();
    assert_eq!(actual, expected);
}

#[test]
fn no_captures() {
    assert_captures(
        r#"
pub fn main() {
  fn(x) { x + 1 }
}
"#,
        &[&[]],
    );
}

#[test]
fn captures_in_visit_order_without_repetition() {
    assert_captures(
        r#"
pub fn main() {
  let a = 1
  let b = 2
  fn(x) { b + a + b + x }
}
"#,
        &[&["b", "a"]],
    );
}

#[test]
fn shadowing() {
    assert_captures(
        r#"
pub fn main() {
  let a = 1
  let f = fn(x) {
    let a = 1
    a + x
  }
  let g = fn(x) {
    let y = a
    let a = 2
    y + a + x
  }
  #(f, g)
}
"#,
        &[&[], &["a"]],
    );
}

#[test]
fn nested_functions() {
    assert_captures(
        r#"
pub fn main() {
  let a = 1
  let f = fn() { fn() { a } }
  let g = fn(x) { fn() { x } }
  #(f, g)
}
"#,
        &[&["a"], &["a"], &[], &["x"]],
    );
}

#[test]
fn pattern_bindings() {
    assert_captures(
        r#"
pub type T {
  A(Int)
  B(Int)
}

pub fn main() {
  fn(x, s, b, t) {
    let #(p, q) = x
    let assert [h, ..rest] as whole = [p]
    let r = case s {
      "pre" as pre <> post -> pre <> post
      _ -> ""
    }
    let w = case b {
      <<len, v:size(len)>> -> v + len
      _ -> 0
    }
    let z = case t {
      A(v) | B(v) -> v
    }
    #(q, h, rest, whole, r, w, z)
  }
}
"#,
        &[&[]],
    );
}

#[test]
fn guards() {
    assert_captures(
        r#"
const limit = 10

pub fn main() {
  let n = 1
  let f = fn(x) {
    case x {
      y if y > n -> 1
      _ -> 0
    }
  }
  let g = fn(x) {
    case x {
      y if y > limit -> 1
      _ -> 0
    }
  }
  #(f, g)
}
"#,
        &[&["n"], &[]],
    );
}

#[test]
fn bit_array_sizes() {
    assert_captures(
        r#"
pub fn main() {
  let n = 8
  let f = fn(b) {
    case b {
      <<x:size(n), _:bits>> -> x
      _ -> 0
    }
  }
  let g = fn(b) {
    case b {
      <<x:size(n * 8), _:bits>> -> x
      _ -> 0
    }
  }
  let h = fn(x) { <<x:size(n)>> }
  #(f, g, h)
}
"#,
        &[&["n"], &["n"], &["n"]],
    );
}

#[test]
fn use_callbacks() {
    assert_captures(
        r#"
fn with(a: Int, f: fn(Int) -> Int) -> Int {
  f(a)
}

fn pair(f: fn(#(Int, Int)) -> Int) -> Int {
  f(#(1, 2))
}

pub fn main() {
  let a = 1
  let b = 2
  fn() {
    use x <- with(a)
    use #(m, n) <- pair
    x + b + m + n
  }
}
"#,
        &[&["a", "b"], &["b"], &["x", "b"]],
    );
}

#[test]
fn pipes_and_function_captures() {
    assert_captures(
        r#"
fn add(a: Int, b: Int) -> Int {
  a + b
}

pub fn main() {
  let a = 1
  let f = fn(x) { x |> add(a, _) }
  let g = 2 |> fn(y) { y + a }
  #(f, g)
}
"#,
        &[&["a"], &["a"], &["a"]],
    );
}

#[test]
fn record_update() {
    assert_captures(
        r#"
pub type Box {
  Box(v: Int, w: Int)
}

pub fn main() {
  let a = 1
  fn(r) { Box(..r, v: a) }
}
"#,
        &[&["a"]],
    );
}

#[test]
fn refined_variable() {
    assert_captures(
        r#"
pub type S {
  A(f: Int)
  B
}

pub fn main() {
  let s = A(1)
  fn() {
    case s {
      A(..) -> s.f
      _ -> 0
    }
  }
}
"#,
        &[&["s"]],
    );
}

#[test]
fn module_values_are_not_captured() {
    assert_captures(
        r#"
const c = 1

pub type Box {
  Box(v: Int)
}

fn inc(x: Int) -> Int {
  x + 1
}

pub fn main() {
  fn(x) { Box(inc(x) + c) }
}
"#,
        &[&[]],
    );
}

#[test]
fn local_functions_are_captured() {
    assert_captures(
        r#"
pub fn main() {
  let f = fn(x) { x }
  let g = fn(y) { f(y) }
  g
}
"#,
        &[&[], &["f"]],
    );
}

#[test]
fn messages() {
    assert_captures(
        r#"
pub fn main() {
  let m1 = "a"
  let m2 = "b"
  let m3 = "c"
  let m4 = "d"
  let f1 = fn(r) {
    let assert Ok(v) = r as m1
    v
  }
  let f2 = fn(x) { assert x as m2 }
  let f3 = fn() { panic as m3 }
  let f4 = fn(x) { echo x as m4 }
  #(f1, f2, f3, f4)
}
"#,
        &[&["m1"], &["m2"], &["m3"], &["m4"]],
    );
}

#[test]
fn label_shorthands() {
    assert_captures(
        r#"
pub type Box {
  Box(v: Int)
}

fn add(a a: Int, b b: Int) -> Int {
  a + b
}

pub fn main() {
  let a = 1
  let v = 2
  let f = fn(b) { add(a:, b:) }
  let g = fn() { Box(v:) }
  let h = fn(x) {
    let Box(v:) = x
    v
  }
  #(f, g, h)
}
"#,
        &[&["a"], &["v"], &[]],
    );
}

#[test]
fn functions_in_case_arms() {
    assert_captures(
        r#"
pub fn main() {
  fn(x) {
    case x {
      0 -> fn() { 0 }
      _ -> fn() { x }
    }
  }
}
"#,
        &[&[], &[], &["x"]],
    );
}
