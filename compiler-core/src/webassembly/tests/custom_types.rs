// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use super::{compile_wasm, run_ok, wasm_type_names};

#[test]
fn record_access_shared_generic_field() {
    run_ok(
        r#"
pub type Pair(a) {
    Left(value: a, extra: Int)
    Right(value: a, name: String)
}

pub fn value(pair: Pair(a)) -> a {
    pair.value
}

pub fn main() {
    let assert 1 = value(Left(1, 99))
    let assert 2 = value(Right(2, "x"))
}
"#,
    );
}

#[test]
fn record_access_shared_field_with_gap() {
    run_ok(
        r#"
pub type T {
    A(x: Int, y: String, z: Int)
    B(x: Int, w: Int, z: Int)
}

pub fn get_x(t: T) -> Int { t.x }
pub fn get_z(t: T) -> Int { t.z }

pub fn main() {
    let assert 1 = get_x(A(1, "s", 3))
    let assert 2 = get_x(B(2, 4, 5))
    let assert 3 = get_z(A(1, "s", 3))
    let assert 5 = get_z(B(2, 4, 5))
}
"#,
    );
}

#[test]
fn record_access_shared_field() {
    run_ok(
        r#"
pub type Shape {
    Circle(label: String, radius: Int)
    Square(label: String, side: Int, color: String)
}

pub fn label(shape: Shape) -> String {
    shape.label
}

pub fn main() {
    let assert "c" = label(Circle("c", 1))
    let assert "s" = label(Square("s", 2, "red"))
}
"#,
    );
}

#[test]
fn enum_constants() {
    run_ok(
        r#"
pub type Color {
    Red
    Green
    Blue
}

const x = Blue
pub const b = x
pub const c = b

pub fn main() {
    assert x == Blue
    assert b == Blue
    assert c == Blue
}
"#,
    );
}

#[test]
fn option_type() {
    run_ok(
        r#"
pub type Option(a) {
    None
    Some(a)
}

pub fn main() {
    let a = None
    let b = Some(10)

    assert a != b
    assert a == None
    assert b == Some(10)

    assert case a {
        None if Some(1) == None -> 0
        None -> 1
        Some(_) -> 2
    } == 1

    assert case b {
        None -> 0
        Some(x) -> x
    } == 10

    assert Some("a") != None
}
"#,
    );
}

#[test]
fn generic_struct() {
    run_ok(
        r#"
type Point(a, b) {
    Point(x: Int, y: a, z: b)
}

const p = Point(10, "a", 2.0)

pub fn main() {
    assert p == Point(10, "a", 2.0)
    let p = Point(10, 20.0, 30)
    assert p == Point(10, 20.0, 30)
    let assert Point(10, z: a, y: b) = p
    assert a == 30
    assert b == 20.0
    assert Point(..p) == Point(10, 20.0, 30)
    assert Point(..Point(1, "2", 3), z: [2]) == Point(1, "2", [2])
    True
}
"#,
    );
}

#[test]
fn simple_struct() {
    run_ok(
        r#"
pub type X {
    X(Int)
}

const p = X(1)

pub fn main() {
    let _x: X = p
}
"#,
    );
}

#[test]
fn mutual_types() {
    run_ok(
        r#"
type A(a) {
    A(a, B(a))
}

type B(a) {
    None
    B(a, A(a))
}

pub fn main() {
    let b = None
    let a = A(1, b)
    let a = A(2, B(3, a))
    assert a != A(1, b)
}
"#,
    );
}

#[test]
fn tree_operations() {
    run_ok(
        r#"
type Tree(a) {
    Empty
    Node(value: a, left: Tree(a), right: Tree(a))
}

fn height(t: Tree(a)) -> Int {
    case t {
        Empty -> -1
        Node(_, Empty, Empty) -> 0
        Node(..) -> 1 + max(height(t.left), height(t.right))
    }
}

fn max(a: Int, b: Int) -> Int {
    case a > b {
        True -> a
        False -> b
    }
}

fn prune_left(t: Tree(a)) -> Tree(a) {
    case t {
        Empty -> Empty
        Node(..) -> Node(..t, left: Empty)
    }
}

fn swap(t: Tree(a)) -> Tree(a) {
    case t {
        Empty -> Empty
        Node(..) -> Node(..t, left: t.right, right: t.left)
    }
}

pub fn main() {
    let a = Empty
    let b = Node(10, Node(20, a, Empty), Empty)
    assert a == Empty
    assert b == b
    assert a != b
    assert height(a) == -1
    assert height(b) == 1
    assert height(Node("b", Empty, Empty)) == 0
    assert prune_left(b) == Node(10, Empty, Empty)
    assert swap(b) == Node(10, Empty, Node(20, a, Empty))
}
"#,
    );
}

#[test]
fn enum_with_result_nil() {
    run_ok(
        r#"
pub type Foo {
    A
    B
    C
}

fn to_result(f: Foo) -> Result(Int, Nil) {
    case f {
        A -> Ok(1)
        B -> Ok(2)
        C -> Error(Nil)
    }
}

pub fn main() {
    assert to_result(A) == Ok(1)
    assert to_result(B) == Ok(2)
    assert to_result(C) == Error(Nil)
}
"#,
    );
}

#[test]
fn echo_option() {
    assert_wasm_echo!(
        r#"
pub type Option(a) {
    None
    Some(a)
}

pub fn main() {
    echo None
    echo Some(10)
    echo Some(Some("a"))
}
"#,
    );
}

#[test]
fn echo_tree() {
    assert_wasm_echo!(
        r#"
pub type Tree(a) {
    Empty
    Node(value: a, left: Tree(a), right: Tree(a))
}

pub fn main() {
    echo Empty
    echo Node(10, Empty, Empty)
}
"#,
    );
}

#[test]
fn user_constructor_shadows_prelude() {
    run_ok(
        r#"
pub type Direction {
    Left
    Right
    True
    False
}

pub fn main() {
    assert True == True
    assert Left != True
    assert Left != Right
    assert Left == Left
    assert False == False
    assert True != False
}
"#,
    );
}

#[test]
fn function_field_access() {
    run_ok(
        r#"
pub type Handler {
    Handler(on_click: fn() -> Int)
}

pub fn main() {
    let h = Handler(fn() { 42 })
    assert h.on_click() == 42
}
"#,
    );
}

#[test]
fn function_field_pattern() {
    run_ok(
        r#"
pub type Handler {
    Handler(on_click: fn() -> Int)
}

pub fn main() {
    let h = Handler(fn() { 42 })
    let Handler(f) = h
    assert f() == 42
}
"#,
    );
}

#[test]
fn function_field_echo() {
    assert_wasm_echo!(
        r#"
pub type Handler {
    Handler(on_click: fn() -> Int)
}

pub fn main() {
    echo Handler(fn() { 42 })
}
"#,
    );
}

#[test]
fn function_field_const() {
    run_ok(
        r#"
pub type Pair {
    Pair(first: fn(Int) -> Int, second: Int)
}

fn double(x: Int) -> Int {
    x * 2
}

const p = Pair(double, 10)

pub fn main() {
    assert p.first(5) == 10
    assert p.second == 10
}
"#,
    );
}

#[test]
fn inferred_variant_field_access() {
    run_ok(
        r#"
pub type Shape {
    Circle(radius: Float)
    Rect(width: Float, height: Float)
}

pub fn area(s: Shape) -> Float {
    case s {
        Circle(r) -> r *. r *. 3.14
        Rect(..) -> s.width *. s.height
    }
}

pub fn main() {
    assert area(Circle(10.0)) == 314.0
    assert area(Rect(3.0, 4.0)) == 12.0
}
"#,
    );
}

#[test]
fn generic_union_shared_supertype() {
    let src = r#"
pub type Option(a) {
    None
    Some(a)
}

pub fn main() {
    let a = Some(1)
    let b = Some("hello")
    let c = Some(1.0)
    assert a == Some(1)
    assert b == Some("hello")
    assert c == Some(1.0)
    0
}
"#;
    let wasm = compile_wasm(src, vec![]);
    let names = wasm_type_names(&wasm);
    // All monomorphizations (Option(Int), Option(String), Option(Float))
    // should share a single "Option" supertype.
    assert_eq!(names.iter().filter(|n| n.as_str() == "Option").count(), 1,);
}

#[test]
fn recursive_struct_through_function() {
    run_ok(
        r#"
type S {
    S(f: fn(S) -> Int)
}

pub fn main() {
    let s = S(fn(_) { 1 })
    assert s.f(s) == 1
    0
}
"#,
    );
}

#[test]
fn recursive_struct_through_tuple() {
    run_ok(
        r#"
type S {
    S(#(Int, Result(S, Nil)))
}

pub fn main() {
    let s = S(#(1, Ok(S(#(2, Error(Nil))))))
    let assert S(#(_, Ok(S(#(x, _))))) = s
    assert x == 2
    0
}
"#,
    );
}

#[test]
fn recursive_union_shared_field() {
    run_ok(
        r#"
type U {
    U1(x: fn(U) -> Int, a: Int)
    U2(x: fn(U) -> Int, b: Int)
}

pub fn main() {
    let u = U1(fn(u) { u.x(U2(fn(_) { 2 }, 0)) }, 1)
    assert u.x(u) == 2
    0
}
"#,
    );
}

#[test]
fn mutually_recursive_types() {
    run_ok(
        r#"
type A {
    A(b: fn() -> B)
}

type B {
    B(a: fn() -> A, n: Int)
}

fn a() -> A {
    A(fn() { B(a, 1) })
}

pub fn main() {
    let b = a().b()
    assert b.n == 1
    assert { b.a().b() }.n == 1
    0
}
"#,
    );
}

#[test]
fn recursive_cycle_of_three_types() {
    run_ok(
        r#"
type A { A(b: fn() -> B) }
type B { B(c: fn() -> C) }
type C { C(a: fn() -> A, n: Int) }
fn a() -> A { A(fn() { B(fn() { C(a, 3) }) }) }
pub fn main() {
    assert a().b().c().n == 3
    assert a().b().c().a().b().c().n == 3
    0
}
"#,
    );
}

#[test]
fn mutually_recursive_unions_shared_field() {
    run_ok(
        r#"
type A { A1(b: fn() -> B, x: Int) A2(b: fn() -> B, y: Int) }
type B { B1(a: fn() -> A) B2(a: fn() -> A) }
fn a() -> A { A1(fn() { B2(fn() { A2(fn() { B1(a) }, 7) }) }, 1) }
pub fn main() {
    let assert A2(_, y) = a().b().a()
    assert y == 7
    0
}
"#,
    );
}

#[test]
fn mutually_recursive_generic_types() {
    run_ok(
        r#"
type A(t) { A(b: fn(t) -> B(t), v: t) }
type B(t) { B(a: fn(t) -> A(t)) }
fn a(v: t) -> A(t) { A(b, v) }
fn b(_: t) -> B(t) { B(a) }
pub fn main() {
    let x = a(1)
    assert { x.b(1).a(2) }.v == 2
    let y = a("s")
    assert { y.b("t").a("u") }.v == "u"
    0
}
"#,
    );
}

#[test]
fn recursive_types_nested_cycles() {
    run_ok(
        r#"
type A { A(a: fn(A) -> Int, b: fn(B) -> Int) }
type B { B(b: fn(B) -> Int, a: fn(A) -> Int) }
pub fn main() {
    let a = A(fn(_) { 1 }, fn(_) { 2 })
    let b = B(fn(_) { 3 }, fn(_) { 4 })
    assert a.a(a) + a.b(b) + b.b(b) + b.a(a) == 10
    0
}
"#,
    );
}

#[test]
fn recursive_types_inner_cycle_closed_first() {
    run_ok(
        r#"
type A { A(b: fn(B) -> Int) }
type B { B(b: fn(B) -> Int, a: fn(A) -> Int) }
pub fn main() {
    let a = A(fn(_) { 1 })
    let b = B(fn(_) { 2 }, fn(_) { 3 })
    assert a.b(b) + b.b(b) + b.a(a) == 6
    0
}
"#,
    );
}

#[test]
fn recursive_unions_inner_cycle_closed_first() {
    run_ok(
        r#"
type A { A1(b: fn(B) -> Int) A2(b: fn(B) -> Int) }
type B { B1(b: fn(B) -> Int, a: fn(A) -> Int) B2(b: fn(B) -> Int, a: fn(A) -> Int) }
pub fn main() {
    let a = A1(fn(_) { 1 })
    let b = B2(fn(_) { 2 }, fn(_) { 3 })
    assert a.b(b) + b.b(b) + b.a(a) == 6
    0
}
"#,
    );
}

#[test]
fn mutually_recursive_types_through_list_and_tuple() {
    run_ok(
        r#"
type A { A(List(#(Int, B))) }
type B { B(fn(A) -> Int) }
pub fn main() {
    let a = A([#(1, B(fn(_) { 5 }))])
    let assert A([#(_, B(f))]) = a
    assert f(a) == 5
    0
}
"#,
    );
}

#[test]
fn mutual_operations() {
    run_ok(
        r#"
type Expr {
    Num(Int)
    Op(BinOp)
}

type BinOp {
    Add(Expr, Expr)
    Mul(Expr, Expr)
}

fn eval(e: Expr) -> Int {
    case e {
        Num(n) -> n
        Op(op) -> eval_op(op)
    }
}

fn eval_op(op: BinOp) -> Int {
    case op {
        Add(a, b) -> eval(a) + eval(b)
        Mul(a, b) -> eval(a) * eval(b)
    }
}

fn depth(e: Expr) -> Int {
    case e {
        Num(_) -> 0
        Op(op) -> 1 + depth_op(op)
    }
}

fn depth_op(op: BinOp) -> Int {
    case op {
        Add(a, b) -> max(depth(a), depth(b))
        Mul(a, b) -> max(depth(a), depth(b))
    }
}

fn max(a: Int, b: Int) -> Int {
    case a > b {
        True -> a
        False -> b
    }
}

pub fn main() {
    let e = Op(Mul(Op(Add(Num(1), Num(2))), Num(3)))
    assert eval(e) == 9
    assert depth(e) == 2

    let e = Op(Add(Num(4), Op(Mul(Num(5), Num(6)))))
    assert eval(e) == 34
    assert depth(e) == 2

    assert eval(Num(7)) == 7
    assert depth(Num(7)) == 0
}
"#,
    );
}

#[test]
fn self_shared_recursive_field() {
    run_ok(
        r#"
pub type X {
    X1(child: X, n: Int)
    X2(child: X, s: String)
}

pub fn first(x: X) -> Int {
    case x {
        X1(_, n) -> n
        X2(_, _) -> 0
    }
}

pub fn main() {
    let _ = first
    0
}
"#,
    );
}

#[test]
fn mutual_unions_shared_cross_refs() {
    run_ok(
        r#"
pub type A {
    A1(b: B, n: Int)
    A2(b: B, s: String)
}

pub type B {
    B1(a: A, n: Int)
    B2(a: A, s: String)
}

pub fn show_a(a: A) -> Int {
    case a {
        A1(_, n) -> n
        A2(_, _) -> 0
    }
}

pub fn show_b(b: B) -> Int {
    case b {
        B1(_, n) -> n
        B2(_, _) -> 0
    }
}

pub fn main() {
    let _ = show_a
    let _ = show_b
    0
}
"#,
    );
}

#[test]
fn function_field_recursive() {
    run_ok(
        r#"
pub type Handler {
    Handler(act: fn(Handler) -> Int)
}

pub fn run(h: Handler) -> Int {
    case h {
        Handler(f) -> f(h)
    }
}

pub fn main() {
    let h = Handler(fn(_) { 42 })
    assert run(h) == 42
}
"#,
    );
}

#[test]
fn mutual_structs_cycle() {
    run_ok(
        r#"
pub type Wrap {
    Wrap(inner: Choice)
}

pub type Choice {
    C1(w: Wrap, n: Int)
}

pub fn first(c: Choice) -> Wrap {
    case c {
        C1(w, _) -> w
    }
}

pub fn main() {
    let _ = first
    0
}
"#,
    );
}

#[test]
fn private_cyclic_mutual_structs() {
    run_ok(
        r#"
type Wrap {
    Wrap(inner: Choice)
}

type Choice {
    C1(w: Wrap, n: Int)
}

fn first(c: Choice) -> Wrap {
    case c {
        C1(w, _) -> w
    }
}

pub fn main() {
    let _ = first
    0
}
"#,
    );
}

#[test]
fn private_cyclic_via_body_only() {
    run_ok(
        r#"
type Wrap {
    Wrap(inner: Choice)
}

type Choice {
    C1(w: Wrap, n: Int)
}

pub fn main() -> Int {
    let unpack = fn(c: Choice) -> Int {
        case c {
            C1(_, n) -> n
        }
    }
    let _ = unpack
    0
}
"#,
    );
}

#[test]
fn unbound_type_var_in_unused_variant() {
    run_ok(
        r#"
pub fn check(r: Result(Int, x)) -> Int {
    case r {
        Ok(n) -> n
        Error(_) -> -1
    }
}

pub fn main() {
    let assert 5 = check(Ok(5))
    0
}
"#,
    );
}

#[test]
fn union_variant_with_external_field() {
    run_ok(
        r#"
pub type I64 {}

@external(webassembly, "builtins", "_int_to_i64")
fn to_i64(value: Int) -> I64

@external(webassembly, "builtins", "_i64_to_int")
fn from_i64(value: I64) -> Int

pub type Holder {
    HoldI64(value: I64)
    HoldStr(value: String)
}

pub fn main() {
    let h = HoldI64(to_i64(42))
    case h {
        HoldI64(v) -> from_i64(v)
        HoldStr(_) -> -1
    }
}
"#,
    );
}

#[test]
fn struct_with_enum_field() {
    run_ok(
        r#"
pub type Color {
    Red
    Green
    Blue
}

pub type Painted {
    Painted(color: Color, label: Int)
}

pub fn main() -> Int {
    let p = Painted(Red, 42)
    case p {
        Painted(_, n) -> n
    }
}
"#,
    );
}

#[test]
fn function_field_with_external_param() {
    run_ok(
        r#"
pub type I64 {}

@external(webassembly, "builtins", "_int_to_i64")
fn to_i64(value: Int) -> I64

@external(webassembly, "builtins", "_i64_to_int")
fn from_i64(value: I64) -> Int

pub type Handler {
    Handler(act: fn(I64) -> I64)
}

pub fn main() {
    let h = Handler(fn(x) { x })
    case h {
        Handler(f) -> {
            let assert 42 = from_i64(f(to_i64(42)))
            0
        }
    }
}
"#,
    );
}

#[test]
fn tuple_with_external_element() {
    run_ok(
        r#"
pub type I64 {}

@external(webassembly, "builtins", "_int_to_i64")
fn to_i64(value: Int) -> I64

@external(webassembly, "builtins", "_i64_to_int")
fn from_i64(value: I64) -> Int

pub fn main() {
    let t = #(to_i64(42), 1)
    assert from_i64(t.0) == 42
    0
}
"#,
    );
}

#[test]
fn struct_with_external_field() {
    run_ok(
        r#"
pub type I64 {}

@external(webassembly, "builtins", "_int_to_i64")
fn to_i64(value: Int) -> I64

@external(webassembly, "builtins", "_i64_to_int")
fn from_i64(value: I64) -> Int

pub type Wrapper {
    Wrapper(value: I64)
}

pub fn main() {
    let w = Wrapper(to_i64(42))
    assert from_i64(w.value) == 42
    0
}
"#,
    );
}

#[test]
fn dual_option_monomorphizations() {
    let src = r#"
pub type Option(a) {
    None
    Some(a)
}

pub fn main() -> Int {
    let a = Some(1)
    let b = Some(1.0)
    assert a == Some(1)
    assert b == Some(1.0)
    0
}
"#;
    run_ok(src);
}

#[test]
fn generic_variant_field_access_multiple_instantiations() {
    run_ok(
        r#"
pub fn unwrap_err(r: Result(Int, x), default: x) -> x {
    case r {
        Ok(_) -> default
        Error(e) -> e
    }
}

pub fn main() {
    let assert "boom" = unwrap_err(Error("boom"), "x")
    let assert 42 = unwrap_err(Error(42), 0)
    0
}
"#,
    );
}

#[test]
fn recursive_type_instantiations_with_same_type_name() {
    let result = super::run_wasm(
        r#"
import thepackage/a/y as ay
import thepackage/b/y as by

type Box(t) { Box(f: fn(Box(t)) -> Int, v: t) }

pub fn main() {
    let a = Box(fn(_) { 1 }, ay.new())
    let b = Box(fn(_) { 2 }, by.new())
    assert a.f(a) + b.f(b) == 3
    assert ay.value(a.v) == 3
    assert by.value(b.v) == 4.5
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

/// The number of fields of the struct type named `name` in `wasm`.
fn struct_field_count(wasm: &[u8], name: &str) -> usize {
    let module = walrus::Module::from_buffer(wasm).expect("valid wasm");
    let type_ = module
        .types
        .iter()
        .find(|type_| type_.name.as_deref() == Some(name))
        .unwrap_or_else(|| panic!("type {name}"));
    let walrus::CompositeType::Struct(struct_) = type_.kind() else {
        panic!("{name} to be a struct")
    };
    struct_.fields.len()
}

/// A union whose variants are a null one and one other has no tag, since a
/// value that is not null can only be of the other variant.
#[test]
fn union_with_null_and_one_other_variant_has_no_tag() {
    let wasm = compile_wasm(
        r#"
pub type Option(a) {
    None
    Some(a)
}

pub type Shape {
    Empty
    Circle(Float)
    Rect(Int, Int)
}

pub fn main() {
    let _ = [Some(1)]
    let _ = Circle(1.0)
}
"#,
        vec![],
    );
    assert_eq!(struct_field_count(&wasm, "Option(Int).Some"), 1);
    assert_eq!(struct_field_count(&wasm, "List(Option(Int)).Cons"), 2);
    assert_eq!(struct_field_count(&wasm, "Shape.Circle"), 2);
}

#[test]
fn union_with_null_and_one_other_variant() {
    run_ok(
        r#"
pub type Option(a) {
    None
    Some(a)
}

pub type Box {
    Nothing
    Full(value: Int, name: String)
}

const some = Some(1)

const full_constant = Full(2, "b")

@external(webassembly, "builtins", "_hash")
fn hash(value: a) -> Int

fn get(option: Option(a), default: a) -> a {
    case option {
        Some(x) -> x
        None -> default
    }
}

pub fn main() {
    let assert 1 = get(some, 0)
    let assert 0 = get(None, 0)
    let assert True = Some([1, 2]) == Some([1, 2])
    let assert False = Some(1) == Some(2)
    let assert False = Some(1) == None
    let assert False = None == Some(1)
    let assert True = hash(Some("a")) == hash(Some("a"))
    let assert False = hash(Some(1)) == hash(None)
    let assert False = hash(Some(1)) == hash(Some(2))
    let assert "Some(1)" = string_inspect(Some(1))
    let assert "Full(2, \"b\")" = string_inspect(full_constant)
    let full = Full(2, "b")
    let assert 2 = full.value
    let assert Full(3, "b") = Full(..full, value: 3)
    let assert [Some(1), None] = [some, None]
}

@external(webassembly, "builtins", "_inspect")
fn string_inspect(value: a) -> String
"#,
    );
}

/// The shared fields of a union come first in its structs, so the fields of a
/// constant record go in a different order than its arguments.
#[test]
fn union_constant_with_shared_field_after_specific_field() {
    run_ok(
        r#"
pub type T {
    A(Int, name: String)
    B(Float, name: String)
}

const a = A(1, "x")

const b = B(2.0, "y")

pub fn main() {
    let assert A(1, "x") = a
    let assert B(2.0, "y") = b
    let assert "x" = a.name
    let assert "y" = b.name
    let assert True = a == A(1, "x")
    let assert "A(1, \"x\")" = string_inspect(a)
}

@external(webassembly, "builtins", "_inspect")
fn string_inspect(value: a) -> String
"#,
    );
}

/// A type without constructors is external, and without
/// `@external(webassembly, ...)` it has no representation, but it can be a
/// phantom type.
#[test]
fn phantom_type_without_constructors() {
    run_ok(
        r#"
pub type Meters

pub type Tagged(a) {
    Tagged(Int)
}

fn meters(n: Int) -> Tagged(Meters) {
    Tagged(n)
}

pub fn main() {
    let assert True = meters(3) == meters(3)
    let assert "Tagged(3)" = string_inspect(meters(3))
}

@external(webassembly, "builtins", "_inspect")
fn string_inspect(value: a) -> String
"#,
    );
}

#[test]
fn prelude_import() {
    run_ok(
        r#"
import gleam

pub fn main() {
    assert Ok(1) == gleam.Ok(1)
    assert Nil == gleam.Nil
}
"#,
    );
}

#[test]
fn enum_value_with_the_name_of_an_imported_value() {
    let result = super::run_wasm(
        r#"
import thepackage/option.{type Option, None}
import thepackage/policy.{Strict as Restrict}

pub fn main() {
    let x: Option(Int) = None
    assert x == option.None
    assert policy.None != policy.Lax
    assert Restrict == policy.Strict
    0
}
"#,
        vec![
            (
                super::TEST_PACKAGE,
                "thepackage/option",
                "pub type Option(a) { Some(a) None }",
            ),
            (
                super::TEST_PACKAGE,
                "thepackage/policy",
                "pub type Policy { Lax Strict None }",
            ),
        ],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

/// A `let` without `assert` can match one variant of a type with several
/// when the variant of the value is inferred, also inside other patterns and
/// as the last statement.
#[test]
fn let_with_an_inferred_variant() {
    run_ok(
        r#"
pub type K {
    Lax
    Strict
}

pub type E {
    A(Int)
    C(Int, String)
}

fn last() -> E {
    let C(_, _) as e = C(3, "c")
}

pub fn main() {
    let k = Lax
    let Lax = k
    let C(n, s) = C(1, "on")
    let #(C(m, _), _) = #(C(2, "off"), Strict)
    let assert 1 = n
    let assert "on" = s
    let assert 2 = m
    let assert C(3, "c") = last()
}
"#,
    );
}

/// A recursive type defined inside the definition of another type that is
/// not recursive has its own rec group, without the other type.
#[test]
fn recursive_type_inside_another_type() {
    let wasm = compile_wasm(
        r#"
type Outer {
  Outer(inner: Inner)
}

type Inner {
  Inner(next: fn() -> Inner)
}

pub fn main() {
  let o = Outer(Inner(fn() { panic }))
  let _ = o.inner
  0
}
"#,
        vec![],
    );
    let mut names = std::collections::HashMap::new();
    let mut groups = vec![];
    for payload in wasmparser::Parser::new(0).parse_all(&wasm) {
        let payload = payload.expect("valid wasm");
        if let wasmparser::Payload::TypeSection(reader) = payload {
            groups.extend(reader.into_iter().map(|group| group.expect("rec group")));
        } else if let wasmparser::Payload::CustomSection(section) = payload
            && let wasmparser::KnownCustom::Name(name_section) = section.as_known()
        {
            for subsection in name_section {
                if let wasmparser::Name::Type(type_names) = subsection.expect("valid wasm") {
                    for naming in type_names {
                        let naming = naming.expect("valid wasm");
                        let _ = names.insert(naming.index, naming.name.to_string());
                    }
                }
            }
        }
    }
    let mut index = 0;
    let mut group_names = vec![];
    for group in &groups {
        let size = group.types().len() as u32;
        group_names.push(
            (index..index + size)
                .filter_map(|i| names.get(&i).cloned())
                .collect::<Vec<_>>(),
        );
        index += size;
    }
    let outer = group_names
        .iter()
        .find(|names| names.contains(&"Outer".to_string()))
        .expect("group of Outer");
    assert_eq!(outer, &["Outer"]);
    assert!(
        group_names
            .iter()
            .any(|names| names.contains(&"Inner".to_string())),
        "{group_names:?}"
    );
}

#[test]
fn recursive_type_rec_group_without_shared_types() {
    // The supertype of Result is created first in the definition of S, but it is not moved to
    // the rec group of S, so it is the same type as in the other programs, which can receive the
    // values of this program through the host.
    let wasm = compile_wasm(
        r#"
type S {
    S(next: fn() -> S, result: Result(Int, Nil))
}

pub fn main() {
    let s = S(fn() { S(fn() { panic }, Error(Nil)) }, Ok(1))
    assert s.next().result == Error(Nil)
    0
}
"#,
        vec![],
    );
    let mut groups = vec![];
    for payload in wasmparser::Parser::new(0).parse_all(&wasm) {
        if let Ok(wasmparser::Payload::TypeSection(reader)) = payload {
            groups.extend(reader.into_iter().map(|group| group.expect("rec group")));
        }
    }
    // The supertype of the unions with a nullable variant has no fields.
    let is_union_supertype = |type_: &wasmparser::SubType| {
        !type_.is_final
            && type_.supertype_idx.is_none()
            && matches!(
                &type_.composite_type.inner,
                wasmparser::CompositeInnerType::Struct(struct_) if struct_.fields.is_empty()
            )
    };
    let union_supertype_groups = groups
        .iter()
        .filter(|group| group.types().any(is_union_supertype))
        .map(|group| group.types().len())
        .collect::<Vec<_>>();
    assert_eq!(union_supertype_groups, [1]);
}

#[test]
fn module_select_constructor_shadowed_by_local_constructor() {
    run_ok(
        r#"
import gleam

pub type Level {
    Info
    Error
}

pub fn parse(level: String) -> Result(Level, Nil) {
    case level {
        "error" -> Ok(Error)
        "info" -> Ok(Info)
        _ -> gleam.Error(Nil)
    }
}

pub fn main() {
    let error = gleam.Error
    assert parse("error") == Ok(Error)
    assert parse("x") == gleam.Error(Nil)
    assert error(1) == gleam.Error(1)
}
"#,
    );
}

/// Two types with the same name in modules whose names end the same way,
/// one of them in a module that the main module does not import, so the
/// main module prints both with the same name.
#[test]
fn types_with_the_same_printed_name() {
    let result = super::run_wasm(
        r#"
import thepackage/a/color
import thepackage/b/make

pub fn main() {
    let assert "Red" = string_inspect(color.Red)
    let assert "Cyan" = string_inspect(make.cyan())
    let assert "Circle(1.0)" = string_inspect(color.Circle(1.0))
    let assert "Square(2.0)" = string_inspect(make.square(2.0))
}

@external(webassembly, "builtins", "_inspect")
fn string_inspect(value: a) -> String
"#,
        vec![
            (
                "thepackage",
                "thepackage/a/color",
                r#"
pub type Color {
    Red
    Green
}

pub type Shape {
    Circle(Float)
}
"#,
            ),
            (
                "thepackage",
                "thepackage/b/color",
                r#"
pub type Color {
    Cyan
}

pub type Shape {
    Square(Float)
}
"#,
            ),
            (
                "thepackage",
                "thepackage/b/make",
                r#"
import thepackage/b/color

pub fn cyan() -> color.Color {
    color.Cyan
}

pub fn square(side: Float) -> color.Shape {
    color.Square(side)
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
