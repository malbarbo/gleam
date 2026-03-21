use super::{compile_wasm, run_ok, wasm_type_names};

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
