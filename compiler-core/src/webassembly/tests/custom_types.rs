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
    // (1 + 2) * 3 = 9
    let e = Op(Mul(Op(Add(Num(1), Num(2))), Num(3)))
    assert eval(e) == 9
    assert depth(e) == 2

    // 4 + (5 * 6) = 34
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
fn wasm_tools_validate_cyclic_outputs() {
    // M0 acceptance: validate WASM output of the 3 cyclic tests + function-in-cycle
    // with wasm-tools to confirm the rec_group emission is well-formed.
    for (name, src) in [
        (
            "self_shared",
            r#"
pub type X { X1(child: X, n: Int)  X2(child: X, s: String) }
pub fn first(x: X) -> Int { case x { X1(_, n) -> n  X2(_, _) -> 0 } }
pub fn main() { let _ = first 0 }
"#,
        ),
        (
            "mutual_unions",
            r#"
pub type A { A1(b: B, n: Int)  A2(b: B, s: String) }
pub type B { B1(a: A, n: Int)  B2(a: A, s: String) }
pub fn show_a(a: A) -> Int { case a { A1(_, n) -> n  A2(_, _) -> 0 } }
pub fn show_b(b: B) -> Int { case b { B1(_, n) -> n  B2(_, _) -> 0 } }
pub fn main() { let _ = show_a  let _ = show_b  0 }
"#,
        ),
        (
            "mutual_structs",
            r#"
pub type Wrap { Wrap(inner: Choice) }
pub type Choice { C1(w: Wrap, n: Int) }
pub fn first(c: Choice) -> Wrap { case c { C1(w, _) -> w } }
pub fn main() { let _ = first  0 }
"#,
        ),
        (
            "function_in_cycle",
            r#"
pub type Handler { Handler(act: fn(Handler) -> Int) }
pub fn run(h: Handler) -> Int { case h { Handler(f) -> f(h) } }
pub fn main() { let h = Handler(fn(_) { 42 }) assert run(h) == 42 }
"#,
        ),
    ] {
        let bytes = compile_wasm(src, vec![]);
        let tmp = std::env::temp_dir().join(format!("validate_{name}.wasm"));
        std::fs::write(&tmp, &bytes).expect("write wasm");
        let out = std::process::Command::new("wasm-tools")
            .args(["validate", "--features", "all"])
            .arg(&tmp)
            .output()
            .expect("wasm-tools not found");
        let _ = std::fs::remove_file(&tmp);
        assert!(
            out.status.success(),
            "wasm-tools validate failed for {name}:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
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

// Cyclic types reachable only through private functions: pre_emit_user_types
// must visit private function signatures as well, otherwise the fallback path
// in val_type/type_index stack-overflows during code generation.
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

// Cyclic types referenced only via an inner lambda's signature (the public
// function's own signature doesn't mention them).
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

// Probe: function pattern-matches Result where the second type parameter
// is never bound to a concrete type. The Error subtype is not monomorphized
// for this instantiation, but the function still needs to be compiled.
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

// Probe: a union variant with an external-type field.
#[test]
fn probe_union_variant_with_external_field() {
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

// Probe: a struct with an enum-typed field. Enums map to ValType::I32 but
// have no TypeNode key.
#[test]
fn probe_struct_with_enum_field() {
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

// Probe: a function-typed field whose params/return reference an external
// type. Function node emission must also handle external val_types.
#[test]
fn probe_function_field_with_external_param() {
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

// Probe: a tuple containing an external type. Same gap as
// `probe_struct_with_external_field` but exercised via tuple emission.
#[test]
fn probe_tuple_with_external_element() {
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

// Probe: a struct with a field of external type (I64) that maps to a
// non-ref ValType. The lazy emit path must resolve the val_type for that
// field without panicking, since External types have no TypeNodeKey.
#[test]
fn probe_struct_with_external_field() {
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

// Probe: two distinct concrete instantiations of a generic union with no
// shared fields. The supertype's TypeNodeKey is identical for both
// (UnionSuper{name:"Option", shared_field_types:[]}), so the second
// monomorphization sees the supertype as already visited and does not add
// it to its batch — but the subtypes still reference it via supertype_key.
// Verifies that subtype emission resolves the supertype via the existing
// type_node_cache rather than only via the current-batch key_to_idx.
#[test]
fn probe_dual_option_monomorphizations_validate() {
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
    let bytes = compile_wasm(src, vec![]);
    let tmp = std::env::temp_dir().join("probe_dual_option.wasm");
    std::fs::write(&tmp, &bytes).expect("write wasm");
    let out = std::process::Command::new("wasm-tools")
        .args(["validate", "--features", "all"])
        .arg(&tmp)
        .output()
        .expect("wasm-tools");
    let _ = std::fs::remove_file(&tmp);
    assert!(
        out.status.success(),
        "wasm-tools validate failed:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

// Probe: generic function that accesses the field of the generic variant.
// Forces emission of the Error subtype for multiple concrete instantiations.
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
