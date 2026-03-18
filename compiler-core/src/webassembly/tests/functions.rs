use super::run_ok;

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
