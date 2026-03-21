use super::run_ok;

#[test]
fn case_float() {
    run_ok(
        r#"
pub fn main() {
    let assert 2.0 = case 2 > 1 + 1 {
        True -> 1.0
        False -> 2.0
    }
}
"#,
    );
}

#[test]
fn guard_field_access() {
    run_ok(
        r#"
pub type Pair(a, b) {
    Pair(first: a, second: b)
}

pub fn main() {
    assert case Pair(1, 2) {
        p if p.first == p.second -> False
        _ -> True
    }
}
"#,
    );
}

#[test]
fn generic_const() {
    run_ok(
        r#"
pub const ok = Ok(10)

pub fn main() {
    assert ok == ok
}
"#,
    );
}

#[test]
fn pattern_assign() {
    run_ok(
        r#"
pub fn main() {
    assert case [1, 2, 3] {
        [_, _] as the_list -> False
        [1, ..] as the_list -> the_list == [1, 2, 3]
        _ -> False
    }
}
"#,
    );
}

#[test]
fn pattern_assign_tuple() {
    run_ok(
        r#"
pub fn main() {
    let assert #(1, x) as pair = #(1, 2)
    assert x == 2
    assert pair == #(1, 2)
}
"#,
    );
}

#[test]
fn case_returns_function() {
    run_ok(
        r#"
pub fn main() {
    let f = case True {
        True -> fn(x) { x + 1 }
        False -> fn(x) { x * 2 }
    }
    assert f(10) == 11
}
"#,
    );
}

#[test]
fn list_pattern_guard_field_access() {
    run_ok(
        r#"
pub type Pair(a, b) {
    Pair(first: a, second: b)
}

pub fn lookup(lst: List(Pair(a, b)), key: a) -> Result(b, Nil) {
    case lst {
        [] -> Error(Nil)
        [p, ..] if p.first == key -> Ok(p.second)
        [_, ..rest] -> lookup(rest, key)
    }
}

pub fn main() {
    assert lookup([], "x") == Error(Nil)
    assert lookup([Pair("a", 1), Pair("b", 2)], "b") == Ok(2)
    assert lookup([Pair("a", 1), Pair("b", 2)], "c") == Error(Nil)
}
"#,
    );
}

#[test]
fn union_pattern_guard_field_access() {
    run_ok(
        r#"
pub type Box(a) {
    Empty
    Full(value: a)
}

pub fn get_or(box: Box(a), default: a) -> a {
    case box {
        Full(v) if v == default -> v
        Full(_) -> default
        Empty -> default
    }
}

pub fn main() {
    assert get_or(Full("a"), "a") == "a"
    assert get_or(Full("b"), "a") == "a"
    assert get_or(Empty, "x") == "x"
}
"#,
    );
}

#[test]
fn let_assert_union_ref_binding() {
    run_ok(
        r#"
pub fn main() {
    let assert Ok(name) = Ok("hello")
    assert name == "hello"
}
"#,
    );
}

#[test]
fn multi_subject_case() {
    run_ok(
        r#"
pub fn main() {
    assert case 1, True {
        1, True -> 10
        _, _ -> 0
    } == 10
    assert case 2, False {
        1, True -> 10
        x, _ -> x
    } == 2
}
"#,
    );
}

#[test]
fn alternative_patterns() {
    run_ok(
        r#"
pub fn main() {
    assert case 2 {
        1 | 2 | 3 -> True
        _ -> False
    }
    assert case 5 {
        1 | 2 | 3 -> True
        _ -> False
    } == False
}
"#,
    );
}

#[test]
fn multi_field_struct_ref_pattern() {
    run_ok(
        r#"
pub type Pair {
    Pair(first: String, second: String)
}

pub fn main() {
    let p = Pair("hello", "world")
    assert case p {
        Pair(a, b) -> a == "hello" && b == "world"
    }
}
"#,
    );
}
