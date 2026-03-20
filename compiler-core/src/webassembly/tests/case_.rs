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
