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
