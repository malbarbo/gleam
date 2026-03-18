use super::run_ok;

#[test]
fn empty_tuple() {
    run_ok(
        r#"
pub fn main() {
    let _ = #()
}
"#,
    );
}

#[test]
fn tuple_const() {
    run_ok(
        r#"
pub const mytuple = #(10, 1.0, False, "other", [1, 2])

pub fn main() {
    let assert #(10, 1.0, False, "other", [1, 2]) = mytuple
}
"#,
    );
}
