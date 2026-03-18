use super::run_ok;

#[test]
fn bool_constants() {
    run_ok(
        r#"
pub const true_ = True
pub const false_ = False

pub fn main() {
    let assert True = true_
    let assert False = false_
}
"#,
    );
}

#[test]
fn bool_not() {
    run_ok(
        r#"
pub fn main() {
    let assert True = !False
    let assert False = !True
}
"#,
    );
}

#[test]
fn bool_and_short_circuit() {
    run_ok(
        r#"
pub fn main() {
    let assert False = False && False
    let assert False = False && True
    let assert False = True && False
    let assert True = True && True
    let assert False = False && panic
}
"#,
    );
}

#[test]
fn bool_or_short_circuit() {
    run_ok(
        r#"
pub fn main() {
    let assert False = False || False
    let assert True = False || True
    let assert True = True || False
    let assert True = True || True
    let assert True = True || panic
}
"#,
    );
}
