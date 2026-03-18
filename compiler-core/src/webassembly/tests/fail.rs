use super::{run_fail, run_fail_stderr};

#[test]
fn assert_failure() {
    run_fail(
        r#"
pub fn main() {
  assert 1 == 1 + 2
  0
}
"#,
    );
}

#[test]
fn let_assert_failure() {
    run_fail(
        r#"
pub fn main() {
  let assert 1 = 1 + 1
}
"#,
    );
}

#[test]
fn panic_with_message() {
    run_fail_stderr(
        r#"
pub fn main() {
  case 0 == 0 + 1 {
    True -> 0
    False -> panic as "crashed"
  }
}
"#,
        "crashed",
    );
}

#[test]
fn todo_with_message() {
    run_fail_stderr(
        r#"
pub fn main() {
  todo as "missing"
  0
}
"#,
        "missing",
    );
}
