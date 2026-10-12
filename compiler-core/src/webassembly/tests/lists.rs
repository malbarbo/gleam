// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use super::run_ok;

#[test]
fn list_const_equality() {
    run_ok(
        r#"
const l = [1]

pub fn main() {
    assert [1] == l
}
"#,
    );
}

#[test]
fn list_construction_and_spread() {
    run_ok(
        r#"
pub fn main() {
    let a = [2, 1]
    let b = [1, 2, ..a]
    let assert [1, b, 2, a] = b
    let assert 3 = a + b
}
"#,
    );
}

#[test]
fn list_int_equality() {
    run_ok(
        r#"
pub fn main() {
    let empty = []
    let assert True = [] == empty
    let assert False = [1] == empty
    let assert False = [] != empty
    let assert True = [1] != empty
    let assert True = [1, 2, 3] == [1, 2, 3, ..empty]
    let assert False = [1, 2, 3] == [1, 2, ..empty]
    let assert False = [1, 2, 3] == [1, 2, 3, 4, ..empty]
}
"#,
    );
}

#[test]
fn list_float_equality() {
    run_ok(
        r#"
pub fn main() {
    let empty = []
    let assert True = [] == empty
    let assert False = [1.0] == empty
    let assert True = [1.0, 2.0, 3.0] == [1.0, 2.0, 3.0, ..empty]
    let assert False = [1.0, 2.0, 3.0] == [1.0, 2.0, ..empty]
}
"#,
    );
}

#[test]
fn list_string_equality() {
    run_ok(
        r#"
pub fn main() {
    let empty = []
    let assert True = [] == empty
    let assert False = ["a"] == empty
    let assert True = ["a", "b", "c"] == ["a", "b", "c", ..empty]
    let assert False = ["a", "b", "c"] == ["a", "b", ..empty]
}
"#,
    );
}

#[test]
fn list_nested_equality() {
    run_ok(
        r#"
pub fn main() {
    let empty = []
    let assert True = [] == empty
    let assert False = [["a"]] == empty
    let assert True = [["a"], ["b", "c"]] == [["a"], ["b", "c"], ..empty]
    let assert False = [["a"], ["b", "c"]] == [["a"], ["b"], ..empty]
}
"#,
    );
}

/// The equality of the rest of a list is a tail call, so long lists don't
/// exhaust the stack.
#[test]
fn long_list_equality() {
    run_ok(
        r#"
type Stream {
  Stream(head: Int, tail: Stream)
  End
}

fn range(i: Int, acc: List(Int)) -> List(Int) {
  case i {
    0 -> acc
    _ -> range(i - 1, [i, ..acc])
  }
}

fn stream(i: Int, acc: Stream) -> Stream {
  case i {
    0 -> acc
    _ -> stream(i - 1, Stream(i, acc))
  }
}

pub fn main() {
    let a = range(1_000_000, [])
    let assert True = a == range(1_000_000, [])
    let assert False = a == range(1_000_000, [0])
    let assert True = [a] == [range(1_000_000, [])]
    let assert True = stream(1_000_000, End) == stream(1_000_000, End)
    let assert False = stream(1_000_000, End) == stream(1_000_000, Stream(0, End))
}
"#,
    );
}
