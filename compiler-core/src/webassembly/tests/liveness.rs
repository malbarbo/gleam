// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use std::collections::HashMap;

use super::{compile_wasm, for_each_operator, run_ok};

/// `build` allocates, `length` calls itself but does not allocate, and `wrap`
/// allocates only through `build`.
const LISTS: &str = r#"
fn build(n: Int, acc: List(Int)) -> List(Int) {
  case n {
    0 -> acc
    _ -> build(n - 1, [n, ..acc])
  }
}

fn length(l: List(Int), acc: Int) -> Int {
  case l {
    [] -> acc
    [_, ..rest] -> length(rest, acc + 1)
  }
}

fn wrap(n: Int) -> List(Int) {
  build(n, [])
}
"#;

/// Runs `src` with the functions of `LISTS`, and returns the locals that are
/// cleared (set to null) in each function, in order, without the location in
/// their names.
#[track_caller]
fn cleared(src: &str) -> HashMap<String, Vec<String>> {
    let src = format!("{LISTS}{src}");
    run_ok(&src);
    let wasm = compile_wasm(&src, vec![]);
    let mut function_names = HashMap::new();
    let mut local_names = HashMap::new();
    for payload in wasmparser::Parser::new(0).parse_all(&wasm) {
        if let wasmparser::Payload::CustomSection(section) = payload.expect("valid wasm")
            && let wasmparser::KnownCustom::Name(name_section) = section.as_known()
        {
            for subsection in name_section {
                let subsection = subsection.expect("valid wasm");
                if let wasmparser::Name::Function(names) = subsection {
                    for naming in names {
                        let naming = naming.expect("valid wasm");
                        let _ = function_names.insert(naming.index, naming.name.to_string());
                    }
                } else if let wasmparser::Name::Local(names) = subsection {
                    for function in names {
                        let function = function.expect("valid wasm");
                        for naming in function.names {
                            let naming = naming.expect("valid wasm");
                            let name = naming.name.split('@').next().unwrap_or_default();
                            let _ = local_names
                                .insert((function.index, naming.index), name.to_string());
                        }
                    }
                }
            }
        }
    }
    let mut cleared: HashMap<String, Vec<String>> = HashMap::new();
    let mut index = 0;
    for payload in wasmparser::Parser::new(0).parse_all(&wasm) {
        let payload = payload.expect("valid wasm");
        if let wasmparser::Payload::ImportSection(imports) = payload {
            index += imports
                .into_imports()
                .filter(|import| {
                    matches!(
                        import.as_ref().expect("valid wasm").ty,
                        wasmparser::TypeRef::Func(_)
                    )
                })
                .count() as u32;
        } else if let wasmparser::Payload::CodeSectionEntry(body) = payload {
            let mut reader = body.get_operators_reader().expect("valid wasm");
            let mut null = false;
            while !reader.eof() {
                let op = reader.read().expect("valid wasm");
                if let wasmparser::Operator::LocalSet { local_index } = op
                    && null
                {
                    let function = function_names.get(&index).cloned().unwrap_or_default();
                    let local = local_names
                        .get(&(index, local_index))
                        .cloned()
                        .unwrap_or_default();
                    cleared.entry(function).or_default().push(local);
                }
                null = matches!(op, wasmparser::Operator::RefNull { .. });
            }
            index += 1;
        }
    }
    cleared
}

/// The locals cleared in `function`.
fn cleared_in<'a>(cleared: &'a HashMap<String, Vec<String>>, function: &str) -> &'a [String] {
    cleared.get(function).map_or(&[], Vec::as_slice)
}

#[test]
fn clears_before_a_call_that_allocates() {
    let cleared = cleared(
        r#"
pub fn direct() -> Int {
  let big = build(1000, [])
  let n = length(big, 0)
  build(n, []) |> length(0)
}

pub fn indirect() -> Int {
  let big = build(1000, [])
  let n = length(big, 0)
  wrap(n) |> length(0)
}

pub fn main() {
  assert direct() == 1000
  assert indirect() == 1000
}
"#,
    );
    assert_eq!(cleared_in(&cleared, "direct"), ["big"]);
    assert_eq!(cleared_in(&cleared, "indirect"), ["big"]);
}

#[test]
fn clears_before_a_call_of_a_function_value() {
    let cleared = cleared(
        r#"
pub fn apply(f: fn(Int) -> List(Int)) -> Int {
  let big = build(1000, [])
  let n = length(big, 0)
  f(n) |> length(0)
}

pub fn main() {
  assert apply(wrap) == 1000
}
"#,
    );
    assert_eq!(cleared_in(&cleared, "apply"), ["big"]);
}

#[test]
fn does_not_clear_before_calls_that_do_not_allocate() {
    let cleared = cleared(
        r#"
fn add(a: Int, b: Int) -> Int {
  a + b
}

pub fn not_allocating(other: List(Int)) -> Int {
  let big = build(1000, [])
  let n = length(big, 0)
  length(other, n) + 1
}

pub fn leaf() -> Int {
  let big = build(1000, [])
  let n = length(big, 0)
  add(n, 1) + 1
}

/// The constructor of the list allocates, but it is short.
pub fn constructor() -> Int {
  let big = build(1000, [])
  let n = length(big, 0)
  length([n, n], 0) + 1
}

pub fn main() {
  assert not_allocating([1]) == 1002
  assert leaf() == 1002
  assert constructor() == 3
}
"#,
    );
    assert!(cleared_in(&cleared, "not_allocating").is_empty());
    assert!(cleared_in(&cleared, "leaf").is_empty());
    assert!(cleared_in(&cleared, "constructor").is_empty());
}

#[test]
fn does_not_clear_small_values() {
    let cleared = cleared(
        r#"
pub type Point {
  Point(x: Int, y: Int)
}

pub fn small() -> Int {
  let p = Point(1000, 2)
  let n = p.x
  wrap(n) |> length(0)
}

pub fn main() {
  assert small() == 1000
}
"#,
    );
    assert!(cleared_in(&cleared, "small").is_empty());
}

#[test]
fn clears_only_nullable_params() {
    let cleared = cleared(
        r#"
pub type Box {
  Box(items: List(Int))
}

fn size(box: Box) -> Int {
  length(box.items, 0)
}

pub fn nullable(l: List(Int)) -> Int {
  let n = length(l, 0)
  wrap(n) |> length(0)
}

pub fn non_nullable(box: Box) -> Int {
  let n = size(box)
  wrap(n) |> length(0)
}

pub fn main() {
  assert nullable(build(10, [])) == 10
  assert non_nullable(Box(build(10, []))) == 10
}
"#,
    );
    assert_eq!(cleared_in(&cleared, "nullable"), ["l"]);
    assert!(cleared_in(&cleared, "non_nullable").is_empty());
}

/// A non-nullable local that is cleared becomes nullable, and is asserted to
/// be non-null where it is read, unless it is read by an instruction that
/// accepts null.
#[test]
fn clears_non_nullable_locals() {
    let src = r#"
pub type Box {
  Box(items: List(Int))
}

fn size(box: Box) -> Int {
  length(box.items, 0)
}

pub fn twice() -> Int {
  let box = Box(build(1000, []))
  let n = size(box)
  let m = length(box.items, n)
  wrap(m) |> length(0)
}

pub fn main() {
  assert twice() == 2000
}
"#;
    let cleared = cleared(src);
    assert_eq!(cleared_in(&cleared, "twice"), ["box"]);
    let mut assertions = 0;
    for_each_operator(&compile_wasm(&format!("{LISTS}{src}"), vec![]), |f, op| {
        if f == "twice" && matches!(op, wasmparser::Operator::RefAsNonNull) {
            assertions += 1;
        }
    });
    assert_eq!(assertions, 1);
}

#[test]
fn clears_in_the_arm_that_does_not_read() {
    let cleared = cleared(
        r#"
pub fn second(flag: Bool) -> Int {
  let big = build(1000, [])
  let n = case flag {
    True -> 1
    False -> length(big, 0)
  }
  wrap(n) |> length(0)
}

pub fn main() {
  assert second(True) == 1
  assert second(False) == 1000
}
"#,
    );
    assert_eq!(cleared_in(&cleared, "second"), ["big", "big"]);
}

#[test]
fn clears_when_a_call_happens_in_one_arm() {
    let cleared = cleared(
        r#"
pub fn one_arm(flag: Bool) -> Int {
  let big = build(1000, [])
  let n = length(big, 0)
  case flag {
    True -> wrap(n) |> length(0)
    False -> n
  }
}

pub fn main() {
  assert one_arm(True) == 1000
  assert one_arm(False) == 1000
}
"#,
    );
    assert_eq!(cleared_in(&cleared, "one_arm"), ["big"]);
}

/// Clearing a capture frees nothing, as the closure still references it.
#[test]
fn does_not_clear_captures() {
    let cleared = cleared(
        r#"
pub fn closure() -> fn(Int) -> Int {
  let big = build(1000, [])
  fn(k) {
    let n = length(big, k)
    wrap(n) |> length(0)
  }
}

pub fn main() {
  assert closure()(0) == 1000
}
"#,
    );
    let anonymous = cleared
        .iter()
        .filter(|(function, _)| function.starts_with("anonymous"))
        .collect::<Vec<_>>();
    assert_eq!(anonymous, []);
}
