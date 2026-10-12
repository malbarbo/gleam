// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Types and functions of the runtime written in Gleam, reached through
//! externals of the `"builtins"` module.

use super::run_wasm;

const DICT: &str = r#"
@external(webassembly, "builtins", "Dict")
pub type Dict(key, value)

@external(webassembly, "builtins", "dict_new")
pub fn new() -> Dict(k, v)

@external(webassembly, "builtins", "dict_insert")
pub fn insert(dict: Dict(k, v), key: k, value: v) -> Dict(k, v)

@external(webassembly, "builtins", "dict_get")
pub fn get(dict: Dict(k, v), key: k) -> Result(v, Nil)

@external(webassembly, "builtins", "dict_size")
pub fn size(dict: Dict(k, v)) -> Int

@external(webassembly, "builtins", "dict_from_list")
pub fn from_list(list: List(#(k, v))) -> Dict(k, v)

@external(webassembly, "builtins", "dict_delete")
pub fn delete(dict: Dict(k, v), key: k) -> Dict(k, v)
"#;

#[track_caller]
fn run_dict(src: &str) -> String {
    let result = run_wasm(src, vec![("thepackage", "dict", DICT)]);
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
    result.stderr
}

#[test]
fn dict_functions() {
    let _ = run_dict(
        r#"
import dict

pub fn main() {
  let d = dict.new() |> dict.insert(1, "one") |> dict.insert(2, "two")
  let assert 2 = dict.size(d)
  let assert Ok("one") = dict.get(d, 1)
  let assert Error(Nil) = dict.get(d, 3)
  let assert 1 = dict.size(dict.delete(d, 1))
}
"#,
    );
}

#[test]
fn dict_equality_ignores_insertion_order() {
    let _ = run_dict(
        r#"
import dict

pub fn main() {
  let a = dict.from_list([#(1, "a"), #(2, "b"), #(3, "c")])
  let b = dict.from_list([#(3, "c"), #(1, "a"), #(2, "b")])
  let assert True = a == b
  let assert False = a == dict.delete(b, 2)
  let assert True = #(a, 1) == #(b, 1)
}
"#,
    );
}

#[test]
fn dicts_of_dicts() {
    let _ = run_dict(
        r#"
import dict

pub fn main() {
  let a = dict.from_list([#(1, "a"), #(2, "b")])
  let b = dict.from_list([#(2, "b"), #(1, "a")])
  let outer = dict.new() |> dict.insert(a, dict.from_list([#("x", b)]))
  let assert Ok(inner) = dict.get(outer, b)
  let assert True = inner == dict.from_list([#("x", a)])
}
"#,
    );
}

#[test]
fn dict_inspect() {
    let stderr = run_dict(
        r#"
import dict

pub type Wrapper {
  Wrapper(dict.Dict(Int, String), Int)
}

pub fn main() {
  echo dict.from_list([#(1, "a")])
  echo Wrapper(dict.from_list([#(2, "b")]), 3)
  echo dict.from_list([#(dict.from_list([#(4, 5)]), [6])])
}
"#,
    );
    assert!(stderr.contains("dict.from_list([#(1, \"a\")])"), "{stderr}");
    assert!(
        stderr.contains("Wrapper(dict.from_list([#(2, \"b\")]), 3)"),
        "{stderr}"
    );
    assert!(
        stderr.contains("dict.from_list([#(dict.from_list([#(4, 5)]), [6])])"),
        "{stderr}"
    );
}

const DYNAMIC: &str = r#"
import dict.{type Dict}

@external(webassembly, "builtins", "Dynamic")
pub type Dynamic

@external(webassembly, "builtins", "_dynamic_cast")
pub fn cast(a: anything) -> Dynamic

@external(webassembly, "builtins", "dynamic_classify")
pub fn classify(data: Dynamic) -> String

@external(webassembly, "builtins", "dynamic_decode_int")
pub fn int(data: Dynamic) -> Result(Int, Int)

@external(webassembly, "builtins", "dynamic_decode_string")
pub fn string(data: Dynamic) -> Result(String, String)

@external(webassembly, "builtins", "dynamic_decode_list_items")
pub fn list_items(data: Dynamic) -> Result(List(Dynamic), Nil)

@external(webassembly, "builtins", "dynamic_decode_dict")
pub fn dict(data: Dynamic) -> Result(Dict(Dynamic, Dynamic), Nil)

@external(webassembly, "builtins", "dynamic_index")
pub fn index(data: Dynamic, key: Dynamic) -> Result(Result(Dynamic, Nil), String)
"#;

#[track_caller]
fn run_dynamic(src: &str) -> String {
    let result = run_wasm(
        src,
        vec![
            ("thepackage", "dict", DICT),
            ("thepackage", "dynamic", DYNAMIC),
        ],
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
    result.stderr
}

#[test]
fn dynamic_cast_and_decode() {
    let _ = run_dynamic(
        r#"
import dict
import dynamic

pub type Wibble {
  Wibble(Int)
  Wobble
}

pub fn main() {
  let assert "Int" = dynamic.classify(dynamic.cast(1))
  let assert "Float" = dynamic.classify(dynamic.cast(1.5))
  let assert "String" = dynamic.classify(dynamic.cast("a"))
  let assert "Bool" = dynamic.classify(dynamic.cast(True))
  let assert "Nil" = dynamic.classify(dynamic.cast(Nil))
  let assert "List" = dynamic.classify(dynamic.cast([1, 2]))
  let assert "Array" = dynamic.classify(dynamic.cast(#(1, "a")))
  let assert "Dict" = dynamic.classify(dynamic.cast(dict.from_list([#(1, 2)])))
  let assert "Function" = dynamic.classify(dynamic.cast(fn(x) { x + 1 }))
  let assert "Object" = dynamic.classify(dynamic.cast(Wibble(1)))
  let assert "Object" = dynamic.classify(dynamic.cast(Wobble))
  let assert Ok(1) = dynamic.int(dynamic.cast(1))
  let assert Error(0) = dynamic.int(dynamic.cast("1"))
  let assert Ok([a, b]) = dynamic.list_items(dynamic.cast(#(1, "b")))
  let assert Ok(1) = dynamic.int(a)
  let assert Ok("b") = dynamic.string(b)
  let assert Ok(d) = dynamic.dict(dynamic.cast(dict.from_list([#("k", 7)])))
  let assert Ok(Ok(v)) = dynamic.index(dynamic.cast(d), dynamic.cast("k"))
  let assert Ok(7) = dynamic.int(v)
  let assert Ok(Error(Nil)) = dynamic.index(dynamic.cast(d), dynamic.cast("x"))
  let assert Error("Indexable") = dynamic.index(dynamic.cast(1), dynamic.cast(0))
}
"#,
    );
}

#[test]
fn dynamic_equality_and_hash() {
    let _ = run_dynamic(
        r#"
import dict
import dynamic

pub type Wibble {
  Wibble(Int)
  Wobble
}

pub fn main() {
  let assert True = dynamic.cast([1, 2]) == dynamic.cast([1, 2])
  let assert False = dynamic.cast([1, 2]) == dynamic.cast([1, 3])
  let assert True = dynamic.cast(Wibble(1)) == dynamic.cast(Wibble(1))
  let assert False = dynamic.cast(Wibble(1)) == dynamic.cast(Wibble(2))
  let assert True = dynamic.cast(Wobble) == dynamic.cast(Wobble)
  let assert False = dynamic.cast(Wobble) == dynamic.cast(1)
  let d = dict.from_list([#(dynamic.cast(Wibble(1)), 1), #(dynamic.cast("a"), 2)])
  let assert Ok(1) = dict.get(d, dynamic.cast(Wibble(1)))
  let assert Ok(2) = dict.get(d, dynamic.cast("a"))
  let assert Error(Nil) = dict.get(d, dynamic.cast(Wibble(2)))
}
"#,
    );
}

#[test]
fn dynamic_inspect() {
    let stderr = run_dynamic(
        r#"
import dynamic

pub type Wibble {
  Wibble(Int)
  Wobble
}

pub fn main() {
  echo dynamic.cast(#(1, [Wibble(2), Wobble], "a"))
}
"#,
    );
    assert!(
        stderr.contains("#(1, [Wibble(2), Wobble], \"a\")"),
        "{stderr}"
    );
}

#[test]
fn dict_of_nil_values() {
    let _ = run_dict(
        r#"
import dict

pub fn main() {
  let d = dict.new() |> dict.insert(1, Nil)
  let assert Ok(Nil) = dict.get(d, 1)
}
"#,
    );
}

/// An external of `"builtins"` is found by name, so the names of the public
/// types and functions of the runtime must be unique, and the functions must
/// not have the name of a function of the Rust builtins.
#[test]
fn names_of_the_runtime_are_unique() {
    use std::collections::HashSet;

    let rust: HashSet<String> = walrus::Module::from_buffer(crate::webassembly::BUILTINS_WASM)
        .expect("builtins wasm to be a valid module")
        .funcs
        .iter()
        .filter_map(|f| f.name.clone())
        .collect();
    let mut types = HashSet::new();
    let mut functions = HashSet::new();
    for runtime_module in crate::webassembly::runtime::modules() {
        let definitions = &runtime_module.module.definitions;
        for custom_type in &definitions.custom_types {
            if custom_type.publicity.is_importable() {
                assert!(
                    types.insert(custom_type.name.clone()),
                    "type {} is not unique",
                    custom_type.name
                );
            }
        }
        for function in &definitions.functions {
            let Some((_, name)) = &function.name else {
                continue;
            };
            if function.publicity.is_importable() {
                assert!(functions.insert(name.clone()), "{name} is not unique");
                assert!(!rust.contains(name.as_str()), "{name} is a Rust builtin");
            }
        }
    }
}

/// The functions of the CHAMP that take the hash, to test collisions, and its
/// invariants.
const DICT_INTERNALS: &str = r#"
import dict.{type Dict}

@external(webassembly, "builtins", "dict_check_invariants")
fn check(dict: Dict(k, v)) -> Bool

@external(webassembly, "builtins", "dict_insert_with_hash")
fn insert_with_hash(dict: Dict(k, v), key: k, value: v, hash: Int) -> Dict(k, v)

@external(webassembly, "builtins", "dict_get_with_hash")
fn get_with_hash(dict: Dict(k, v), key: k, hash: Int) -> Result(v, Nil)

@external(webassembly, "builtins", "dict_delete_with_hash")
fn delete_with_hash(dict: Dict(k, v), key: k, hash: Int) -> Dict(k, v)
"#;

#[track_caller]
fn run_dict_i64(src: &str) {
    let result = super::run_wasm_with_config(
        &format!("{DICT_INTERNALS}\n{src}"),
        vec![("thepackage", "dict", DICT)],
        crate::config::WasmInt::I64,
        crate::config::WasmFloat::F64,
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
    );
}

#[test]
fn dict_full_collision() {
    run_dict_i64(
        r#"
pub fn main() {
  let d =
    dict.new()
    |> insert_with_hash("a", "a!", 42)
    |> insert_with_hash("b", "b!", 42)
    |> insert_with_hash("c", "c!", 42)
    |> insert_with_hash("d", "d!", 42)
    |> insert_with_hash("e", "e!", 42)
  let assert True = check(d)
  let assert 5 = dict.size(d)
  let assert Ok("c!") = get_with_hash(d, "c", 42)
  let assert Error(Nil) = get_with_hash(d, "z", 42)

  // Overwriting inside a collision node does not change the size.
  let d = insert_with_hash(d, "c", "new", 42)
  let assert 5 = dict.size(d)
  let assert Ok("new") = get_with_hash(d, "c", 42)
  let assert True = check(d)

  // Deleting shrinks the collision node until the last entry moves up.
  let assert 5 = dict.size(delete_with_hash(d, "z", 42))
  let d = delete_with_hash(d, "a", 42)
  let assert True = check(d)
  let d = delete_with_hash(d, "b", 42)
  let assert True = check(d)
  let d = delete_with_hash(d, "c", 42)
  let assert True = check(d)
  let d = delete_with_hash(d, "d", 42)
  let assert True = check(d)
  let assert 1 = dict.size(d)
  let assert Ok("e!") = get_with_hash(d, "e", 42)
  let assert Error(Nil) = get_with_hash(d, "a", 42)
}
"#,
    );
}

#[test]
fn dict_partial_collision() {
    run_dict_i64(
        r#"
pub fn main() {
  // These hashes share their lowest 30 bits, so they only differ at the last
  // level of the trie; the last two are fully equal.
  let low = 0x1234567
  let d =
    dict.new()
    |> insert_with_hash("a", 1, low)
    |> insert_with_hash("b", 2, low + 0x40000000)
    |> insert_with_hash("c", 3, low + 0x80000000)
    |> insert_with_hash("d", 4, low + 0xC0000000)
    |> insert_with_hash("e", 5, low + 0xC0000000)
    |> insert_with_hash("f", 6, low + 0x20)
  let assert True = check(d)
  let assert 6 = dict.size(d)
  let assert Ok(4) = get_with_hash(d, "d", low + 0xC0000000)
  let assert Ok(5) = get_with_hash(d, "e", low + 0xC0000000)
  let assert Error(Nil) = get_with_hash(d, "a", low + 0x40000000)
  let d = delete_with_hash(d, "e", low + 0xC0000000)
  let assert True = check(d)
  let d = delete_with_hash(d, "a", low)
  let assert True = check(d)
  let d = delete_with_hash(d, "f", low + 0x20)
  let assert True = check(d)
  let assert 3 = dict.size(d)
  let assert Ok(4) = get_with_hash(d, "d", low + 0xC0000000)
}
"#,
    );
}

#[test]
fn dict_many_insertions_and_deletions() {
    run_dict_i64(
        r#"
const n = 2003

/// Inserts the keys `i * 7919 % n`, a permutation of `0..n-1`.
fn insert_all(d: Dict(Int, Int), i: Int) -> Dict(Int, Int) {
  case i == n {
    True -> d
    False -> insert_all(dict.insert(d, i * 7919 % n, i), i + 1)
  }
}

fn delete_every_third(d: Dict(Int, Int), key: Int) -> Dict(Int, Int) {
  case key < 0 {
    True -> d
    False -> delete_every_third(dict.delete(d, key), key - 3)
  }
}

fn all_found(d: Dict(Int, Int), key: Int) -> Bool {
  case key == n {
    True -> True
    False ->
      case dict.get(d, key), key % 3 == 0 && key <= 2001 {
        Error(Nil), True -> all_found(d, key + 1)
        Ok(_), False -> all_found(d, key + 1)
        _, _ -> False
      }
  }
}

pub fn main() {
  let d = insert_all(dict.new(), 0)
  let assert True = check(d)
  let assert 2003 = dict.size(d)
  let d = delete_every_third(d, 2001)
  let assert True = check(d)
  let assert 1335 = dict.size(d)
  let assert True = all_found(d, 0)
}
"#,
    );
}
