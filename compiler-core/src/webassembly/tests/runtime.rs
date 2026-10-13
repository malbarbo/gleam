// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Types and functions of the runtime written in Gleam, reached through
//! externals of the `"builtins"` module.

use super::{run_wasm, run_wasm_in};
use crate::webassembly::runtime::RUNTIME_PACKAGE;

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
    let result = run_wasm(src, vec![(super::TEST_PACKAGE, "dict", DICT)]);
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

/// `map_values` calls the function on the entries in the order of `fold`.
#[test]
fn dict_map_values_in_fold_order() {
    let stderr = run_dict(
        r#"
import dict.{type Dict}

@external(webassembly, "builtins", "dict_fold")
fn fold(dict: Dict(k, v), initial: acc, fun: fn(acc, k, v) -> acc) -> acc

@external(webassembly, "builtins", "dict_map_values")
fn map_values(dict: Dict(k, v), fun: fn(k, v) -> a) -> Dict(k, a)

pub fn main() {
  let d =
    dict.from_list([#(40, 0), #(4, 0), #(2, 0), #(3, 0), #(1, 0), #(77, 0)])
  let _ = map_values(d, fn(k, _) { echo #("map", k) })
  fold(d, Nil, fn(_, k, _) {
    echo #("fold", k)
    Nil
  })
}
"#,
    );
    let keys = |prefix: &str| {
        stderr
            .lines()
            .filter_map(|line| line.strip_prefix(prefix))
            .collect::<Vec<_>>()
    };
    let mapped = keys("#(\"map\", ");
    assert_eq!(mapped.len(), 6, "{stderr}");
    assert_eq!(mapped, keys("#(\"fold\", "), "{stderr}");
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

@external(webassembly, "builtins", "dynamic_array")
pub fn array(items: List(Dynamic)) -> Dynamic
"#;

#[track_caller]
fn run_dynamic(src: &str) -> String {
    let result = run_wasm(
        src,
        vec![
            (super::TEST_PACKAGE, "dict", DICT),
            (super::TEST_PACKAGE, "dynamic", DYNAMIC),
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
  let assert Ok(Ok(v)) = dynamic.index(dynamic.cast([5, 6]), dynamic.cast(1))
  let assert Ok(6) = dynamic.int(v)
  let assert Error("Indexable") =
    dynamic.index(dynamic.cast([5, 6]), dynamic.cast(2))
}
"#,
    );
}

/// An `Array` is an array, as a tuple, with its elements cast too. Only the
/// runtime has arrays, so the modules are in its package.
#[test]
fn dynamic_of_arrays() {
    let result = run_wasm_in(
        RUNTIME_PACKAGE,
        r##"
import dict
import dynamic

@external(webassembly, "builtins", "Array")
type Array(a)

@external(webassembly, "builtins", "_array_from_list")
fn from_list(list: List(a)) -> Array(a)

pub fn main() {
  let assert "Array" = dynamic.classify(dynamic.cast(from_list([1, 2])))
  let assert Ok([a, b]) = dynamic.list_items(dynamic.cast(from_list(["a", "b"])))
  let assert Ok("a") = dynamic.string(a)
  let assert Ok("b") = dynamic.string(b)
  let assert Ok([]) = dynamic.list_items(dynamic.cast(from_list([])))
  let assert True = dynamic.cast(from_list([1, 2])) == dynamic.cast(#(1, 2))
  // The items of a big array are compared without growing the stack.
  let big = from_list(range(100_000, []))
  let assert True = dynamic.cast(big) == dynamic.cast(from_list(range(100_000, [])))
  let assert False = dynamic.cast(big) == dynamic.cast(from_list(range(100_000, [0])))
  // And hashed without growing the stack.
  let d = dict.from_list([#(dynamic.cast(big), 1), #(dynamic.cast(range(100_000, [])), 2)])
  let assert Ok(1) = dict.get(d, dynamic.cast(from_list(range(100_000, []))))
  let assert Ok(2) = dict.get(d, dynamic.cast(range(100_000, [])))
  let assert Error(Nil) = dict.get(d, dynamic.cast(range(100_000, [0])))
  // And indexed in constant time.
  let assert True = index_all(dynamic.cast(big), 0)
  let assert True = index_all(dynamic.array(cast_range(100_000, [])), 0)
  let assert Ok(Error(Nil)) = dynamic.index(dynamic.cast(big), dynamic.cast(100_000))
  let assert Ok(Error(Nil)) = dynamic.index(dynamic.cast(big), dynamic.cast(-1))
  let assert Ok(Ok(x)) = dynamic.index(dynamic.cast(#(1, "a", 2.0)), dynamic.cast(1))
  let assert Ok("a") = dynamic.string(x)
  let assert Ok(Error(Nil)) = dynamic.index(dynamic.cast(#(1, "a")), dynamic.cast(2))
  let assert Ok([]) = dynamic.list_items(dynamic.cast(#()))
  let assert "#(1, \"a\", [#()])" = inspect(dynamic.cast(#(1, "a", [#()])))
  let assert "#(1, 2)" = inspect(dynamic.cast(from_list([1, 2])))
  let assert "#()" = inspect(dynamic.cast(from_list([])))
}

/// Indexes each element of the `Dynamic` of `range(100_000, [])`.
fn index_all(data: dynamic.Dynamic, i: Int) -> Bool {
  case i == 100_000 {
    True -> True
    False -> {
      let assert Ok(Ok(x)) = dynamic.index(data, dynamic.cast(i))
      let assert Ok(n) = dynamic.int(x)
      n == i + 1 && index_all(data, i + 1)
    }
  }
}

/// The casts of `range(i, acc)`.
fn cast_range(i: Int, acc: List(dynamic.Dynamic)) -> List(dynamic.Dynamic) {
  case i {
    0 -> acc
    _ -> cast_range(i - 1, [dynamic.cast(i), ..acc])
  }
}

@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

fn range(i: Int, acc: List(Int)) -> List(Int) {
  case i {
    0 -> acc
    _ -> range(i - 1, [i, ..acc])
  }
}
"##,
        vec![
            (RUNTIME_PACKAGE, "dict", DICT),
            (RUNTIME_PACKAGE, "dynamic", DYNAMIC),
        ],
        crate::config::WasmInt::I32,
        crate::config::WasmFloat::F64,
    );
    assert!(
        result.status.success(),
        "WASM execution failed:\n{}",
        result.stderr
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
        vec![(super::TEST_PACKAGE, "dict", DICT)],
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

/// The hooks return an `Int`, which is folded to 32 bits for the hash of the
/// values that hold them.
#[test]
fn hash_of_nested_dicts_i64() {
    run_dict_i64(
        r#"
@external(webassembly, "builtins", "_hash")
fn hash(a: a) -> Int

pub fn main() {
  let a = dict.from_list([#(1, 2)])
  let b = dict.from_list([#(1, 3)])
  let c = dict.from_list([#(2, 2)])
  let assert False = hash([a]) == hash([b])
  let assert False = hash([a]) == hash([c])
  let assert False = hash([b]) == hash([c])
  let assert True = hash([a]) == hash([dict.from_list([#(1, 2)])])
}
"#,
    );
}

#[test]
fn module_with_the_name_of_a_runtime_module() {
    let src = "import builtins/dict as d\npub fn main() { d.hello() }";
    let (main_module, dep_modules) = super::compile(
        src,
        vec![(super::TEST_PACKAGE, "builtins/dict", "pub fn hello() { 1 }")],
    );
    let line_numbers = src_span::LineNumbers::new(src);
    let mut all_modules: std::collections::HashMap<_, _> = dep_modules
        .iter()
        .map(|(name, module)| (name.clone(), module))
        .collect();
    let _ = all_modules.insert(main_module.name.clone(), &main_module);
    let error = crate::webassembly::module(
        &main_module,
        &line_numbers,
        &all_modules,
        &std::collections::HashMap::new(),
        crate::config::WasmInt::I32,
        crate::config::WasmFloat::F64,
    )
    .expect_err("a reserved module name");
    assert_eq!(
        error,
        crate::webassembly::Error::ReservedModuleName {
            name: "builtins/dict".into()
        }
    );
}

#[test]
fn runtime_external_with_a_wrong_signature() {
    let error = super::compile_wasm_error(
        r#"
@external(webassembly, "builtins", "dict_size")
fn size(dict: a) -> Int

pub fn main() { size(1) }
"#,
    );
    assert!(
        matches!(
            error,
            crate::webassembly::Error::WrongBuiltinFunctionSignature { ref name, .. }
                if name == "dict_size"
        ),
        "{error:?}"
    );
}

#[test]
fn runtime_external_with_a_specific_signature() {
    let _ = run_dict(
        r#"
@external(webassembly, "builtins", "Dict")
type Dict(k, v)

@external(webassembly, "builtins", "dict_from_list")
fn from_list(list: List(#(Int, String))) -> Dict(Int, String)

@external(webassembly, "builtins", "dict_size")
fn size(dict: Dict(Int, String)) -> Int

pub fn main() {
  let assert 2 = size(from_list([#(1, "a"), #(2, "b")]))
}
"#,
    );
}

/// Values that are not references, like the external numbers, are boxed in
/// a struct, without losing bits.
#[test]
fn dynamic_of_external_numbers() {
    let _ = run_dynamic(
        r#"
import dynamic

@external(webassembly, "builtins", "I32")
pub type I32

@external(webassembly, "builtins", "F64")
pub type F64

@external(webassembly, "builtins", "_int_to_i32")
fn to_i32(a: Int) -> I32

@external(webassembly, "builtins", "_float_to_f64")
fn to_f64(a: Float) -> F64

pub fn main() {
  let big = dynamic.cast(to_i32(2_147_483_647))
  let assert True = big == dynamic.cast(to_i32(2_147_483_647))
  let assert False = big == dynamic.cast(to_i32(1_073_741_823))
  let assert True = dynamic.cast(to_f64(1.5)) == dynamic.cast(to_f64(1.5))
  let assert False = dynamic.cast(to_f64(1.5)) == dynamic.cast(to_f64(2.5))
  let assert False = big == dynamic.cast(to_f64(1.5))
}
"#,
    );
}

/// Types that grow in their recursion. The equality, hash and inspect of a
/// type only handle its constructors with values, so they do not need the
/// ones of every `#(…)` level.
#[test]
fn growing_types() {
    let _ = run_dynamic(
        r##"
import dict
import dynamic

pub type T(a) {
  T(a, List(T(#(a, a))))
}

pub type N(a) {
  Flat(a)
  Nest(N(#(a, a)))
}

pub type D(a) {
  D(a, dict.Dict(D(#(a, a)), Int))
}

@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

fn t(x: Int) -> T(Int) {
  T(1, [T(#(2, 3), [T(#(#(4, 5), #(6, x)), [])])])
}

fn n(x: Int) -> N(Int) {
  Nest(Nest(Flat(#(#(1, 2), #(3, x)))))
}

fn d(x: Int) -> D(Int) {
  D(1, dict.from_list([#(D(#(2, 3), dict.new()), x)]))
}

pub fn main() {
  let assert True = t(7) == t(7)
  let assert False = t(7) == t(8)
  let assert "T(1, [T(#(2, 3), [T(#(#(4, 5), #(6, 7)), [])])])" = inspect(t(7))
  let assert True = n(7) == n(7)
  let assert False = n(7) == n(8)
  let assert False = n(7) == Flat(7)
  let assert "Nest(Nest(Flat(#(#(1, 2), #(3, 7)))))" = inspect(n(7))
  let assert True = d(7) == d(7)
  let assert False = d(7) == d(8)
  let assert "D(1, dict.from_list([#(D(#(2, 3), dict.from_list([])), 7)]))" =
    inspect(d(7))
  let keys = dict.from_list([#(t(7), "t"), #(t(8), "u")])
  let assert Ok("t") = dict.get(keys, t(7))
  let assert Ok("u") = dict.get(keys, t(8))
  let assert Error(Nil) = dict.get(keys, t(9))
  let keys = dict.from_list([#(n(7), "n"), #(n(8), "m")])
  let assert Ok("n") = dict.get(keys, n(7))
  let assert Error(Nil) = dict.get(keys, n(9))
  let keys = dict.from_list([#(d(7), "d")])
  let assert Ok("d") = dict.get(keys, d(7))
  let assert Error(Nil) = dict.get(keys, d(8))
  let assert True = dynamic.cast(t(7)) == dynamic.cast(t(7))
  let assert False = dynamic.cast(n(7)) == dynamic.cast(n(8))
  let assert True = [1, 2] == [1, 2]
}
"##,
    );
}

/// The values of constants have the derived functions of their types, and a
/// constructor used only in a pattern changes nothing.
#[test]
fn derived_functions_of_constants() {
    let _ = run_dynamic(
        r#"
pub type P {
  P(Int)
  Q(Int)
  R(Int)
}

const a = [P(1), Q(2)]

const b = [P(1), Q(2)]

const c = [P(1), P(2)]

pub type S {
  S(Int)
}

const s = [S(1)]

const t = [S(1)]

@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

fn is_r(p: P) -> Bool {
  case p {
    R(_) -> True
    _ -> False
  }
}

pub fn main() {
  let assert True = a == b
  let assert False = a == c
  let assert "[P(1), Q(2)]" = inspect(a)
  let assert True = s == t
  let assert "[S(1)]" = inspect(s)
  let assert [False, False] = [is_r(P(1)), is_r(Q(2))]
}
"#,
    );
}

#[test]
fn constructors_in_constants() {
    let _ = run_dynamic(
        r#"
pub type T {
  A
  B(String)
  C(String, Int)
}

pub type W {
  W(fn(String) -> T)
}

const fs = [B, B]

const t = #(C, 1)

const w = W(B)

pub fn main() {
  let assert [f, g] = fs
  let assert True = f("x") == B("x")
  let assert True = g("y") == B("y")
  let assert #(h, n) = t
  let assert True = h("z", n) == C("z", 1)
  let assert W(k) = w
  let assert True = k("w") == B("w")
  let assert True = A != B("a")
}
"#,
    );
}
