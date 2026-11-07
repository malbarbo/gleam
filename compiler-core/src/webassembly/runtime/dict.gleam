//// A persistent hash map, implemented as a CHAMP (Compressed Hash-Array
//// Mapped Prefix-tree, Steindorfer and Vinju, 2015).
////
//// This is the `Dict` that the WebAssembly target provides to
//// `@external(webassembly, "builtins", "Dict")`, as there is no runtime map
//// that can hold Gleam values. Its public functions are reached with
//// `@external(webassembly, "builtins", "dict_...")`. It is plain Gleam that
//// imports only `builtins/array` and `builtins/base`, so it also compiles
//// (and can be tested) on Erlang.
////
//// Keys are hashed to 32 bits and consumed 5 bits at a time, so the trie has
//// at most 7 levels of bitmap nodes. Keys whose 32 bit hashes are equal end up
//// in a collision node below the last level.
////
//// Each bitmap node keeps its entries and its children in two arrays, ordered
//// by hash fragment. A position is found with a popcount of the bitmap, as in
//// the paper; the arrays hold at most 32 elements and are copied on change.
//// Collision nodes keep their entries in a list. Sub-nodes are always kept
//// with at least two entries below them (deletion inlines singletons), which
//// makes the shape of the trie canonical for a given set of keys, except for
//// the order of the entries inside collision nodes. That is why equality is
//// not structural (see `dict_equal`).
////
//// No closures are stored in the trie.

// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

import builtins/array.{
  type Array, array_from_list, array_get, array_insert, array_remove, array_set,
  array_size,
}
import builtins/base.{
  bitwise_and, bitwise_exclusive_or, bitwise_or, bitwise_shift_left,
  bitwise_shift_right, concat_strings, hash, inspect, low_32_bits,
  reverse_and_prepend,
}

pub opaque type Dict(k, v) {
  Dict(size: Int, root: Node(k, v))
}

type Node(k, v) {
  /// `datamap` has a bit set for each fragment that holds an entry inline,
  /// `nodemap` for each fragment that holds a sub-node.
  Node(
    datamap: Int,
    nodemap: Int,
    entries: Array(Entry(k, v)),
    children: Array(Node(k, v)),
  )
  /// Entries whose 32 bit hashes are all equal to `hash`.
  Collision(hash: Int, entries: List(Entry(k, v)))
}

type Entry(k, v) {
  Entry(hash: Int, key: k, value: v)
}

const bits_per_level = 5

const fragment_mask = 31

/// The shift of the last bitmap level. A 32 bit hash is exhausted after it.
const max_shift = 30

fn hash_key(key: k) -> Int {
  low_32_bits(hash(key))
}

// -- Construction and queries -------------------------------------------------

pub fn dict_new() -> Dict(k, v) {
  Dict(0, empty_node())
}

fn empty_node() -> Node(k, v) {
  Node(0, 0, array_from_list([]), array_from_list([]))
}

/// Returns the argument. Used for the transient operations of `Dict`, which
/// are persistent on WebAssembly.
pub fn dict_identity(champ: Dict(k, v)) -> Dict(k, v) {
  champ
}

pub fn dict_size(champ: Dict(k, v)) -> Int {
  champ.size
}

pub fn dict_get(champ: Dict(k, v), key: k) -> Result(v, Nil) {
  dict_get_with_hash(champ, key, hash_key(key))
}

pub fn dict_has_key(champ: Dict(k, v), key: k) -> Bool {
  case dict_get(champ, key) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// `get` with an explicit hash, for testing collisions.
@internal
pub fn dict_get_with_hash(
  champ: Dict(k, v),
  key: k,
  hash: Int,
) -> Result(v, Nil) {
  lookup(champ.root, key, hash, 0)
}

fn lookup(node: Node(k, v), key: k, hash: Int, shift: Int) -> Result(v, Nil) {
  case node {
    Node(datamap:, nodemap:, entries:, children:) -> {
      let bit = bitpos(hash, shift)
      case bitwise_and(datamap, bit) != 0 {
        True ->
          case array_get(entries, index(datamap, bit)) {
            Entry(hash: h, key: k, value: v) if h == hash && k == key -> Ok(v)
            _ -> Error(Nil)
          }
        False ->
          case bitwise_and(nodemap, bit) != 0 {
            True ->
              lookup(
                array_get(children, index(nodemap, bit)),
                key,
                hash,
                shift + bits_per_level,
              )
            False -> Error(Nil)
          }
      }
    }
    Collision(entries:, ..) -> find_entry(entries, key)
  }
}

fn find_entry(entries: List(Entry(k, v)), key: k) -> Result(v, Nil) {
  case entries {
    [] -> Error(Nil)
    [Entry(key: k, value: v, ..), ..] if k == key -> Ok(v)
    [_, ..rest] -> find_entry(rest, key)
  }
}

// -- Insertion ----------------------------------------------------------------

pub fn dict_insert(champ: Dict(k, v), key: k, value: v) -> Dict(k, v) {
  dict_insert_with_hash(champ, key, value, hash_key(key))
}

/// `insert` with the arguments in the order of `Dict`'s transient insert.
pub fn dict_transient_insert(
  key: k,
  value: v,
  champ: Dict(k, v),
) -> Dict(k, v) {
  dict_insert(champ, key, value)
}

/// `insert` with an explicit hash, for testing collisions.
@internal
pub fn dict_insert_with_hash(
  champ: Dict(k, v),
  key: k,
  value: v,
  hash: Int,
) -> Dict(k, v) {
  let #(root, added) = insert_node(champ.root, Entry(hash, key, value), 0)
  case added {
    True -> Dict(champ.size + 1, root)
    False -> Dict(champ.size, root)
  }
}

/// Returns the new node and whether the key was not present before.
fn insert_node(
  node: Node(k, v),
  entry: Entry(k, v),
  shift: Int,
) -> #(Node(k, v), Bool) {
  case node {
    Node(datamap:, nodemap:, entries:, children:) -> {
      let bit = bitpos(entry.hash, shift)
      case bitwise_and(datamap, bit) != 0 {
        True -> {
          let i = index(datamap, bit)
          let existing = array_get(entries, i)
          case existing.hash == entry.hash && existing.key == entry.key {
            True -> #(
              Node(datamap, nodemap, array_set(entries, i, entry), children),
              False,
            )
            False -> {
              let child = merge_entries(existing, entry, shift + bits_per_level)
              let node =
                Node(
                  bitwise_exclusive_or(datamap, bit),
                  bitwise_or(nodemap, bit),
                  array_remove(entries, i),
                  array_insert(children, index(nodemap, bit), child),
                )
              #(node, True)
            }
          }
        }
        False ->
          case bitwise_and(nodemap, bit) != 0 {
            True -> {
              let j = index(nodemap, bit)
              let #(child, added) =
                insert_node(
                  array_get(children, j),
                  entry,
                  shift + bits_per_level,
                )
              #(
                Node(datamap, nodemap, entries, array_set(children, j, child)),
                added,
              )
            }
            False -> {
              let node =
                Node(
                  bitwise_or(datamap, bit),
                  nodemap,
                  array_insert(entries, index(datamap, bit), entry),
                  children,
                )
              #(node, True)
            }
          }
      }
    }
    Collision(hash:, entries:) -> {
      let #(entries, added) = collision_insert(entries, entry, [])
      #(Collision(hash, entries), added)
    }
  }
}

/// Builds the smallest sub-node holding two entries with different keys.
fn merge_entries(a: Entry(k, v), b: Entry(k, v), shift: Int) -> Node(k, v) {
  case shift > max_shift {
    // All the 32 bits were consumed: the hashes are equal.
    True -> Collision(a.hash, [a, b])
    False -> {
      let fa = fragment(a.hash, shift)
      let fb = fragment(b.hash, shift)
      case fa == fb {
        True ->
          Node(
            0,
            bitwise_shift_left(1, fa),
            array_from_list([]),
            array_from_list([merge_entries(a, b, shift + bits_per_level)]),
          )
        False -> {
          let datamap =
            bitwise_or(bitwise_shift_left(1, fa), bitwise_shift_left(1, fb))
          case fa < fb {
            True ->
              Node(datamap, 0, array_from_list([a, b]), array_from_list([]))
            False ->
              Node(datamap, 0, array_from_list([b, a]), array_from_list([]))
          }
        }
      }
    }
  }
}

fn collision_insert(
  entries: List(Entry(k, v)),
  entry: Entry(k, v),
  acc: List(Entry(k, v)),
) -> #(List(Entry(k, v)), Bool) {
  case entries {
    [] -> #(reverse_and_prepend(acc, [entry]), True)
    [first, ..rest] if first.key == entry.key -> #(
      reverse_and_prepend(acc, [entry, ..rest]),
      False,
    )
    [first, ..rest] -> collision_insert(rest, entry, [first, ..acc])
  }
}

/// Replaces the value of `key` with `fun(existing)`, or inserts `init` when
/// the key is not present.
pub fn dict_update_with(
  champ: Dict(k, v),
  key: k,
  fun: fn(v) -> v,
  init: v,
) -> Dict(k, v) {
  case dict_get(champ, key) {
    Ok(existing) -> dict_insert(champ, key, fun(existing))
    Error(_) -> dict_insert(champ, key, init)
  }
}

/// `update_with` with the arguments in the order of `Dict`'s transient
/// update.
pub fn dict_transient_update_with(
  key: k,
  fun: fn(v) -> v,
  init: v,
  champ: Dict(k, v),
) -> Dict(k, v) {
  dict_update_with(champ, key, fun, init)
}

// -- Deletion -----------------------------------------------------------------

pub fn dict_delete(champ: Dict(k, v), key: k) -> Dict(k, v) {
  dict_delete_with_hash(champ, key, hash_key(key))
}

/// `delete` with the arguments in the order of `Dict`'s transient delete.
pub fn dict_transient_delete(key: k, champ: Dict(k, v)) -> Dict(k, v) {
  dict_delete(champ, key)
}

/// `delete` with an explicit hash, for testing collisions.
@internal
pub fn dict_delete_with_hash(
  champ: Dict(k, v),
  key: k,
  hash: Int,
) -> Dict(k, v) {
  let #(root, removed) = delete_node(champ.root, key, hash, 0)
  case removed {
    True -> Dict(champ.size - 1, root)
    False -> champ
  }
}

/// Returns the new node and whether the key was present.
fn delete_node(
  node: Node(k, v),
  key: k,
  hash: Int,
  shift: Int,
) -> #(Node(k, v), Bool) {
  case node {
    Node(datamap:, nodemap:, entries:, children:) -> {
      let bit = bitpos(hash, shift)
      case bitwise_and(datamap, bit) != 0 {
        True -> {
          let i = index(datamap, bit)
          case array_get(entries, i) {
            existing if existing.hash == hash && existing.key == key -> #(
              Node(
                bitwise_exclusive_or(datamap, bit),
                nodemap,
                array_remove(entries, i),
                children,
              ),
              True,
            )
            _ -> #(node, False)
          }
        }
        False ->
          case bitwise_and(nodemap, bit) != 0 {
            False -> #(node, False)
            True -> {
              let j = index(nodemap, bit)
              let #(child, removed) =
                delete_node(
                  array_get(children, j),
                  key,
                  hash,
                  shift + bits_per_level,
                )
              case removed, singleton(child) {
                False, _ -> #(node, False)
                // The sub-node has a single entry left: move it up.
                True, Ok(entry) -> #(
                  Node(
                    bitwise_or(datamap, bit),
                    bitwise_exclusive_or(nodemap, bit),
                    array_insert(entries, index(datamap, bit), entry),
                    array_remove(children, j),
                  ),
                  True,
                )
                True, Error(_) -> #(
                  Node(datamap, nodemap, entries, array_set(children, j, child)),
                  True,
                )
              }
            }
          }
      }
    }
    Collision(hash: h, entries:) -> {
      let #(entries, removed) = collision_delete(entries, key, [])
      #(Collision(h, entries), removed)
    }
  }
}

fn singleton(node: Node(k, v)) -> Result(Entry(k, v), Nil) {
  case node {
    Node(nodemap: 0, entries:, ..) ->
      case array_size(entries) {
        1 -> Ok(array_get(entries, 0))
        _ -> Error(Nil)
      }
    Collision(entries: [entry], ..) -> Ok(entry)
    _ -> Error(Nil)
  }
}

fn collision_delete(
  entries: List(Entry(k, v)),
  key: k,
  acc: List(Entry(k, v)),
) -> #(List(Entry(k, v)), Bool) {
  case entries {
    [] -> #(reverse_and_prepend(acc, []), False)
    [first, ..rest] if first.key == key -> #(
      reverse_and_prepend(acc, rest),
      True,
    )
    [first, ..rest] -> collision_delete(rest, key, [first, ..acc])
  }
}

// -- Traversal ----------------------------------------------------------------

/// Folds over the entries in trie order. The recursion over the children is
/// at most as deep as the trie, 8 levels.
pub fn dict_fold(
  over champ: Dict(k, v),
  from initial: acc,
  with fun: fn(acc, k, v) -> acc,
) -> acc {
  fold_node(champ.root, initial, fun)
}

fn fold_node(node: Node(k, v), acc: acc, fun: fn(acc, k, v) -> acc) -> acc {
  case node {
    Node(entries:, children:, ..) ->
      fold_children(children, 0, fold_entries(entries, 0, acc, fun), fun)
    Collision(entries:, ..) -> fold_list(entries, acc, fun)
  }
}

fn fold_entries(
  entries: Array(Entry(k, v)),
  i: Int,
  acc: acc,
  fun: fn(acc, k, v) -> acc,
) -> acc {
  case i < array_size(entries) {
    False -> acc
    True -> {
      let Entry(key:, value:, ..) = array_get(entries, i)
      fold_entries(entries, i + 1, fun(acc, key, value), fun)
    }
  }
}

fn fold_children(
  children: Array(Node(k, v)),
  i: Int,
  acc: acc,
  fun: fn(acc, k, v) -> acc,
) -> acc {
  case i < array_size(children) {
    False -> acc
    True -> {
      let child = array_get(children, i)
      fold_children(children, i + 1, fold_node(child, acc, fun), fun)
    }
  }
}

fn fold_list(
  entries: List(Entry(k, v)),
  acc: acc,
  fun: fn(acc, k, v) -> acc,
) -> acc {
  case entries {
    [] -> acc
    [Entry(key:, value:, ..), ..rest] ->
      fold_list(rest, fun(acc, key, value), fun)
  }
}

/// All the entries, in reverse trie order. Without closures, so the other
/// traversals are simple loops over this list.
fn entries_of(champ: Dict(k, v)) -> List(Entry(k, v)) {
  entries_node(champ.root, [])
}

fn entries_node(node: Node(k, v), acc: List(Entry(k, v))) -> List(Entry(k, v)) {
  case node {
    Node(entries:, children:, ..) ->
      entries_children(children, 0, entries_array(entries, 0, acc))
    Collision(entries:, ..) -> reverse_and_prepend(entries, acc)
  }
}

fn entries_array(
  entries: Array(Entry(k, v)),
  i: Int,
  acc: List(Entry(k, v)),
) -> List(Entry(k, v)) {
  case i < array_size(entries) {
    False -> acc
    True -> {
      let entry = array_get(entries, i)
      entries_array(entries, i + 1, [entry, ..acc])
    }
  }
}

fn entries_children(
  children: Array(Node(k, v)),
  i: Int,
  acc: List(Entry(k, v)),
) -> List(Entry(k, v)) {
  case i < array_size(children) {
    False -> acc
    True -> {
      let child = array_get(children, i)
      entries_children(children, i + 1, entries_node(child, acc))
    }
  }
}

/// The first entry in trie order, or `Error(Nil)` when `champ` is empty.
pub fn dict_first(champ: Dict(k, v)) -> Result(#(k, v), Nil) {
  first_node(champ.root)
}

fn first_node(node: Node(k, v)) -> Result(#(k, v), Nil) {
  case node {
    Collision(entries: [Entry(key:, value:, ..), ..], ..) -> Ok(#(key, value))
    Collision(entries: [], ..) -> Error(Nil)
    Node(entries:, children:, ..) ->
      case array_size(entries) > 0, array_size(children) > 0 {
        True, _ -> {
          let Entry(key:, value:, ..) = array_get(entries, 0)
          Ok(#(key, value))
        }
        False, True -> first_node(array_get(children, 0))
        False, False -> Error(Nil)
      }
  }
}

pub fn dict_to_list(champ: Dict(k, v)) -> List(#(k, v)) {
  to_list_loop(entries_of(champ), [])
}

fn to_list_loop(
  entries: List(Entry(k, v)),
  acc: List(#(k, v)),
) -> List(#(k, v)) {
  case entries {
    [] -> acc
    [Entry(key:, value:, ..), ..rest] ->
      to_list_loop(rest, [#(key, value), ..acc])
  }
}

pub fn dict_from_list(list: List(#(k, v))) -> Dict(k, v) {
  from_list_loop(list, dict_new())
}

fn from_list_loop(list: List(#(k, v)), champ: Dict(k, v)) -> Dict(k, v) {
  case list {
    [] -> champ
    [#(key, value), ..rest] ->
      from_list_loop(rest, dict_insert(champ, key, value))
  }
}

pub fn dict_keys(champ: Dict(k, v)) -> List(k) {
  keys_loop(entries_of(champ), [])
}

fn keys_loop(entries: List(Entry(k, v)), acc: List(k)) -> List(k) {
  case entries {
    [] -> acc
    [entry, ..rest] -> keys_loop(rest, [entry.key, ..acc])
  }
}

pub fn dict_values(champ: Dict(k, v)) -> List(v) {
  values_loop(entries_of(champ), [])
}

fn values_loop(entries: List(Entry(k, v)), acc: List(v)) -> List(v) {
  case entries {
    [] -> acc
    [entry, ..rest] -> values_loop(rest, [entry.value, ..acc])
  }
}

/// Keeps the shape of the trie and replaces each value.
pub fn dict_map_values(champ: Dict(k, v), fun: fn(k, v) -> a) -> Dict(k, a) {
  Dict(champ.size, map_node(champ.root, fun))
}

fn map_node(node: Node(k, v), fun: fn(k, v) -> a) -> Node(k, a) {
  case node {
    Node(datamap:, nodemap:, entries:, children:) -> {
      // In trie order, as the other traversals call `fun`.
      let entries = map_entries_array(entries, 0, fun, [])
      let children = map_children(children, 0, fun, [])
      Node(
        datamap,
        nodemap,
        array_from_list(reverse_and_prepend(entries, [])),
        array_from_list(reverse_and_prepend(children, [])),
      )
    }
    Collision(hash:, entries:) -> Collision(hash, map_entries(entries, fun, []))
  }
}

fn map_entries(
  entries: List(Entry(k, v)),
  fun: fn(k, v) -> a,
  acc: List(Entry(k, a)),
) -> List(Entry(k, a)) {
  case entries {
    [] -> reverse_and_prepend(acc, [])
    [Entry(hash:, key:, value:), ..rest] ->
      map_entries(rest, fun, [Entry(hash, key, fun(key, value)), ..acc])
  }
}

/// The mapped entries, in reverse order.
fn map_entries_array(
  entries: Array(Entry(k, v)),
  i: Int,
  fun: fn(k, v) -> a,
  acc: List(Entry(k, a)),
) -> List(Entry(k, a)) {
  case i < array_size(entries) {
    False -> acc
    True -> {
      let Entry(hash:, key:, value:) = array_get(entries, i)
      map_entries_array(entries, i + 1, fun, [
        Entry(hash, key, fun(key, value)),
        ..acc
      ])
    }
  }
}

/// The mapped children, in reverse order.
fn map_children(
  children: Array(Node(k, v)),
  i: Int,
  fun: fn(k, v) -> a,
  acc: List(Node(k, a)),
) -> List(Node(k, a)) {
  case i < array_size(children) {
    False -> acc
    True -> {
      let child = array_get(children, i)
      map_children(children, i + 1, fun, [map_node(child, fun), ..acc])
    }
  }
}

pub fn dict_filter(
  champ: Dict(k, v),
  predicate: fn(k, v) -> Bool,
) -> Dict(k, v) {
  filter_loop(entries_of(champ), predicate, champ)
}

fn filter_loop(
  entries: List(Entry(k, v)),
  predicate: fn(k, v) -> Bool,
  acc: Dict(k, v),
) -> Dict(k, v) {
  case entries {
    [] -> acc
    [Entry(hash:, key:, value:), ..rest] ->
      case predicate(key, value) {
        True -> filter_loop(rest, predicate, acc)
        False ->
          filter_loop(rest, predicate, dict_delete_with_hash(acc, key, hash))
      }
  }
}

pub fn dict_take(champ: Dict(k, v), desired_keys: List(k)) -> Dict(k, v) {
  take_loop(champ, desired_keys, dict_new())
}

fn take_loop(
  champ: Dict(k, v),
  desired_keys: List(k),
  acc: Dict(k, v),
) -> Dict(k, v) {
  case desired_keys {
    [] -> acc
    [key, ..rest] ->
      case dict_get(champ, key) {
        Ok(value) -> take_loop(champ, rest, dict_insert(acc, key, value))
        Error(_) -> take_loop(champ, rest, acc)
      }
  }
}

pub fn dict_drop(champ: Dict(k, v), disallowed_keys: List(k)) -> Dict(k, v) {
  case disallowed_keys {
    [] -> champ
    [key, ..rest] -> dict_drop(dict_delete(champ, key), rest)
  }
}

/// Inserts all the entries of `new_entries` into `champ`, overwriting.
pub fn dict_merge(champ: Dict(k, v), new_entries: Dict(k, v)) -> Dict(k, v) {
  merge_loop(entries_of(new_entries), champ)
}

fn merge_loop(entries: List(Entry(k, v)), acc: Dict(k, v)) -> Dict(k, v) {
  case entries {
    [] -> acc
    [Entry(hash:, key:, value:), ..rest] ->
      merge_loop(rest, dict_insert_with_hash(acc, key, value, hash))
  }
}

/// Union of the two maps; for keys in both, the value is
/// `fun(left_value, right_value)`. The smaller map is folded into the bigger.
pub fn dict_combine(
  left: Dict(k, v),
  right: Dict(k, v),
  fun: fn(v, v) -> v,
) -> Dict(k, v) {
  case left.size >= right.size {
    True -> combine_loop(entries_of(right), fun, True, left)
    False -> combine_loop(entries_of(left), fun, False, right)
  }
}

/// Inserts the entries of the smaller map into `acc`, the bigger one.
/// `small_is_right` tells the order of the arguments of `fun`.
fn combine_loop(
  entries: List(Entry(k, v)),
  fun: fn(v, v) -> v,
  small_is_right: Bool,
  acc: Dict(k, v),
) -> Dict(k, v) {
  case entries {
    [] -> acc
    [Entry(hash:, key:, value:), ..rest] -> {
      let value = case dict_get_with_hash(acc, key, hash), small_is_right {
        Ok(existing), True -> fun(existing, value)
        Ok(existing), False -> fun(value, existing)
        Error(_), _ -> value
      }
      combine_loop(
        rest,
        fun,
        small_is_right,
        dict_insert_with_hash(acc, key, value, hash),
      )
    }
  }
}

// -- Type hooks ---------------------------------------------------------------

/// Equality of two maps: same size and every key of `a` is in `b` with an
/// equal value. The structure is not compared, because the order inside
/// collision nodes depends on the order of insertion.
pub fn dict_equal(a: Dict(k, v), b: Dict(k, v)) -> Bool {
  a.size == b.size && all_in(a.root, b)
}

fn all_in(node: Node(k, v), other: Dict(k, v)) -> Bool {
  case node {
    Node(entries:, children:, ..) ->
      all_entries_in(entries, 0, other) && all_children_in(children, 0, other)
    Collision(entries:, ..) -> all_list_in(entries, other)
  }
}

fn is_in(entry: Entry(k, v), other: Dict(k, v)) -> Bool {
  case lookup(other.root, entry.key, entry.hash, 0) {
    Ok(value) -> value == entry.value
    Error(_) -> False
  }
}

fn all_entries_in(
  entries: Array(Entry(k, v)),
  i: Int,
  other: Dict(k, v),
) -> Bool {
  case i < array_size(entries) {
    False -> True
    True ->
      is_in(array_get(entries, i), other)
      && all_entries_in(entries, i + 1, other)
  }
}

fn all_children_in(
  children: Array(Node(k, v)),
  i: Int,
  other: Dict(k, v),
) -> Bool {
  case i < array_size(children) {
    False -> True
    True ->
      all_in(array_get(children, i), other)
      && all_children_in(children, i + 1, other)
  }
}

fn all_list_in(entries: List(Entry(k, v)), other: Dict(k, v)) -> Bool {
  case entries {
    [] -> True
    [entry, ..rest] -> is_in(entry, other) && all_list_in(rest, other)
  }
}

/// Hash of a map, independent of the order of the entries: the sum of a mix
/// of the hash of each key with the hash of its value.
pub fn dict_hash(champ: Dict(k, v)) -> Int {
  hash_loop(entries_of(champ), champ.size)
}

fn hash_loop(entries: List(Entry(k, v)), acc: Int) -> Int {
  case entries {
    [] -> acc
    [Entry(hash:, value:, ..), ..rest] -> {
      let mixed =
        bitwise_exclusive_or(
          hash,
          low_32_bits(hash_key(value) * 31 - 1_640_531_527),
        )
      hash_loop(rest, low_32_bits(acc + mixed))
    }
  }
}

/// The text of a map, as `string.inspect` shows a `Dict`:
/// `dict.from_list([#(key, value), ...])`.
/// The entries are sorted by the text of their keys, so the text doesn't
/// depend on the order of insertion.
pub fn dict_inspect(champ: Dict(k, v)) -> String {
  let entries =
    entries_of(champ)
    |> texts([])
    |> sort_texts
  concat_strings(["dict.from_list([", ..inspect_parts(entries)])
}

fn texts(
  entries: List(Entry(k, v)),
  acc: List(#(String, String)),
) -> List(#(String, String)) {
  case entries {
    [] -> acc
    [Entry(key:, value:, ..), ..rest] ->
      texts(rest, [#(inspect(key), inspect(value)), ..acc])
  }
}

/// The parts of the text, built from the end.
fn inspect_parts(entries: List(#(String, String))) -> List(String) {
  inspect_loop(reverse_and_prepend(entries, []), True, ["])"])
}

fn inspect_loop(
  reversed: List(#(String, String)),
  is_last: Bool,
  acc: List(String),
) -> List(String) {
  case reversed {
    [] -> acc
    [#(key, value), ..rest] -> {
      let acc = case is_last {
        True -> acc
        False -> [", ", ..acc]
      }
      inspect_loop(rest, False, ["#(", key, ", ", value, ")", ..acc])
    }
  }
}

/// A merge sort of the entries by their keys.
fn sort_texts(entries: List(#(String, String))) -> List(#(String, String)) {
  case entries {
    [] | [_] -> entries
    _ -> {
      let #(left, right) = split(entries, [], [])
      merge(sort_texts(left), sort_texts(right), [])
    }
  }
}

fn split(list: List(a), left: List(a), right: List(a)) -> #(List(a), List(a)) {
  case list {
    [] -> #(left, right)
    [first, ..rest] -> split(rest, right, [first, ..left])
  }
}

fn merge(
  left: List(#(String, String)),
  right: List(#(String, String)),
  acc: List(#(String, String)),
) -> List(#(String, String)) {
  case left, right {
    [], rest | rest, [] -> reverse_and_prepend(acc, rest)
    [a, ..left_rest], [b, ..right_rest] ->
      case string_less_than(b.0, a.0) {
        True -> merge(left, right_rest, [b, ..acc])
        False -> merge(left_rest, right, [a, ..acc])
      }
  }
}

@external(erlang, "gleam_stdlib", "less_than")
@external(webassembly, "builtins", "_string_less_than")
fn string_less_than(a: String, b: String) -> Bool

// -- Invariants (for tests) ---------------------------------------------------

/// Checks the structural invariants of the trie: the stored size, bitmaps
/// that match the lists, entries placed at the fragment of their hash,
/// sub-nodes with at least two entries below them and collision nodes only
/// below the last level, with equal hashes that have the prefix of their
/// position.
@internal
pub fn dict_check_invariants(champ: Dict(k, v)) -> Bool {
  case check_node(champ.root, 0, 0, True) {
    Ok(count) -> count == champ.size
    Error(_) -> False
  }
}

/// Returns the number of entries below `node`.
fn check_node(
  node: Node(k, v),
  shift: Int,
  prefix: Int,
  is_root: Bool,
) -> Result(Int, Nil) {
  case node {
    Collision(hash:, entries:) -> {
      let count = list_length(entries, 0)
      case
        shift > max_shift
        && count >= 2
        && bitwise_and(hash, bitwise_shift_left(1, shift) - 1) == prefix
        && same_hash(entries, hash)
        && distinct_keys(entries)
      {
        True -> Ok(count)
        False -> Error(Nil)
      }
    }
    Node(datamap:, nodemap:, entries:, children:) -> {
      let count = array_size(entries)
      let ok =
        shift <= max_shift
        && bitwise_and(datamap, nodemap) == 0
        && popcount(datamap) == count
        && popcount(nodemap) == array_size(children)
        && entries_in_place(entries, datamap, shift, prefix, 0)
      case ok {
        False -> Error(Nil)
        True ->
          case check_children(children, 0, nodemap, 0, shift, prefix, count) {
            Ok(total) ->
              case is_root || total >= 2 && singleton(node) == Error(Nil) {
                True -> Ok(total)
                False -> Error(Nil)
              }
            Error(_) -> Error(Nil)
          }
      }
    }
  }
}

/// Each entry has the prefix of its node and is at the position of its
/// fragment, in increasing fragment order.
fn entries_in_place(
  entries: Array(Entry(k, v)),
  datamap: Int,
  shift: Int,
  prefix: Int,
  i: Int,
) -> Bool {
  let prefix_mask = bitwise_shift_left(1, shift) - 1
  case i < array_size(entries) {
    False -> True
    True -> {
      let entry = array_get(entries, i)
      let bit = bitpos(entry.hash, shift)
      bitwise_and(entry.hash, prefix_mask) == prefix
      && bitwise_and(datamap, bit) != 0
      && index(datamap, bit) == i
      && entries_in_place(entries, datamap, shift, prefix, i + 1)
    }
  }
}

fn same_hash(entries: List(Entry(k, v)), hash: Int) -> Bool {
  case entries {
    [] -> True
    [entry, ..rest] -> entry.hash == hash && same_hash(rest, hash)
  }
}

fn distinct_keys(entries: List(Entry(k, v))) -> Bool {
  case entries {
    [] -> True
    [entry, ..rest] ->
      find_entry(rest, entry.key) == Error(Nil) && distinct_keys(rest)
  }
}

fn check_children(
  children: Array(Node(k, v)),
  i: Int,
  nodemap: Int,
  fragment: Int,
  shift: Int,
  prefix: Int,
  count: Int,
) -> Result(Int, Nil) {
  case i < array_size(children), fragment > fragment_mask {
    False, _ -> Ok(count)
    _, True -> Error(Nil)
    True, False -> {
      let child = array_get(children, i)
      let bit = bitwise_shift_left(1, fragment)
      case bitwise_and(nodemap, bit) != 0 {
        False ->
          check_children(
            children,
            i,
            nodemap,
            fragment + 1,
            shift,
            prefix,
            count,
          )
        True -> {
          let child_prefix =
            bitwise_or(prefix, bitwise_shift_left(fragment, shift))
          case check_node(child, shift + bits_per_level, child_prefix, False) {
            Ok(n) ->
              check_children(
                children,
                i + 1,
                nodemap,
                fragment + 1,
                shift,
                prefix,
                count + n,
              )
            Error(_) -> Error(Nil)
          }
        }
      }
    }
  }
}

// -- Bits and small lists -----------------------------------------------------

fn fragment(hash: Int, shift: Int) -> Int {
  bitwise_and(bitwise_shift_right(hash, shift), fragment_mask)
}

fn bitpos(hash: Int, shift: Int) -> Int {
  bitwise_shift_left(1, fragment(hash, shift))
}

/// Position of `bit` among the bits set in `bitmap`.
fn index(bitmap: Int, bit: Int) -> Int {
  popcount(bitwise_and(bitmap, bit - 1))
}

fn popcount(x: Int) -> Int {
  popcount_loop(x, 0)
}

fn popcount_loop(x: Int, count: Int) -> Int {
  case x {
    0 -> count
    _ -> popcount_loop(bitwise_and(x, x - 1), count + 1)
  }
}

fn list_length(list: List(a), acc: Int) -> Int {
  case list {
    [] -> acc
    [_, ..rest] -> list_length(rest, acc + 1)
  }
}
