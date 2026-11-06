// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use super::run_ok;

#[test]
fn large_inspect() {
    run_ok(
        r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

fn repeat(s: String, n: Int) -> String {
    case n {
        0 -> s
        _ -> repeat(s <> s, n - 1)
    }
}

pub fn main() {
    // Creates a string of 2^14 = 16384 'a' characters
    // inspect wraps it in quotes, writing ~16386 bytes to memory
    let big = repeat("a", 14)
    let s = inspect(big)
    assert s == "\"" <> big <> "\""
}
"#,
    );
}

#[test]
fn very_large_inspect() {
    run_ok(
        r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

fn repeat(s: String, n: Int) -> String {
    case n {
        0 -> s
        _ -> repeat(s <> s, n - 1)
    }
}

pub fn main() {
    // Creates a string of 2^21 = 2097152 'a' characters (~2MB)
    // inspect needs ~2MB which exceeds the 1.1MB memory
    let big = repeat("a", 21)
    let s = inspect(big)
    assert s == "\"" <> big <> "\""
}
"#,
    );
}

/// The builtins of `gleam/wasm/memory`: an allocation in the arena, the
/// loads and the stores, the copy of a bit array to the memory and back,
/// also of one that does not start at a whole byte, and the release of the
/// arena.
#[test]
fn memory_of_the_arena() {
    run_ok(
        r#"
type I32
type I64

@external(webassembly, "builtins", "_arena_base")
fn arena_base() -> I32
@external(webassembly, "builtins", "_arena_mark")
fn arena_mark() -> I32
@external(webassembly, "builtins", "_memory_release")
fn release(mark: I32) -> I32
@external(webassembly, "builtins", "_arena_alloc")
fn alloc(size: I32) -> I32
@external(webassembly, "builtins", "_memory_load_u32")
fn load_u32(address: I32) -> I32
@external(webassembly, "builtins", "_memory_load_i64")
fn load_i64(address: I32) -> I64
@external(webassembly, "builtins", "_memory_store_u32")
fn store_u32(address: I32, value: I32) -> I32
@external(webassembly, "builtins", "_memory_store_i64")
fn store_i64(address: I32, value: I64) -> I32
@external(webassembly, "builtins", "memory_write_bits")
fn write_bits(bits: BitArray, address: Int) -> Nil
@external(webassembly, "builtins", "memory_read_bits")
fn read_bits(address: Int, size: Int) -> BitArray
@external(webassembly, "builtins", "_i32_to_int")
fn to_int(n: I32) -> Int
@external(webassembly, "builtins", "_int_to_i32")
fn to_i32(n: Int) -> I32
@external(webassembly, "builtins", "_i64_to_int")
fn i64_to_int(n: I64) -> Int
@external(webassembly, "builtins", "_int_to_i64")
fn int_to_i64(n: Int) -> I64

pub fn main() {
    let mark = arena_mark()
    let address = to_int(alloc(to_i32(16)))
    let assert True = address >= to_int(arena_base())
    let assert True = address + 16 <= to_int(arena_mark())
    let assert 0 = to_int(store_u32(to_i32(address), to_i32(-1)))
    let assert -1 = to_int(load_u32(to_i32(address)))
    let assert 0 = to_int(store_i64(to_i32(address + 8), int_to_i64(-5)))
    let assert -5 = i64_to_int(load_i64(to_i32(address + 8)))
    let assert <<255, 255, 255, 255>> = read_bits(address, 4)
    let assert <<_:4, sliced:bits-size(16), _:bits>> = <<1, 2, 3>>
    write_bits(sliced, address)
    let assert <<16, 32>> = read_bits(address, 2)
    let assert 0 = to_int(release(mark))
    let assert True = to_int(arena_mark()) == to_int(mark)
}
"#,
    );
}
