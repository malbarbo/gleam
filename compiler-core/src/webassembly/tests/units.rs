// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use super::{compile_wasm, run_fail, run_ok, wasm_struct_fields};

const TYPES: &str = r#"
@external(webassembly, "builtins", "_inspect")
fn inspect(value: a) -> String

pub type Unit {
    Unit
}

pub type Record {
    Record(n: Nil, x: Float, u: Unit)
}

pub type Box(a) {
    Box(a: a, b: Int, c: a)
}

pub type Two(a) {
    One(a, Float)
    Other(String, a)
}

pub type Shared(a) {
    First(u: a, k: Int)
    Second(u: a, j: Float)
}
"#;

#[test]
fn unit_fields_are_not_stored() {
    let src = format!(
        r#"{TYPES}
pub fn main() {{
    assert inspect(#(Nil, "1", Unit)) != ""
    assert inspect(Record(Nil, 2.0, Unit)) != ""
    assert inspect(Box(Nil, 3, Nil)) != ""
    assert inspect([Other("4", Nil), One(Nil, 5.0)]) != ""
    assert inspect([First(Nil, 6), Second(Nil, 7.0)]) != ""
    assert inspect([Nil]) != ""
    let e: Result(Int, Nil) = Error(Nil)
    assert inspect(e) != ""
}}
"#
    );
    let fields = wasm_struct_fields(&compile_wasm(&src, vec![]));
    let field = |name: &str| *fields.get(name).unwrap_or_else(|| panic!("type {name}"));
    assert_eq!(field("Tuple(Nil, String, Unit)"), 1);
    assert_eq!(field("Record"), 1);
    assert_eq!(field("Box(Nil)"), 1);
    // The tag and the field that is not of a unit type. Structs with the same
    // fields are the same type, which keeps only one of their names.
    assert_eq!(field("Two(Nil).Other"), 2);
    assert_eq!(field("Shared(Nil).Second"), 2);
    // Only the tag, as the shared field `u` is of a unit type.
    assert_eq!(field("Shared"), 1);
    assert_eq!(field("List(Nil).Cons"), 1);
    assert_eq!(field("Result(Int, Nil).Error"), 1);
}

#[test]
fn unit_fields() {
    run_ok(&format!(
        r##"{TYPES}
pub const tuple = #(Nil, 1, Unit)

pub const record = Record(Nil, 2.0, Unit)

pub const box = Box(Nil, 3, Nil)

pub const other = Other("4", Nil)

pub const nils = [Nil, Nil]

pub fn main() {{
    let t = #(Nil, 1, Unit)
    let assert #(Nil, 1, Unit) = t
    let #(a, b, c) = t
    assert a == Nil && b == 1 && c == Unit
    assert t.0 == Nil && t.1 == 1 && t.2 == Unit
    assert t == tuple

    let r = Record(Nil, 2.0, Unit)
    assert r.n == Nil && r.x == 2.0 && r.u == Unit
    assert r == record
    let Record(n:, x:, u:) = r
    assert n == Nil && x == 2.0 && u == Unit
    let r2 = Record(..r, x: 5.0)
    assert r2.x == 5.0 && r2.n == Nil && r2 != r

    let bx = Box(Nil, 3, Nil)
    assert bx == box && bx.a == Nil && bx.b == 3 && bx.c == Nil

    let o: Two(Nil) = Other("4", Nil)
    assert o == other && o != One(Nil, 4.0)
    case o {{
        One(_, _) -> panic
        Other(s, Nil) -> {{
            assert s == "4"
        }}
    }}

    let shared = [First(Nil, 6), Second(Nil, 7.0)]
    let assert [first, ..] = shared
    assert first.u == Nil

    case [Nil, Nil, Nil] {{
        [Nil, Nil, ..rest] -> {{
            assert rest == [Nil]
        }}
        _ -> panic
    }}
    assert nils == [Nil, Nil]

    let f = fn() {{ #(n, u, x) }}
    assert f() == #(Nil, Unit, 2.0)

    let e: Result(Int, Nil) = Error(Nil)
    case e {{
        Error(Nil) -> Nil
        Ok(_) -> panic
    }}
    let ok: Result(Nil, Nil) = Ok(Nil)
    assert ok != Error(Nil) && ok == Ok(Nil)

    case t {{
        x if x.0 == Nil -> Nil
        _ -> panic
    }}
    case r {{
        rr if rr.n == Nil -> Nil
        _ -> panic
    }}

    assert inspect(t) == "#(Nil, 1, Unit)"
    assert inspect(r) == "Record(Nil, 2.0, Unit)"
    assert inspect(o) == "Other(\"4\", Nil)"
    assert inspect([Nil, Nil]) == "[Nil, Nil]"
    assert inspect(e) == "Error(Nil)"
}}
"##
    ));
}

#[test]
fn unit_elements_are_evaluated() {
    run_fail(
        r#"
pub fn main() {
    let assert #(Nil, 1) = #(fail(), 1)
}

fn fail() -> Nil {
    panic
}
"#,
    );
}
