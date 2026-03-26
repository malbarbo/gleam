use super::run_ok;

/// Count the number of locals (not params) in a wasm function.
fn wasm_func_local_count(wasm_bytes: &[u8], func_name: &str) -> u32 {
    let mut func_index = None;
    let mut func_indices_order = vec![];
    let mut local_counts: Vec<u32> = vec![];
    let mut import_count = 0u32;
    for payload in wasmparser::Parser::new(0).parse_all(wasm_bytes) {
        match payload.unwrap() {
            wasmparser::Payload::ImportSection(section) => {
                import_count = section.count();
            }
            wasmparser::Payload::CodeSectionEntry(body) => {
                let count: u32 = body
                    .get_locals_reader()
                    .unwrap()
                    .into_iter()
                    .map(|l| l.unwrap().0)
                    .sum();
                local_counts.push(count);
            }
            wasmparser::Payload::CustomSection(section) => {
                if let wasmparser::KnownCustom::Name(name_section) = section.as_known() {
                    for subsection in name_section {
                        if let wasmparser::Name::Function(names) = subsection.unwrap() {
                            for n in names {
                                let n = n.unwrap();
                                func_indices_order.push((n.index, n.name.to_string()));
                                if n.name == func_name {
                                    func_index = Some(n.index);
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    let idx = func_index.expect("function not found") - import_count;
    local_counts[idx as usize]
}

#[test]
fn case_var_subject_no_extra_local() {
    // With var subject: should have 1 local (x), no extra subject local
    let wasm_var = super::compile_wasm(
        r#"
pub fn f(x) {
    case x {
        1 -> x
        _ -> 0
    }
}

pub fn main() {
    echo f(1)
}
"#,
        vec![],
    );
    // With expression subject: needs a subject local
    let wasm_expr = super::compile_wasm(
        r#"
pub fn f(x) {
    case x + 0 {
        1 -> x
        _ -> 0
    }
}

pub fn main() {
    echo f(1)
}
"#,
        vec![],
    );
    let var_locals = wasm_func_local_count(&wasm_var, "f");
    let expr_locals = wasm_func_local_count(&wasm_expr, "f");
    // f(x) with "case x" should have 0 extra locals (x is a param)
    // f(x) with "case x + 0" needs 1 extra local for the subject
    assert_eq!(
        var_locals, 0,
        "case with param subject should need no extra locals"
    );
    assert_eq!(
        expr_locals, 1,
        "case with expression subject should need 1 extra local"
    );

    // Also test with let binding (local var, not param)
    let wasm_let = super::compile_wasm(
        r#"
pub fn f(x) {
    let y = x + 1
    case y {
        1 -> y
        _ -> 0
    }
}

pub fn main() {
    echo f(1)
}
"#,
        vec![],
    );
    let let_locals = wasm_func_local_count(&wasm_let, "f");
    // f(x) needs 1 local for y, but no extra for subject
    assert_eq!(
        let_locals, 1,
        "case with let-binding subject should need only 1 local (for y)"
    );
}

#[test]
fn case_float() {
    run_ok(
        r#"
pub fn main() {
    let assert 2.0 = case 2 > 1 + 1 {
        True -> 1.0
        False -> 2.0
    }
}
"#,
    );
}

#[test]
fn guard_field_access() {
    run_ok(
        r#"
pub type Pair(a, b) {
    Pair(first: a, second: b)
}

pub fn main() {
    assert case Pair(1, 2) {
        p if p.first == p.second -> False
        _ -> True
    }
}
"#,
    );
}

#[test]
fn generic_const() {
    run_ok(
        r#"
pub const ok = Ok(10)

pub fn main() {
    assert ok == ok
}
"#,
    );
}

#[test]
fn pattern_assign() {
    run_ok(
        r#"
pub fn main() {
    assert case [1, 2, 3] {
        [_, _] as the_list -> False
        [1, ..] as the_list -> the_list == [1, 2, 3]
        _ -> False
    }
}
"#,
    );
}

#[test]
fn pattern_assign_tuple() {
    run_ok(
        r#"
pub fn main() {
    let assert #(1, x) as pair = #(1, 2)
    assert x == 2
    assert pair == #(1, 2)
}
"#,
    );
}

#[test]
fn case_returns_function() {
    run_ok(
        r#"
pub fn main() {
    let f = case True {
        True -> fn(x) { x + 1 }
        False -> fn(x) { x * 2 }
    }
    assert f(10) == 11
}
"#,
    );
}

#[test]
fn list_pattern_guard_field_access() {
    run_ok(
        r#"
pub type Pair(a, b) {
    Pair(first: a, second: b)
}

pub fn lookup(lst: List(Pair(a, b)), key: a) -> Result(b, Nil) {
    case lst {
        [] -> Error(Nil)
        [p, ..] if p.first == key -> Ok(p.second)
        [_, ..rest] -> lookup(rest, key)
    }
}

pub fn main() {
    assert lookup([], "x") == Error(Nil)
    assert lookup([Pair("a", 1), Pair("b", 2)], "b") == Ok(2)
    assert lookup([Pair("a", 1), Pair("b", 2)], "c") == Error(Nil)
}
"#,
    );
}

#[test]
fn union_pattern_guard_field_access() {
    run_ok(
        r#"
pub type Box(a) {
    Empty
    Full(value: a)
}

pub fn get_or(box: Box(a), default: a) -> a {
    case box {
        Full(v) if v == default -> v
        Full(_) -> default
        Empty -> default
    }
}

pub fn main() {
    assert get_or(Full("a"), "a") == "a"
    assert get_or(Full("b"), "a") == "a"
    assert get_or(Empty, "x") == "x"
}
"#,
    );
}

#[test]
fn let_assert_union_ref_binding() {
    run_ok(
        r#"
pub fn main() {
    let assert Ok(name) = Ok("hello")
    assert name == "hello"
}
"#,
    );
}

#[test]
fn multi_subject_case() {
    run_ok(
        r#"
pub fn main() {
    assert case 1, True {
        1, True -> 10
        _, _ -> 0
    } == 10
    assert case 2, False {
        1, True -> 10
        x, _ -> x
    } == 2
}
"#,
    );
}

#[test]
fn alternative_patterns() {
    run_ok(
        r#"
pub fn main() {
    assert case 2 {
        1 | 2 | 3 -> True
        _ -> False
    }
    assert case 5 {
        1 | 2 | 3 -> True
        _ -> False
    } == False
}
"#,
    );
}

#[test]
fn multi_field_struct_ref_pattern() {
    run_ok(
        r#"
pub type Pair {
    Pair(first: String, second: String)
}

pub fn main() {
    let p = Pair("hello", "world")
    assert case p {
        Pair(a, b) -> a == "hello" && b == "world"
    }
}
"#,
    );
}
