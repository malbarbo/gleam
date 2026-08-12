//! sgleam: `let` at module level, behind `parse::set_module_let_enabled`.

use crate::{assert_js, assert_js_module_error};

fn enable() {
    crate::parse::set_module_let_enabled(true);
}

#[test]
fn simple_value() {
    enable();
    assert_js!(
        r#"
pub let x = 1
"#
    );
}

#[test]
fn calls_a_function_defined_below() {
    enable();
    assert_js!(
        r#"
pub let x = double(21)

pub fn double(n) {
  n * 2
}
"#
    );
}

#[test]
fn a_function_reads_it() {
    enable();
    assert_js!(
        r#"
pub let x = 1

pub fn read() {
  x + 1
}
"#
    );
}

#[test]
fn a_case_expression() {
    enable();
    assert_js!(
        r#"
pub let x = case 1 {
  1 -> "a"
  _ -> "b"
}
"#
    );
}

#[test]
fn a_block() {
    enable();
    assert_js!(
        r#"
pub let x = {
  let a = 1
  a + 1
}
"#
    );
}

#[test]
fn reads_an_earlier_one() {
    enable();
    assert_js!(
        r#"
pub let a = 1

pub let b = a + 1
"#
    );
}

#[test]
fn an_annotation() {
    enable();
    assert_js!(
        r#"
pub let x: Int = 1
"#
    );
}

#[test]
fn a_custom_type() {
    enable();
    assert_js!(
        r#"
pub type Wibble {
  Wibble(Int)
}

pub let x = Wibble(1)
"#
    );
}

#[test]
fn read_from_another_module() {
    enable();
    assert_js!(
        ("other", "pub let x = 1"),
        r#"
import other

pub fn main() {
  other.x
}
"#
    );
}

#[test]
fn private_one_that_nothing_reads_is_still_emitted() {
    enable();
    assert_js!(
        r#"
let x = 1
"#
    );
}

#[test]
fn a_function_that_does_not_read_it() {
    enable();
    assert_js!(
        r#"
pub let x = 1

pub fn other() {
  1
}
"#
    );
}

#[test]
fn only_a_function() {
    enable();
    assert_js!(
        r#"
pub fn other() {
  1
}
"#
    );
}

/// The order they run in is the order they were written, so reading one
/// written below is rejected here rather than left to fail when the module is
/// loaded.
#[test]
fn reads_one_written_below() {
    enable();
    assert_js_module_error!(
        r#"
pub let b = a + 1

pub let a = 1
"#
    );
}

/// Through a function, which is where the load time failure would be hardest
/// to read.
#[test]
fn reads_one_written_below_through_a_function() {
    enable();
    assert_js_module_error!(
        r#"
pub let x = read()

pub fn read() {
  y
}

pub let y = 1
"#
    );
}

#[test]
fn two_that_read_each_other() {
    enable();
    assert_js_module_error!(
        r#"
pub let a = b

pub let b = a
"#
    );
}

/// The check follows a name that is mentioned, not one that is called, so a
/// closure that would only read it later is rejected too. Writing the two in
/// the other order is accepted.
#[test]
fn a_closure_that_reads_one_written_below() {
    enable();
    assert_js_module_error!(
        r#"
pub let f = fn() { y }

pub let y = 1
"#
    );
}

/// A function may still read one written below it: it runs when it is called,
/// which is after the module has loaded.
#[test]
fn a_function_reads_one_written_below() {
    enable();
    assert_js!(
        r#"
pub fn read() {
  y
}

pub let y = 1
"#
    );
}

#[test]
fn side_effects_run_in_order() {
    enable();
    assert_js!(
        r#"
@external(javascript, "./ffi.mjs", "print")
pub fn print(a: String) -> Nil

pub let a = print("first")

pub let b = print("second")
"#
    );
}
