// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! `call_lines = true`: the custom section `gleam.lines` gives the line of
//! each call of the program and the stdlib, so a host can show the line of
//! each frame of a backtrace. A call is found by the index of its function
//! and its offset in the body of the function, which counts from the
//! declarations of the locals, as `func_offset` of a wasmtime frame does.
//!
//! Each `call` and `call_ref` that a function of the program makes gets an
//! `InstrLocId`, the index of its line, which is the line of the expression
//! or statement that is being generated. walrus gives the offset of each
//! one when it emits the module, and the section, made after the code, uses
//! it. A tail call leaves no frame, so it gets none. The runtime has no
//! lines, so a host skips its frames. `wasm-opt` moves the code, so the
//! section is left out with `opt_level`.
//!
//! The section has, in LEB128 and with strings as their length and their
//! bytes, the number of functions, then for each one its index, its name
//! and its file, then the number of calls, then for each one the index of
//! its function, its offset and its line.

use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::ops::Range;

use walrus::ir::InstrLocId;
use walrus::{CodeTransform, IdsToIndices};
use wasm_encoder::Encode;

use super::*;

/// The name of the custom section.
pub const SECTION: &str = "gleam.lines";

/// The lines of the calls, shared by the instructions of the functions that
/// are generated.
#[derive(Debug, Default)]
pub(super) struct CallLines {
    /// The line of the expression that is being generated, 0 when there is
    /// none, so the call gets no line.
    line: Cell<u32>,
    /// The line of each call, by its `InstrLocId`.
    lines: RefCell<Vec<u32>>,
}

impl CallLines {
    /// The `InstrLocId` of a call at the current line, or `None` outside
    /// an expression.
    pub(super) fn call(&self) -> Option<InstrLocId> {
        let line = self.line.get();
        if line == 0 {
            return None;
        }
        let mut lines = self.lines.borrow_mut();
        let id = u32::try_from(lines.len()).expect("fewer calls than u32::MAX");
        lines.push(line);
        Some(InstrLocId::new(id))
    }
}

impl Generator<'_> {
    /// The lines of the calls when the function that is being generated
    /// records them, see `lines`.
    pub(super) fn call_lines(&self) -> Option<Rc<CallLines>> {
        self.call_lines.clone().filter(|_| {
            !self
                .runtime
                .iter()
                .any(|module| module.module.name == self.current_module_name)
        })
    }

    /// Makes `start` the position of the calls that follow, and gives the
    /// line it replaces, for `restore_call_line`. A `start` of `None` is no
    /// position, as at the start of a function.
    pub(super) fn set_call_line(&self, start: Option<u32>) -> Option<u32> {
        let lines = self.call_lines()?;
        let line = start.map_or(0, |start| self.source_location(start).1);
        Some(lines.line.replace(line))
    }

    pub(super) fn restore_call_line(&self, line: Option<u32>) {
        if let (Some(lines), Some(line)) = (&self.call_lines, line) {
            lines.line.set(line);
        }
    }

    /// Records `name` as the name of the function `id` of the module that
    /// is being compiled, when it records its lines.
    pub(super) fn record_call_lines_function(&mut self, id: FunctionId, name: EcoString) {
        if self.call_lines().is_some() {
            let file = self.source_file(&self.current_module_name);
            let _ = self.call_lines_functions.insert(id, (name, file));
        }
    }

    /// Adds the section to the module, when it records the lines.
    pub(super) fn add_lines_section(&mut self) {
        let Some(lines) = self.call_lines.take() else {
            return;
        };
        let _ = self.wasm_module.customs.add(LinesSection {
            lines: lines.lines.take(),
            functions: std::mem::take(&mut self.call_lines_functions),
            calls: vec![],
            ranges: vec![],
        });
    }
}

/// The section, which takes the offsets from walrus when it emits the
/// module.
#[derive(Debug)]
struct LinesSection {
    lines: Vec<u32>,
    functions: HashMap<FunctionId, (EcoString, EcoString)>,
    /// The `InstrLocId` and the offset in the module of each call.
    calls: Vec<(InstrLocId, usize)>,
    /// The range of each function in the module, from its size.
    ranges: Vec<(FunctionId, Range<usize>)>,
}

impl walrus::CustomSection for LinesSection {
    fn name(&self) -> &str {
        SECTION
    }

    fn apply_code_transform(&mut self, transform: &CodeTransform) {
        self.calls = transform.instruction_map.clone();
        self.ranges = transform.function_ranges.clone();
        self.ranges.sort_by_key(|(_, range)| range.start);
    }

    /// The functions and the calls of the module that walrus emitted, which
    /// has none of the functions that `gc` removed.
    fn data(&self, ids: &IdsToIndices) -> Cow<'_, [u8]> {
        let mut strings = Strings::default();
        let mut functions = vec![];
        for (id, _) in &self.ranges {
            if let Some((name, file)) = self.functions.get(id) {
                functions.push((ids.get_func_index(*id), strings.of(name), strings.of(file)));
            }
        }
        let mut calls = vec![];
        for (loc, offset) in &self.calls {
            let index = self
                .ranges
                .partition_point(|(_, range)| range.start <= *offset);
            let Some((id, range)) = index.checked_sub(1).and_then(|i| self.ranges.get(i)) else {
                continue;
            };
            let Some(&line) = self.lines.get(loc.data() as usize) else {
                continue;
            };
            let body = range.start + size_length(range.end - range.start);
            let offset = u32::try_from(*offset - body).expect("offset in u32");
            calls.push((ids.get_func_index(*id), offset, line));
        }
        calls.sort_unstable();
        let mut data = vec![];
        strings.0.len().encode(&mut data);
        for string in &strings.0 {
            string.as_str().encode(&mut data);
        }
        functions.len().encode(&mut data);
        for (index, name, file) in functions {
            index.encode(&mut data);
            name.encode(&mut data);
            file.encode(&mut data);
        }
        calls.len().encode(&mut data);
        for (index, offset, line) in calls {
            index.encode(&mut data);
            offset.encode(&mut data);
            line.encode(&mut data);
        }
        Cow::Owned(data)
    }
}

/// The strings of the section, each once.
#[derive(Default)]
struct Strings(indexmap::IndexSet<EcoString>);

impl Strings {
    fn of(&mut self, string: &EcoString) -> u32 {
        let (index, _) = self.0.insert_full(string.clone());
        u32::try_from(index).expect("fewer strings than u32::MAX")
    }
}

/// The length of the LEB128 size at the start of a function of `length`
/// bytes, size included.
fn size_length(length: usize) -> usize {
    (1..=5)
        .find(|&n| length >= n && leb_length(length - n) == n)
        .expect("valid function size")
}

fn leb_length(value: usize) -> usize {
    let mut length = 1;
    let mut value = value >> 7;
    while value != 0 {
        length += 1;
        value >>= 7;
    }
    length
}

/// A function of the section `gleam.lines`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinesFunction {
    pub name: String,
    pub file: String,
}

/// The section `gleam.lines` of a module, read back.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Lines {
    /// The functions, by their index.
    pub functions: HashMap<u32, LinesFunction>,
    /// The line of each call, by the index of its function and its offset.
    pub calls: HashMap<(u32, u32), u32>,
}

impl Lines {
    /// The section of `wasm`, or `None` when it has none or it is not
    /// valid.
    pub fn of(wasm: &[u8]) -> Option<Lines> {
        let data =
            wasmparser::Parser::new(0)
                .parse_all(wasm)
                .find_map(|payload| match payload {
                    Ok(wasmparser::Payload::CustomSection(section))
                        if section.name() == SECTION =>
                    {
                        Some(section.data())
                    }
                    _ => None,
                })?;
        Lines::read(data).ok()
    }

    fn read(data: &[u8]) -> wasmparser::Result<Lines> {
        let mut reader = wasmparser::BinaryReader::new(data, 0);
        let strings = (0..reader.read_var_u32()?)
            .map(|_| reader.read_string().map(String::from))
            .collect::<wasmparser::Result<Vec<_>>>()?;
        let string = |index: u32| strings.get(index as usize).cloned().unwrap_or_default();
        let mut lines = Lines::default();
        for _ in 0..reader.read_var_u32()? {
            let index = reader.read_var_u32()?;
            let name = string(reader.read_var_u32()?);
            let file = string(reader.read_var_u32()?);
            let _ = lines.functions.insert(index, LinesFunction { name, file });
        }
        for _ in 0..reader.read_var_u32()? {
            let index = reader.read_var_u32()?;
            let offset = reader.read_var_u32()?;
            let line = reader.read_var_u32()?;
            let _ = lines.calls.insert((index, offset), line);
        }
        Ok(lines)
    }
}
