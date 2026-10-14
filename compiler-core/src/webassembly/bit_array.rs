// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Bit arrays. The `BitArray` of the prelude is the type `Bits` of the
//! runtime module `builtins/bit_array`, which also has their functions. Here
//! is how a bit array expression is created: one whose segments are literals
//! is computed when compiling, and created once by the start function, and
//! any other one writes its segments with the functions of the runtime.

use super::*;
use crate::ast::{BitArraySegment, Endianness, TypedExprBitArraySegment};
use num_traits::ToPrimitive;

/// The runtime module of bit arrays.
pub(super) const BIT_ARRAY_MODULE: &str = "builtins/bit_array";

/// The bits of a bit array, from the most significant bit of the first byte.
#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub(super) struct Bits {
    /// The bytes, whose bits after `size` are zero.
    pub(super) bytes: Vec<u8>,
    pub(super) size: u64,
}

impl Bits {
    fn push_bit(&mut self, bit: bool) {
        if self.size.is_multiple_of(8) {
            self.bytes.push(0);
        }
        if bit && let Some(last) = self.bytes.last_mut() {
            *last |= 0x80 >> (self.size % 8);
        }
        self.size += 1;
    }

    fn bit(&self, index: u64) -> bool {
        self.bytes
            .get((index / 8) as usize)
            .is_some_and(|byte| byte & (0x80 >> (index % 8)) != 0)
    }

    /// Pushes the `count` bits of `value` from bit `from`, the most
    /// significant first.
    fn push_value_bits(&mut self, value: &BigInt, from: u64, count: u64) {
        for index in (from..from + count).rev() {
            self.push_bit(value.bit(index));
        }
    }

    /// Pushes `value` in `size` bits, in two's complement. A little endian
    /// value has its bytes from the least significant, and then the bits that
    /// are left.
    fn push_int(&mut self, value: &BigInt, size: u64, endianness: Endianness) {
        if endianness.is_big() {
            self.push_value_bits(value, 0, size);
        } else {
            let whole = size / 8;
            for byte in 0..whole {
                self.push_value_bits(value, byte * 8, 8);
            }
            self.push_value_bits(value, whole * 8, size % 8);
        }
    }

    /// Pushes `segment` when it is a literal.
    fn push_literal(&mut self, segment: &TypedExprBitArraySegment) -> bool {
        let mut bits = self.clone();
        let pushed = bits.push_segment(segment).is_some();
        if pushed {
            *self = bits;
        }
        pushed
    }

    fn push_bits(&mut self, bits: &Bits, size: u64) {
        for index in 0..size {
            self.push_bit(bits.bit(index));
        }
    }

    /// The bits of `segments`, when they are literals.
    pub(super) fn of_literal<V: Literal>(
        segments: &[BitArraySegment<V, Arc<Type>>],
    ) -> Option<Bits> {
        let mut bits = Bits::default();
        for segment in segments {
            bits.push_segment(segment)?;
        }
        Some(bits)
    }

    fn push_segment<V: Literal>(&mut self, segment: &BitArraySegment<V, Arc<Type>>) -> Option<()> {
        let size = match segment.size() {
            Some(size) => {
                let LiteralValue::Int(size) = size.literal()? else {
                    return None;
                };
                Some((size * segment.unit()).to_u64()?)
            }
            None => None,
        };
        let endianness = segment.endianness();
        let type_ = &segment.type_;
        match segment.value.literal()? {
            LiteralValue::Int(value) if type_.is_int() => {
                self.push_int(&value, size.unwrap_or(8), endianness);
            }
            LiteralValue::Float(value) if type_.is_float() => {
                let (bits, size) = match size.unwrap_or(64) {
                    64 => (value.to_bits(), 64),
                    32 => (u64::from((value as f32).to_bits()), 32),
                    _ => return None,
                };
                self.push_int(&bits.into(), size, endianness);
            }
            LiteralValue::String(value) if type_.is_string() => {
                let value = unescape(value);
                if segment.has_utf16_option() {
                    for unit in value.encode_utf16() {
                        self.push_int(&unit.into(), 16, endianness);
                    }
                } else if segment.has_utf32_option() {
                    for char in value.chars() {
                        self.push_int(&u32::from(char).into(), 32, endianness);
                    }
                } else {
                    for byte in value.bytes() {
                        self.push_int(&byte.into(), 8, endianness);
                    }
                }
            }
            LiteralValue::BitArray(bits) if type_.is_bit_array() => {
                let size = size.unwrap_or(bits.size);
                // A segment longer than its value, or a `bytes` segment that
                // is not whole bytes, fails when it is created.
                if size > bits.size || segment.has_bytes_option() && !size.is_multiple_of(8) {
                    return None;
                }
                self.push_bits(&bits, size);
            }
            LiteralValue::Int(_)
            | LiteralValue::Float(_)
            | LiteralValue::String(_)
            | LiteralValue::BitArray(_) => return None,
        }
        Some(())
    }
}

impl Segment {
    fn constant(value: LocalId, size: u64, write: Write) -> Self {
        Segment {
            value: SegmentValue::Local(value),
            size: SegmentSize::Constant(size),
            write,
        }
    }
}

/// The value of a literal of a bit array.
pub(super) enum LiteralValue<'a> {
    Int(BigInt),
    Float(f64),
    String(&'a EcoString),
    BitArray(Bits),
}

/// An expression or a constant that can be a literal of a bit array.
pub(super) trait Literal {
    fn literal(&self) -> Option<LiteralValue<'_>>;
}

impl Literal for TypedExpr {
    fn literal(&self) -> Option<LiteralValue<'_>> {
        match self {
            TypedExpr::Int { int_value, .. } => Some(LiteralValue::Int(int_value.clone())),
            TypedExpr::NegateInt { value, .. } => match value.literal()? {
                LiteralValue::Int(value) => Some(LiteralValue::Int(-value)),
                LiteralValue::Float(_) | LiteralValue::String(_) | LiteralValue::BitArray(_) => {
                    None
                }
            },
            TypedExpr::Float { float_value, .. } => Some(LiteralValue::Float(float_value.value())),
            TypedExpr::String { value, .. } => Some(LiteralValue::String(value)),
            TypedExpr::BitArray { segments, .. } => {
                Bits::of_literal(segments).map(LiteralValue::BitArray)
            }
            TypedExpr::Var { constructor, .. } => constant_literal(&constructor.variant),
            TypedExpr::Block { .. }
            | TypedExpr::Pipeline { .. }
            | TypedExpr::Fn { .. }
            | TypedExpr::List { .. }
            | TypedExpr::Call { .. }
            | TypedExpr::BinOp { .. }
            | TypedExpr::Case { .. }
            | TypedExpr::RecordAccess { .. }
            | TypedExpr::PositionalAccess { .. }
            | TypedExpr::ModuleSelect { .. }
            | TypedExpr::Tuple { .. }
            | TypedExpr::TupleIndex { .. }
            | TypedExpr::Todo { .. }
            | TypedExpr::Panic { .. }
            | TypedExpr::Echo { .. }
            | TypedExpr::RecordUpdate { .. }
            | TypedExpr::NegateBool { .. }
            | TypedExpr::Invalid { .. } => None,
        }
    }
}

impl Literal for TypedConstant {
    fn literal(&self) -> Option<LiteralValue<'_>> {
        match self {
            Constant::Int { int_value, .. } => Some(LiteralValue::Int(int_value.clone())),
            Constant::Float { float_value, .. } => Some(LiteralValue::Float(float_value.value())),
            Constant::String { value, .. } => Some(LiteralValue::String(value)),
            Constant::BitArray { segments, .. } => {
                Bits::of_literal(segments).map(LiteralValue::BitArray)
            }
            Constant::Var { constructor, .. } => constructor
                .as_ref()
                .and_then(|constructor| constant_literal(&constructor.variant)),
            Constant::Tuple { .. }
            | Constant::List { .. }
            | Constant::Record { .. }
            | Constant::RecordUpdate { .. }
            | Constant::BinaryOperator { .. }
            | Constant::Invalid { .. }
            | Constant::Todo { .. } => None,
        }
    }
}

/// The literal of the module constant that `variant` names.
fn constant_literal(variant: &ValueConstructorVariant) -> Option<LiteralValue<'_>> {
    if let ValueConstructorVariant::ModuleConstant { literal, .. } = variant {
        literal.literal()
    } else {
        None
    }
}

/// A segment of a bit array expression that is not a literal: its value,
/// its size in bits and how it is written.
struct Segment {
    value: SegmentValue,
    size: SegmentSize,
    write: Write,
}

enum SegmentValue {
    Local(LocalId),
    /// The bits of consecutive literal segments.
    Global(GlobalId),
}

/// The `size` option of a segment, multiplied by its unit.
enum SizeOption {
    Literal(BigInt),
    Local(LocalId),
}

enum SegmentSize {
    Constant(u64),
    Local(LocalId),
}

/// The function of the runtime that writes a segment, with whether it is
/// little endian when it takes it.
#[derive(Clone, Copy)]
enum Write {
    Int(bool),
    Float(bool),
    Bits,
    Utf8,
    Utf16(bool),
    Utf32(bool),
    CodepointUtf8,
    CodepointUtf16(bool),
    CodepointUtf32(bool),
}

impl Write {
    fn of(segment: &TypedExprBitArraySegment) -> Self {
        let little = segment.endianness() == Endianness::Little;
        let type_ = &segment.type_;
        if type_.is_int() {
            Write::Int(little)
        } else if type_.is_float() {
            Write::Float(little)
        } else if type_.is_bit_array() {
            Write::Bits
        } else if type_.is_string() {
            if segment.has_utf16_option() {
                Write::Utf16(little)
            } else if segment.has_utf32_option() {
                Write::Utf32(little)
            } else {
                Write::Utf8
            }
        } else if type_.is_utf_codepoint() {
            if segment.has_utf16_codepoint_option() {
                Write::CodepointUtf16(little)
            } else if segment.has_utf32_codepoint_option() {
                Write::CodepointUtf32(little)
            } else {
                Write::CodepointUtf8
            }
        } else {
            panic!("invalid bit array segment type {type_:?}")
        }
    }

    /// The name of the function and whether it takes the size.
    fn function(self) -> (&'static str, bool) {
        match self {
            Write::Int(_) => ("bit_array_write_int", true),
            Write::Float(_) => ("bit_array_write_float", true),
            Write::Bits => ("bit_array_write_bits", true),
            Write::Utf8 => ("bit_array_write_utf8", false),
            Write::Utf16(_) => ("bit_array_write_utf16", false),
            Write::Utf32(_) => ("bit_array_write_utf32", false),
            Write::CodepointUtf8 => ("bit_array_write_codepoint_utf8", false),
            Write::CodepointUtf16(_) => ("bit_array_write_codepoint_utf16", false),
            Write::CodepointUtf32(_) => ("bit_array_write_codepoint_utf32", false),
        }
    }

    fn little(self) -> Option<bool> {
        match self {
            Write::Int(little)
            | Write::Float(little)
            | Write::Utf16(little)
            | Write::Utf32(little)
            | Write::CodepointUtf16(little)
            | Write::CodepointUtf32(little) => Some(little),
            Write::Bits | Write::Utf8 | Write::CodepointUtf8 => None,
        }
    }
}

impl Generator<'_> {
    /// Creates the bit array of `segments`. A literal one is a global, a
    /// single `bits` segment is its value, and any other one evaluates the
    /// values and sizes of its segments in order, creates bytes for all of
    /// them and writes each of them with the runtime:
    ///
    /// ```text
    /// from_bytes(bytes, write_2(bytes, write_1(bytes, 0, v1, s1), v2, s2))
    /// ```
    pub(super) fn expression_bit_array(
        &mut self,
        locals: &Locals,
        scope: Scope,
        instructions: &mut Instructions<'_, '_>,
        segments: &[TypedExprBitArraySegment],
    ) {
        if let Some(bits) = Bits::of_literal(segments) {
            let index = self.bit_array_global(bits);
            let _ = instructions.global_as_non_null(index);
            return;
        }
        if let [segment] = segments
            && segment.type_.is_bit_array()
            && segment.size().is_none()
        {
            let _ = instructions.expression(self, locals, scope, &segment.value);
            return;
        }
        let mut literal = Bits::default();
        let mut parts = vec![];
        for segment in segments {
            if literal.push_literal(segment) {
                continue;
            }
            if literal.size > 0 {
                parts.push(self.literal_segment(std::mem::take(&mut literal)));
            }
            if let Some(part) = self.segment(locals, &scope, instructions, segment) {
                parts.push(part);
            }
        }
        if literal.size > 0 {
            parts.push(self.literal_segment(literal));
        }

        let bytes_type = self.runtime_named_type(BIT_ARRAY_MODULE, "Bytes", vec![]);
        let bits_type = self.runtime_named_type(BIT_ARRAY_MODULE, "Bits", vec![]);
        let bytes = self.wasm_local(self.string.bytes_val_type());
        // The bytes of the total size.
        let constant: u64 = parts
            .iter()
            .filter_map(|part| match part.size {
                SegmentSize::Constant(size) => Some(size),
                SegmentSize::Local(_) => None,
            })
            .sum();
        let _ = instructions.int_const(&constant.into());
        for part in &parts {
            if let SegmentSize::Local(size) = part.size {
                let _ = instructions.local_get(size).int_add();
            }
        }
        let _ = instructions
            .int_const(&7.into())
            .int_add()
            .int_shift_by(false, &3.into())
            .int_to_i32()
            .bytes_new()
            .local_set(bytes);
        for _ in 0..=parts.len() {
            let _ = instructions.local_get(bytes);
        }
        let _ = instructions.int_const(&0.into());
        for part in parts {
            let (name, takes_size) = part.write.function();
            let value_type = match part.write {
                Write::Int(_) => type_::int(),
                Write::Float(_) => type_::float(),
                Write::Bits => bits_type.clone(),
                Write::Utf8 | Write::Utf16(_) | Write::Utf32(_) => type_::string(),
                Write::CodepointUtf8 | Write::CodepointUtf16(_) | Write::CodepointUtf32(_) => {
                    type_::utf_codepoint()
                }
            };
            let mut arguments = vec![bytes_type.clone(), type_::int(), value_type];
            let _ = match part.value {
                SegmentValue::Local(local) => instructions.local_get(local),
                SegmentValue::Global(global) => instructions.global_as_non_null(global),
            };
            if takes_size {
                arguments.push(type_::int());
                let _ = match part.size {
                    SegmentSize::Constant(size) => instructions.int_const(&size.into()),
                    SegmentSize::Local(local) => instructions.local_get(local),
                };
            }
            if let Some(little) = part.write.little() {
                arguments.push(type_::bool());
                let _ = instructions.bool_const(little);
            }
            let id = self.runtime_function_id(BIT_ARRAY_MODULE, name, arguments, type_::int());
            let _ = instructions.call(id);
        }
        let from_bytes = self.runtime_function_id(
            BIT_ARRAY_MODULE,
            "bit_array_from_bytes",
            vec![bytes_type, type_::int()],
            bits_type,
        );
        let _ = instructions.call(from_bytes);
    }

    fn literal_segment(&mut self, bits: Bits) -> Segment {
        let size = SegmentSize::Constant(bits.size);
        Segment {
            value: SegmentValue::Global(self.bit_array_global(bits)),
            size,
            write: Write::Bits,
        }
    }

    /// Evaluates the value and the size of `segment` to locals, and its size
    /// in bits when it is not constant, which fails when the segment cannot
    /// be created. None for an empty segment.
    fn segment(
        &mut self,
        locals: &Locals,
        scope: &Scope,
        instructions: &mut Instructions<'_, '_>,
        segment: &TypedExprBitArraySegment,
    ) -> Option<Segment> {
        let val_type = self.val_type(&segment.type_);
        let value = self.wasm_local(val_type);
        let _ = instructions
            .expression(self, locals, scope.clone(), &segment.value)
            .local_set(value);
        let unit = BigInt::from(segment.unit());
        let size = match segment.size() {
            None => None,
            Some(size) => match size.literal() {
                Some(LiteralValue::Int(size)) => Some(SizeOption::Literal(size * &unit)),
                _ => {
                    let local = self.wasm_local(self.int.val_type());
                    let _ = instructions.expression(self, locals, scope.clone(), size);
                    if unit != BigInt::from(1) {
                        let _ = instructions.int_const(&unit).int_mul();
                    }
                    let _ = instructions.local_set(local);
                    Some(SizeOption::Local(local))
                }
            },
        };
        let write = Write::of(segment);
        let bits_type = self.runtime_named_type(BIT_ARRAY_MODULE, "Bits", vec![]);
        // The function of the runtime that computes the size from the value
        // and the size option, if any.
        let (name, with_value) = match (write, &size) {
            (Write::Int(_), None) => return Some(Segment::constant(value, 8, write)),
            (Write::Float(_), None) => return Some(Segment::constant(value, 64, write)),
            (Write::CodepointUtf32(_), _) => return Some(Segment::constant(value, 32, write)),
            (Write::Int(_) | Write::Float(_), Some(SizeOption::Literal(size))) => {
                return size
                    .to_u64()
                    .filter(|size| *size > 0)
                    .map(|size| Segment::constant(value, size, write));
            }
            (Write::Int(_), Some(SizeOption::Local(_))) => ("bit_array_int_size", false),
            (Write::Float(_), Some(SizeOption::Local(_))) => ("bit_array_float_size", false),
            (Write::Bits, None) => ("bit_array_bit_size", true),
            (Write::Bits, Some(_)) => ("bit_array_bits_size", true),
            (Write::Utf8, _) => ("bit_array_utf8_size", true),
            (Write::Utf16(_), _) => ("bit_array_utf16_size", true),
            (Write::Utf32(_), _) => ("bit_array_utf32_size", true),
            (Write::CodepointUtf8, _) => ("bit_array_codepoint_utf8_size", true),
            (Write::CodepointUtf16(_), _) => ("bit_array_codepoint_utf16_size", true),
        };
        let mut arguments = vec![];
        if with_value {
            let value_type = if segment.type_.is_bit_array() {
                bits_type
            } else {
                segment.type_.clone()
            };
            arguments.push(value_type);
            let _ = instructions.local_get(value);
        }
        match size {
            None => {}
            Some(SizeOption::Literal(size)) => {
                arguments.push(type_::int());
                let _ = instructions.int_const(&size);
            }
            Some(SizeOption::Local(local)) => {
                arguments.push(type_::int());
                let _ = instructions.local_get(local);
            }
        }
        let id = self.runtime_function_id(BIT_ARRAY_MODULE, name, arguments, type_::int());
        let local = self.wasm_local(self.int.val_type());
        let _ = instructions.call(id).local_set(local);
        Some(Segment {
            value: SegmentValue::Local(value),
            size: SegmentSize::Local(local),
            write,
        })
    }

    /// The global with the bit array of `bits`, which the start function
    /// creates (see `code_bit_arrays`).
    pub(super) fn bit_array_global(&mut self, bits: Bits) -> GlobalId {
        if let Some(id) = self.bit_arrays.get(&bits) {
            return *id;
        }
        if self.bit_array_from_bytes.is_none() {
            let bytes = self.runtime_named_type(BIT_ARRAY_MODULE, "Bytes", vec![]);
            let from_bytes = self.runtime_function_id(
                BIT_ARRAY_MODULE,
                "bit_array_from_bytes",
                vec![bytes, type_::int()],
                self.runtime_named_type(BIT_ARRAY_MODULE, "Bits", vec![]),
            );
            self.bit_array_from_bytes = Some(from_bytes);
        }
        let type_index = self.type_index(&type_::bit_array());
        let id = self.wasm_module.globals.add_local(
            self.val_type_ref_nullable(type_index),
            true,
            false,
            const_expr_ref_null(type_index),
        );
        self.wasm_module.globals.get_mut(id).name = Some(format!(
            "bit_array@{}<{} bits>",
            self.bit_arrays.len(),
            bits.size
        ));
        let _ = self.bit_arrays.insert(bits, id);
        id
    }

    /// Creates the bit arrays of the literals, in the start function.
    pub(super) fn code_bit_arrays(&mut self, instructions: &mut Instructions<'_, '_>) {
        let Some(from_bytes) = self.bit_array_from_bytes else {
            return;
        };
        for (bits, index) in self.bit_arrays.clone() {
            let length = bits.bytes.len() as i32;
            let data = self
                .wasm_module
                .data
                .add(walrus::DataKind::Passive, bits.bytes);
            let _ = instructions
                .i32_const(0)
                .i32_const(length)
                .array_new_data(self.string.bytes_index, data)
                .int_const(&bits.size.into())
                .call(from_bytes)
                .global_set(index);
        }
    }
}
