// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Bit arrays. The `BitArray` of the prelude is the type `Bits` of the
//! runtime module `builtins/bit_array`, which also has their functions. Here
//! is how a bit array expression is created: one whose segments are literals
//! is computed when compiling, and created once by the start function, and
//! any other one writes its segments with the functions of the runtime. And
//! here is how a bit array pattern is matched, segment by segment, with the
//! functions of the runtime that read them.

use super::*;
use crate::ast::{
    BitArraySegment, BitArraySize, Endianness, IntOperator, TypedBitArraySize,
    TypedConstantBitArraySegment, TypedExprBitArraySegment, TypedPatternBitArraySegment,
};
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
    fn push_literal<V: Literal>(
        &mut self,
        segment: &BitArraySegment<V, Arc<Type>>,
        float: FloatType,
    ) -> bool {
        let (size, length) = (self.size, self.bytes.len());
        let pushed = self.push_segment(segment, float).is_some();
        if !pushed {
            self.size = size;
            self.bytes.truncate(length);
            // The bits after `size` are zero.
            if let Some(last) = self.bytes.last_mut()
                && !size.is_multiple_of(8)
            {
                *last &= !(0xFF >> (size % 8));
            }
        }
        pushed
    }

    /// The unsigned integer of the bits.
    fn unsigned(&self) -> BigInt {
        (0..self.size).fold(BigInt::ZERO, |value, index| {
            value * 2 + u8::from(self.bit(index))
        })
    }

    fn push_bits(&mut self, bits: &Bits, size: u64) {
        for index in 0..size {
            self.push_bit(bits.bit(index));
        }
    }

    /// The bits of `segments`, when they are literals, with floats of type
    /// `float`.
    pub(super) fn of_literal<V: Literal>(
        segments: &[BitArraySegment<V, Arc<Type>>],
        float: FloatType,
    ) -> Option<Bits> {
        let mut bits = Bits::default();
        for segment in segments {
            bits.push_segment(segment, float)?;
        }
        Some(bits)
    }

    fn push_segment<V: Literal>(
        &mut self,
        segment: &BitArraySegment<V, Arc<Type>>,
        float: FloatType,
    ) -> Option<()> {
        let size = match segment.size() {
            Some(size) => {
                let LiteralValue::Int(size) = size.literal(float)? else {
                    return None;
                };
                Some((size * segment.unit()).to_u64()?)
            }
            None => None,
        };
        let endianness = segment.endianness();
        let type_ = &segment.type_;
        match segment.value.literal(float)? {
            LiteralValue::Int(value) if type_.is_int() => {
                let size = size.unwrap_or(8);
                // A larger one is written by the runtime.
                if size > MAX_LITERAL_INT_SIZE {
                    return None;
                }
                self.push_int(&value, size, endianness);
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

/// An expression or a constant that can be a literal of a bit array. A
/// float is its value in `float`, as the program sees it.
pub(super) trait Literal {
    fn literal(&self, float: FloatType) -> Option<LiteralValue<'_>>;
}

impl Literal for TypedExpr {
    fn literal(&self, float: FloatType) -> Option<LiteralValue<'_>> {
        match self {
            TypedExpr::Int { int_value, .. } => Some(LiteralValue::Int(int_value.clone())),
            TypedExpr::NegateInt { value, .. } => match value.literal(float)? {
                LiteralValue::Int(value) => Some(LiteralValue::Int(-value)),
                LiteralValue::Float(_) | LiteralValue::String(_) | LiteralValue::BitArray(_) => {
                    None
                }
            },
            TypedExpr::Float { value, .. } => Some(LiteralValue::Float(float.literal_value(value))),
            TypedExpr::String { value, .. } => Some(LiteralValue::String(value)),
            TypedExpr::BitArray { segments, .. } => {
                Bits::of_literal(segments, float).map(LiteralValue::BitArray)
            }
            TypedExpr::Var { constructor, .. } => constant_literal(&constructor.variant, float),
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
    fn literal(&self, float: FloatType) -> Option<LiteralValue<'_>> {
        match self {
            Constant::Int { int_value, .. } => Some(LiteralValue::Int(int_value.clone())),
            Constant::Float { value, .. } => Some(LiteralValue::Float(float.literal_value(value))),
            Constant::String { value, .. } => Some(LiteralValue::String(value)),
            Constant::BitArray { segments, .. } => {
                Bits::of_literal(segments, float).map(LiteralValue::BitArray)
            }
            Constant::Var { constructor, .. } => constructor
                .as_ref()
                .and_then(|constructor| constant_literal(&constructor.variant, float)),
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

impl Literal for TypedPattern {
    fn literal(&self, _float: FloatType) -> Option<LiteralValue<'_>> {
        match self {
            Pattern::Int { int_value, .. } => Some(LiteralValue::Int(int_value.clone())),
            Pattern::String { value, .. } => Some(LiteralValue::String(value)),
            Pattern::BitArraySize(size) => size.compile_time_number().map(LiteralValue::Int),
            // A float is matched by its value, see `pattern_segment`.
            Pattern::Float { .. }
            | Pattern::Variable { .. }
            | Pattern::Assign { .. }
            | Pattern::Discard { .. }
            | Pattern::List { .. }
            | Pattern::Constructor { .. }
            | Pattern::Tuple { .. }
            | Pattern::BitArray { .. }
            | Pattern::StringPrefix { .. }
            | Pattern::Invalid { .. } => None,
        }
    }
}

/// The literal of the module constant that `variant` names.
fn constant_literal(
    variant: &ValueConstructorVariant,
    float: FloatType,
) -> Option<LiteralValue<'_>> {
    if let ValueConstructorVariant::ModuleConstant { literal, .. } = variant {
        literal.literal(float)
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
#[derive(Clone, Copy, Debug)]
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
    fn of<V>(segment: &BitArraySegment<V, Arc<Type>>) -> Self {
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

    /// The type of the value that it writes.
    fn value_type(self) -> Arc<Type> {
        match self {
            Write::Int(_) => type_::int(),
            Write::Float(_) => type_::float(),
            Write::Bits => type_::bit_array(),
            Write::Utf8 | Write::Utf16(_) | Write::Utf32(_) => type_::string(),
            Write::CodepointUtf8 | Write::CodepointUtf16(_) | Write::CodepointUtf32(_) => {
                type_::utf_codepoint()
            }
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
    ///
    /// When the first segment is a whole bit array, the others are written
    /// after it, to the bytes of `bit_array_extend(v1, s2 + s3)`.
    pub(super) fn expression_bit_array(
        &mut self,
        locals: &Locals,
        scope: Scope,
        instructions: &mut Instructions<'_, '_>,
        segments: &[TypedExprBitArraySegment],
    ) {
        self.bit_array(
            instructions,
            segments,
            &mut |generator, instructions, value| {
                let _ = instructions.expression(generator, locals, scope.clone(), value);
            },
        );
    }

    /// Creates the bit array of the constant `segments`, as
    /// `expression_bit_array` does.
    pub(super) fn constant_bit_array(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        segments: &[TypedConstantBitArraySegment],
    ) {
        self.bit_array(
            instructions,
            segments,
            &mut |generator, instructions, value| {
                let _ = instructions.constant(generator, value);
            },
        );
    }

    /// Creates the bit array of `segments`, whose values and sizes are pushed
    /// by `evaluate`.
    fn bit_array<V: Literal>(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        segments: &[BitArraySegment<V, Arc<Type>>],
        evaluate: &mut impl FnMut(&mut Generator<'_>, &mut Instructions<'_, '_>, &V),
    ) {
        if let Some(bits) = Bits::of_literal(segments, self.float) {
            let index = self.bit_array_global(bits);
            let _ = instructions.global_as_non_null(index);
            return;
        }
        if let [segment] = segments
            && segment.type_.is_bit_array()
            && segment.size().is_none()
        {
            evaluate(self, instructions, &segment.value);
            return;
        }
        // A first segment that is a whole bit array is extended (see
        // `bit_array_extend`).
        let extended = match segments {
            [first, ..] if first.size().is_none() && first.value.literal(self.float).is_none() => {
                first.type_.is_bit_array().then(|| {
                    let val_type = self.val_type(&type_::bit_array());
                    let local = self.wasm_local(val_type);
                    evaluate(self, instructions, &first.value);
                    let _ = instructions.local_set(local);
                    local
                })
            }
            _ => None,
        };
        let mut literal = Bits::default();
        let mut parts = vec![];
        for segment in segments.iter().skip(usize::from(extended.is_some())) {
            if literal.push_literal(segment, self.float) {
                continue;
            }
            if literal.size > 0 {
                parts.push(self.literal_segment(std::mem::take(&mut literal)));
            }
            if let Some(part) = self.segment(instructions, segment, evaluate) {
                parts.push(part);
            }
        }
        if literal.size > 0 {
            parts.push(self.literal_segment(literal));
        }

        let bytes_type = self.runtime_named_type(BIT_ARRAY_MODULE, "Bytes", vec![]);
        let bytes = self.wasm_local(self.string.bytes_val_type());
        // The size of the segments, after the extended bit array if any.
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
        // The bytes, and the position of the first segment on the stack.
        let start = match extended {
            None => {
                let _ = instructions
                    .int_const(&7.into())
                    .int_add()
                    .int_shift_by(false, &3.into())
                    .int_to_i32()
                    .bytes_new()
                    .local_set(bytes);
                None
            }
            Some(first) => {
                let size = self.wasm_local(self.int.val_type());
                let val_type = self.val_type(&type_::bit_array());
                let result = self.wasm_local(val_type);
                let extend = self.runtime_function_id(
                    BIT_ARRAY_MODULE,
                    "bit_array_extend",
                    vec![type_::bit_array(), type_::int()],
                    type_::bit_array(),
                );
                let _ = instructions
                    .local_set(size)
                    .local_get(first)
                    .local_get(size)
                    .call(extend)
                    .local_tee(result);
                self.bits_field(instructions, "bytes");
                let _ = instructions.local_set(bytes);
                Some((size, result))
            }
        };
        for _ in 0..parts.len() + usize::from(start.is_none()) {
            let _ = instructions.local_get(bytes);
        }
        match start {
            None => {
                let _ = instructions.int_const(&0.into());
            }
            Some((size, result)) => {
                let _ = instructions.local_get(result);
                self.bits_field(instructions, "end");
                let _ = instructions.local_get(size).int_sub();
            }
        }
        for part in parts {
            let (name, takes_size) = part.write.function();
            let value_type = part.write.value_type();
            let mut arguments = vec![bytes_type.clone(), type_::int(), value_type];
            let _ = match part.value {
                SegmentValue::Local(local) => instructions.local_get(local),
                SegmentValue::Global(global) => instructions.global_as_non_null(global),
            };
            if takes_size {
                arguments.push(type_::int());
                push_size(instructions, &part.size);
            }
            if let Some(little) = part.write.little() {
                arguments.push(type_::bool());
                let _ = instructions.bool_const(little);
            }
            let id = self.runtime_function_id(BIT_ARRAY_MODULE, name, arguments, type_::int());
            let _ = instructions.call(id);
        }
        match start {
            None => {
                let from_bytes = self.bit_array_from_bytes_id();
                let _ = instructions.call(from_bytes);
            }
            Some((_, result)) => {
                let _ = instructions.drop().local_get(result);
            }
        }
    }

    /// The type `Bits` of the runtime, which represents `BitArray`.
    fn bits_type(&mut self) -> Arc<Type> {
        self.runtime_named_type(BIT_ARRAY_MODULE, "Bits", vec![])
    }

    /// The `bit_array_from_bytes` of the runtime.
    fn bit_array_from_bytes_id(&mut self) -> FunctionId {
        if let Some(id) = self.bit_array_from_bytes {
            return id;
        }
        let bytes_type = self.runtime_named_type(BIT_ARRAY_MODULE, "Bytes", vec![]);
        let id = self.runtime_function_id(
            BIT_ARRAY_MODULE,
            "bit_array_from_bytes",
            vec![bytes_type, type_::int()],
            type_::bit_array(),
        );
        self.bit_array_from_bytes = Some(id);
        id
    }

    /// Pushes the number of bits of the `Bits` of `bits`.
    fn push_bit_size(&mut self, instructions: &mut Instructions<'_, '_>, bits: LocalId) {
        let _ = instructions.local_get(bits);
        self.bits_field(instructions, "end");
        let _ = instructions.local_get(bits);
        self.bits_field(instructions, "start");
        let _ = instructions.int_sub();
    }

    /// Replaces the `Bits` of the runtime on the stack by its field `label`.
    fn bits_field(&mut self, instructions: &mut Instructions<'_, '_>, label: &str) {
        let bits_type = self.bits_type();
        let (
            CustomType::Struct {
                custom_type,
                constructor,
            },
            args,
        ) = self.custom_type_expect(&bits_type)
        else {
            panic!("Bits to be a struct");
        };
        let (type_index, types) =
            self.mono_struct_type_index(&bits_type, &custom_type, &constructor, &args);
        let index = constructor
            .arguments
            .iter()
            .position(|argument| matches!(&argument.label, Some((_, l)) if l == label))
            .expect("Bits to have the field");
        let field = self.field_index(&types, index as u64);
        let _ = instructions.field_get(type_index, field);
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
    fn segment<V: Literal>(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        segment: &BitArraySegment<V, Arc<Type>>,
        evaluate: &mut impl FnMut(&mut Generator<'_>, &mut Instructions<'_, '_>, &V),
    ) -> Option<Segment> {
        let val_type = self.val_type(&segment.type_);
        let value = self.wasm_local(val_type);
        evaluate(self, instructions, &segment.value);
        let _ = instructions.local_set(value);
        let unit = BigInt::from(segment.unit());
        let size = match segment.size() {
            None => None,
            Some(size) => match size.literal(self.float) {
                Some(LiteralValue::Int(size)) => Some(SizeOption::Literal(size * &unit)),
                _ => {
                    let local = self.wasm_local(self.int.val_type());
                    evaluate(self, instructions, size);
                    if unit != BigInt::from(1) {
                        let _ = instructions.int_const(&unit).int_mul();
                    }
                    let _ = instructions.local_set(local);
                    Some(SizeOption::Local(local))
                }
            },
        };
        let write = Write::of(segment);
        // The size is computed in place when it is simple, and otherwise by a
        // function of the runtime, from the value and the size option, if
        // any.
        let runtime = match (write, &size) {
            (Write::Int(_), None) => return Some(Segment::constant(value, 8, write)),
            (Write::Float(_), None) => return Some(Segment::constant(value, 64, write)),
            (Write::CodepointUtf32(_), _) => return Some(Segment::constant(value, 32, write)),
            (Write::Int(_), Some(SizeOption::Literal(size))) => {
                return size
                    .to_u64()
                    .filter(|size| *size > 0)
                    .map(|size| Segment::constant(value, size, write));
            }
            // The size of a float, maybe a constant, is checked by the
            // runtime when it is not 16, 32 or 64.
            (Write::Float(_), Some(SizeOption::Literal(size)))
                if matches!(size.to_u64(), Some(16 | 32 | 64)) =>
            {
                return size
                    .to_u64()
                    .map(|size| Segment::constant(value, size, write));
            }
            // A negative size is 0.
            (Write::Int(_), Some(SizeOption::Local(size))) => {
                let _ = instructions
                    .local_get(*size)
                    .int_const(&0.into())
                    .local_get(*size)
                    .int_const(&0.into())
                    .int_gt()
                    .select(None);
                None
            }
            (Write::Bits, None) => {
                self.push_bit_size(instructions, value);
                None
            }
            (Write::Utf8, _) => {
                let _ = instructions
                    .local_get(value)
                    .string_len()
                    .i32_to_int()
                    .int_const(&8.into())
                    .int_mul();
                None
            }
            (Write::CodepointUtf8 | Write::CodepointUtf16(_), _) => {
                push_codepoint_size(instructions, value, false, write);
                None
            }
            (Write::Float(_), Some(_)) => Some(("bit_array_float_size", false)),
            (Write::Bits, Some(_)) => Some(("bit_array_bits_size", true)),
            (Write::Utf16(_), _) => Some(("bit_array_utf16_size", true)),
            (Write::Utf32(_), _) => Some(("bit_array_utf32_size", true)),
        };
        if let Some((name, with_value)) = runtime {
            let mut arguments = vec![];
            if with_value {
                arguments.push(write.value_type());
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
            let _ = instructions.call(id);
        }
        let local = self.wasm_local(self.int.val_type());
        let _ = instructions.local_set(local);
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
        // Asked now, as the start function is generated after the
        // functions.
        let _ = self.bit_array_from_bytes_id();
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
            self.push_bytes(instructions, bits.bytes);
            let _ = instructions
                .int_const(&bits.size.into())
                .call(from_bytes)
                .global_set(index);
        }
    }
}

/// The largest literal integer segment whose bits are computed when
/// compiling: in an expression, they are part of the literal bits, and in a
/// pattern, they are compared with the bits of the other literals, instead
/// of with its value.
const MAX_LITERAL_INT_SIZE: u64 = 1 << 16;

/// The position of a segment of a bit array pattern: a constant and the sum
/// of the sizes before it that are not constant, if any.
#[derive(Default)]
struct Position {
    constant: u64,
    local: Option<LocalId>,
}

/// The locals of a bit array pattern while its segments are matched.
struct Matching {
    /// The bit array.
    bits: LocalId,
    /// Its size.
    size: LocalId,
    position: Position,
    fail_target: InstrSeqId,
}

impl Generator<'_> {
    /// Matches the bit array on the stack with the `segments` of `pattern`,
    /// from its first bit, see the runtime module of bit arrays: consecutive
    /// literal segments are compared with the bits of their literal, and any
    /// other one checks that the bit array has its bits before reading them.
    pub(super) fn pattern_bit_array(
        &mut self,
        locals: &Locals,
        mut scope: Scope,
        instructions: &mut Instructions<'_, '_>,
        pattern: &TypedPattern,
        segments: &[TypedPatternBitArraySegment],
        fail_target: InstrSeqId,
    ) -> Scope {
        let mut matching = Matching {
            bits: locals.for_pattern(pattern),
            size: self.wasm_local(self.int.val_type()),
            position: Position::default(),
            fail_target,
        };
        let _ = instructions.local_set(matching.bits);
        self.push_bit_size(instructions, matching.bits);
        let _ = instructions.local_set(matching.size);
        let mut literal = Bits::default();
        let mut ends_with_literal = true;
        for (index, segment) in segments.iter().enumerate() {
            if is_literal_segment(segment) && literal.push_literal(segment, self.float) {
                ends_with_literal = true;
                continue;
            }
            ends_with_literal = false;
            if literal.size > 0 {
                self.pattern_literal(
                    instructions,
                    &mut matching,
                    std::mem::take(&mut literal),
                    false,
                );
            }
            let last = index + 1 == segments.len();
            scope = self.pattern_segment(locals, scope, instructions, &mut matching, segment, last);
        }
        if ends_with_literal {
            self.pattern_literal(instructions, &mut matching, literal, true);
        }
        scope
    }

    /// Compares the bits of the bit array at the position with `bits`, the
    /// bits of consecutive literal segments, the `last` ones of the pattern
    /// when it ends with them. The bits of a non-negative `Int` are compared
    /// with the integer that is read.
    fn pattern_literal(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        matching: &mut Matching,
        bits: Bits,
        last: bool,
    ) {
        let size = SegmentSize::Constant(bits.size);
        self.check_room(instructions, matching, &size, last);
        if bits.size == 0 {
            // Nothing to compare.
        } else if bits.size < instructions.int_bits() as u64 {
            let read = self.runtime_function_id(
                BIT_ARRAY_MODULE,
                "bit_array_read_int",
                vec![
                    type_::bit_array(),
                    type_::int(),
                    type_::int(),
                    type_::bool(),
                    type_::bool(),
                ],
                type_::int(),
            );
            self.push_read_arguments(instructions, matching, &size);
            let _ = instructions
                .bool_const(false)
                .bool_const(false)
                .call(read)
                .int_const(&bits.unsigned())
                .int_ne()
                .br_if(matching.fail_target);
        } else {
            let match_ = self.runtime_function_id(
                BIT_ARRAY_MODULE,
                "bit_array_match",
                vec![type_::bit_array(), type_::int(), type_::bit_array()],
                type_::bool(),
            );
            let global = self.bit_array_global(bits);
            push_bits_at(instructions, matching);
            let _ = instructions
                .global_as_non_null(global)
                .call(match_)
                .bool_not()
                .br_if(matching.fail_target);
        }
        if !last {
            self.advance(instructions, matching, size);
        }
    }

    /// Matches `segment`, which is not a literal, the `last` one of the
    /// pattern when it is.
    fn pattern_segment(
        &mut self,
        locals: &Locals,
        mut scope: Scope,
        instructions: &mut Instructions<'_, '_>,
        matching: &mut Matching,
        segment: &TypedPatternBitArraySegment,
        last: bool,
    ) -> Scope {
        let type_ = &segment.type_;
        if type_.is_utf_codepoint() {
            return self.pattern_codepoint(locals, scope, instructions, matching, segment, last);
        }
        let value = segment.value.as_ref();
        if let Pattern::Assign { name, pattern, .. } = value
            && let Pattern::String { value: string, .. } = pattern.as_ref()
        {
            // The string of the literal is the value of the assignment.
            let literal = BitArraySegment {
                value: pattern.clone(),
                ..segment.clone()
            };
            let bits = Bits::of_literal(std::slice::from_ref(&literal), self.float)
                .expect("literal string segment");
            self.pattern_literal(instructions, matching, bits, last);
            let local = locals.for_pattern(value);
            let index = self.string_index(string);
            let _ = instructions.global_as_non_null(index).local_set(local);
            return scope.insert_local(name.clone(), local);
        }
        let little = segment.endianness() == Endianness::Little;
        let size = match segment.size() {
            Some(Pattern::BitArraySize(size)) => Some(self.pattern_segment_size(
                &scope,
                instructions,
                matching,
                size,
                segment.unit(),
            )),
            Some(size) => panic!("invalid bit array segment size {size:?}"),
            None if type_.is_int() => Some(SegmentSize::Constant(8)),
            None if type_.is_float() => Some(SegmentSize::Constant(64)),
            None => None,
        };
        let size = match size {
            Some(size) => {
                self.check_room(instructions, matching, &size, last);
                size
            }
            None => {
                // The rest of the bit array, of whole bytes for `bytes`.
                let local = self.wasm_local(self.int.val_type());
                let _ = instructions.local_get(matching.size);
                push_position(instructions, &matching.position);
                let _ = instructions.int_sub().local_tee(local);
                if segment.has_bytes_option() {
                    let _ = instructions
                        .int_const(&7.into())
                        .int_and()
                        .int_eqz()
                        .bool_not()
                        .br_if(matching.fail_target);
                } else {
                    let _ = instructions.drop();
                }
                SegmentSize::Local(local)
            }
        };
        let arguments = vec![type_::bit_array(), type_::int(), type_::int()];
        // A float is read even when discarded, as it must be finite.
        if !value.is_discard() || type_.is_float() {
            // The read function, with its flags after the arguments.
            let (name, flags, result) = if type_.is_int() {
                (
                    "bit_array_read_int",
                    vec![little, segment.signed()],
                    type_::int(),
                )
            } else if type_.is_float() {
                ("bit_array_read_float", vec![little], type_::float())
            } else if type_.is_bit_array() {
                ("bit_array_read_bits", vec![], type_::bit_array())
            } else {
                panic!("invalid bit array segment type {type_:?}")
            };
            self.push_read_arguments(instructions, matching, &size);
            let mut arguments = arguments;
            for flag in flags {
                arguments.push(type_::bool());
                let _ = instructions.bool_const(flag);
            }
            let read = self.runtime_function_id(BIT_ARRAY_MODULE, name, arguments, result);
            let _ = instructions.call(read);
            if type_.is_float() {
                // It is finite when subtracting it from itself gives 0.
                let float = self.wasm_local(self.float.val_type());
                let _ = instructions
                    .local_tee(float)
                    .local_get(float)
                    .float_sub()
                    .float_const("0.0")
                    .float_ne()
                    .br_if(matching.fail_target)
                    .local_get(float);
            }
            if let Pattern::Float { value, .. } = value {
                // A float is equal to the literal when its bits are, so 0.0
                // and -0.0 are different.
                let value = self.float.literal_value(value);
                let _ = match self.float {
                    FloatType::F32 => instructions
                        .float_reinterpret()
                        .i32_const((value as f32).to_bits() as i32)
                        .i32_ne(),
                    FloatType::F64 => instructions
                        .float_reinterpret()
                        .i64_const(value.to_bits() as i64)
                        .i64_ne(),
                };
                let _ = instructions.br_if(matching.fail_target);
            } else {
                scope = self._pattern(locals, scope, instructions, value, matching.fail_target);
            }
        }
        if !last {
            self.advance(instructions, matching, size);
        }
        scope
    }

    /// Matches the code point `segment`, whose size is the size of the code
    /// point that is read.
    fn pattern_codepoint(
        &mut self,
        locals: &Locals,
        mut scope: Scope,
        instructions: &mut Instructions<'_, '_>,
        matching: &mut Matching,
        segment: &TypedPatternBitArraySegment,
        last: bool,
    ) -> Scope {
        let write = Write::of(segment);
        let read = match write {
            Write::CodepointUtf8 => "bit_array_read_codepoint_utf8",
            Write::CodepointUtf16(_) => "bit_array_read_codepoint_utf16",
            Write::CodepointUtf32(_) => "bit_array_read_codepoint_utf32",
            Write::Int(_)
            | Write::Float(_)
            | Write::Bits
            | Write::Utf8
            | Write::Utf16(_)
            | Write::Utf32(_) => panic!("invalid code point segment {write:?}"),
        };
        let mut arguments = vec![type_::bit_array(), type_::int()];
        push_bits_at(instructions, matching);
        if let Some(little) = write.little() {
            arguments.push(type_::bool());
            let _ = instructions.bool_const(little);
        }
        let read = self.runtime_function_id(BIT_ARRAY_MODULE, read, arguments, type_::int());
        let codepoint = self.wasm_local(self.int.val_type());
        let _ = instructions
            .call(read)
            .local_tee(codepoint)
            .int_const(&0.into())
            .int_lt()
            .br_if(matching.fail_target);
        let size = if let Write::CodepointUtf32(_) = write {
            SegmentSize::Constant(32)
        } else {
            let local = self.wasm_local(self.int.val_type());
            push_codepoint_size(instructions, codepoint, true, write);
            let _ = instructions.local_set(local);
            SegmentSize::Local(local)
        };
        if last {
            self.check_room(instructions, matching, &size, true);
        }
        if !segment.value.is_discard() {
            let _ = instructions.local_get(codepoint).int_to_i32();
            scope = self._pattern(
                locals,
                scope,
                instructions,
                &segment.value,
                matching.fail_target,
            );
        }
        if !last {
            self.advance(instructions, matching, size);
        }
        scope
    }

    /// The size in bits of a segment of `size` in `unit`s, in a local that
    /// fails the match when it is negative, unless it is constant.
    fn pattern_segment_size(
        &mut self,
        scope: &Scope,
        instructions: &mut Instructions<'_, '_>,
        matching: &Matching,
        size: &TypedBitArraySize,
        unit: u8,
    ) -> SegmentSize {
        if let Some(size) = size.compile_time_number() {
            // No bit array has more bits than the largest `Int`, and none
            // has a segment of a negative size.
            let max = self.int_max();
            return match (size * unit).to_u64() {
                Some(size) => SegmentSize::Constant(size.min(max)),
                None => {
                    let _ = instructions.bool_const(true).br_if(matching.fail_target);
                    SegmentSize::Constant(0)
                }
            };
        }
        let local = self.wasm_local(self.int.val_type());
        self.bit_array_size(scope, instructions, size);
        if unit != 1 {
            let _ = instructions.int_const(&unit.into()).int_mul();
        }
        let _ = instructions
            .local_tee(local)
            .int_const(&0.into())
            .int_lt()
            .br_if(matching.fail_target);
        SegmentSize::Local(local)
    }

    /// The value of `size`, with the operators of `Int`.
    fn bit_array_size(
        &mut self,
        scope: &Scope,
        instructions: &mut Instructions<'_, '_>,
        size: &TypedBitArraySize,
    ) {
        match size {
            BitArraySize::Int { int_value, .. } => {
                let _ = instructions.int_const(int_value);
            }
            BitArraySize::Variable {
                name, constructor, ..
            } => match constructor
                .as_deref()
                .map(|constructor| &constructor.variant)
            {
                // A constant, maybe of another module.
                Some(ValueConstructorVariant::ModuleConstant { literal, .. }) => {
                    self.constant_value(instructions, literal, &type_::int());
                }
                Some(ValueConstructorVariant::LocalVariable { location, .. }) => {
                    let location = Some(*location);
                    self.expression_var(scope, instructions, name, &type_::int(), location);
                }
                _ => self.expression_var(scope, instructions, name, &type_::int(), None),
            },
            BitArraySize::Block { inner, .. } => self.bit_array_size(scope, instructions, inner),
            BitArraySize::BinaryOperator {
                operator,
                left,
                right,
                ..
            } => {
                self.bit_array_size(scope, instructions, left);
                self.bit_array_size(scope, instructions, right);
                let _ = match operator {
                    IntOperator::Add => instructions.int_add(),
                    IntOperator::Subtract => instructions.int_sub(),
                    IntOperator::Multiply => instructions.int_mul(),
                    IntOperator::Divide | IntOperator::Remainder => {
                        let dividend = self.wasm_local(self.int.val_type());
                        let divisor = self.wasm_local(self.int.val_type());
                        if *operator == IntOperator::Divide {
                            instructions.int_div(dividend, divisor)
                        } else {
                            instructions.int_rem(dividend, divisor)
                        }
                    }
                };
            }
        }
    }

    /// Fails the match when the bit array does not have `size` bits from the
    /// position, or more of them after the `last` segment. The bits after
    /// the position are compared, as the position is not after the end.
    fn check_room(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        matching: &Matching,
        size: &SegmentSize,
        last: bool,
    ) {
        let _ = instructions.local_get(matching.size);
        if matching.position.constant > 0 || matching.position.local.is_some() {
            push_position(instructions, &matching.position);
            let _ = instructions.int_sub();
        }
        push_size(instructions, size);
        let _ = if last {
            instructions.int_ne()
        } else {
            instructions.int_lt()
        };
        let _ = instructions.br_if(matching.fail_target);
    }

    /// Pushes the arguments of a `bit_array_read_*` function: the bit array,
    /// the position and the size.
    fn push_read_arguments(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        matching: &Matching,
        size: &SegmentSize,
    ) {
        push_bits_at(instructions, matching);
        push_size(instructions, size);
    }

    /// The largest `Int`, which is larger than the number of bits of any bit
    /// array.
    fn int_max(&self) -> u64 {
        match self.int {
            IntType::I32 => i32::MAX as u64,
            IntType::I64 => i64::MAX as u64,
        }
    }

    /// Moves the position after a segment of `size` bits.
    fn advance(
        &mut self,
        instructions: &mut Instructions<'_, '_>,
        matching: &mut Matching,
        size: SegmentSize,
    ) {
        match size {
            SegmentSize::Constant(size) => {
                matching.position.constant = (matching.position.constant + size).min(self.int_max())
            }
            SegmentSize::Local(size) => {
                matching.position.local = Some(match matching.position.local {
                    None => size,
                    Some(position) => {
                        let local = self.wasm_local(self.int.val_type());
                        let _ = instructions
                            .local_get(position)
                            .local_get(size)
                            .int_add()
                            .local_set(local);
                        local
                    }
                });
            }
        }
    }
}

/// Pushes the size in bits of the code point of `codepoint`, an `Int` or a
/// `UtfCodepoint`, in the UTF-8 or UTF-16 of `write`: one unit, and one more
/// from each limit.
fn push_codepoint_size(
    instructions: &mut Instructions<'_, '_>,
    codepoint: LocalId,
    is_int: bool,
    write: Write,
) {
    let (limits, unit): (&[i32], i32) = if let Write::CodepointUtf8 = write {
        (&[0x80, 0x800, 0x10000], 8)
    } else {
        (&[0x10000], 16)
    };
    let _ = instructions.i32_const(1);
    for limit in limits {
        let _ = instructions.local_get(codepoint);
        if is_int {
            let _ = instructions.int_to_i32();
        }
        let _ = instructions.i32_const(*limit).i32_ge_s().i32_add();
    }
    let _ = instructions.i32_const(unit).i32_mul().i32_to_int();
}

/// Pushes the bit array of a pattern and the position.
fn push_bits_at(instructions: &mut Instructions<'_, '_>, matching: &Matching) {
    let _ = instructions.local_get(matching.bits);
    push_position(instructions, &matching.position);
}

fn push_size(instructions: &mut Instructions<'_, '_>, size: &SegmentSize) {
    let _ = match size {
        SegmentSize::Constant(size) => instructions.int_const(&(*size).into()),
        SegmentSize::Local(size) => instructions.local_get(*size),
    };
}

fn push_position(instructions: &mut Instructions<'_, '_>, position: &Position) {
    match position.local {
        None => {
            let _ = instructions.int_const(&position.constant.into());
        }
        Some(local) => {
            let _ = instructions.local_get(local);
            if position.constant > 0 {
                let _ = instructions.int_const(&position.constant.into()).int_add();
            }
        }
    }
}

/// Whether `segment` is a literal whose bits are compared with the bits of
/// the other literals: a string, or an integer of a constant size that has
/// its value.
fn is_literal_segment(segment: &TypedPatternBitArraySegment) -> bool {
    match segment.value.as_ref() {
        Pattern::String { .. } => true,
        Pattern::Int { int_value, .. } => {
            let size = match segment.size() {
                None => Some(BigInt::from(8)),
                Some(Pattern::BitArraySize(size)) => {
                    size.compile_time_number().map(|size| size * segment.unit())
                }
                Some(_) => None,
            };
            size.and_then(|size| size.to_u64()).is_some_and(|size| {
                size <= MAX_LITERAL_INT_SIZE && fits(int_value, size, segment.signed())
            })
        }
        Pattern::Float { .. }
        | Pattern::Variable { .. }
        | Pattern::BitArraySize(_)
        | Pattern::Assign { .. }
        | Pattern::Discard { .. }
        | Pattern::List { .. }
        | Pattern::Constructor { .. }
        | Pattern::Tuple { .. }
        | Pattern::BitArray { .. }
        | Pattern::StringPrefix { .. }
        | Pattern::Invalid { .. } => false,
    }
}

/// Whether `value` is an integer of `size` bits.
fn fits(value: &BigInt, size: u64, signed: bool) -> bool {
    if size == 0 {
        return *value == BigInt::ZERO;
    }
    let bound = BigInt::from(1) << size;
    if signed {
        let half = &bound >> 1;
        -&half <= *value && *value < half
    } else {
        BigInt::ZERO <= *value && *value < bound
    }
}
