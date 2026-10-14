// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

//! Bit arrays. The `BitArray` of the prelude is the type `Bits` of the
//! runtime module `builtins/bit_array`, which also has their functions. Here
//! are the bit arrays of literals: a bit array whose segments are literals is
//! computed when compiling, and created once by the start function.

use super::*;
use crate::ast::{BitArraySegment, Endianness};
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

impl Generator<'_> {
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
