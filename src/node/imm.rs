use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Shl, Shr, Sub};

use crate::node::{BinOpKind, UnOpKind, ValueSize};

/// a node in the ang which represents an immediate value
/// an immediate value.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Imm {
    pub val: u64,
    pub size: ValueSize,
}
impl Imm {
    pub fn u64(x: u64) -> Self {
        Self {
            val: x,
            size: ValueSize::U64,
        }
    }
    pub fn u32(x: u32) -> Self {
        Self {
            val: x as u64,
            size: ValueSize::U32,
        }
    }
    pub fn u16(x: u16) -> Self {
        Self {
            val: x as u64,
            size: ValueSize::U16,
        }
    }
    pub fn u8(x: u8) -> Self {
        Self {
            val: x as u64,
            size: ValueSize::U8,
        }
    }

    pub fn apply_un_op(kind: UnOpKind, x: Imm) -> Imm {
        match x.size.bytes {
            1 => Self::apply_un_op_to_type::<u8>(kind, x.val),
            2 => Self::apply_un_op_to_type::<u16>(kind, x.val),
            4 => Self::apply_un_op_to_type::<u32>(kind, x.val),
            8 => Self::apply_un_op_to_type::<u64>(kind, x.val),
            _ => panic!("invalid immediate size: {:?}", x.size),
        }
    }
    fn apply_un_op_to_type<T: UnsignedImmType>(kind: UnOpKind, x: u64) -> Imm {
        let x = T::from_u64(x);
        let res_val = match kind {
            UnOpKind::Neg => x.my_wrapping_neg(),
            UnOpKind::BitNot => !x,
        };
        res_val.to_imm()
    }
    pub fn apply_bin_op(lhs: Imm, kind: BinOpKind, rhs: Imm) -> Imm {
        assert_eq!(lhs.size, rhs.size);
        let size = lhs.size;
        match size.bytes {
            1 => Self::apply_bin_op_to_type::<u8>(lhs.val, kind, rhs.val),
            2 => Self::apply_bin_op_to_type::<u16>(lhs.val, kind, rhs.val),
            4 => Self::apply_bin_op_to_type::<u32>(lhs.val, kind, rhs.val),
            8 => Self::apply_bin_op_to_type::<u64>(lhs.val, kind, rhs.val),
            _ => panic!("invalid immediate size: {:?}", size),
        }
    }
    fn apply_bin_op_to_type<T: UnsignedImmType>(lhs: u64, kind: BinOpKind, rhs: u64) -> Imm {
        let lhs = T::from_u64(lhs);
        let rhs = T::from_u64(rhs);

        let res_val = match kind {
            BinOpKind::Add => lhs.my_wrapping_add(rhs),
            BinOpKind::Mul => lhs * rhs,
            BinOpKind::BitAnd => lhs & rhs,
            BinOpKind::BitOr => lhs | rhs,
            BinOpKind::UnsignedLess => T::from(lhs < rhs),
        };
        res_val.to_imm()
    }
}

trait UnsignedImmType: ImmType + Into<u64> {
    type Signed: SignedImmType;

    const SIZE_IN_BYTES: u32;

    const ZERO: Self;

    fn to_signed(&self) -> Self::Signed;
    fn from_signed(signed: Self::Signed) -> Self;
    fn from_u64(x: u64) -> Self;
    fn carry(&self, rhs: Self) -> bool;
    fn popcount(&self) -> Self;
    fn my_wrapping_add(&self, rhs: Self) -> Self;
    fn my_wrapping_sub(&self, rhs: Self) -> Self;
    fn my_wrapping_neg(&self) -> Self;

    fn to_imm(self) -> Imm {
        Imm {
            val: self.into(),
            size: ValueSize {
                bytes: Self::SIZE_IN_BYTES,
            },
        }
    }
    fn to_bool(&self) -> bool {
        *self != Self::ZERO
    }
    fn sborrow(&self, rhs: Self) -> bool {
        self.to_signed().borrow(rhs.to_signed())
    }
    fn scarry(&self, rhs: Self) -> bool {
        self.to_signed().carry(rhs.to_signed())
    }
}

trait SignedImmType: ImmType {
    fn borrow(&self, rhs: Self) -> bool;
    fn carry(&self, rhs: Self) -> bool;
}

trait ImmType:
    Sized
    + Add<Self, Output = Self>
    + Sub<Self, Output = Self>
    + Mul<Self, Output = Self>
    + Div<Self, Output = Self>
    + Rem<Self, Output = Self>
    + BitAnd<Self, Output = Self>
    + BitOr<Self, Output = Self>
    + BitXor<Self, Output = Self>
    + Shl<Self, Output = Self>
    + Shr<Self, Output = Self>
    + Not<Output = Self>
    + PartialEq<Self>
    + Eq
    + PartialEq<Self>
    + Eq
    + PartialOrd<Self>
    + Ord
    + From<bool>
{
}
macro_rules! impl_imm_type {
    {$($t: ty),+} => {
        $(
        impl ImmType for $t {}
        )+
    };
}
impl_imm_type! {u8,u16,u32,u64,i8,i16,i32,i64}

macro_rules! impl_singed_unsigned_pair {
    {$signed: ty, $unsigned: ty, $size_in_bytes: expr} => {
        impl SignedImmType for $signed {
            fn borrow(&self, rhs: Self) -> bool {
                self.checked_sub(rhs).is_none()
            }
            fn carry(&self, rhs: Self) -> bool {
                self.checked_add(rhs).is_none()
            }
        }
        impl UnsignedImmType for $unsigned {
            type Signed = $signed;

            const SIZE_IN_BYTES: u32 = $size_in_bytes;

            const ZERO: Self = 0;

            fn to_signed(&self) -> Self::Signed {
                *self as $signed
            }
            fn from_signed(signed: Self::Signed) -> Self {
                signed as $unsigned
            }
            fn from_u64(x: u64) -> Self {
                x as $unsigned
            }
            fn carry(&self, rhs: Self) -> bool {
                self.checked_add(rhs).is_none()
            }
            fn popcount(&self) -> Self {
                self.count_ones() as $unsigned
            }
            fn my_wrapping_add(&self, rhs: Self) -> Self {
                self.wrapping_add(rhs)
            }
            fn my_wrapping_sub(&self, rhs: Self) -> Self {
                self.wrapping_sub(rhs)
            }
            fn my_wrapping_neg(&self) -> Self {
                self.wrapping_neg()
            }
        }
    };
}
impl_singed_unsigned_pair! {i8, u8, 1}
impl_singed_unsigned_pair! {i16, u16, 2}
impl_singed_unsigned_pair! {i32, u32, 4}
impl_singed_unsigned_pair! {i64, u64, 8}
