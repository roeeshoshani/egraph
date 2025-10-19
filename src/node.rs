use arrayvec::ArrayVec;
use derive_more::From;
use enum_display::EnumDisplay;
use enum_variant_accessors::{EnumAsVariant, EnumIsVariant};
use rsleigh::{SleighCtx, Vn, VnSpace};

use crate::{array_vec, utils::display_vn};

/// the max amount of fixed links in a node.
pub const NODE_MAX_FIXED_LINKS: usize = 3;

/// an array of all links of a node.
#[derive(Debug)]
pub struct NodeLinks<'a, L> {
    pub fixed: ArrayVec<&'a L, NODE_MAX_FIXED_LINKS>,
    pub dynamic: &'a [L],
}
impl<'a, L> NodeLinks<'a, L> {
    pub const EMPTY: Self = Self {
        fixed: ArrayVec::new_const(),
        dynamic: &[],
    };

    pub fn empty() -> Self {
        Self::EMPTY
    }

    pub fn len(&self) -> usize {
        self.fixed.len() + self.dynamic.len()
    }

    pub fn is_empty(&self) -> bool {
        self.fixed.is_empty() && self.dynamic.is_empty()
    }
    pub fn iter(&self) -> impl Iterator<Item = &'a L> + use<'_, 'a, L> {
        self.fixed.iter().copied().chain(self.dynamic.iter())
    }
    pub fn into_iter(self) -> impl Iterator<Item = &'a L> + use<'a, L> {
        self.fixed.into_iter().chain(self.dynamic.into_iter())
    }
    pub fn get(&self, index: usize) -> &'a L {
        if let Some(dynamic_index) = index.checked_sub(self.fixed.len()) {
            &self.dynamic[dynamic_index]
        } else {
            &self.fixed[index]
        }
    }
}
impl<'a, L> std::ops::Add for NodeLinks<'a, L> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let dynamic = match (self.dynamic.is_empty(), rhs.dynamic.is_empty()) {
            (true, true) => {
                // both are empty, so keep it empty
                &[]
            }
            (true, false) => {
                // use the rhs
                rhs.dynamic
            }
            (false, true) => {
                // use the lhs
                rhs.dynamic
            }
            (false, false) => {
                // both of them already have a dynamic part, so we can't combine them
                panic!("attempted to add two node links which both already have a dynamic part");
            }
        };

        // combine the fixed parts
        let mut combined_fixed = self.fixed;
        combined_fixed
            .try_extend_from_slice(&rhs.fixed[..])
            .unwrap();

        Self {
            fixed: combined_fixed,
            dynamic,
        }
    }
}

/// an immediate value.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Imm {
    pub val: u64,
    pub size_in_bytes: u32,
}

impl Imm {
    pub fn apply_un_op(kind: UnOpKind, x: Imm) -> Imm {
        let size = x.size_in_bytes;
        match size {
            1 => Self::apply_un_op_to_type::<u8>(kind, x.val),
            2 => Self::apply_un_op_to_type::<u16>(kind, x.val),
            4 => Self::apply_un_op_to_type::<u32>(kind, x.val),
            8 => Self::apply_un_op_to_type::<u64>(kind, x.val),
            _ => panic!("invalid immediate size: {}", size),
        }
    }
    fn apply_un_op_to_type<T: UnsignedImmType>(kind: UnOpKind, x: u64) -> Imm {
        let x = T::from_u64(x);
        let res_val = match kind {
            UnOpKind::IntNeg => x.my_wrapping_neg(),
            UnOpKind::Popcount => x.popcount(),
            UnOpKind::BoolNeg => T::from(!x.to_bool()),
            UnOpKind::IntNot => !x,
        };
        res_val.to_imm()
    }
    pub fn apply_bin_op(lhs: Imm, kind: BinOpKind, rhs: Imm) -> Imm {
        assert_eq!(lhs.size_in_bytes, rhs.size_in_bytes);
        let size = lhs.size_in_bytes;
        match size {
            1 => Self::apply_bin_op_to_type::<u8>(lhs.val, kind, rhs.val),
            2 => Self::apply_bin_op_to_type::<u16>(lhs.val, kind, rhs.val),
            4 => Self::apply_bin_op_to_type::<u32>(lhs.val, kind, rhs.val),
            8 => Self::apply_bin_op_to_type::<u64>(lhs.val, kind, rhs.val),
            _ => panic!("invalid immediate size: {}", size),
        }
    }
    fn apply_bin_op_to_type<T: UnsignedImmType>(lhs: u64, kind: BinOpKind, rhs: u64) -> Imm {
        let lhs = T::from_u64(lhs);
        let rhs = T::from_u64(rhs);

        let res_val = match kind {
            BinOpKind::IntAnd => lhs & rhs,
            BinOpKind::IntSub => lhs.my_wrapping_sub(rhs),
            BinOpKind::IntAdd => lhs.my_wrapping_add(rhs),
            BinOpKind::IntMul => lhs * rhs,
            BinOpKind::IntDiv => lhs / rhs,
            BinOpKind::IntRem => lhs % rhs,
            BinOpKind::IntOr => lhs | rhs,
            BinOpKind::IntXor => lhs ^ rhs,
            BinOpKind::IntNotEqual => T::from(lhs != rhs),
            BinOpKind::IntLeft => lhs << rhs,
            BinOpKind::IntRight => lhs >> rhs,
            BinOpKind::IntEqual => T::from(lhs == rhs),
            BinOpKind::IntSless => T::from(lhs.to_signed() < rhs.to_signed()),
            BinOpKind::IntSlessEqual => T::from(lhs.to_signed() <= rhs.to_signed()),
            BinOpKind::IntLess => T::from(lhs < rhs),
            BinOpKind::IntLessEqual => T::from(lhs <= rhs),
            BinOpKind::IntSborrow => T::from(lhs.sborrow(rhs)),
            BinOpKind::IntCarry => T::from(lhs.carry(rhs)),
            BinOpKind::IntScarry => T::from(lhs.scarry(rhs)),
            BinOpKind::BoolAnd => T::from(lhs.to_bool() && rhs.to_bool()),
            BinOpKind::BoolOr => T::from(lhs.to_bool() | rhs.to_bool()),
            BinOpKind::BoolXor => T::from(lhs.to_bool() ^ rhs.to_bool()),
            BinOpKind::IntSdiv => T::from_signed(lhs.to_signed() / rhs.to_signed()),
            BinOpKind::IntSrem => T::from_signed(lhs.to_signed() % rhs.to_signed()),
            BinOpKind::IntSright => T::from_signed(lhs.to_signed() >> rhs.to_signed()),
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
            size_in_bytes: Self::SIZE_IN_BYTES,
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
    + std::ops::Add<Self, Output = Self>
    + std::ops::Sub<Self, Output = Self>
    + std::ops::Mul<Self, Output = Self>
    + std::ops::Div<Self, Output = Self>
    + std::ops::Rem<Self, Output = Self>
    + std::ops::BitAnd<Self, Output = Self>
    + std::ops::BitOr<Self, Output = Self>
    + std::ops::BitXor<Self, Output = Self>
    + std::ops::Shl<Self, Output = Self>
    + std::ops::Shr<Self, Output = Self>
    + std::ops::Not<Output = Self>
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

/// an internal variable. exists to serve as placeholder for nodes who don't have a proper value.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct InternalVar(pub u64);

/// the kind of a binary operation.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, EnumDisplay)]
pub enum BinOpKind {
    #[display("&")]
    IntAnd,

    #[display("-")]
    IntSub,

    #[display("+")]
    IntAdd,

    #[display("*")]
    IntMul,

    #[display("u/")]
    IntDiv,

    #[display("s/")]
    IntSdiv,

    #[display("u%")]
    IntRem,

    #[display("s%")]
    IntSrem,

    #[display("|")]
    IntOr,

    #[display("^")]
    IntXor,

    #[display("==")]
    IntEqual,

    #[display("!=")]
    IntNotEqual,

    #[display("s<")]
    IntSless,

    #[display("s<=")]
    IntSlessEqual,

    #[display("u<")]
    IntLess,

    #[display("u<=")]
    IntLessEqual,

    #[display("<<")]
    IntLeft,

    #[display("u>>")]
    IntRight,

    #[display("s>>")]
    IntSright,

    #[display("sborrow")]
    IntSborrow,

    #[display("carry")]
    IntCarry,

    #[display("scarry")]
    IntScarry,

    #[display("&&")]
    BoolAnd,

    #[display("||")]
    BoolOr,

    #[display("^^")]
    BoolXor,
}

/// a binary operation. this is basically an operation with 2 operands, a lhs operand and a rhs operand.
/// an example of a binary operation is an addition operation (`x + y`).
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BinOp<L> {
    /// the kind of the binary operation (e.g addition, multiplication, etc)
    pub kind: BinOpKind,

    /// the lhs operand.
    pub lhs: L,

    /// the rhs operand.
    pub rhs: L,
}

impl<L> BinOp<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> BinOp<L2>
    where
        F: FnMut(&L) -> L2,
    {
        BinOp {
            kind: self.kind,
            lhs: conversion(&self.lhs),
            rhs: conversion(&self.rhs),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        NodeLinks {
            fixed: array_vec![&self.lhs, &self.rhs],
            dynamic: &[],
        }
    }
}

/// the kind of a unary operation.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, EnumDisplay)]
pub enum UnOpKind {
    /// integer arithmetic negation (as in `-x`). this is also called the 2s complement of the integer.
    #[display("-")]
    IntNeg,

    /// an integer not operation, which basically means inverting all the bits of the integer.
    #[display("~")]
    IntNot,

    /// popcount. evaluates to the amount of 1 bits in the integer.
    #[display("popcnt ")]
    Popcount,

    /// negates a boolean value.
    #[display("!")]
    BoolNeg,
}

/// a unary operation. this is basically an operation with only a single operand.
/// an example of a unary operation is a negation operation (`-x`).
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct UnOp<L> {
    /// the kind of the unary operation (e.g negation, bitwise not, etc)
    pub kind: UnOpKind,

    /// the operand to which the operation is applied.
    pub operand: L,
}

impl<L> UnOp<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> UnOp<L2>
    where
        F: FnMut(&L) -> L2,
    {
        UnOp {
            kind: self.kind,
            operand: conversion(&self.operand),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        NodeLinks {
            fixed: array_vec![&self.operand],
            dynamic: &[],
        }
    }
}

/// a node which represents an extension calculation, which increases the bit length of a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExtNode<L> {
    pub new_size_in_bytes: u32,
    pub kind: ExtKind,
    pub operand: L,
}

impl<L> ExtNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> ExtNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        ExtNode {
            new_size_in_bytes: self.new_size_in_bytes,
            kind: self.kind,
            operand: conversion(&self.operand),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        NodeLinks {
            fixed: array_vec![&self.operand],
            dynamic: &[],
        }
    }
}

/// a node which represents a truncation calculation, which shortens the bit length of a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TruncNode<L> {
    pub new_size_in_bytes: u32,
    pub operand: L,
}

impl<L> TruncNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> TruncNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        TruncNode {
            new_size_in_bytes: self.new_size_in_bytes,
            operand: conversion(&self.operand),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        NodeLinks {
            fixed: array_vec![&self.operand],
            dynamic: &[],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExtKind {
    Zero,
    Sign,
}

impl ExtKind {
    /// returns a letter which represents the kind of extension
    pub fn letter(&self) -> char {
        match self {
            ExtKind::Zero => 'Z',
            ExtKind::Sign => 'S',
        }
    }
}

/// a node which represents a memory load operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LoadNode<L> {
    /// the address space to load from
    pub space: VnSpace,

    /// the size of the loaded value
    pub size_in_bytes: u32,

    /// the address to load from
    pub address: L,

    pub cf_input: L,
}

impl<L> LoadNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> LoadNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        LoadNode {
            space: self.space,
            size_in_bytes: self.size_in_bytes,
            address: conversion(&self.address),
            cf_input: conversion(&self.cf_input),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        NodeLinks {
            fixed: array_vec![&self.address, &self.cf_input],
            dynamic: &[],
        }
    }
}

/// a node which represents a memory store operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StoreNode<L> {
    /// the address space to store to
    pub space: VnSpace,

    /// the address to store into
    pub address: L,

    /// the value to store
    pub value: L,

    pub cf_input: L,
}

impl<L> StoreNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> StoreNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        StoreNode {
            space: self.space,
            address: conversion(&self.address),
            value: conversion(&self.value),
            cf_input: conversion(&self.cf_input),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        NodeLinks {
            fixed: array_vec![&self.address, &self.value, &self.cf_input],
            dynamic: &[],
        }
    }
}

/// the final value of a vn at the exit of control flow.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExitVnNode<L> {
    pub vn: Vn,
    pub value: L,
    pub cf_input: L,
}

impl<L> ExitVnNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> ExitVnNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        ExitVnNode {
            vn: self.vn,
            value: conversion(&self.value),
            cf_input: conversion(&self.cf_input),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        NodeLinks {
            fixed: array_vec![&self.value, &self.cf_input],
            dynamic: &[],
        }
    }
}

/// the address of a machine insn
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MachineInsnAddr {
    /// the offset, inside the default code address space, of the machine insn
    pub code_space_off: u64,
}
impl std::ops::Add<u64> for MachineInsnAddr {
    type Output = Self;

    fn add(self, rhs: u64) -> Self::Output {
        Self {
            code_space_off: self.code_space_off + rhs,
        }
    }
}

/// the kind of a call node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallNodeKind<L> {
    /// a direct call, which is a call to a function with a statically known address.
    Direct(MachineInsnAddr),

    /// an indirect call, which is a call to a function whose address is determined at runtime.
    Indirect(L),
}

impl<L> CallNodeKind<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> CallNodeKind<L2>
    where
        F: FnMut(&L) -> L2,
    {
        match self {
            CallNodeKind::Direct(addr) => CallNodeKind::Direct(*addr),
            CallNodeKind::Indirect(l) => CallNodeKind::Indirect(conversion(l)),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        match self {
            CallNodeKind::Direct(_) => NodeLinks::empty(),
            CallNodeKind::Indirect(l) => NodeLinks {
                fixed: array_vec![l],
                dynamic: &[],
            },
        }
    }
}

/// a node which represents a call to another function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallNode<L> {
    pub cf_input: L,
    pub kind: CallNodeKind<L>,
}

impl<L> CallNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> CallNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        CallNode {
            cf_input: conversion(&self.cf_input),
            kind: self.kind.convert_links(|x| conversion(x)),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        match &self.kind {
            CallNodeKind::Direct(_) => NodeLinks {
                fixed: array_vec![&self.cf_input],
                dynamic: &[],
            },
            CallNodeKind::Indirect(tgt) => NodeLinks {
                fixed: array_vec![&self.cf_input, tgt],
                dynamic: &[],
            },
        }
    }
}

/// the call target of a call node
pub enum CallNodeTarget<L> {
    Direct(MachineInsnAddr),
    Indirect(L),
}

impl<L> CallNodeTarget<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> CallNodeTarget<L2>
    where
        F: FnMut(&L) -> L2,
    {
        match self {
            CallNodeTarget::Direct(addr) => CallNodeTarget::Direct(*addr),
            CallNodeTarget::Indirect(l) => CallNodeTarget::Indirect(conversion(l)),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        match self {
            CallNodeTarget::Direct(_) => NodeLinks::empty(),
            CallNodeTarget::Indirect(l) => NodeLinks {
                fixed: array_vec![l],
                dynamic: &[],
            },
        }
    }
}

/// the value of a vn when calling a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallVnNode<L> {
    pub vn: Vn,
    pub value: L,
    pub call_node: L,
}

impl<L> CallVnNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> CallVnNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        CallVnNode {
            vn: self.vn,
            value: conversion(&self.value),
            call_node: conversion(&self.call_node),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        NodeLinks {
            fixed: array_vec![&self.value, &self.call_node],
            dynamic: &[],
        }
    }
}

/// a value which may be clobbered by a function call.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClobberNode<L> {
    pub vn: Vn,
    pub original_vn_value: L,
    pub call_node: L,
}

impl<L> ClobberNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> ClobberNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        ClobberNode {
            vn: self.vn,
            original_vn_value: conversion(&self.original_vn_value),
            call_node: conversion(&self.call_node),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        NodeLinks {
            fixed: array_vec![&self.original_vn_value, &self.call_node],
            dynamic: &[],
        }
    }
}

/// a conditional if operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IfNode<L> {
    pub cond: L,
    pub cf_input: L,
}

impl<L> IfNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> IfNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        IfNode {
            cond: conversion(&self.cond),
            cf_input: conversion(&self.cf_input),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        NodeLinks {
            fixed: array_vec![&self.cond, &self.cf_input],
            dynamic: &[],
        }
    }
}

/// one of the cases of an if condition
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IfCase {
    /// the "true" case of an if operation.
    True,

    /// the "false" case of an if operation.
    False,
}

/// a node to represent one of the cases of an if condition
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IfCaseNode<L> {
    pub case: IfCase,
    pub cf_input: L,
}

impl<L> IfCaseNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> IfCaseNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        IfCaseNode {
            case: self.case,
            cf_input: conversion(&self.cf_input),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        NodeLinks {
            fixed: array_vec![&self.cf_input],
            dynamic: &[],
        }
    }
}

/// the operation number of a sleigh callother operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallOtherOpNum(pub u64);

/// a phi node which merges multiple values coming from different paths in the code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PhiNode<L> {
    pub inputs: Vec<L>,
    pub region_node: L,
}

impl<L> PhiNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> PhiNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        PhiNode {
            inputs: self.inputs.iter().map(|l| conversion(l)).collect(),
            region_node: conversion(&self.region_node),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        NodeLinks {
            fixed: array_vec![&self.region_node],
            dynamic: &self.inputs[..],
        }
    }
}

/// a region node which merges multiple control flow paths.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RegionNode<L> {
    pub cf_inputs: Vec<L>,
}

impl<L> RegionNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> RegionNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        RegionNode {
            cf_inputs: self.cf_inputs.iter().map(|l| conversion(l)).collect(),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        NodeLinks {
            fixed: array_vec![],
            dynamic: &self.cf_inputs[..],
        }
    }
}

/// an exit of control flow.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExitNode<L> {
    pub cf_input: L,
}

impl<L> ExitNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> ExitNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        ExitNode {
            cf_input: conversion(&self.cf_input),
        }
    }

    pub fn links(&self) -> NodeLinks<'_, L> {
        NodeLinks {
            fixed: array_vec![&self.cf_input],
            dynamic: &[],
        }
    }
}

/// a node type that is generic over the link type. the link type determines how the node points to other nodes that it uses as inputs.
#[derive(Debug, Clone, From, Hash, PartialEq, Eq, EnumIsVariant, EnumAsVariant)]
pub enum GenericNode<L> {
    /// an immediate value.
    Imm(Imm),

    /// an internal variable. exists to serve as placeholder for nodes who don't have a proper value.
    InternalVar(InternalVar),

    /// the initial value of a varnode
    Vn(Vn),

    /// a binary operation.
    BinOp(BinOp<L>),

    /// a unary operation.
    UnOp(UnOp<L>),

    /// an extension calculation.
    Ext(ExtNode<L>),

    /// a truncation calculation.
    Trunc(TruncNode<L>),

    /// a memory load operation
    Load(LoadNode<L>),

    /// a memory store operation
    Store(StoreNode<L>),

    /// the final value of a vn at the exit of control flow.
    ExitVn(ExitVnNode<L>),

    /// a call to another function.
    Call(CallNode<L>),

    /// the value of a vn when calling a function.
    CallVn(CallVnNode<L>),

    /// a value which may be clobbered by a function call.
    Clobber(ClobberNode<L>),

    /// a sleigh callother operation.
    CallOther(CallOtherOpNum),

    /// a conditional if operation.
    If(IfNode<L>),

    /// one of the cases of an if condition
    IfCase(IfCaseNode<L>),

    /// a phi node which merges multiple values coming from different paths in the code.
    Phi(PhiNode<L>),

    /// a region node which merges multiple control flow paths.
    Region(RegionNode<L>),

    /// the entrypoint of control flow.
    Entry,

    /// an exit of control flow.
    Exit(ExitNode<L>),
}

impl<L> GenericNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> GenericNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        match self {
            GenericNode::Imm(imm) => GenericNode::Imm(*imm),
            GenericNode::InternalVar(v) => GenericNode::InternalVar(*v),
            GenericNode::Vn(vn) => GenericNode::Vn(*vn),

            GenericNode::BinOp(b) => GenericNode::BinOp(b.convert_links(|x| conversion(x))),
            GenericNode::UnOp(u) => GenericNode::UnOp(u.convert_links(|x| conversion(x))),
            GenericNode::Ext(e) => GenericNode::Ext(e.convert_links(|x| conversion(x))),
            GenericNode::Trunc(t) => GenericNode::Trunc(t.convert_links(|x| conversion(x))),
            GenericNode::Load(l) => GenericNode::Load(l.convert_links(|x| conversion(x))),
            GenericNode::Store(s) => GenericNode::Store(s.convert_links(|x| conversion(x))),
            GenericNode::ExitVn(ev) => GenericNode::ExitVn(ev.convert_links(|x| conversion(x))),
            GenericNode::Call(c) => GenericNode::Call(c.convert_links(|x| conversion(x))),
            GenericNode::CallVn(cv) => GenericNode::CallVn(cv.convert_links(|x| conversion(x))),
            GenericNode::Clobber(cl) => GenericNode::Clobber(cl.convert_links(|x| conversion(x))),

            GenericNode::CallOther(co) => GenericNode::CallOther(*co),

            GenericNode::If(i) => GenericNode::If(i.convert_links(|x| conversion(x))),
            GenericNode::IfCase(ic) => GenericNode::IfCase(ic.convert_links(|x| conversion(x))),
            GenericNode::Phi(p) => GenericNode::Phi(p.convert_links(|x| conversion(x))),
            GenericNode::Region(r) => GenericNode::Region(r.convert_links(|x| conversion(x))),

            GenericNode::Entry => GenericNode::Entry,
            GenericNode::Exit(e) => GenericNode::Exit(e.convert_links(|x| conversion(x))),
        }
    }

    /// returns an array of all links of this node.
    pub fn links(&self) -> NodeLinks<'_, L> {
        match self {
            GenericNode::Imm(_) => NodeLinks::empty(),
            GenericNode::InternalVar(_) => NodeLinks::empty(),
            GenericNode::Vn(_) => NodeLinks::empty(),
            GenericNode::BinOp(x) => x.links(),
            GenericNode::UnOp(x) => x.links(),
            GenericNode::Ext(x) => x.links(),
            GenericNode::Trunc(x) => x.links(),
            GenericNode::Load(x) => x.links(),
            GenericNode::Store(x) => x.links(),
            GenericNode::ExitVn(x) => x.links(),
            GenericNode::Call(x) => x.links(),
            GenericNode::CallVn(x) => x.links(),
            GenericNode::Clobber(x) => x.links(),
            GenericNode::CallOther(_) => NodeLinks::empty(),
            GenericNode::If(x) => x.links(),
            GenericNode::IfCase(x) => x.links(),
            GenericNode::Phi(x) => x.links(),
            GenericNode::Region(x) => x.links(),
            GenericNode::Entry => NodeLinks::empty(),
            GenericNode::Exit(x) => x.links(),
        }
    }

    /// returns a string which represents a human readable formatting of this node's structure. the returned string
    /// contains no information about the node's links, only the structure itself, which is everything other than the links.
    pub fn structural_display(&self, sleigh_ctx: &SleighCtx) -> String {
        match self {
            GenericNode::InternalVar(internal_var) => format!("Internal Var {}", internal_var.0),
            GenericNode::UnOp(un_op) => un_op.kind.to_string(),
            GenericNode::BinOp(bin_op) => bin_op.kind.to_string(),
            GenericNode::Ext(ext_node) => {
                format!(
                    "{}ext:u{}",
                    ext_node.kind.letter(),
                    ext_node.new_size_in_bytes * 8
                )
            }
            GenericNode::Trunc(trunc_node) => {
                format!("Trunc:u{}", trunc_node.new_size_in_bytes * 8)
            }
            GenericNode::Vn(vn) => display_vn(*vn, sleigh_ctx),
            GenericNode::Imm(imm_node) => {
                format!("0x{:x}:u{}", imm_node.val, imm_node.size_in_bytes * 8)
            }
            GenericNode::Load(load_node) => {
                let load_space_info = sleigh_ctx.space_info(load_node.space);
                format!(
                    "Load[{}]:u{}",
                    load_space_info.name().unwrap(),
                    load_node.size_in_bytes * 8
                )
            }
            GenericNode::Store(store_node) => {
                let store_space_info = sleigh_ctx.space_info(store_node.space);
                format!("Store[{}]", store_space_info.name().unwrap())
            }
            GenericNode::ExitVn(exit_vn_node) => {
                format!("Exit {}", display_vn(exit_vn_node.vn, sleigh_ctx))
            }
            GenericNode::Call(call_node) => match call_node.kind {
                CallNodeKind::Direct(machine_insn_addr) => {
                    format!("Call 0x{:x}", machine_insn_addr.code_space_off)
                }
                CallNodeKind::Indirect(_) => "Call Indirect".into(),
            },
            GenericNode::CallOther(op_num) => {
                format!("CallOther {}", op_num.0)
            }
            GenericNode::Clobber(clobber_node) => {
                format!("Clobber {}", display_vn(clobber_node.vn, sleigh_ctx))
            }
            GenericNode::IfCase(if_case_node) => format!("{:?}", if_case_node.case),
            GenericNode::CallVn(call_vn_node) => {
                format!("Call Vn {}", display_vn(call_vn_node.vn, sleigh_ctx))
            }
            GenericNode::If(_) => "If".into(),
            GenericNode::Phi(_) => "Phi".into(),
            GenericNode::Region(_) => "Region".into(),
            GenericNode::Entry => "Entry".into(),
            GenericNode::Exit(_) => "Exit".into(),
        }
    }
}
