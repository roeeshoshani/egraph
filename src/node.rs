use arrayvec::ArrayVec;
use derive_more::From;
use enum_display::EnumDisplay;
use enum_variant_accessors::{EnumAsVariant, EnumIsVariant};
use rsleigh::{Vn, VnSpace};

use crate::array_vec;

/// the max amount of links in a node.
pub const NODE_MAX_LINKS: usize = 2;

/// an array of all links of a node.
pub type NodeLinks<'a, L> = ArrayVec<&'a L, NODE_MAX_LINKS>;

/// an immediate value.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Imm(pub u64);

/// an internal variable. exists to serve as placeholder for nodes who don't have a proper value.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct InternalVar(pub u64);

/// the kind of a binary operation.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, EnumDisplay)]
pub enum BinOpKind {
    #[display("+")]
    Add,
    #[display("*")]
    Mul,
    #[display("&")]
    BitAnd,
    #[display("|")]
    BitOr,
}

impl BinOpKind {
    /// applies this binary operation to the given immediates.
    pub fn apply_to_imms(&self, lhs: Imm, rhs: Imm) -> Imm {
        let res = match self {
            BinOpKind::Add => lhs.0.wrapping_add(rhs.0),
            BinOpKind::Mul => lhs.0.wrapping_mul(rhs.0),
            BinOpKind::BitAnd => lhs.0 & rhs.0,
            BinOpKind::BitOr => lhs.0 | rhs.0,
        };
        Imm(res)
    }
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

/// the kind of a unary operation.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, EnumDisplay)]
pub enum UnOpKind {
    /// integer arithmetic negation (as in `-x`). this is also called the 2s complement of the integer.
    #[display("-")]
    Neg,

    /// integer not operation, which basically means inverting all the bits of the integer.
    #[display("!")]
    BitNot,
}
impl UnOpKind {
    /// applies this unary operation to the given immediate.
    pub fn apply_to_imm(&self, operand: Imm) -> Imm {
        let res = match self {
            UnOpKind::Neg => operand.0.wrapping_neg(),
            UnOpKind::BitNot => !operand.0,
        };
        Imm(res)
    }
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

/// a node which represents an extension calculation, which increases the bit length of a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExtNode<L> {
    pub new_size: u32,
    pub kind: ExtKind,
    pub operand: L,
}

/// a node which represents a truncation calculation, which shortens the bit length of a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TruncNode<L> {
    pub new_size: u32,
    pub operand: L,
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
    pub size: u32,

    /// the address to load from
    pub address: L,

    pub cf_input: L,
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

/// the final value of a vn at the exit of control flow.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExitVnNode<L> {
    pub vn: Vn,
    pub value: L,
    pub cf_input: L,
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

/// a node which represents a call to another function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallNode<L> {
    pub cf_input: L,
    pub kind: CallNodeKind<L>,
}

/// the call target of a call node
pub enum CallNodeTarget<L> {
    Direct(MachineInsnAddr),
    Indirect(L),
}

/// the value of a vn when calling a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallVnNode<L> {
    pub vn: Vn,
    pub value: L,
    pub call_node: L,
}

/// a value which may be clobbered by a function call.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClobberNode<L> {
    pub vn: Vn,
    pub original_vn_value: L,
    pub call_node: L,
}

/// a conditional if operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IfNode<L> {
    pub cond: L,
    pub cf_input: L,
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

/// the operation number of a sleigh callother operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallOtherOpNum(pub u64);

/// a phi node which merges multiple values coming from different paths in the code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PhiNode<L> {
    pub inputs: Vec<L>,
    pub region_node: L,
}

/// a region node which merges multiple control flow paths.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RegionNode<L> {
    pub cf_inputs: Vec<L>,
}

/// an exit of control flow.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExitNode<L> {
    pub cf_input: L,
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

// convert from an integer value to an immediate node.
impl<L> From<u64> for GenericNode<L> {
    fn from(value: u64) -> Self {
        Imm(value).into()
    }
}

impl<L> GenericNode<L> {
    /// converts the links of the given node using the given conversion function, generating a new node with the converted link values.
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> GenericNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        match self {
            GenericNode::Imm(imm) => GenericNode::Imm(*imm),
            GenericNode::InternalVar(internal_var) => GenericNode::InternalVar(*internal_var),
            GenericNode::Vn(vn) => GenericNode::Vn(*vn),
            GenericNode::BinOp(BinOp { kind, lhs, rhs }) => GenericNode::BinOp(BinOp {
                kind: *kind,
                lhs: conversion(lhs),
                rhs: conversion(rhs),
            }),
            GenericNode::UnOp(UnOp { kind, operand }) => GenericNode::UnOp(UnOp {
                kind: *kind,
                operand: conversion(operand),
            }),
            GenericNode::Ext(ExtNode {
                new_size,
                kind,
                operand,
            }) => GenericNode::Ext(ExtNode {
                new_size: *new_size,
                kind: *kind,
                operand: conversion(operand),
            }),
            GenericNode::Trunc(TruncNode { new_size, operand }) => GenericNode::Trunc(TruncNode {
                new_size: *new_size,
                operand: conversion(operand),
            }),
            GenericNode::Load(LoadNode {
                space,
                size,
                address,
                cf_input,
            }) => GenericNode::Load(LoadNode {
                space: *space,
                size: *size,
                address: conversion(address),
                cf_input: conversion(cf_input),
            }),
            GenericNode::Store(StoreNode {
                space,
                address,
                value,
                cf_input,
            }) => GenericNode::Store(StoreNode {
                space: *space,
                address: conversion(address),
                value: conversion(value),
                cf_input: conversion(cf_input),
            }),
            GenericNode::ExitVn(ExitVnNode {
                vn,
                value,
                cf_input,
            }) => GenericNode::ExitVn(ExitVnNode {
                vn: *vn,
                value: conversion(value),
                cf_input: conversion(cf_input),
            }),
            GenericNode::Call(CallNode { cf_input, kind }) => GenericNode::Call(CallNode {
                cf_input: conversion(cf_input),
                kind: *kind,
            }),
            GenericNode::CallVn(call_vn_node) => GenericNode::CallVn(call_vn_node),
            GenericNode::Clobber(clobber_node) => GenericNode::Clobber(clobber_node),
            GenericNode::CallOther(call_other_op_num) => GenericNode::CallOther(call_other_op_num),
            GenericNode::If(if_node) => GenericNode::If(if_node),
            GenericNode::IfCase(if_case_node) => GenericNode::IfCase(if_case_node),
            GenericNode::Phi(phi_node) => GenericNode::Phi(phi_node),
            GenericNode::Region(region_node) => GenericNode::Region(region_node),
            GenericNode::Entry => GenericNode::Entry,
            GenericNode::Exit(exit_node) => GenericNode::Exit(exit_node),
        }
    }

    /// returns an array of all links of this node.
    pub fn links(&self) -> NodeLinks<'_, L> {
        match self {
            GenericNode::BinOp(bin_op) => array_vec![&bin_op.lhs, &bin_op.rhs],
            GenericNode::UnOp(un_op) => array_vec![&un_op.operand],
            _ => array_vec![],
        }
    }

    /// returns a string which represents a human readable formatting of this node's structure. the returned string
    /// contains no information about the node's links, only the structure itself, which is everything other than the links.
    pub fn structural_display(&self) -> String {
        match self {
            GenericNode::Imm(imm) => format!("0x{:x}", imm.0),
            GenericNode::Var(var) => format!("var{}", var.0),
            GenericNode::InternalVar(internal_var) => format!("internal_var{}", internal_var.0),
            GenericNode::BinOp(bin_op) => bin_op.kind.to_string(),
            GenericNode::UnOp(un_op) => un_op.kind.to_string(),
        }
    }
}
