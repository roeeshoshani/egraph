use arrayvec::ArrayVec;
use derive_more::From;
use enum_display::EnumDisplay;
use enum_variant_accessors::{EnumAsVariant, EnumIsVariant};

use crate::array_vec;

/// the max amount of links in a node.
pub const NODE_MAX_LINKS: usize = 2;

/// an array of all links of a node.
pub type NodeLinks<'a, L> = ArrayVec<&'a L, NODE_MAX_LINKS>;

/// an immediate value.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Imm(pub u64);

/// a variable.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Var(pub u64);

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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Backlink<L>(pub L);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LoopVar<L> {
    pub initial_value: L,

    pub next_iteration_value: Backlink<L>,
}

/// a node type that is generic over the link type. the link type determines how the node points to other nodes that it uses as inputs.
#[derive(Debug, Clone, From, Hash, PartialEq, Eq, EnumIsVariant, EnumAsVariant)]
pub enum GenericNode<L> {
    /// an immediate value.
    Imm(Imm),

    /// a variable
    Var(Var),

    /// a binary operation.
    BinOp(BinOp<L>),

    /// a unary operation.
    UnOp(UnOp<L>),

    LoopVar(LoopVar<L>),
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
            GenericNode::Var(var) => GenericNode::Var(*var),
            GenericNode::BinOp(BinOp { kind, lhs, rhs }) => GenericNode::BinOp(BinOp {
                kind: *kind,
                lhs: conversion(lhs),
                rhs: conversion(rhs),
            }),
            GenericNode::UnOp(UnOp { kind, operand }) => GenericNode::UnOp(UnOp {
                kind: *kind,
                operand: conversion(operand),
            }),
            GenericNode::LoopVar(LoopVar {
                initial_value,
                next_iteration_value,
            }) => GenericNode::LoopVar(LoopVar {
                initial_value: conversion(initial_value),
                next_iteration_value: Backlink(conversion(&next_iteration_value.0)),
            }),
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
            GenericNode::BinOp(bin_op) => bin_op.kind.to_string(),
            GenericNode::UnOp(un_op) => un_op.kind.to_string(),
            GenericNode::LoopVar(_) => format!("loopvar"),
        }
    }
}
