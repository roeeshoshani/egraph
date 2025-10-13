use arrayvec::ArrayVec;
use derive_more::From;
use enum_display::EnumDisplay;

use crate::array_vec;

pub const NODE_MAX_LINKS: usize = 2;
pub type NodeLinks<'a, L> = ArrayVec<&'a L, NODE_MAX_LINKS>;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Imm(pub u64);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Var(pub u64);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, EnumDisplay)]
pub enum BinOpKind {
    #[display("+")]
    Add,
    #[display("*")]
    Mul,
    #[display("&")]
    And,
    #[display("|")]
    Or,
}
impl BinOpKind {
    pub fn apply_to_imms(&self, lhs: Imm, rhs: Imm) -> u64 {
        match self {
            BinOpKind::Add => lhs.0.wrapping_add(rhs.0),
            BinOpKind::Mul => lhs.0.wrapping_mul(rhs.0),
            BinOpKind::And => lhs.0 & rhs.0,
            BinOpKind::Or => lhs.0 | rhs.0,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BinOp<L> {
    pub kind: BinOpKind,
    pub lhs: L,
    pub rhs: L,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, EnumDisplay)]
pub enum UnOpKind {
    #[display("-")]
    Neg,
    #[display("!")]
    Not,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct UnOp<L> {
    pub kind: UnOpKind,
    pub operand: L,
}

#[derive(Debug, Clone, From, Hash, PartialEq, Eq)]
pub enum GenericNode<L> {
    Imm(Imm),
    Var(Var),
    BinOp(BinOp<L>),
    UnOp(UnOp<L>),
}
impl<L> From<u64> for GenericNode<L> {
    fn from(value: u64) -> Self {
        Imm(value).into()
    }
}
impl<L> GenericNode<L> {
    pub fn convert_link<L2, F>(&self, mut conversion: F) -> GenericNode<L2>
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
        }
    }
    pub fn links(&self) -> NodeLinks<'_, L> {
        match self {
            GenericNode::BinOp(bin_op) => array_vec![&bin_op.lhs, &bin_op.rhs],
            GenericNode::UnOp(un_op) => array_vec![&un_op.operand],
            _ => array_vec![],
        }
    }
    pub fn structural_display(&self) -> String {
        match self {
            GenericNode::Imm(imm) => format!("0x{:x}", imm.0),
            GenericNode::Var(var) => format!("var{}", var.0),
            GenericNode::BinOp(bin_op) => bin_op.kind.to_string(),
            GenericNode::UnOp(un_op) => un_op.kind.to_string(),
        }
    }
}
