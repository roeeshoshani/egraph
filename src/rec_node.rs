use std::fmt::Display;

use crate::node::{BinOp, BinOpKind, GenericNode, UnOp, UnOpKind};

/// the link of a recursive node.
///
/// we can't use a type alias here since type aliases can't be recursive, so we must create a new data type to break the recursion.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecLink(pub Box<RecNode>);

/// a recursive node, which is a node whose link value directly owns the linked-to node.
pub type RecNode = GenericNode<RecLink>;

/// a bin op which uses recursive nodes.
pub type RecBinOp = BinOp<RecLink>;

/// a un op which uses recursive nodes.
pub type RecUnOp = UnOp<RecLink>;

// convenience, convert a recursive node value directly to a recursive link value.
impl<T> From<T> for RecLink
where
    RecNode: From<T>,
{
    fn from(value: T) -> Self {
        Self(Box::new(value.into()))
    }
}

pub trait ToRecLink {
    fn to_rec_link(self) -> RecLink;
}
impl<T> ToRecLink for T
where
    RecNode: From<T>,
{
    fn to_rec_link(self) -> RecLink {
        self.into()
    }
}

impl Display for RecNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericNode::Imm(_) => {
                write!(f, "{}", self.structural_display())
            }
            GenericNode::BinOp(bin_op) => {
                write!(f, "({}) {} ({})", bin_op.lhs, bin_op.kind, bin_op.rhs)
            }
            GenericNode::UnOp(un_op) => write!(f, "{}({})", un_op.kind, un_op.operand),
            GenericNode::VnInitialValue(vn) => todo!(),
            GenericNode::TupleGet(tuple_get) => todo!(),
            GenericNode::TupleBuild(tuple_build) => todo!(),
            GenericNode::FnParams(fn_params) => todo!(),
            GenericNode::Function(function) => todo!(),
            GenericNode::FnCall(fn_call) => todo!(),
            GenericNode::LoopInputs(loop_inputs) => todo!(),
            GenericNode::Loop(_) => todo!(),
            GenericNode::Switch(switch) => todo!(),
        }
    }
}
impl Display for RecLink {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl RecLink {
    /// applies the bin op with the given kind to this link as the lhs, and with the given rhs link as the rhs.
    pub fn apply_bin_op(self, kind: BinOpKind, rhs: Self) -> Self {
        Self(Box::new(GenericNode::BinOp(BinOp {
            kind,
            lhs: self,
            rhs: rhs,
        })))
    }

    /// applies the un op with the given kind to this link as the operand.
    pub fn apply_un_op(self, kind: UnOpKind) -> Self {
        Self(Box::new(GenericNode::UnOp(UnOp {
            kind,
            operand: self,
        })))
    }
}

macro_rules! impl_binop_for_rec_link {
    ($trait: ty, $trait_fn_name: ident, $bin_op_kind: expr) => {
        impl $trait for RecLink {
            type Output = Self;

            fn $trait_fn_name(self, rhs: Self) -> Self::Output {
                self.apply_bin_op($bin_op_kind, rhs)
            }
        }
    };
}

impl_binop_for_rec_link! {std::ops::Add, add, BinOpKind::Add}
impl_binop_for_rec_link! {std::ops::Mul, mul, BinOpKind::Mul}
impl_binop_for_rec_link! {std::ops::BitAnd, bitand, BinOpKind::BitAnd}
impl_binop_for_rec_link! {std::ops::BitOr, bitor, BinOpKind::BitOr}

macro_rules! impl_unop_for_rec_link {
    ($trait: ty, $trait_fn_name: ident, $un_op_kind: expr) => {
        impl $trait for RecLink {
            type Output = Self;

            fn $trait_fn_name(self) -> Self::Output {
                self.apply_un_op($un_op_kind)
            }
        }
    };
}

impl_unop_for_rec_link! {std::ops::Neg, neg,UnOpKind::Neg}
impl_unop_for_rec_link! {std::ops::Not, not,UnOpKind::BitNot}
