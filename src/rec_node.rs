use std::fmt::Display;

use crate::node::{BinOp, GenericNode, UnOp};

pub enum RecNodeLink {
    Regular(Box<RecNode>),
    Loop,
}

pub type RecNode = GenericNode<RecNodeLink>;
pub type RecBinOp = BinOp<RecNodeLink>;
pub type RecUnOp = UnOp<RecNodeLink>;

impl<T> From<T> for RecNodeLink
where
    RecNode: From<T>,
{
    fn from(value: T) -> Self {
        Self::Regular(Box::new(value.into()))
    }
}
impl Display for RecNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericNode::Imm(_) | GenericNode::Var(_) | GenericNode::InternalVar(_) => {
                write!(f, "{}", self.structural_display())
            }
            GenericNode::BinOp(bin_op) => {
                write!(f, "({}) {} ({})", bin_op.lhs, bin_op.kind, bin_op.rhs)
            }
            GenericNode::UnOp(un_op) => write!(f, "{}({})", un_op.kind, un_op.operand),
        }
    }
}
impl Display for RecNodeLink {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RecNodeLink::Regular(node) => write!(f, "{}", node),
            RecNodeLink::Loop => write!(f, "..."),
        }
    }
}
