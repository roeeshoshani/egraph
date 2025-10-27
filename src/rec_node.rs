use std::fmt::Display;

use crate::node::{BinOp, GenericNode, UnOp};

/// the link of a recursive node.
///
/// we can't use a type alias here since type aliases can't be recursive, so we must create a new data type to break the recursion.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecNodeLink(pub Box<RecNode>);

/// a recursive node, which is a node whose link value directly owns the linked-to node.
pub type RecNode = GenericNode<RecNodeLink>;

/// a bin op which uses recursive nodes.
pub type RecBinOp = BinOp<RecNodeLink>;

/// a un op which uses recursive nodes.
pub type RecUnOp = UnOp<RecNodeLink>;

// convenience, convert a recursive node value directly to a recursive link value.
impl<T> From<T> for RecNodeLink
where
    RecNode: From<T>,
{
    fn from(value: T) -> Self {
        Self(Box::new(value.into()))
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
        write!(f, "{}", self.0)
    }
}
