use std::fmt::Display;

use crate::node::{BinOp, GenericNode, UnOp};

/// the link of a recursive node.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecNodeLink {
    /// a regular recursive node link. just points to another node.
    Regular(Box<RecNode>),

    /// a special link value used to mark a link that loops to a parent node.
    ///
    /// loops are not representable in recursive nodes, since each node must have exactly one parent.
    ///
    /// but, in some cases, we want to convert some graph representation to a recursive node, and have a representation that
    /// still encodes all information that could be encoded, but excluding the loops, which obviously can't be encoded in recursive
    /// nodes.
    ///
    /// so, this variant is used to mark such unrepresentable link, while still allowing us to represent the rest of the graph's
    /// structure in a convenient recursive way.
    // TODO: maybe we should just avoid using recursive nodes in those scenarios, and instead use graphs, because this is very hacky.
    Loop,
}

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
