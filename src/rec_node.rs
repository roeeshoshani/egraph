use std::fmt::Display;

use crate::node::{BinOp, GenericNode, UnOp};

#[derive(Debug, Clone)]
pub struct RecNode(pub GenericNode<Box<RecNode>>);
impl<T> From<T> for RecNode
where
    GenericNode<Box<RecNode>>: From<T>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}
pub type RecBinOp = BinOp<Box<RecNode>>;
pub type RecUnOp = UnOp<Box<RecNode>>;

impl<T> From<T> for Box<RecNode>
where
    GenericNode<Box<RecNode>>: From<T>,
{
    fn from(value: T) -> Self {
        Self::new(value.into())
    }
}
impl Display for RecNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            GenericNode::Imm(imm) => write!(f, "0x{:x}", imm.0),
            GenericNode::Var(var) => write!(f, "var{}", var.0),
            GenericNode::BinOp(bin_op) => {
                write!(f, "({}) {} ({})", bin_op.lhs, bin_op.kind, bin_op.rhs)
            }
            GenericNode::UnOp(un_op) => write!(f, "{}({})", un_op.kind, un_op.operand),
        }
    }
}
