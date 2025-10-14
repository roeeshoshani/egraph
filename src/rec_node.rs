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
            GenericNode::Imm(_) | GenericNode::Var(_) | GenericNode::InternalVar(_) => {
                write!(f, "{}", self.0.structural_display())
            }
            GenericNode::BinOp(bin_op) => {
                write!(f, "({}) {} ({})", bin_op.lhs, bin_op.kind, bin_op.rhs)
            }
            GenericNode::UnOp(un_op) => write!(f, "{}({})", un_op.kind, un_op.operand),
        }
    }
}
