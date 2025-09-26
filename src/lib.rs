use derive_more::From;
use stable_vec::StableVec;

#[derive(Debug, Clone, Copy)]
pub struct Imm(pub u64);

#[derive(Debug, Clone, Copy)]
pub struct Var(pub u64);

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    Add,
    Mul,
    And,
    Or,
}
#[derive(Debug, Clone)]
pub struct BinOp<L> {
    pub kind: BinOpKind,
    pub lhs: L,
    pub rhs: L,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOpKind {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
pub struct UnOp<L> {
    pub kind: UnOpKind,
    pub operand: L,
}

#[derive(Debug, Clone, From)]
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

#[derive(Debug, Clone, Copy)]
pub struct EClassId(pub usize);

pub type ENode = GenericNode<EClassId>;

#[derive(Debug, Clone)]
pub struct EClass {
    pub nodes: Vec<ENode>,
}

#[derive(Debug, Clone)]
pub struct EGraph {
    eclasses: StableVec<EClass>,
}

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

#[derive(Debug, Clone, Copy)]
pub struct GraphNodeId(pub usize);

pub type GraphNode = GenericNode<GraphNodeId>;

#[derive(Debug, Clone)]
pub struct Graph {
    nodes: StableVec<GraphNode>,
}
impl Graph {
    pub fn new() -> Self {
        Self {
            nodes: StableVec::new(),
        }
    }
    pub fn from_rec_node(rec_node: &RecNode) -> Self {
        let mut graph = Self::new();
        graph.add_rec_node(rec_node);
        graph
    }
    pub fn add_rec_node(&mut self, rec_node: &RecNode) -> GraphNodeId {
        // first, convert the recursive node into a graph node
        let graph_node = match &rec_node.0 {
            GenericNode::Imm(imm) => GenericNode::Imm(*imm),
            GenericNode::Var(var) => GenericNode::Var(*var),
            GenericNode::BinOp(bin_op) => GenericNode::BinOp(BinOp {
                kind: bin_op.kind,
                lhs: self.add_rec_node(&bin_op.lhs),
                rhs: self.add_rec_node(&bin_op.rhs),
            }),
            GenericNode::UnOp(un_op) => GenericNode::UnOp(UnOp {
                kind: un_op.kind,
                operand: self.add_rec_node(&un_op.operand),
            }),
        };
        self.add_node(graph_node)
    }
    pub fn add_node(&mut self, node: GraphNode) -> GraphNodeId {
        GraphNodeId(self.nodes.push(node))
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_basic() {
        // 0xff & ((x & 0xff00) || (x & 0xff0000))
        let rec_node: RecNode = RecBinOp {
            kind: BinOpKind::And,
            lhs: 0xff.into(),
            rhs: RecBinOp {
                kind: BinOpKind::Or,
                lhs: RecBinOp {
                    kind: BinOpKind::And,
                    lhs: Var(0).into(),
                    rhs: 0xff00.into(),
                }
                .into(),
                rhs: RecBinOp {
                    kind: BinOpKind::And,
                    lhs: Var(0).into(),
                    rhs: 0xff0000.into(),
                }
                .into(),
            }
            .into(),
        }
        .into();
    }
}
