use hashbrown::{Equivalent, HashMap, hash_map::RawEntryMut};

mod union_find;

use derive_more::From;
use stable_vec::StableVec;

use crate::union_find::{UnionFind, UnionFindItemId};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Imm(pub u64);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Var(pub u64);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BinOpKind {
    Add,
    Mul,
    And,
    Or,
}
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BinOp<L> {
    pub kind: BinOpKind,
    pub lhs: L,
    pub rhs: L,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum UnOpKind {
    Neg,
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

// NOTE: this should NOT implement `PartialEq` and `Eq` due to how it is implemented.
// we can have 2 instances of this type which point to different enodes, so the derived `Eq` implementation will say that they are not
// equal, but in practice the 2 enodes that they point to are part of the same eclass, so the 2 eclass ids should be equal.
//
// checking if 2 instances of this type are equal requires accessing the union find tree.
#[derive(Debug, Clone, Copy, Hash)]
pub struct EClassId {
    /// an id of some enode which is part of this eclass.
    /// this can be used to iterate over all
    pub enode_id: ENodeId,
}

pub type ENode = GenericNode<EClassId>;

struct ENodeQuery<'a> {
    enode: &'a ENode,
    union_find: &'a UnionFind<ENode>,
}
impl<'a> std::hash::Hash for ENodeQuery<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.enode.hash(state);
    }
}
impl<'a> Equivalent<ENode> for ENodeQuery<'a> {
    fn equivalent(&self, key: &ENode) -> bool {
        todo!()
    }
}

/// the id of an enode.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ENodeId(pub UnionFindItemId);

#[derive(derive_debug::Dbg, Clone)]
pub struct EGraph {
    enodes: UnionFind<ENode>,

    /// a hashmap for de-duplication
    #[dbg(skip)]
    enode_to_id: HashMap<ENode, ENodeId>,
}
impl EGraph {
    pub fn new() -> Self {
        Self {
            enodes: UnionFind::new(),
            enode_to_id: HashMap::new(),
        }
    }

    /// adds an enode to the egraph and returns the eclass id which contains it.
    pub fn add_enode(&mut self, enode: ENode) -> EClassId {
        let query = ENodeQuery {
            enode: &enode,
            union_find: &self.enodes,
        };
        let enode_id = match self.enode_to_id.raw_entry_mut().from_key(&query) {
            RawEntryMut::Occupied(entry) => *entry.get(),
            RawEntryMut::Vacant(entry) => {
                let enode_id = ENodeId(self.enodes.create_new_item(enode.clone()));
                entry.insert(enode, enode_id);
                enode_id
            }
        };

        EClassId { enode_id }
    }

    pub fn add_rec_node(&mut self, rec_node: &RecNode) -> EClassId {
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
        self.add_enode(graph_node)
    }
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

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct GraphNodeId(pub usize);

pub type GraphNode = GenericNode<GraphNodeId>;

#[derive(derive_debug::Dbg, Clone)]
pub struct Graph {
    nodes: StableVec<GraphNode>,

    /// a hashmap for de-duplication
    #[dbg(skip)]
    node_to_id: HashMap<GraphNode, GraphNodeId>,
}
impl Graph {
    pub fn new() -> Self {
        Self {
            nodes: StableVec::new(),
            node_to_id: HashMap::new(),
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
        if let Some(existing_id) = self.node_to_id.get(&node) {
            return *existing_id;
        }
        let new_id = GraphNodeId(self.nodes.push(node.clone()));
        self.node_to_id.insert(node, new_id);
        new_id
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

        let graph = Graph::from_rec_node(&rec_node);
        dbg!(graph);
        panic!();
    }
}
