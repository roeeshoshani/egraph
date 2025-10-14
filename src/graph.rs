use std::{num::NonZeroUsize, ops::Index};

use hashbrown::HashMap;
use stable_vec::StableVec;

use crate::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct GraphNodeId(pub NonZeroUsize);
impl GraphNodeId {
    /// creates a new graph node id from the index of the node in the nodes array.
    pub fn from_index(index: usize) -> Self {
        Self(unsafe {
            // SAFETY: we add 1, so it can't be 0
            NonZeroUsize::new_unchecked(index + 1)
        })
    }

    /// the index of the node represented by this id in the nodes array.
    pub fn index(&self) -> usize {
        self.0.get() - 1
    }
}

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
    pub fn from_rec_node(rec_node: &RecNode) -> (Self, GraphNodeId) {
        let mut graph = Self::new();
        let root_node_id = graph.add_rec_node(rec_node);
        (graph, root_node_id)
    }
    pub fn add_rec_node(&mut self, rec_node: &RecNode) -> GraphNodeId {
        let graph_node = rec_node.0.convert_link(|link| self.add_rec_node(link));
        self.add_node(graph_node)
    }
    pub fn add_node(&mut self, node: GraphNode) -> GraphNodeId {
        if let Some(existing_id) = self.node_to_id.get(&node) {
            return *existing_id;
        }
        let index = self.nodes.push(node.clone());
        let new_id = GraphNodeId::from_index(index);
        self.node_to_id.insert(node, new_id);
        new_id
    }
    pub fn valid_node_ids(&self) -> impl Iterator<Item = GraphNodeId> {
        self.nodes.indices().map(GraphNodeId::from_index)
    }
}
impl Index<GraphNodeId> for Graph {
    type Output = GraphNode;

    fn index(&self, node_id: GraphNodeId) -> &Self::Output {
        &self.nodes[node_id.index()]
    }
}
