use hashbrown::HashMap;
use stable_vec::StableVec;

use crate::*;

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
        let graph_node = rec_node.0.convert_link(|link| self.add_rec_node(link));
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
