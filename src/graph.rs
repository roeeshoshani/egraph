use derivative::Derivative;
use hashbrown::HashMap;
use stable_vec::StableVec;

use crate::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct GraphNodeId(pub usize);

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub struct GraphNode<N: NodeProvider>(pub N::Node<GraphNodeId>);

#[derive(derive_debug::Dbg, Clone)]
pub struct Graph<N: NodeProvider> {
    nodes: StableVec<GraphNode<N>>,

    /// a hashmap for de-duplication
    #[dbg(skip)]
    node_to_id: HashMap<GraphNode<N>, GraphNodeId>,
}
impl<N: NodeProvider> Graph<N> {
    pub fn new() -> Self {
        Self {
            nodes: StableVec::new(),
            node_to_id: HashMap::new(),
        }
    }
    pub fn from_rec_node(rec_node: &RecNode<N>) -> Self {
        let mut graph = Self::new();
        graph.add_rec_node(rec_node);
        graph
    }
    pub fn add_rec_node(&mut self, rec_node: &RecNode<N>) -> GraphNodeId {
        let graph_node = N::convert_link(&rec_node.0, |link| self.add_rec_node(link));
        self.add_node(GraphNode(graph_node))
    }
    pub fn add_node(&mut self, node: GraphNode<N>) -> GraphNodeId {
        if let Some(existing_id) = self.node_to_id.get(&node) {
            return *existing_id;
        }
        let new_id = GraphNodeId(self.nodes.push(node.clone()));
        self.node_to_id.insert(node, new_id);
        new_id
    }
}
