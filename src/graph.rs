use std::{
    num::NonZeroUsize,
    ops::{Index, IndexMut},
};

use hashbrown::HashMap;
use stable_vec::StableVec;

use crate::*;

/// the id of a node in a graph.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct GraphNodeId(pub NonZeroUsize);
impl GraphNodeId {
    /// creates a new graph node id from the index of the node in the nodes array.
    pub fn from_index(index: usize) -> Self {
        Self(
            // SAFETY: we add 1, so it can't be 0
            unsafe { NonZeroUsize::new_unchecked(index + 1) },
        )
    }

    /// the index of the node represented by this id in the nodes array.
    pub fn index(&self) -> usize {
        self.0.get() - 1
    }
}

/// a node in the graph.
pub type GraphNode = GenericNode<GraphNodeId>;

/// the array of nodes as stored in the graph.
pub type GraphNodes = StableVec<GraphNode>;

/// a graph.
#[derive(derive_debug::Dbg, Clone)]
pub struct Graph {
    /// the nodes in the graph
    nodes: GraphNodes,

    /// a hashmap for de-duplication
    #[dbg(skip)]
    node_to_id: HashMap<GraphNode, GraphNodeId>,
}
impl Graph {
    /// creates a new empty graph
    pub fn new() -> Self {
        Self {
            nodes: GraphNodes::new(),
            node_to_id: HashMap::new(),
        }
    }

    /// creates a new graph from the given recursive node.
    pub fn from_rec_node(rec_node: &RecNode) -> (Self, GraphNodeId) {
        let mut graph = Self::new();
        let root_node_id = graph.add_rec_node(rec_node);
        (graph, root_node_id)
    }

    /// adds the given recursive node to the graph, and returns the id of the node in the graph which represents the root of the rec node.
    pub fn add_rec_node(&mut self, rec_node: &RecNode) -> GraphNodeId {
        let graph_node = rec_node.convert_links(|link| self.add_rec_node_link(link));
        self.add_node(graph_node)
    }

    /// adds the given recursive node link to the graph, and returns the id of the node in the graph which represents what this link
    /// should point to.
    pub fn add_rec_node_link(&mut self, rec_node_link: &RecNodeLink) -> GraphNodeId {
        match rec_node_link {
            RecNodeLink::Regular(node) => self.add_rec_node(node),
            RecNodeLink::Loop => unreachable!(),
        }
    }

    /// adds the given node to the graph and returns its id. if the node already exists in the graph, returns the id of the existing node.
    pub fn add_node(&mut self, node: GraphNode) -> GraphNodeId {
        if let Some(existing_id) = self.node_to_id.get(&node) {
            return *existing_id;
        }
        let index = self.nodes.push(node.clone());
        let new_id = GraphNodeId::from_index(index);
        self.node_to_id.insert(node, new_id);
        new_id
    }

    /// returns an iterator over all valid node ids in the graph.
    pub fn valid_node_ids(&self) -> impl Iterator<Item = GraphNodeId> {
        self.nodes.indices().map(GraphNodeId::from_index)
    }

    /// returns an iterator over all node ids in the graph, regardless of whether they are valid or not.
    pub fn all_node_ids(&self) -> impl Iterator<Item = GraphNodeId> {
        (0..self.nodes.next_push_index()).map(GraphNodeId::from_index)
    }

    /// checks if the given graph node id is valid and points to an occupied slot in the nodes array.
    pub fn is_id_valid(&self, id: GraphNodeId) -> bool {
        self.nodes.has_element_at(id.index())
    }

    /// returns a reference to the array of nodes of the graph
    pub fn nodes(&self) -> &GraphNodes {
        &self.nodes
    }

    /// returns a mutable reference to the array of nodes of the graph
    pub fn nodes_mut(&mut self) -> &mut GraphNodes {
        &mut self.nodes
    }
}
impl Index<GraphNodeId> for Graph {
    type Output = GraphNode;

    fn index(&self, node_id: GraphNodeId) -> &Self::Output {
        &self.nodes[node_id.index()]
    }
}
impl IndexMut<GraphNodeId> for Graph {
    fn index_mut(&mut self, node_id: GraphNodeId) -> &mut Self::Output {
        &mut self.nodes[node_id.index()]
    }
}
