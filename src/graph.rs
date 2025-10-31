use std::{
    num::NonZeroUsize,
    ops::{Index, IndexMut},
};

use hashbrown::HashMap;
use stable_vec::StableVec;

use crate::{node::*, rec_node::*};

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
        let graph_node = rec_node.convert_links(|link| self.add_rec_node(&link.0));
        self.add_node(graph_node)
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

    /// an internal recursive version of the cyclicity calculation.
    fn cyclicity_recursive(
        &self,
        node: GraphNodeId,
        node_marks: &mut [CyclicityMark],
    ) -> Cyclicity {
        match node_marks[node.index()] {
            CyclicityMark::Unexplored => {
                // this node was unexplored, so we can just explore it now.
            }
            CyclicityMark::Acyclic => {
                // we have already visited this node and didn't detect any cycles, so this-far we are acyclic.
                return Cyclicity::Acyclic;
            }
            CyclicityMark::InRecursionStack => {
                // this node is already in our recursion stack, and we are now about to traverse it again, so we have a cycle.
                return Cyclicity::Cyclic;
            }
        }

        node_marks[node.index()] = CyclicityMark::InRecursionStack;

        for &link in self[node].links() {
            match self.cyclicity_recursive(link, node_marks) {
                Cyclicity::Cyclic => {
                    // the linked node is cyclic, so this node is also cyclic.
                    return Cyclicity::Cyclic;
                }
                Cyclicity::Acyclic => {
                    // the linked node is acyclic. keep scanning the other links.
                }
            }
        }

        node_marks[node.index()] = CyclicityMark::Acyclic;

        Cyclicity::Acyclic
    }

    /// returns the cyclicity of this graph. this basically tells us if the graph is cyclic or acyclic.
    pub fn cyclicity(&self) -> Cyclicity {
        let mut marks: Vec<CyclicityMark> =
            vec![CyclicityMark::Unexplored; self.nodes.next_push_index()];

        for id in self.valid_node_ids() {
            match marks[id.index()] {
                CyclicityMark::Unexplored => {
                    match self.cyclicity_recursive(id, &mut marks) {
                        Cyclicity::Cyclic => {
                            // this node is cyclic, so the entire graph is cyclic
                            return Cyclicity::Cyclic;
                        }
                        Cyclicity::Acyclic => {
                            // this node is acyclic, continue scanning other nodes to try to find cycles.
                        }
                    }
                }
                CyclicityMark::InRecursionStack => {
                    // we should never reach here, since we are not inside any recursion stack here
                    unreachable!()
                }
                CyclicityMark::Acyclic => {
                    // we already checked this node and found that it is acyclic, so we don't need to scan it again.
                    // continue to the next node.
                    continue;
                }
            }
        }

        // we checked all nodes, and didn't find any cyclic node, so the graph is acyclic.
        Cyclicity::Acyclic
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

/// the cyclicity of a graph. this basically tells us if the graph is cyclic or acyclic.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Cyclicity {
    Cyclic,
    Acyclic,
}

/// a mark of a graph node which calculating the cyclicity of the graph.
#[derive(Copy, Clone, PartialEq, Eq)]
enum CyclicityMark {
    /// this node is unexplored, and was never previously encountered.
    Unexplored,

    /// this node is in our current recursion stack. we are currently in the process of traversing this node.
    InRecursionStack,

    /// we have already finished checking this node and found it to be acyclic.
    Acyclic,
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper: make a fresh graph with no nodes.
    fn empty_graph() -> Graph {
        Graph::new()
    }

    #[test]
    fn empty_graph_is_acyclic() {
        let g = empty_graph();
        assert_eq!(g.cyclicity(), Cyclicity::Acyclic);
    }

    #[test]
    fn single_imm_is_acyclic() {
        let mut g = Graph::new();
        let nid = g.add_node(GenericNode::Imm(Imm(42)));
        assert!(g.is_id_valid(nid));
        assert_eq!(g.cyclicity(), Cyclicity::Acyclic);
    }

    #[test]
    fn linear_chain_unops_is_acyclic() {
        // Build a chain: n2 -> n1 -> leaf
        let mut g = Graph::new();
        let leaf = g.add_node(GenericNode::Var(Var(1)));

        let n1 = g.add_node(GenericNode::UnOp(UnOp {
            kind: UnOpKind::Neg,
            operand: leaf,
        }));

        let _n2 = g.add_node(GenericNode::UnOp(UnOp {
            kind: UnOpKind::BitNot,
            operand: n1,
        }));

        assert_eq!(g.cyclicity(), Cyclicity::Acyclic);
    }

    #[test]
    fn diamond_dag_is_acyclic() {
        //      add
        //     /   \
        //   mul   mul   (shared operands)
        //   / \   / \
        //  x   y x   y
        let mut g = Graph::new();
        let x = g.add_node(GenericNode::Var(Var(10)));
        let y = g.add_node(GenericNode::Var(Var(20)));

        let m1 = g.add_node(GenericNode::BinOp(BinOp {
            kind: BinOpKind::Mul,
            lhs: x,
            rhs: y,
        }));
        let m2 = g.add_node(GenericNode::BinOp(BinOp {
            kind: BinOpKind::Mul,
            lhs: x,
            rhs: y,
        }));

        // Because of de-duplication, m1 == m2 should point to the same node id.
        assert_eq!(m1, m2);

        let _add = g.add_node(GenericNode::BinOp(BinOp {
            kind: BinOpKind::Add,
            lhs: m1,
            rhs: m2,
        }));

        assert_eq!(g.cyclicity(), Cyclicity::Acyclic);
    }

    #[test]
    fn self_loop_is_cyclic() {
        // Create a placeholder, then mutate it to point to itself.
        let mut g = Graph::new();
        let a = g.add_node(GenericNode::InternalVar(InternalVar(0)));

        // Turn 'a' into an UnOp whose operand is itself.
        g[a] = GenericNode::UnOp(UnOp {
            kind: UnOpKind::Neg,
            operand: a,
        });

        assert_eq!(g.cyclicity(), Cyclicity::Cyclic);
    }

    #[test]
    fn two_node_cycle_is_cyclic() {
        // a -> b, b -> a
        let mut g = Graph::new();

        // Placeholders so we can cross-link them afterwards.
        let a = g.add_node(GenericNode::InternalVar(InternalVar(1)));
        let b = g.add_node(GenericNode::InternalVar(InternalVar(2)));

        // Now wire the cycle.
        g[a] = GenericNode::UnOp(UnOp {
            kind: UnOpKind::Neg,
            operand: b,
        });
        g[b] = GenericNode::UnOp(UnOp {
            kind: UnOpKind::BitNot,
            operand: a,
        });

        assert_eq!(g.cyclicity(), Cyclicity::Cyclic);
    }

    #[test]
    fn disconnected_with_one_cycle_is_cyclic() {
        // Component 1: chain (acyclic)
        let mut g = Graph::new();
        let v = g.add_node(GenericNode::Var(Var(1)));
        let u1 = g.add_node(GenericNode::UnOp(UnOp {
            kind: UnOpKind::Neg,
            operand: v,
        }));
        let _u2 = g.add_node(GenericNode::UnOp(UnOp {
            kind: UnOpKind::BitNot,
            operand: u1,
        }));

        // Component 2: cycle
        let c1 = g.add_node(GenericNode::InternalVar(InternalVar(10)));
        let c2 = g.add_node(GenericNode::InternalVar(InternalVar(11)));
        g[c1] = GenericNode::BinOp(BinOp {
            kind: BinOpKind::Add,
            lhs: c2,
            rhs: v, // mix with other component to ensure cross-links don't matter
        });
        g[c2] = GenericNode::UnOp(UnOp {
            kind: UnOpKind::Neg,
            operand: c1,
        });

        assert_eq!(g.cyclicity(), Cyclicity::Cyclic);
    }

    #[test]
    fn from_rec_node_builds_acyclic_graph() {
        // -x + (x * 7)
        let expr: RecLink = -Var(0).to_rec_link() + (Var(0).to_rec_link() * 7.into());

        let (g, _root) = Graph::from_rec_node(&expr.0);

        // rec nodes are acyclic
        assert_eq!(g.cyclicity(), Cyclicity::Acyclic);
    }
}
