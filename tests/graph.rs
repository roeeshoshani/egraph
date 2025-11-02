mod utils;

use egraph::{
    graph::*,
    node::{imm::Imm, *},
    rec_node::*,
};
use utils::vn;

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
    let nid = g.add_node(GenericNode::Imm(Imm::u64(42)));
    assert!(g.is_id_valid(nid));
    assert_eq!(g.cyclicity(), Cyclicity::Acyclic);
}

#[test]
fn linear_chain_unops_is_acyclic() {
    // build a chain: n2 -> n1 -> leaf
    let mut g = Graph::new();
    let leaf = g.add_node(GenericNode::Imm(Imm::u64(1)));

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
    let x = g.add_node(vn(0).into());
    let y = g.add_node(vn(1).into());

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

    // because of de-duplication, m1 == m2 should point to the same node id.
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
    // create a placeholder, then mutate it to point to itself.
    let mut g = Graph::new();
    let a = g.add_node(vn(0).into());

    // turn 'a' into an UnOp whose operand is itself.
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

    // placeholders so we can cross-link them afterwards.
    let a = g.add_node(vn(0).into());
    let b = g.add_node(vn(1).into());

    // now wire the cycle.
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
    // component 1: chain (acyclic)
    let mut g = Graph::new();
    let v = g.add_node(vn(1).into());
    let u1 = g.add_node(GenericNode::UnOp(UnOp {
        kind: UnOpKind::Neg,
        operand: v,
    }));
    let _u2 = g.add_node(GenericNode::UnOp(UnOp {
        kind: UnOpKind::BitNot,
        operand: u1,
    }));

    // component 2: cycle
    let c1 = g.add_node(vn(10).into());
    let c2 = g.add_node(vn(11).into());
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
    let expr: RecLink = -vn(0).to_rec_link() + (vn(0).to_rec_link() * Imm::u64(7).into());

    let (g, _root) = Graph::from_rec_node(&expr.0);

    // rec nodes are acyclic
    assert_eq!(g.cyclicity(), Cyclicity::Acyclic);
}
