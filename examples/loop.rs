use egraph::{
    BinOpKind, BinOpTemplate, EGraph, RecBinOp, RecNode, RewriteRuleParams, RewriteRuleSet,
    TemplateVar, Var, graph::Graph,
};

fn main() {
    // 0xff & ((x & 0xff00) | (y & 0xff0000))
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
                lhs: Var(1).into(),
                rhs: 0xff0000.into(),
            }
            .into(),
        }
        .into(),
    }
    .into();

    let (mut graph, root_eclass) = Graph::from_rec_node(&rec_node);
    let (mut egraph, translation_map) = EGraph::from_graph(&graph);

    std::fs::create_dir_all("./graphs").unwrap();
    egraph.dump_dot_svg("./graphs/graph.svg");
}
