use egraph::{
    const_fold::BinOpConstFoldRewrite, egraph::*, graph::*, node::*, rec_node::*, rewrites,
    template_rewrite::*,
};
use rsleigh::{
    SleighCtx,
    specs::{PSPEC_X86_64, SLA_SPEC_X86_64},
};

fn main() {
    let rec_node: RecNode = RecBinOp {
        kind: BinOpKind::IntMul,
        lhs: {
            // result multiplier
            Imm {
                val: 3,
                size_in_bytes: 8,
            }
            .into()
        },
        rhs: RecBinOp {
            kind: BinOpKind::IntAdd,
            lhs: {
                // loop increment value
                Imm {
                    val: 3,
                    size_in_bytes: 8,
                }
                .into()
            },
            rhs: PhiNode {
                inputs: vec![
                    // initial loop value
                    Imm {
                        val: 37,
                        size_in_bytes: 8,
                    }
                    .into(),
                    // magic placeholder which will later point to the add node
                    Imm {
                        val: 0xdeadbeef,
                        size_in_bytes: 8,
                    }
                    .into(),
                ],
                region_node: {
                    // a dummy region node which won't even be used, just to make it compile
                    RegionNode {
                        cf_inputs: Vec::new(),
                    }
                    .into()
                },
            }
            .into(),
        }
        .into(),
    }
    .into();

    let (mut graph, root_id) = Graph::from_rec_node(&rec_node);

    // make the phi point to the add node to build the loop
    let add_node_id = graph[root_id].as_bin_op().unwrap().rhs;
    let phi_node_id = graph[add_node_id].as_bin_op().unwrap().rhs;
    graph[phi_node_id].as_phi_mut().unwrap().inputs[1] = add_node_id;

    let (mut egraph, translation_map) = EGraph::from_graph(&graph);

    let root_eclass = translation_map[root_id];

    let rule_set = rewrites![
        // a + (b + c) => (a + b) + c
        TemplateRewrite::bin_op_associativity(BinOpKind::IntAdd).build(),
        // a + b => b + a
        TemplateRewrite::bin_op_commutativity(BinOpKind::IntAdd).build(),
        BinOpConstFoldRewrite,
    ];

    egraph.apply_rewrites(&rule_set, None);

    let extract_res = egraph.extract_eclass(root_eclass);
    println!("{:#?}", extract_res);

    let sleigh_ctx = SleighCtx::new(SLA_SPEC_X86_64, PSPEC_X86_64).unwrap();

    std::fs::create_dir_all("./graphs").unwrap();
    egraph.dump_dot_svg("./graphs/graph.svg", &sleigh_ctx);
}
