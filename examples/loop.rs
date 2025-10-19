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
        // a + b => b + a
        TemplateRewrite::bin_op_commutativity(BinOpKind::IntAdd).build(),
        // x * (phi(initial_term, add(loop_increment, <parent phi>)))
        // =>
        // phi(initial_term * x, add(loop_increment * x, <parent phi>))
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::IntMul,
                lhs: {
                    // multiplier (aka `x`)
                    TemplateVar::new(1).into()
                },
                rhs: TemplateLink::SpecificBind(
                    TemplateVar::new(2),
                    Box::new(
                        PhiNode {
                            inputs: vec![
                                // initial term
                                TemplateVar::new(3).into(),
                                // loop body
                                TemplateBinOp {
                                    kind: BinOpKind::IntAdd,
                                    lhs: {
                                        // loop increment
                                        TemplateVar::new(4).into()
                                    },
                                    rhs: {
                                        // point back to parent phi
                                        TemplateVar::new(2).into()
                                    }
                                }
                                .into()
                            ],
                            region_node: TemplateVar::new(5).into(),
                        }
                        .into()
                    )
                ),
            }
            .into(),
            rewrite: TemplateLink::SpecificBind(
                TemplateVar::new(6),
                Box::new(
                    PhiNode {
                        inputs: vec![
                            // multiplied initial term
                            TemplateBinOp {
                                kind: BinOpKind::IntMul,
                                lhs: TemplateVar::new(3).into(),
                                rhs: TemplateVar::new(1).into(),
                            }
                            .into(),
                            // loop body
                            TemplateBinOp {
                                kind: BinOpKind::IntAdd,
                                lhs: {
                                    // multiplied loop increment
                                    TemplateBinOp {
                                        kind: BinOpKind::IntMul,
                                        lhs: TemplateVar::new(4).into(),
                                        rhs: TemplateVar::new(1).into(),
                                    }
                                    .into()
                                },
                                rhs: {
                                    // point back to new parent phi
                                    TemplateVar::new(6).into()
                                }
                            }
                            .into()
                        ],
                        region_node: TemplateVar::new(5).into()
                    }
                    .into()
                )
            ),
        }
        .build(),
        BinOpConstFoldRewrite,
    ];

    egraph.apply_rewrites(&rule_set, None);

    let extract_res = egraph.extract_eclass(root_eclass);
    println!("{:#?}", extract_res);

    let sleigh_ctx = SleighCtx::new(SLA_SPEC_X86_64, PSPEC_X86_64).unwrap();

    std::fs::create_dir_all("./graphs").unwrap();
    egraph.dump_dot_svg("./graphs/graph.svg", &sleigh_ctx);
}
