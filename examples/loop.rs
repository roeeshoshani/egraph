use egraph::{
    const_fold::BinOpConstFoldRewrite, egraph::*, graph::*, node::*, rec_node::*, rewrites,
    template_rewrite::*,
};
use rsleigh::{
    SleighCtx, Vn, VnAddr, VnSpace,
    specs::{PSPEC_X86_64, SLA_SPEC_X86_64},
};

fn main() {
    let vn0 = Vn {
        size: 8,
        addr: VnAddr {
            off: 0,
            space: VnSpace::REGISTER,
        },
    };
    let vn1 = Vn {
        size: 8,
        addr: VnAddr {
            off: 8,
            space: VnSpace::REGISTER,
        },
    };

    // 5 + (0xff & ((x & 0xff00) | (y & 0xff0000)))
    //
    // the 5 is just a placeholder which will be later replaced to make the node point to itself.
    let rec_node: RecNode = RecBinOp {
        kind: BinOpKind::IntAdd,
        lhs: Imm {
            val: 5,
            size_in_bytes: 8,
        }
        .into(),
        rhs: RecBinOp {
            kind: BinOpKind::IntAnd,
            lhs: Imm {
                val: 0xff,
                size_in_bytes: 8,
            }
            .into(),
            rhs: RecBinOp {
                kind: BinOpKind::IntOr,
                lhs: RecBinOp {
                    kind: BinOpKind::IntAnd,
                    lhs: vn0.into(),
                    rhs: Imm {
                        val: 0xff00,
                        size_in_bytes: 8,
                    }
                    .into(),
                }
                .into(),
                rhs: RecBinOp {
                    kind: BinOpKind::IntAnd,
                    lhs: vn1.into(),
                    rhs: Imm {
                        val: 0xff0000,
                        size_in_bytes: 8,
                    }
                    .into(),
                }
                .into(),
            }
            .into(),
        }
        .into(),
    }
    .into();

    let (mut graph, root_id) = Graph::from_rec_node(&rec_node);

    // make the root bin op point to itself in its lhs.
    //
    // so, now it is:
    // self + (0xff & ((x & 0xff00) | (y & 0xff0000)))
    //
    // which can be simplified to 0.
    let GenericNode::BinOp(root_bin_op) = &mut graph[root_id] else {
        unreachable!();
    };
    root_bin_op.lhs = root_id;

    let (mut egraph, translation_map) = EGraph::from_graph(&graph);

    let root_eclass = translation_map[root_id];

    let rule_set = rewrites![
        // a & (b & c) => (a & b) & c
        TemplateRewrite::bin_op_associativity(BinOpKind::IntAnd).build(),
        // a | (b | c) => (a | b) | c
        TemplateRewrite::bin_op_associativity(BinOpKind::IntOr).build(),
        // a + (b + c) => (a + b) + c
        TemplateRewrite::bin_op_associativity(BinOpKind::IntAdd).build(),
        // a & b => b & a
        TemplateRewrite::bin_op_commutativity(BinOpKind::IntAnd).build(),
        // a | b => b | a
        TemplateRewrite::bin_op_commutativity(BinOpKind::IntOr).build(),
        // a + b => b + a
        TemplateRewrite::bin_op_commutativity(BinOpKind::IntAdd).build(),
        // (x & 0) => 0
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::IntAnd,
                lhs: TemplateVar::new(1).into(),
                rhs: Imm {
                    val: 0,
                    size_in_bytes: 8,
                }
                .into(),
            }
            .into(),
            rewrite: Imm {
                val: 0,
                size_in_bytes: 8,
            }
            .into(),
        }
        .build(),
        // (x | 0) => x
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::IntOr,
                lhs: TemplateVar::new(1).into(),
                rhs: Imm {
                    val: 0,
                    size_in_bytes: 8,
                }
                .into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
        }
        .build(),
        // (x + 0) => x
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::IntAdd,
                lhs: TemplateVar::new(1).into(),
                rhs: Imm {
                    val: 0,
                    size_in_bytes: 8,
                }
                .into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
        }
        .build(),
        // a & (b | c) => (a & b) | (a & c)
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::IntAnd,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateBinOp {
                    kind: BinOpKind::IntOr,
                    lhs: TemplateVar::new(2).into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
            }
            .into(),
            rewrite: TemplateBinOp {
                kind: BinOpKind::IntOr,
                lhs: TemplateBinOp {
                    kind: BinOpKind::IntAnd,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(2).into(),
                }
                .into(),
                rhs: TemplateBinOp {
                    kind: BinOpKind::IntAnd,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
            }
            .into(),
        }
        .build(),
        // a & a => a
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::IntAnd,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
        }
        .build(),
        // a | a => a
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::IntOr,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
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
