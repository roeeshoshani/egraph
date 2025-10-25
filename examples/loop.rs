use egraph::{
    const_fold::BinOpConstFoldRewrite, egraph::*, graph::*, node::*, rec_node::*, rewrites_arr,
    template_rewrite::*,
};

fn main() {
    // 5 + (0xff & ((x & 0xff00) | (y & 0xff0000)))
    //
    // the 5 is just a placeholder which will be later replaced to make the node point to itself.
    let rec_node: RecNode = RecBinOp {
        kind: BinOpKind::Add,
        lhs: 5.into(),
        rhs: RecBinOp {
            kind: BinOpKind::BitAnd,
            lhs: 0xff.into(),
            rhs: RecBinOp {
                kind: BinOpKind::BitOr,
                lhs: RecBinOp {
                    kind: BinOpKind::BitAnd,
                    lhs: Var(0).into(),
                    rhs: 0xff00.into(),
                }
                .into(),
                rhs: RecBinOp {
                    kind: BinOpKind::BitAnd,
                    lhs: Var(1).into(),
                    rhs: 0xff0000.into(),
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

    let rule_set = rewrites_arr![
        // a & (b & c) => (a & b) & c
        TemplateRewriteBuilder::bin_op_associativity(BinOpKind::BitAnd).build(),
        // a | (b | c) => (a | b) | c
        TemplateRewriteBuilder::bin_op_associativity(BinOpKind::BitOr).build(),
        // a + (b + c) => (a + b) + c
        TemplateRewriteBuilder::bin_op_associativity(BinOpKind::Add).build(),
        // a & b => b & a
        TemplateRewriteBuilder::bin_op_commutativity(BinOpKind::BitAnd).build(),
        // a | b => b | a
        TemplateRewriteBuilder::bin_op_commutativity(BinOpKind::BitOr).build(),
        // a + b => b + a
        TemplateRewriteBuilder::bin_op_commutativity(BinOpKind::Add).build(),
        // (x & 0) => 0
        TemplateRewriteBuilder {
            query: TemplateBinOp {
                kind: BinOpKind::BitAnd,
                lhs: TemplateVar::new(1).into(),
                rhs: 0.into(),
            }
            .into(),
            rewrite: 0.into(),
        }
        .build(),
        // (x | 0) => x
        TemplateRewriteBuilder {
            query: TemplateBinOp {
                kind: BinOpKind::BitOr,
                lhs: TemplateVar::new(1).into(),
                rhs: 0.into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
        }
        .build(),
        // (x + 0) => x
        TemplateRewriteBuilder {
            query: TemplateBinOp {
                kind: BinOpKind::Add,
                lhs: TemplateVar::new(1).into(),
                rhs: 0.into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
        }
        .build(),
        // a & (b | c) => (a & b) | (a & c)
        TemplateRewriteBuilder {
            query: TemplateBinOp {
                kind: BinOpKind::BitAnd,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateBinOp {
                    kind: BinOpKind::BitOr,
                    lhs: TemplateVar::new(2).into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
            }
            .into(),
            rewrite: TemplateBinOp {
                kind: BinOpKind::BitOr,
                lhs: TemplateBinOp {
                    kind: BinOpKind::BitAnd,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(2).into(),
                }
                .into(),
                rhs: TemplateBinOp {
                    kind: BinOpKind::BitAnd,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
            }
            .into(),
        }
        .build(),
        // a & a => a
        TemplateRewriteBuilder {
            query: TemplateBinOp {
                kind: BinOpKind::BitAnd,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
        }
        .build(),
        // a | a => a
        TemplateRewriteBuilder {
            query: TemplateBinOp {
                kind: BinOpKind::BitOr,
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

    std::fs::create_dir_all("./graphs").unwrap();
    egraph.dump_dot_svg("./graphs/graph.svg");
}
