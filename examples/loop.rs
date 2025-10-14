use egraph::{
    BinOpKind, BinOpTemplate, EGraph, GenericNode, RecBinOp, RecNode, RewriteRuleParams,
    RewriteRuleSet, TemplateVar, Var, graph::Graph,
};

fn main() {
    // 5 + (0xff & ((x & 0xff00) | (y & 0xff0000)))
    //
    // the 5 is just a placeholder which will be later replaced to make the node point to itself.
    let rec_node: RecNode = RecBinOp {
        kind: BinOpKind::Add,
        lhs: 5.into(),
        rhs: RecBinOp {
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

    let rule_set = RewriteRuleSet::from_rules([
        // (x & 0) => 0
        RewriteRuleParams {
            query: BinOpTemplate {
                kind: BinOpKind::And,
                lhs: TemplateVar::new(1).into(),
                rhs: 0.into(),
            }
            .into(),
            rewrite: 0.into(),
            keep_original: false,
            bi_directional: false,
        },
        // (x | 0) => x
        RewriteRuleParams {
            query: BinOpTemplate {
                kind: BinOpKind::Or,
                lhs: TemplateVar::new(1).into(),
                rhs: 0.into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
            keep_original: false,
            bi_directional: false,
        },
        // a & (b | c) => (a & b) | (a & c)
        RewriteRuleParams {
            query: BinOpTemplate {
                kind: BinOpKind::And,
                lhs: TemplateVar::new(1).into(),
                rhs: BinOpTemplate {
                    kind: BinOpKind::Or,
                    lhs: TemplateVar::new(2).into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
            }
            .into(),
            rewrite: BinOpTemplate {
                kind: BinOpKind::Or,
                lhs: BinOpTemplate {
                    kind: BinOpKind::And,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(2).into(),
                }
                .into(),
                rhs: BinOpTemplate {
                    kind: BinOpKind::And,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
            }
            .into(),
            keep_original: true,
            bi_directional: false,
        },
        // a & (b & c) => (a & b) & c
        RewriteRuleParams {
            query: BinOpTemplate {
                kind: BinOpKind::And,
                lhs: TemplateVar::new(1).into(),
                rhs: BinOpTemplate {
                    kind: BinOpKind::And,
                    lhs: TemplateVar::new(2).into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
            }
            .into(),
            rewrite: BinOpTemplate {
                kind: BinOpKind::And,
                lhs: BinOpTemplate {
                    kind: BinOpKind::And,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(2).into(),
                }
                .into(),
                rhs: TemplateVar::new(3).into(),
            }
            .into(),
            keep_original: true,
            bi_directional: false,
        },
        // a | (b | c) => (a | b) | c
        RewriteRuleParams {
            query: BinOpTemplate {
                kind: BinOpKind::Or,
                lhs: TemplateVar::new(1).into(),
                rhs: BinOpTemplate {
                    kind: BinOpKind::Or,
                    lhs: TemplateVar::new(2).into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
            }
            .into(),
            rewrite: BinOpTemplate {
                kind: BinOpKind::Or,
                lhs: BinOpTemplate {
                    kind: BinOpKind::Or,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(2).into(),
                }
                .into(),
                rhs: TemplateVar::new(3).into(),
            }
            .into(),
            keep_original: true,
            bi_directional: false,
        },
        // a & b => b & a
        RewriteRuleParams {
            query: BinOpTemplate {
                kind: BinOpKind::And,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(2).into(),
            }
            .into(),
            rewrite: BinOpTemplate {
                kind: BinOpKind::And,
                lhs: TemplateVar::new(2).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
            keep_original: true,
            bi_directional: false,
        },
        // a | b => b | a
        RewriteRuleParams {
            query: BinOpTemplate {
                kind: BinOpKind::Or,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(2).into(),
            }
            .into(),
            rewrite: BinOpTemplate {
                kind: BinOpKind::Or,
                lhs: TemplateVar::new(2).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
            keep_original: true,
            bi_directional: false,
        },
        // a & a => a
        RewriteRuleParams {
            query: BinOpTemplate {
                kind: BinOpKind::And,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
            keep_original: true,
            bi_directional: false,
        },
        // a | a => a
        RewriteRuleParams {
            query: BinOpTemplate {
                kind: BinOpKind::Or,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
            keep_original: true,
            bi_directional: false,
        },
    ]);

    egraph.apply_rule_set(&rule_set, None);

    let extract_res = egraph.extract_eclass(root_eclass);
    println!("{:#?}", extract_res);

    std::fs::create_dir_all("./graphs").unwrap();
    egraph.dump_dot_svg("./graphs/graph.svg");
}
