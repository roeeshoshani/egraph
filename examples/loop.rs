use egraph::{egraph::*, graph::*, node::*, rec_node::*, rewrites_arr, template_rewrite::*};

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
        // (x & 0) => 0
        RewriteRule {
            query: BinOpTemplate {
                kind: BinOpKind::BitAnd,
                lhs: TemplateVar::new(1).into(),
                rhs: 0.into(),
            }
            .into(),
            rewrite: 0.into(),
            keep_original: false,
        },
        // (x | 0) => x
        RewriteRule {
            query: BinOpTemplate {
                kind: BinOpKind::BitOr,
                lhs: TemplateVar::new(1).into(),
                rhs: 0.into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
            keep_original: false,
        },
        // a & (b | c) => (a & b) | (a & c)
        RewriteRule {
            query: BinOpTemplate {
                kind: BinOpKind::BitAnd,
                lhs: TemplateVar::new(1).into(),
                rhs: BinOpTemplate {
                    kind: BinOpKind::BitOr,
                    lhs: TemplateVar::new(2).into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
            }
            .into(),
            rewrite: BinOpTemplate {
                kind: BinOpKind::BitOr,
                lhs: BinOpTemplate {
                    kind: BinOpKind::BitAnd,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(2).into(),
                }
                .into(),
                rhs: BinOpTemplate {
                    kind: BinOpKind::BitAnd,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
            }
            .into(),
            keep_original: true,
        },
        // a & (b & c) => (a & b) & c
        RewriteRule {
            query: BinOpTemplate {
                kind: BinOpKind::BitAnd,
                lhs: TemplateVar::new(1).into(),
                rhs: BinOpTemplate {
                    kind: BinOpKind::BitAnd,
                    lhs: TemplateVar::new(2).into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
            }
            .into(),
            rewrite: BinOpTemplate {
                kind: BinOpKind::BitAnd,
                lhs: BinOpTemplate {
                    kind: BinOpKind::BitAnd,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(2).into(),
                }
                .into(),
                rhs: TemplateVar::new(3).into(),
            }
            .into(),
            keep_original: true,
        },
        // a | (b | c) => (a | b) | c
        RewriteRule {
            query: BinOpTemplate {
                kind: BinOpKind::BitOr,
                lhs: TemplateVar::new(1).into(),
                rhs: BinOpTemplate {
                    kind: BinOpKind::BitOr,
                    lhs: TemplateVar::new(2).into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
            }
            .into(),
            rewrite: BinOpTemplate {
                kind: BinOpKind::BitOr,
                lhs: BinOpTemplate {
                    kind: BinOpKind::BitOr,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(2).into(),
                }
                .into(),
                rhs: TemplateVar::new(3).into(),
            }
            .into(),
            keep_original: true,
        },
        // a & b => b & a
        RewriteRule {
            query: BinOpTemplate {
                kind: BinOpKind::BitAnd,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(2).into(),
            }
            .into(),
            rewrite: BinOpTemplate {
                kind: BinOpKind::BitAnd,
                lhs: TemplateVar::new(2).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
            keep_original: true,
        },
        // a | b => b | a
        RewriteRule {
            query: BinOpTemplate {
                kind: BinOpKind::BitOr,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(2).into(),
            }
            .into(),
            rewrite: BinOpTemplate {
                kind: BinOpKind::BitOr,
                lhs: TemplateVar::new(2).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
            keep_original: true,
        },
        // a & a => a
        RewriteRule {
            query: BinOpTemplate {
                kind: BinOpKind::BitAnd,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
            keep_original: true,
        },
        // a | a => a
        RewriteRule {
            query: BinOpTemplate {
                kind: BinOpKind::BitOr,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
            keep_original: true,
        },
    ];

    egraph.apply_rewrites(&rule_set, None);

    let extract_res = egraph.extract_eclass(root_eclass);
    println!("{:#?}", extract_res);

    std::fs::create_dir_all("./graphs").unwrap();
    egraph.dump_dot_svg("./graphs/graph.svg");
}
