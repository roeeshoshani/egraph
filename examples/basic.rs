use egraph::{egraph::*, node::*, rec_node::*, rewrite::*, rewrites_arr};

fn main() {
    // 0xff & ((x & 0xff00) | (y & 0xff0000))
    let rec_node: RecNode = RecBinOp {
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
    .into();

    let (mut egraph, root_eclass) = EGraph::from_rec_node(&rec_node);

    let rewrites = rewrites_arr![
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

    egraph.apply_rewrites(&rewrites, None);

    std::fs::create_dir_all("./graphs").unwrap();
    egraph.dump_dot_svg("./graphs/graph.svg");

    egraph.extract_eclass(root_eclass);
}
