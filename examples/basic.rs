use egraph::{
    const_fold::BinOpConstFoldRewrite, egraph::*, node::*, rec_node::*, rewrites,
    template_rewrite::*,
};

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

    let rewrites = rewrites![
        // (x & 0) => 0
        TemplateRewrite {
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
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::BitOr,
                lhs: TemplateVar::new(1).into(),
                rhs: 0.into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
        }
        .build(),
        // a & (b | c) => (a & b) | (a & c)
        TemplateRewrite {
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
        // a & (b & c) => (a & b) & c
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::BitAnd,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateBinOp {
                    kind: BinOpKind::BitAnd,
                    lhs: TemplateVar::new(2).into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
            }
            .into(),
            rewrite: TemplateBinOp {
                kind: BinOpKind::BitAnd,
                lhs: TemplateBinOp {
                    kind: BinOpKind::BitAnd,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(2).into(),
                }
                .into(),
                rhs: TemplateVar::new(3).into(),
            }
            .into(),
        }
        .build(),
        // a | (b | c) => (a | b) | c
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::BitOr,
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
                    kind: BinOpKind::BitOr,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(2).into(),
                }
                .into(),
                rhs: TemplateVar::new(3).into(),
            }
            .into(),
        }
        .build(),
        // a & b => b & a
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::BitAnd,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(2).into(),
            }
            .into(),
            rewrite: TemplateBinOp {
                kind: BinOpKind::BitAnd,
                lhs: TemplateVar::new(2).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
        }
        .build(),
        // a | b => b | a
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::BitOr,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(2).into(),
            }
            .into(),
            rewrite: TemplateBinOp {
                kind: BinOpKind::BitOr,
                lhs: TemplateVar::new(2).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
        }
        .build(),
        // a & a => a
        TemplateRewrite {
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
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::BitOr,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
        }
        .build(),
        BinOpConstFoldRewrite
    ];

    egraph.apply_rewrites(&rewrites, None);

    std::fs::create_dir_all("./graphs").unwrap();
    egraph.dump_dot_svg("./graphs/graph.svg");

    egraph.extract_eclass(root_eclass);
}
