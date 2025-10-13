use egraph::{
    BinOpKind, BinOpTemplate, EGraph, RecBinOp, RecNode, RewriteRuleParams, RewriteRuleSet,
    TemplateVar, Var,
};

fn main() {
    // (x | y) & 0
    let rec_node: RecNode = RecBinOp {
        kind: BinOpKind::And,
        lhs: RecBinOp {
            kind: BinOpKind::Or,
            lhs: Var(0).into(),
            rhs: Var(1).into(),
        }
        .into(),
        rhs: 0.into(),
    }
    .into();

    let (mut egraph, _root_eclass) = EGraph::from_rec_node(&rec_node);

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

    std::fs::create_dir_all("./graphs").unwrap();
    egraph.dump_dot_svg("./graphs/graph.svg");
}
