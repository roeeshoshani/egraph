use egraph::{
    BinOpKind, BinOpTemplate, EGraph, RecBinOp, RecNode, RewriteRuleParams, RewriteRuleSet,
    TemplateVar, Var,
};

fn main() {
    // 0xff & (x & 0xff00)
    let rec_node: RecNode = RecBinOp {
        kind: BinOpKind::And,
        lhs: 0xff.into(),
        rhs: RecBinOp {
            kind: BinOpKind::And,
            lhs: Var(0).into(),
            rhs: 0xff00.into(),
        }
        .into(),
    }
    .into();

    let mut egraph = EGraph::from_rec_node(&rec_node);

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
    ]);

    egraph.apply_rule_set(&rule_set, Some(5));

    std::fs::create_dir_all("./graphs").unwrap();
    std::fs::write("./graphs/graph.gexf", egraph.to_gexf()).unwrap();
    egraph.dump_dot_svg("./graphs/graph.svg");
}
