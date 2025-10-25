use egraph::{
    const_fold::BinOpConstFoldRewrite, egraph::*, node::*, rec_node::*, rewrites_arr,
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

    let rewrites = rewrites_arr![
        // a & (b & c) => (a & b) & c
        TemplateRewriteBuilder::bin_op_associativity(BinOpKind::BitAnd).build(),
        // a | (b | c) => (a | b) | c
        TemplateRewriteBuilder::bin_op_associativity(BinOpKind::BitOr).build(),
        // a & b => b & a
        TemplateRewriteBuilder::bin_op_commutativity(BinOpKind::BitAnd).build(),
        // a | b => b | a
        TemplateRewriteBuilder::bin_op_commutativity(BinOpKind::BitOr).build(),
        // (a & 0) => 0
        TemplateRewriteBuilder {
            query: BinOp {
                kind: BinOpKind::BitAnd,
                lhs: "a".into(),
                rhs: 0.into(),
            }
            .into(),
            rewrite: 0.into(),
        }
        .build(),
        // (a | 0) => a
        TemplateRewriteBuilder {
            query: BinOp {
                kind: BinOpKind::BitOr,
                lhs: "a".into(),
                rhs: 0.into(),
            }
            .into(),
            rewrite: "a".into(),
        }
        .build(),
        // a & (b | c) => (a & b) | (a & c)
        TemplateRewriteBuilder {
            query: BinOp {
                kind: BinOpKind::BitAnd,
                lhs: "a".into(),
                rhs: BinOp {
                    kind: BinOpKind::BitOr,
                    lhs: "b".into(),
                    rhs: "c".into(),
                }
                .into(),
            }
            .into(),
            rewrite: BinOp {
                kind: BinOpKind::BitOr,
                lhs: BinOp {
                    kind: BinOpKind::BitAnd,
                    lhs: "a".into(),
                    rhs: "b".into(),
                }
                .into(),
                rhs: BinOp {
                    kind: BinOpKind::BitAnd,
                    lhs: "a".into(),
                    rhs: "c".into(),
                }
                .into(),
            }
            .into(),
        }
        .build(),
        // a & a => a
        TemplateRewriteBuilder {
            query: BinOp {
                kind: BinOpKind::BitAnd,
                lhs: "a".into(),
                rhs: "a".into(),
            }
            .into(),
            rewrite: "a".into(),
        }
        .build(),
        // a | a => a
        TemplateRewriteBuilder {
            query: BinOp {
                kind: BinOpKind::BitOr,
                lhs: "a".into(),
                rhs: "a".into(),
            }
            .into(),
            rewrite: "a".into(),
        }
        .build(),
        BinOpConstFoldRewrite
    ];

    egraph.apply_rewrites(rewrites.as_slice(), None);

    egraph.extract_eclass(root_eclass);
}
