use egraph::{
    egraph::rewrite::const_fold::BinOpConstFoldRewrite, egraph::rewrite::template_rewrite::*,
    egraph::*, node::*, rec_node::*, rewrites_arr,
};

fn main() {
    // 0xff & ((x & 0xff00) | (y & 0xff0000))
    let expr: RecLink = 0xff.to_rec_link()
        & ((Var(0).to_rec_link() & 0xff00.into()) | (Var(1).to_rec_link() & 0xff0000.into()));

    let (mut egraph, root_eclass) = EGraph::from_rec_node(&expr.0);

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
            query: tv("a") & 0.into(),
            rewrite: 0.into(),
        }
        .build(),
        // (a | 0) => a
        TemplateRewriteBuilder {
            query: tv("a") | 0.into(),
            rewrite: tv("a"),
        }
        .build(),
        // a & (b | c) => (a & b) | (a & c)
        TemplateRewriteBuilder {
            query: tv("a") & (tv("b") | tv("c")),
            rewrite: (tv("a") & tv("b")) | (tv("a") & tv("c")),
        }
        .build(),
        // a & a => a
        TemplateRewriteBuilder {
            query: tv("a") & tv("a"),
            rewrite: tv("a"),
        }
        .build(),
        // a | a => a
        TemplateRewriteBuilder {
            query: tv("a") | tv("a"),
            rewrite: tv("a"),
        }
        .build(),
        BinOpConstFoldRewrite
    ];

    egraph.apply_rewrites(rewrites.as_slice(), None);

    let res = egraph.union_find().extract_eclass(root_eclass);
    println!("{}", res);
}
