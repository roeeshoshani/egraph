use egraph::{
    egraph::{
        rewrite::{const_fold::BinOpConstFoldRewrite, template_rewrite::*},
        *,
    },
    node::{imm::Imm, *},
    rec_node::*,
    rewrites_arr,
};
use rsleigh::{VnAddr, VnSpace};

fn main() {
    let x = Vn {
        size: ValueSize::U64,
        addr: VnAddr {
            off: 0,
            space: VnSpace::REGISTER,
        },
    }
    .to_rec_link();

    // the below code calculates the sum of all values in the range 0..x
    let expr: RecLink = Loop {
        inputs: TupleBuild {
            values: vec![Imm::u64(0).into()],
        }
        .into(),
        outputs: TupleBuild {
            values: vec![
                TupleGet {
                    tuple: LoopParams.into(),
                    index: 0,
                }
                .to_rec_link()
                    + Imm::u64(1).into(),
            ],
        }
        .into(),
        condition: BinOp {
            kind: BinOpKind::UnsignedLess,
            lhs: TupleGet {
                tuple: LoopParams.into(),
                index: 0,
            }
            .into(),
            rhs: x,
        }
        .into(),
    }
    .into();

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
            query: tv("a") & Imm::u64(0).into(),
            rewrite: Imm::u64(0).into(),
        }
        .build(),
        // (a | 0) => a
        TemplateRewriteBuilder {
            query: tv("a") | Imm::u64(0).into(),
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
