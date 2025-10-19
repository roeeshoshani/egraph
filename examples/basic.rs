use egraph::{
    const_fold::BinOpConstFoldRewrite, egraph::*, node::*, rec_node::*, rewrites,
    template_rewrite::*,
};
use rsleigh::{Vn, VnAddr, VnSpace};

fn main() {
    let vn0 = Vn {
        size: 8,
        addr: VnAddr {
            off: 0,
            space: VnSpace::REGISTER,
        },
    };
    let vn1 = Vn {
        size: 8,
        addr: VnAddr {
            off: 8,
            space: VnSpace::REGISTER,
        },
    };

    // 0xff & ((x & 0xff00) | (y & 0xff0000))
    let rec_node: RecNode = RecBinOp {
        kind: BinOpKind::IntAnd,
        lhs: Imm {
            val: 0xff,
            size_in_bytes: 8,
        }
        .into(),
        rhs: RecBinOp {
            kind: BinOpKind::IntOr,
            lhs: RecBinOp {
                kind: BinOpKind::IntAnd,
                lhs: vn0.into(),
                rhs: Imm {
                    val: 0xff00,
                    size_in_bytes: 8,
                }
                .into(),
            }
            .into(),
            rhs: RecBinOp {
                kind: BinOpKind::IntAnd,
                lhs: vn1.into(),
                rhs: Imm {
                    val: 0xff0000,
                    size_in_bytes: 8,
                }
                .into(),
            }
            .into(),
        }
        .into(),
    }
    .into();

    let (mut egraph, root_eclass) = EGraph::from_rec_node(&rec_node);

    let rewrites = rewrites![
        // a & (b & c) => (a & b) & c
        TemplateRewrite::bin_op_associativity(BinOpKind::IntAnd).build(),
        // a | (b | c) => (a | b) | c
        TemplateRewrite::bin_op_associativity(BinOpKind::IntOr).build(),
        // a & b => b & a
        TemplateRewrite::bin_op_commutativity(BinOpKind::IntAnd).build(),
        // a | b => b | a
        TemplateRewrite::bin_op_commutativity(BinOpKind::IntOr).build(),
        // (x & 0) => 0
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::IntAnd,
                lhs: TemplateVar::new(1).into(),
                rhs: Imm {
                    val: 0,
                    size_in_bytes: 8
                }
                .into(),
            }
            .into(),
            rewrite: Imm {
                val: 0,
                size_in_bytes: 8
            }
            .into(),
        }
        .build(),
        // (x | 0) => x
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::IntOr,
                lhs: TemplateVar::new(1).into(),
                rhs: Imm {
                    val: 0,
                    size_in_bytes: 8
                }
                .into(),
            }
            .into(),
            rewrite: TemplateVar::new(1).into(),
        }
        .build(),
        // a & (b | c) => (a & b) | (a & c)
        TemplateRewrite::bin_op_distribute(BinOpKind::IntAnd, BinOpKind::IntOr).build(),
        // a & a => a
        TemplateRewrite {
            query: TemplateBinOp {
                kind: BinOpKind::IntAnd,
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
                kind: BinOpKind::IntOr,
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

    egraph.extract_eclass(root_eclass);
}
