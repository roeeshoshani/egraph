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

    // the below code calculates `sum(i + 6 for i in 0..x)`
    //
    // should be equal to `sum(i for i in 6..x + 6)`
    let expr1: RecLink = LoopEval {
        loop_node: Loop {
            outputs: vec![
                LoopParam(0).to_rec_link() + Imm::u64(1).into(),
                LoopParam(1).to_rec_link()
                    + (
                        // i + 6
                        LoopParam(0).to_rec_link() + Imm::u64(6).into()
                    ),
            ],
            cond: BinOp {
                kind: BinOpKind::UnsignedLess,
                lhs: LoopParam(0).into(),
                rhs: x.clone(),
            }
            .into(),
        }
        .into(),
        inputs: vec![Imm::u64(0).into(), Imm::u64(0).into()],
    }
    .into();

    // `sum(i for i in 6..x + 6)`
    let expr2: RecLink = LoopEval {
        loop_node: Loop {
            outputs: vec![
                LoopParam(0).to_rec_link() + Imm::u64(1).into(),
                LoopParam(1).to_rec_link()
                    + (
                        // i + 6
                        LoopParam(0).to_rec_link() + Imm::u64(6).into()
                    ),
            ],
            cond: BinOp {
                kind: BinOpKind::UnsignedLess,
                lhs: LoopParam(0).into(),
                rhs: x + Imm::u64(6).into(),
            }
            .into(),
        }
        .into(),
        inputs: vec![Imm::u64(6).into(), Imm::u64(0).into()],
    }
    .into();
    println!("{}", expr1);
    println!("{}", expr2);
}
