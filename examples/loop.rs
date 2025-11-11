use std::num::NonZeroUsize;

use egraph::{
    node::{imm::Imm, *},
    rec_node::*,
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
    let loop_id = LoopId(NonZeroUsize::new(1).unwrap());
    let expr1: RecLink = Loop {
        inner_vars: vec![],
        vars: vec![
            // i
            LoopVar {
                initial_value: Imm::u64(0).into(),
            }
            .to_rec_link()
                + Imm::u64(1).into(),
            // accumulator var
            LoopVar {
                initial_value: Imm::u64(0).into(),
            }
            .to_rec_link()
                + OtherLoopVar { loop_id, index: 0 }.into()
                + Imm::u64(6).into(),
        ],
        cond: BinOp {
            kind: BinOpKind::UnsignedLess,
            lhs: OtherLoopVar { loop_id, index: 0 }.into(),
            rhs: x.clone(),
        }
        .into(),
        id: loop_id,
    }
    .into();

    // `sum(i for i in 6..x + 6)`
    let expr2: RecLink = Loop {
        inner_vars: vec![],
        vars: vec![
            LoopVar {
                initial_value: Imm::u64(6).into(),
            }
            .to_rec_link()
                + Imm::u64(1).into(),
            LoopVar {
                initial_value: Imm::u64(0).into(),
            }
            .to_rec_link()
                + OtherLoopVar { loop_id, index: 0 }.into(),
        ],
        cond: BinOp {
            kind: BinOpKind::UnsignedLess,
            lhs: OtherLoopVar { loop_id, index: 0 }.into(),
            rhs: x + Imm::u64(6).into(),
        }
        .into(),
        id: loop_id,
    }
    .into();
    println!("{}", expr1);
    println!("{}", expr2);
}
