mod utils;

use egraph::{
    egraph::{
        rewrite::{const_fold::BinOpConstFoldRewrite, template_rewrite::*},
        *,
    },
    node::{imm::Imm, *},
    rec_node::*,
    rewrites_arr,
    union_find::*,
};

use crate::utils::vn;

#[test]
fn test_dedup_basic() {
    let mut egraph = EGraph::new();
    let enode: ENode = vn(0).into();
    let res1 = egraph.add_enode(enode.clone());
    let res2 = egraph.add_enode(enode.clone());
    assert_eq!(res1.dedup_info, ENodeDedupInfo::New);
    assert_eq!(res2.dedup_info, ENodeDedupInfo::Duplicate);
    assert_eq!(res1.eclass_id.enode_id, res2.eclass_id.enode_id);
    assert_eq!(egraph.union_find().len(), 1);
}

#[test]
fn test_dedup_nested() {
    let mut egraph = EGraph::new();
    let var1_res = egraph.add_enode(vn(0).into());
    let var2_res = egraph.add_enode(vn(1).into());

    let enode = ENode::BinOp(BinOp {
        kind: BinOpKind::Add,
        lhs: var1_res.eclass_id,
        rhs: var2_res.eclass_id,
    });

    let res1 = egraph.add_enode(enode.clone());

    // add something in between just to add some noise
    let var3_res = egraph.add_enode(vn(3).into());

    // re-add the same enode
    let res2 = egraph.add_enode(enode.clone());

    // make sure that it got de-duplicated
    assert_eq!(res1.dedup_info, ENodeDedupInfo::New);
    assert_eq!(res2.dedup_info, ENodeDedupInfo::Duplicate);
    assert_eq!(res1.eclass_id.enode_id, res2.eclass_id.enode_id);
    assert_eq!(egraph.union_find().len(), 4);

    // now do some more
    let enode = ENode::UnOp(UnOp {
        kind: UnOpKind::Neg,
        operand: var1_res.eclass_id,
    });

    let res1 = egraph.add_enode(enode.clone());
    let res2 = egraph.add_enode(enode.clone());

    // make sure that it got de-duplicated
    assert_eq!(res1.dedup_info, ENodeDedupInfo::New);
    assert_eq!(res2.dedup_info, ENodeDedupInfo::Duplicate);
    assert_eq!(res1.eclass_id.enode_id, res2.eclass_id.enode_id);
    assert_eq!(egraph.union_find().len(), 5);

    // even more
    let enode = ENode::BinOp(BinOp {
        kind: BinOpKind::Mul,
        lhs: var3_res.eclass_id,
        rhs: var2_res.eclass_id,
    });

    let res1 = egraph.add_enode(enode.clone());
    let res2 = egraph.add_enode(enode.clone());

    // make sure that it got de-duplicated
    assert_eq!(res1.dedup_info, ENodeDedupInfo::New);
    assert_eq!(res2.dedup_info, ENodeDedupInfo::Duplicate);
    assert_eq!(res1.eclass_id.enode_id, res2.eclass_id.enode_id);
    assert_eq!(egraph.union_find().len(), 6);
}

#[test]
fn test_basic() {
    // 0xff & ((x & 0xff00) | (y & 0xff0000))
    let expr: RecLink = Imm::u64(0xff).to_rec_link()
        & ((vn(0).to_rec_link() & Imm::u64(0xff00).into())
            | (vn(1).to_rec_link() & Imm::u64(0xff0000).into()));

    let (mut egraph, root_eclass) = EGraph::from_rec_node(&expr.0);

    let rule_set = rewrites_arr![
        // (x & 0) => 0
        TemplateRewriteBuilder {
            query: tv("x") & Imm::u64(0).into(),
            rewrite: Imm::u64(0).into(),
        }
        .build(),
        // a & (b | c) => (a & b) | (a & c)
        TemplateRewriteBuilder {
            query: tv("a") & (tv("b") | tv("c")),
            rewrite: (tv("a") & tv("b")) | (tv("a") & tv("c")),
        }
        .build(),
        // a & (b & c) => (a & b) & c
        TemplateRewriteBuilder::bin_op_associativity(BinOpKind::BitAnd).build(),
        // a & b => b & a
        TemplateRewriteBuilder::bin_op_commutativity(BinOpKind::BitAnd).build(),
        BinOpConstFoldRewrite,
    ];

    let zero_eclass = egraph.add_enode(Imm::u64(0).into()).eclass_id;

    // before applying re-write rules, the expression should not equal 0
    assert!(
        !egraph
            .union_find()
            .are_eclasses_eq(zero_eclass, root_eclass)
    );
    assert_ne!(
        egraph.union_find().extract_eclass(root_eclass),
        Imm::u64(0).into()
    );

    // apply the rewrites
    egraph.apply_rewrites(rule_set.as_slice(), None);

    // now it should equal 0
    assert!(
        egraph
            .union_find()
            .are_eclasses_eq(zero_eclass, root_eclass)
    );
    assert_eq!(
        egraph.union_find().extract_eclass(root_eclass),
        Imm::u64(0).into()
    );

    // now do the same for a similar expression, but this time the expression shouldn't be simplified to 0

    // 0xffff & ((x & 0xff00) | (y & 0xff0000))
    let expr: RecLink = Imm::u64(0xffff).to_rec_link()
        & ((vn(0).to_rec_link() & Imm::u64(0xff00).into())
            | (vn(1).to_rec_link() & Imm::u64(0xff0000).into()));
    let (mut egraph, root_eclass) = EGraph::from_rec_node(&expr.0);
    let zero_eclass = egraph.add_enode(Imm::u64(0).into()).eclass_id;
    egraph.apply_rewrites(rule_set.as_slice(), None);

    // it should not equal 0, even after applying rewrites
    assert!(
        !egraph
            .union_find()
            .are_eclasses_eq(zero_eclass, root_eclass)
    );
    assert_ne!(
        egraph.union_find().extract_eclass(root_eclass),
        Imm::u64(0).into()
    );
}

#[test]
fn test_propegate_union() {
    let mut egraph = EGraph::new();
    let var0 = egraph.add_enode(vn(0).into()).eclass_id;
    let var1 = egraph.add_enode(vn(1).into()).eclass_id;
    let un_op_var0 = egraph
        .add_enode(
            UnOp {
                kind: UnOpKind::Neg,
                operand: var0,
            }
            .into(),
        )
        .eclass_id;
    let un_op_var1 = egraph
        .add_enode(
            UnOp {
                kind: UnOpKind::Neg,
                operand: var1,
            }
            .into(),
        )
        .eclass_id;

    assert!(!egraph.union_find().are_eclasses_eq(un_op_var0, un_op_var1));

    let union_res = egraph.union_find_mut().union_eclasses(var0, var1);
    assert_eq!(union_res, UnionRes::New);
    egraph.propegate_unions();

    assert!(egraph.union_find().are_eclasses_eq(un_op_var0, un_op_var1));
}

#[test]
fn test_propegate_union_multi_level() {
    let mut egraph = EGraph::new();
    let var0 = egraph.add_enode(vn(0).into()).eclass_id;
    let var1 = egraph.add_enode(vn(1).into()).eclass_id;
    let un_op_var0 = egraph
        .add_enode(
            UnOp {
                kind: UnOpKind::Neg,
                operand: var0,
            }
            .into(),
        )
        .eclass_id;
    let un_op_var1 = egraph
        .add_enode(
            UnOp {
                kind: UnOpKind::Neg,
                operand: var1,
            }
            .into(),
        )
        .eclass_id;

    let bin_op0 = egraph
        .add_enode(
            BinOp {
                kind: BinOpKind::Add,
                lhs: un_op_var0,
                rhs: var1,
            }
            .into(),
        )
        .eclass_id;
    let bin_op1 = egraph
        .add_enode(
            BinOp {
                kind: BinOpKind::Add,
                lhs: un_op_var1,
                rhs: var0,
            }
            .into(),
        )
        .eclass_id;

    // sanity
    assert!(!egraph.union_find().are_eclasses_eq(bin_op0, bin_op1));

    let union_res = egraph.union_find_mut().union_eclasses(var0, var1);
    assert_eq!(union_res, UnionRes::New);
    egraph.propegate_unions();

    assert!(egraph.union_find().are_eclasses_eq(un_op_var0, un_op_var1));
    assert!(egraph.union_find().are_eclasses_eq(bin_op0, bin_op1));
}
