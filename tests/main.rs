use arrayvec::ArrayVec;
use derive_more::From;
use egraph::{NodeLinks, NodeProvider};
use enum_display::EnumDisplay;

pub const NODE_MAX_LINKS: usize = 2;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Imm(pub u64);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Var(pub u64);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, EnumDisplay)]
pub enum BinOpKind {
    #[display("+")]
    Add,
    #[display("*")]
    Mul,
    #[display("&")]
    And,
    #[display("|")]
    Or,
}
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BinOp<L> {
    pub kind: BinOpKind,
    pub lhs: L,
    pub rhs: L,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, EnumDisplay)]
pub enum UnOpKind {
    #[display("-")]
    Neg,
    #[display("!")]
    Not,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct UnOp<L> {
    pub kind: UnOpKind,
    pub operand: L,
}

#[derive(Debug, Clone, From, Hash, PartialEq, Eq)]
pub enum MyNode<L> {
    Imm(Imm),
    Var(Var),
    BinOp(BinOp<L>),
    UnOp(UnOp<L>),
}
impl<L> From<u64> for MyNode<L> {
    fn from(value: u64) -> Self {
        Imm(value).into()
    }
}

pub struct MyNodeLinks<'a, L: 'a>(pub ArrayVec<&'a L, NODE_MAX_LINKS>);
impl<'a, L: 'a> NodeLinks<'a, L> for MyNodeLinks<'a, L> {
    fn len(&self) -> usize {
        self.0.len()
    }

    fn iter(&self) -> impl Iterator<Item = &'a L> {
        self.0.iter().copied()
    }

    fn get_index(&self, index: usize) -> &'a L {
        &self.0[index]
    }
}

pub struct MyNodeProvider;
impl NodeProvider for MyNodeProvider {
    type Node<L> = MyNode<L>;

    type NodeLinks<'a, L: 'a> = MyNodeLinks<'a, L>;

    fn convert_link<L1, L2, F>(node: &Self::Node<L1>, mut f: F) -> Self::Node<L2>
    where
        F: FnMut(&L1) -> L2,
        Self::Node<()>: std::hash::Hash,
    {
        match node {
            MyNode::Imm(imm) => MyNode::Imm(*imm),
            MyNode::Var(var) => MyNode::Var(*var),
            MyNode::BinOp(BinOp { kind, lhs, rhs }) => MyNode::BinOp(BinOp {
                kind: *kind,
                lhs: f(lhs),
                rhs: f(rhs),
            }),
            MyNode::UnOp(UnOp { kind, operand }) => MyNode::UnOp(UnOp {
                kind: *kind,
                operand: f(operand),
            }),
        }
    }

    fn links<L>(node: &Self::Node<L>) -> Self::NodeLinks<'_, L> {
        todo!()
    }

    fn dot_graph_label<L>(node: &Self::Node<L>) -> String {
        todo!()
    }
}

// #[test]
// fn test_dedup_basic() {
//     let mut egraph = EGraph::new();
//     let enode = ENode::Var(Var(5));
//     let res1 = egraph.add_enode(enode.clone());
//     let res2 = egraph.add_enode(enode.clone());
//     assert_eq!(res1.dedup_info, ENodeDedupInfo::New);
//     assert_eq!(res2.dedup_info, ENodeDedupInfo::Duplicate);
//     assert_eq!(res1.eclass_id.enode_id, res2.eclass_id.enode_id);
//     assert_eq!(egraph.enodes_union_find.len(), 1);
// }

// #[test]
// fn test_dedup_nested() {
//     let mut egraph = EGraph::new();
//     let var1_res = egraph.add_enode(ENode::Var(Var(1)));
//     let var2_res = egraph.add_enode(ENode::Var(Var(2)));

//     let enode = ENode::BinOp(BinOp {
//         kind: BinOpKind::Add,
//         lhs: var1_res.eclass_id,
//         rhs: var2_res.eclass_id,
//     });

//     let res1 = egraph.add_enode(enode.clone());

//     // add something in between just to add some noise
//     let var3_res = egraph.add_enode(ENode::Var(Var(3)));

//     // re-add the same enode
//     let res2 = egraph.add_enode(enode.clone());

//     // make sure that it got de-duplicated
//     assert_eq!(res1.dedup_info, ENodeDedupInfo::New);
//     assert_eq!(res2.dedup_info, ENodeDedupInfo::Duplicate);
//     assert_eq!(res1.eclass_id.enode_id, res2.eclass_id.enode_id);
//     assert_eq!(egraph.enodes_union_find.len(), 4);

//     // now do some more
//     let enode = ENode::UnOp(UnOp {
//         kind: UnOpKind::Neg,
//         operand: var1_res.eclass_id,
//     });

//     let res1 = egraph.add_enode(enode.clone());
//     let res2 = egraph.add_enode(enode.clone());

//     // make sure that it got de-duplicated
//     assert_eq!(res1.dedup_info, ENodeDedupInfo::New);
//     assert_eq!(res2.dedup_info, ENodeDedupInfo::Duplicate);
//     assert_eq!(res1.eclass_id.enode_id, res2.eclass_id.enode_id);
//     assert_eq!(egraph.enodes_union_find.len(), 5);

//     // even more
//     let enode = ENode::BinOp(BinOp {
//         kind: BinOpKind::Mul,
//         lhs: var3_res.eclass_id,
//         rhs: var2_res.eclass_id,
//     });

//     let res1 = egraph.add_enode(enode.clone());
//     let res2 = egraph.add_enode(enode.clone());

//     // make sure that it got de-duplicated
//     assert_eq!(res1.dedup_info, ENodeDedupInfo::New);
//     assert_eq!(res2.dedup_info, ENodeDedupInfo::Duplicate);
//     assert_eq!(res1.eclass_id.enode_id, res2.eclass_id.enode_id);
//     assert_eq!(egraph.enodes_union_find.len(), 6);
// }

// #[test]
// fn test_basic_rewrite() {
//     // 0xff & ((x & 0xff00) | (x & 0xff0000))
//     let rec_node: RecNode = RecBinOp {
//         kind: BinOpKind::And,
//         lhs: 0xff.into(),
//         rhs: RecBinOp {
//             kind: BinOpKind::Or,
//             lhs: RecBinOp {
//                 kind: BinOpKind::And,
//                 lhs: Var(0).into(),
//                 rhs: 0xff00.into(),
//             }
//             .into(),
//             rhs: RecBinOp {
//                 kind: BinOpKind::And,
//                 lhs: Var(0).into(),
//                 rhs: 0xff0000.into(),
//             }
//             .into(),
//         }
//         .into(),
//     }
//     .into();

//     let mut egraph = EGraph::from_rec_node(&rec_node);

//     let rule_set = RewriteRuleSet::from_rules([
//         // (x & 0) => 0
//         RewriteRuleParams {
//             query: BinOpTemplate {
//                 kind: BinOpKind::And,
//                 lhs: TemplateVar::new(1).into(),
//                 rhs: 0.into(),
//             }
//             .into(),
//             rewrite: 0.into(),
//             keep_original: false,
//             bi_directional: false,
//         },
//         // a & (b | c) => (a & b) | (a & c)
//         RewriteRuleParams {
//             query: BinOpTemplate {
//                 kind: BinOpKind::And,
//                 lhs: TemplateVar::new(1).into(),
//                 rhs: BinOpTemplate {
//                     kind: BinOpKind::Or,
//                     lhs: TemplateVar::new(2).into(),
//                     rhs: TemplateVar::new(3).into(),
//                 }
//                 .into(),
//             }
//             .into(),
//             rewrite: BinOpTemplate {
//                 kind: BinOpKind::Or,
//                 lhs: BinOpTemplate {
//                     kind: BinOpKind::And,
//                     lhs: TemplateVar::new(1).into(),
//                     rhs: TemplateVar::new(2).into(),
//                 }
//                 .into(),
//                 rhs: BinOpTemplate {
//                     kind: BinOpKind::And,
//                     lhs: TemplateVar::new(1).into(),
//                     rhs: TemplateVar::new(3).into(),
//                 }
//                 .into(),
//             }
//             .into(),
//             keep_original: true,
//             bi_directional: true,
//         },
//         // a & (b & c) => (a & b) & c
//         RewriteRuleParams {
//             query: BinOpTemplate {
//                 kind: BinOpKind::And,
//                 lhs: TemplateVar::new(1).into(),
//                 rhs: BinOpTemplate {
//                     kind: BinOpKind::And,
//                     lhs: TemplateVar::new(2).into(),
//                     rhs: TemplateVar::new(3).into(),
//                 }
//                 .into(),
//             }
//             .into(),
//             rewrite: BinOpTemplate {
//                 kind: BinOpKind::And,
//                 lhs: BinOpTemplate {
//                     kind: BinOpKind::And,
//                     lhs: TemplateVar::new(1).into(),
//                     rhs: TemplateVar::new(2).into(),
//                 }
//                 .into(),
//                 rhs: TemplateVar::new(3).into(),
//             }
//             .into(),
//             keep_original: true,
//             bi_directional: true,
//         },
//         // a & b => b & a
//         RewriteRuleParams {
//             query: BinOpTemplate {
//                 kind: BinOpKind::And,
//                 lhs: TemplateVar::new(1).into(),
//                 rhs: TemplateVar::new(2).into(),
//             }
//             .into(),
//             rewrite: BinOpTemplate {
//                 kind: BinOpKind::And,
//                 lhs: TemplateVar::new(2).into(),
//                 rhs: TemplateVar::new(1).into(),
//             }
//             .into(),
//             keep_original: true,
//             bi_directional: true,
//         },
//     ]);

//     std::fs::write("/tmp/graph.dot", egraph.to_dot()).unwrap();

//     egraph.apply_rule_set(&rule_set);

//     std::fs::write("/tmp/graph2.dot", egraph.to_dot()).unwrap();

//     panic!();
// }
