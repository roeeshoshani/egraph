use std::borrow::Cow;

use arrayvec::ArrayVec;

use crate::{
    egraph::{rewrite::*, *},
    node::{imm::Imm, *},
    utils::CowBox,
};

/// the context for the bin op constant folding re-write.
#[derive(Debug, Clone)]
pub struct Ctx {
    bin_op_kind: Option<BinOpKind>,
    operands: ArrayVec<Imm, 2>,
}
impl Ctx {
    fn new() -> Self {
        Self {
            bin_op_kind: None,
            operands: ArrayVec::new(),
        }
    }
}

/// a binary operation constant folding re-write rule.
pub struct ConstFoldRewrite;
impl SimpleRewrite for ConstFoldRewrite {
    type Ctx = Ctx;

    fn create_initial_ctx(&self) -> Self::Ctx {
        Ctx::new()
    }

    fn query(&self) -> CowBox<'_, dyn QueryLinkMatcher<Self::Ctx>> {
        CowBox::Borrowed(&RootLinkMatcher)
    }

    fn build_rewrite(&self, ctx: Self::Ctx, egraph: &mut EGraph) -> AddENodeRes {
        let Some(kind) = ctx.bin_op_kind else {
            unreachable!()
        };
        let [lhs, rhs] = &ctx.operands[..] else {
            unreachable!()
        };
        let res = kind.apply_to_imms(*lhs, *rhs);
        egraph.add_enode(res.into())
    }
}

struct RootLinkMatcher;
impl QueryLinkMatcher<Ctx> for RootLinkMatcher {
    fn match_link<'a, 'c>(
        &'a self,
        _link_effective_eclass_id: EffectiveEClassId,
        _egraph: &EGraph,
        ctx: Cow<'c, Ctx>,
    ) -> QueryMatchLinkRes<'a, 'c, Ctx> {
        QueryMatchLinkRes::RecurseIntoENodes {
            new_ctx: ctx,
            enode_matcher: CowBox::Borrowed(&RootNodeMatcher),
        }
    }
}

struct RootNodeMatcher;
impl QueryENodeMatcher<Ctx> for RootNodeMatcher {
    fn match_enode<'a, 'c>(
        &'a self,
        _enode_id: ENodeId,
        enode: &ENode,
        _egraph: &EGraph,
        ctx: Cow<'c, Ctx>,
    ) -> QueryMatchENodeRes<'a, 'c, Ctx> {
        let Some(bin_op) = enode.as_bin_op() else {
            return QueryMatchENodeRes::NoMatch;
        };

        let mut new_ctx = ctx.into_owned();
        new_ctx.bin_op_kind = Some(bin_op.kind);

        QueryMatchENodeRes::RecurseIntoLinks {
            new_ctx: Cow::Owned(new_ctx),
            links_matcher: CowBox::Borrowed(&LinksMatcher),
        }
    }
}
struct LinksMatcher;
impl QueryLinksMatcher<Ctx> for LinksMatcher {
    fn match_links_amount<'c>(
        &self,
        links_amount: usize,
        ctx: Cow<'c, Ctx>,
    ) -> Option<QueryMatch<'c, Ctx>> {
        if links_amount != 2 {
            return None;
        }
        Some(QueryMatch { new_ctx: ctx })
    }

    fn get_link_matcher(&self, link_index: usize) -> CowBox<'_, dyn QueryLinkMatcher<Ctx>> {
        match link_index {
            0 | 1 => CowBox::Borrowed(&OperandMatcher),
            _ => unreachable!(),
        }
    }
}

struct OperandMatcher;
impl QueryLinkMatcher<Ctx> for OperandMatcher {
    fn match_link<'a, 'c>(
        &'a self,
        link_effective_eclass_id: EffectiveEClassId,
        egraph: &EGraph,
        ctx: Cow<'c, Ctx>,
    ) -> QueryMatchLinkRes<'a, 'c, Ctx> {
        let Some(imm) = egraph
            .union_find()
            .enodes_in_effective_eclass(link_effective_eclass_id)
            .find_map(|enode| enode.as_imm())
        else {
            return QueryMatchLinkRes::NoMatch;
        };

        let mut new_ctx = ctx.into_owned();
        new_ctx.operands.push(*imm);

        QueryMatchLinkRes::Match(QueryMatch {
            new_ctx: Cow::Owned(new_ctx),
        })
    }
}
