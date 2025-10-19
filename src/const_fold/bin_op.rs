use arrayvec::ArrayVec;

use crate::{egraph::*, node::*, rewrite::*, utils::CowBox};

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
impl Rewrite for ConstFoldRewrite {
    type Ctx = Ctx;

    fn create_initial_ctx(&self) -> Self::Ctx {
        Ctx::new()
    }

    fn query(&self) -> CowBox<'_, dyn QueryENodeMatcher<Self::Ctx>> {
        CowBox::Borrowed(&Query)
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

struct Query;
impl QueryENodeMatcher<Ctx> for Query {
    fn match_enode(
        &self,
        _enode_id: ENodeId,
        enode: &ENode,
        _egraph: &EGraph,
        ctx: &Ctx,
    ) -> QueryMatchENodeRes<'_, Ctx> {
        let Some(bin_op) = enode.as_bin_op() else {
            return QueryMatchENodeRes::NoMatch;
        };

        let mut new_ctx = ctx.clone();
        new_ctx.bin_op_kind = Some(bin_op.kind);

        QueryMatchENodeRes::RecurseIntoLinks {
            new_ctx,
            links_matcher: CowBox::Borrowed(&LinksMatcher),
        }
    }
}

struct LinksMatcher;
impl QueryLinksMatcher<Ctx> for LinksMatcher {
    fn match_links_amount(&self, links_amount: usize, ctx: Ctx) -> Option<QueryMatch<Ctx>> {
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
    fn match_link(
        &self,
        link_eclass_id: EClassId,
        egraph: &EGraph,
        ctx: &Ctx,
    ) -> QueryMatchLinkRes<'_, Ctx> {
        let Some(imm) = egraph
            .union_find()
            .enodes_in_eclass(link_eclass_id)
            .find_map(|enode_id| egraph[enode_id].as_imm())
        else {
            return QueryMatchLinkRes::NoMatch;
        };

        let mut new_ctx = ctx.clone();
        new_ctx.operands.push(*imm);

        QueryMatchLinkRes::Match(QueryMatch { new_ctx })
    }
}
