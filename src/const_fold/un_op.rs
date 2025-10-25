use crate::{egraph::*, node::*, rewrite::*, utils::CowBox};

/// the context for the un op constant folding re-write.
#[derive(Debug, Clone)]
pub struct Ctx {
    un_op_kind: Option<UnOpKind>,
    operand: Option<Imm>,
}
impl Ctx {
    fn new() -> Self {
        Self {
            un_op_kind: None,
            operand: None,
        }
    }
}

/// a unary operation constant folding re-write rule.
pub struct ConstFoldRewrite;
impl SimpleRewrite for ConstFoldRewrite {
    type Ctx = Ctx;

    fn create_initial_ctx(&self) -> Self::Ctx {
        Ctx::new()
    }

    fn query(&self) -> CowBox<'_, dyn QueryENodeMatcher<Self::Ctx>> {
        CowBox::Borrowed(&Query)
    }

    fn build_rewrite(&self, ctx: Self::Ctx, egraph: &mut EGraph) -> AddENodeRes {
        let Some(kind) = ctx.un_op_kind else {
            unreachable!()
        };
        let Some(operand) = ctx.operand else {
            unreachable!()
        };
        let res = kind.apply_to_imm(operand);
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
        let Some(un_op) = enode.as_un_op() else {
            return QueryMatchENodeRes::NoMatch;
        };

        let mut new_ctx = ctx.clone();
        new_ctx.un_op_kind = Some(un_op.kind);

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
        new_ctx.operand = Some(*imm);

        QueryMatchLinkRes::Match(QueryMatch { new_ctx })
    }
}
