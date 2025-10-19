use enum_variant_accessors::EnumIsVariant;

use crate::{egraph::*, node::*, rewrite::*, utils::CowBox};

/// the context for the un op constant folding re-write.
#[derive(Debug, EnumIsVariant)]
pub enum Ctx {
    /// initial state. at this state, we haven't captured any information about the unary operation that is about to be folded.
    Initial,

    /// we have captured the un op kind.
    GotKind { kind: UnOpKind },

    /// we are done capturing all of the relevant information.
    Done { kind: UnOpKind, operand: Imm },
}

/// a unary operation constant folding re-write rule.
pub struct ConstFoldRewrite;
impl Rewrite for ConstFoldRewrite {
    type Ctx = Ctx;

    fn create_initial_ctx(&self) -> Self::Ctx {
        Ctx::Initial
    }

    fn query(&self) -> CowBox<'_, dyn QueryENodeMatcher<Self::Ctx>> {
        CowBox::Borrowed(&Query)
    }

    fn build_rewrite(&self, ctx: Self::Ctx, egraph: &mut EGraph) -> AddENodeRes {
        let Ctx::Done { kind, operand } = ctx else {
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
        assert!(ctx.is_initial());
        let Some(un_op) = enode.as_un_op() else {
            return QueryMatchENodeRes::NoMatch;
        };
        QueryMatchENodeRes::RecurseIntoLinks {
            new_ctx: Ctx::GotKind { kind: un_op.kind },
            links_matcher: CowBox::Borrowed(&LinksMatcher),
        }
    }
}

struct LinksMatcher;
impl QueryLinksMatcher<Ctx> for LinksMatcher {
    fn match_links_amount(&self, links_amount: usize, ctx: Ctx) -> Option<QueryMatch<Ctx>> {
        assert!(ctx.is_got_kind());
        if links_amount != 1 {
            return None;
        }
        Some(QueryMatch { new_ctx: ctx })
    }

    fn get_link_matcher(&self, link_index: usize) -> CowBox<'_, dyn QueryLinkMatcher<Ctx>> {
        match link_index {
            0 => CowBox::Borrowed(&OperandMatcher),
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
        let Ctx::GotKind { kind } = ctx else {
            unreachable!()
        };
        let Some(imm) = egraph
            .union_find()
            .enodes_in_eclass(link_eclass_id)
            .find_map(|enode_id| egraph[enode_id].as_imm())
        else {
            return QueryMatchLinkRes::NoMatch;
        };
        QueryMatchLinkRes::Match(QueryMatch {
            new_ctx: Ctx::Done {
                kind: *kind,
                operand: *imm,
            },
        })
    }
}
