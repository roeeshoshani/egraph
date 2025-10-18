use crate::{egraph::*, node::*, rewrite::*, utils::CowBox};

/// the context for the bin op constant folding re-write.
pub enum BinOpConstFoldRewriteCtx {
    /// initial state. at this state, we haven't captured any information about the binary operation that is about to be folded.
    Initial,

    /// we have captured the bin op kind.
    GotKind { kind: BinOpKind },

    /// we have captured the bin op kind and the lhs.
    GotKindAndLhs { kind: BinOpKind, lhs: Imm },

    /// we are done capturing all of the relevant information.
    Done { kind: BinOpKind, lhs: Imm, rhs: Imm },
}

/// a binary operation constant folding re-write rule.
pub struct BinOpConstFoldRewrite;
impl Rewrite for BinOpConstFoldRewrite {
    type Ctx = BinOpConstFoldRewriteCtx;

    fn create_initial_ctx(&self) -> Self::Ctx {
        BinOpConstFoldRewriteCtx::Initial
    }

    fn query(&self) -> CowBox<'_, dyn QueryENodeMatcher<Self::Ctx>> {
        CowBox::Borrowed(&BinOpConstFoldQuery)
    }

    fn query_structural_hash(&self, _egraph: &EGraph) -> Option<u64> {
        // we can't specify a specific structural hash, since we don't want to match a specific bin op kind, we want to match all of them.
        None
    }

    fn build_rewrite(&self, ctx: Self::Ctx, egraph: &mut EGraph) -> AddENodeRes {
        let BinOpConstFoldRewriteCtx::Done { kind, lhs, rhs } = ctx else {
            unreachable!()
        };
        let res = kind.apply_to_imms(lhs, rhs);
        egraph.add_enode(res.into())
    }
}

struct BinOpConstFoldQuery;
impl QueryENodeMatcher<BinOpConstFoldRewriteCtx> for BinOpConstFoldQuery {
    fn match_enode(
        &self,
        enode_id: ENodeId,
        enode: &ENode,
        egraph: &EGraph,
        ctx: &BinOpConstFoldRewriteCtx,
    ) -> QueryMatchENodeRes<'_, BinOpConstFoldRewriteCtx> {
        todo!()
    }
}
