use std::borrow::Cow;

use crate::{
    egraph::{
        AddENodeRes, EGraph, EffectiveEClassId,
        rewrite::{QueryLinkMatcher, QueryMatchLinkRes, SimpleRewrite},
    },
    utils::CowBox,
};

#[derive(Clone)]
pub struct Ctx {}
impl Ctx {
    fn new() -> Self {
        Self {}
    }
}
pub struct TmpAttemptRewrite;
impl SimpleRewrite for TmpAttemptRewrite {
    type Ctx = Ctx;

    fn create_initial_ctx(&self) -> Self::Ctx {
        Ctx::new()
    }

    fn query(&self) -> CowBox<'_, dyn QueryLinkMatcher<Self::Ctx>> {
        CowBox::Borrowed(&RootLinkMatcher)
    }

    fn build_rewrite(&self, ctx: Self::Ctx, egraph: &mut EGraph) -> AddENodeRes {
        todo!()
    }
}

pub struct RootLinkMatcher;
impl QueryLinkMatcher<Ctx> for RootLinkMatcher {
    fn match_link<'a, 'c>(
        &'a self,
        link_effective_eclass_id: EffectiveEClassId,
        egraph: &EGraph,
        ctx: Cow<'c, Ctx>,
    ) -> QueryMatchLinkRes<'a, 'c, Ctx> {
        // in the root, we want a loop node
        todo!()
    }
}
