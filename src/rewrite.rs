use crate::{did_anything::DidAnything, egraph::*, utils::CowBox};

/// a re-write rule.
pub trait Rewrite {
    /// the context that is accumulated when matching the re-write rule's query and is used to build the final re-write result.
    type Ctx;
    fn create_initial_ctx(&self) -> Self::Ctx;
    fn query(&self) -> CowBox<'_, dyn QueryENodeMatcher<Self::Ctx>>;
    fn query_structural_hash(&self, egraph: &EGraph) -> Option<u64>;
    fn build_rewrite(&self, ctx: Self::Ctx, egraph: &mut EGraph) -> AddENodeRes;
}

pub trait QueryENodeMatcher<C> {
    fn match_enode(
        &self,
        enode_id: ENodeId,
        enode: &ENode,
        egraph: &EGraph,
        ctx: &C,
    ) -> QueryMatchENodeRes<'_, C>;
}

pub trait QueryLinksMatcher<C> {
    fn match_links_amount(&self, links_amount: usize, ctx: C) -> Option<QueryMatch<C>>;
    fn get_link_matcher(&self, link_index: usize) -> CowBox<'_, dyn QueryLinkMatcher<C>>;
}

pub trait QueryLinkMatcher<C> {
    fn match_link(
        &self,
        link_eclass_id: EClassId,
        egraph: &EGraph,
        ctx: &C,
    ) -> QueryMatchLinkRes<'_, C>;
}

/// a match of a rewrite rule query.
pub struct QueryMatch<C> {
    /// the resulting context after matching.
    pub new_ctx: C,
}

/// the result of matching an enode against a re-write query.
pub enum QueryMatchENodeRes<'a, C> {
    /// the enode did not match the query.
    NoMatch,

    /// the enode fully matched the query.
    Match(QueryMatch<C>),

    /// the enode structually matched, but now we want to also match all of its links against the query.
    RecurseIntoLinks {
        /// the new ctx to use as a starting point when matching the links of this enode.
        // TODO: can we use `Cow` here instead of cloning it every single time?
        new_ctx: C,

        /// the links matcher to use when matching the enode's links.
        links_matcher: CowBox<'a, dyn QueryLinksMatcher<C>>,
    },
}

/// the result of matching an eclass against a re-write query.
pub enum QueryMatchLinkRes<'a, C> {
    /// the eclass did not match the query.
    NoMatch,

    /// the eclass fully matched the query
    Match(QueryMatch<C>),

    /// the eclass may potentially match, so recurse into each of its enodes and try to match each of them.
    RecurseIntoENodes {
        /// the new ctx to use as a starting point when matching the enodes of this eclass.
        new_ctx: C,

        /// the enode matcher to use when matching the enodes.
        enode_matcher: CowBox<'a, dyn QueryENodeMatcher<C>>,
    },
}
pub trait Rewrites: Sized {
    const LEN: usize;

    fn apply_rewrite(&self, rewrite_index: usize, egraph: &mut EGraph) -> DidAnything;

    fn push<R: Rewrite>(self, rewrite: R) -> (R, Self) {
        (rewrite, self)
    }

    fn len(&self) -> usize {
        Self::LEN
    }
}

impl Rewrites for () {
    const LEN: usize = 0;

    fn apply_rewrite(&self, _rewrite_index: usize, _egraph: &mut EGraph) -> DidAnything {
        unreachable!()
    }
}
impl<R: Rewrite> Rewrites for R {
    const LEN: usize = 1;

    fn apply_rewrite(&self, rewrite_index: usize, egraph: &mut EGraph) -> DidAnything {
        assert_eq!(rewrite_index, 0);
        egraph.apply_rewrite(self)
    }
}

impl<A: Rewrite, B: Rewrites> Rewrites for (A, B) {
    const LEN: usize = 1 + B::LEN;

    fn apply_rewrite(&self, rewrite_index: usize, egraph: &mut EGraph) -> DidAnything {
        assert!(rewrite_index <= Self::LEN);
        if rewrite_index + 1 == Self::LEN {
            // apply the current rule
            egraph.apply_rewrite(&self.0)
        } else {
            // apply some inner rule, recurse
            self.1.apply_rewrite(rewrite_index, egraph)
        }
    }
}

#[macro_export]
macro_rules! rewrites {
    () => { () };
    (...$rest:expr) => { $rest };
    ($a:expr) => { $crate::rewrites![$a,] };
    ($a:expr, $($tok:tt)*) => {
        ($a, $crate::rewrites![$($tok)*])
    };
}
