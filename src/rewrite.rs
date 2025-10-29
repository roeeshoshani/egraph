use std::borrow::Cow;

use crate::{did_anything::DidAnything, egraph::*, utils::CowBox};

/// a re-write rule.
pub trait Rewrite {
    /// applies this re-write rule to the egraph. returns whether applying the rule actually did anything.
    fn apply(&self, egraph: &mut EGraph) -> DidAnything;
}

/// a simple re-write rule which follows the generic e-matching algorithm.
pub trait SimpleRewrite {
    /// the context that is accumulated when matching the re-write rule's query and is used to build the
    /// final re-write result.
    type Ctx: Clone;

    /// create the initial context to use when starting to match the root query of this re-write rule.
    fn create_initial_ctx(&self) -> Self::Ctx;

    /// returns the root query of this re-write rule.
    fn query(&self) -> CowBox<'_, dyn QueryLinkMatcher<Self::Ctx>>;

    /// builds the re-write result of this re-write rule into the egraph given the final match context generated
    /// while matching a specific enode to this rule.
    fn build_rewrite(&self, ctx: Self::Ctx, egraph: &mut EGraph) -> AddENodeRes;
}

impl<T: SimpleRewrite> Rewrite for T {
    fn apply(&self, egraph: &mut EGraph) -> DidAnything {
        egraph.apply_simple_rewrite(self)
    }
}

/// an enode matcher, as part of a re-write rule query. matches enodes in the egraph.
pub trait QueryENodeMatcher<C: Clone> {
    /// match the given enode with the given current context against this enode matcher.
    fn match_enode<'a, 'c>(
        &'a self,
        enode_id: ENodeId,
        enode: &ENode,
        egraph: &EGraph,
        ctx: Cow<'c, C>,
    ) -> QueryMatchENodeRes<'a, 'c, C>;
}

/// an links matcher, as part of a re-write rule query. matches the links of an enode.
pub trait QueryLinksMatcher<C: Clone> {
    /// match the amount of links of the currently matched enode against this links matcher.
    /// gives the link matcher a way to discard this enode according to its amount of links.
    fn match_links_amount<'c>(
        &self,
        links_amount: usize,
        ctx: Cow<'c, C>,
    ) -> Option<QueryMatch<'c, C>>;

    /// returns the link matcher for the link with the given index.
    fn get_link_matcher(&self, link_index: usize) -> CowBox<'_, dyn QueryLinkMatcher<C>>;
}

/// a link matcher, as part of a re-write rule query. matches a specific link of some enode.
pub trait QueryLinkMatcher<C: Clone> {
    /// match the given link, given the link's eclass id.
    fn match_link<'a, 'c>(
        &'a self,
        link_effective_eclass_id: EffectiveEClassId,
        egraph: &EGraph,
        ctx: Cow<'c, C>,
    ) -> QueryMatchLinkRes<'a, 'c, C>;
}

/// a match of a rewrite rule query.
pub struct QueryMatch<'c, C: Clone> {
    /// the resulting context after matching.
    pub new_ctx: Cow<'c, C>,
}

/// the result of matching an enode against a re-write query.
pub enum QueryMatchENodeRes<'a, 'c, C: Clone> {
    /// the enode did not match the query.
    NoMatch,

    /// the enode fully matched the query.
    Match(QueryMatch<'c, C>),

    /// the enode structually matched, but now we want to also match all of its links against the query.
    RecurseIntoLinks {
        /// the new ctx to use as a starting point when matching the links of this enode.
        new_ctx: Cow<'c, C>,

        /// the links matcher to use when matching the enode's links.
        links_matcher: CowBox<'a, dyn QueryLinksMatcher<C>>,
    },
}

/// the result of matching a link against a re-write query.
pub enum QueryMatchLinkRes<'a, 'c, C: Clone> {
    /// the eclass did not match the query.
    NoMatch,

    /// the eclass fully matched the query
    Match(QueryMatch<'c, C>),

    /// the eclass may potentially match, so recurse into each of its enodes and try to match each of them.
    RecurseIntoENodes {
        /// the new ctx to use as a starting point when matching the enodes of this eclass.
        new_ctx: Cow<'c, C>,

        /// the enode matcher to use when matching the enodes.
        enode_matcher: CowBox<'a, dyn QueryENodeMatcher<C>>,
    },
}

/// a convenience macro for defining a set of re-write rules stored as an array of boxed rules.
#[macro_export]
macro_rules! rewrites_arr {
    () => (
        []
    );
    ($($x:expr),+ $(,)?) => (
        {
            let ___result: [Box<dyn $crate::rewrite::Rewrite>; _] = [
                $(
                    Box::new($x)
                ),+
            ];
            ___result
        }
    );
}
