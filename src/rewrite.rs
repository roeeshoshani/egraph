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
    type Ctx;

    /// create the initial context to use when starting to match the root query of this re-write rule.
    fn create_initial_ctx(&self) -> Self::Ctx;

    /// returns the root query of this re-write rule.
    fn query(&self) -> CowBox<'_, dyn QueryENodeMatcher<Self::Ctx>>;

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
pub trait QueryENodeMatcher<C> {
    /// match the given enode with the given current context against this enode matcher.
    fn match_enode(
        &self,
        enode_id: ENodeId,
        enode: &ENode,
        egraph: &EGraph,
        ctx: &C,
    ) -> QueryMatchENodeRes<'_, C>;
}

/// an links matcher, as part of a re-write rule query. matches the links of an enode.
pub trait QueryLinksMatcher<C> {
    /// match the amount of links of the currently matched enode against this links matcher.
    /// gives the link matcher a way to discard this enode according to its amount of links.
    fn match_links_amount(&self, links_amount: usize, ctx: C) -> Option<QueryMatch<C>>;

    /// returns the link matcher for the link with the given index.
    fn get_link_matcher(&self, link_index: usize) -> CowBox<'_, dyn QueryLinkMatcher<C>>;
}

/// a link matcher, as part of a re-write rule query. matches a specific link of some enode.
pub trait QueryLinkMatcher<C> {
    /// match the given link, given the link's eclass id.
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

/// the result of matching a link against a re-write query.
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
