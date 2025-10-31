use std::borrow::Cow;

use crate::{did_anything::DidAnything, egraph::*, union_find::UnionRes, utils::CowBox};

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

impl EGraph {
    /// applies the given simple re-write rule.
    pub fn apply_simple_rewrite<R: SimpleRewrite>(&mut self, rewrite: &R) -> DidAnything {
        let matches = self.match_simple_rewrite(rewrite);
        self.handle_simple_rewrite_matches(matches, rewrite)
    }

    /// handles the given matches of a simple re-write rule.
    pub fn handle_simple_rewrite_matches<R: SimpleRewrite>(
        &mut self,
        matches: Vec<SimpleRewriteMatch<R::Ctx>>,
        rewrite: &R,
    ) -> DidAnything {
        let mut did_anything = DidAnything::False;

        // for each match, add the rerwrite result to the egraph
        for match_obj in matches {
            let add_res = rewrite.build_rewrite(match_obj.final_ctx, self);

            let union_res = self.union_find_mut().union_enodes(
                add_res.eclass_id.enode_id,
                match_obj.effective_eclass_id.eclass_root,
            );
            if add_res.dedup_info == ENodeDedupInfo::New || union_res == UnionRes::New {
                did_anything = DidAnything::True;
            }
        }

        did_anything
    }

    /// matches the given simple re-write rule against all eclasses and enodes in the egraph, returning all matches.
    pub fn match_simple_rewrite<R: SimpleRewrite>(
        &self,
        rewrite: &R,
    ) -> Vec<SimpleRewriteMatch<R::Ctx>> {
        let mut matches = Vec::new();

        let initial_ctx = rewrite.create_initial_ctx();
        let query = rewrite.query();

        for effective_eclass_id in self.union_find().effective_eclass_ids() {
            let mut match_ctxs = Vec::new();

            // match the current enode
            self.match_enode_link(
                effective_eclass_id,
                &*query,
                Cow::Borrowed(&initial_ctx),
                &mut match_ctxs,
            );

            matches.extend(match_ctxs.into_iter().map(|ctx| SimpleRewriteMatch {
                final_ctx: ctx,
                effective_eclass_id,
            }));
        }

        matches
    }

    fn match_enode_link<C: Clone>(
        &self,
        link_effective_eclass_id: EffectiveEClassId,
        link_matcher: &dyn QueryLinkMatcher<C>,
        ctx: Cow<C>,
        match_ctxs: &mut Vec<C>,
    ) {
        match link_matcher.match_link(link_effective_eclass_id, self, ctx) {
            QueryMatchLinkRes::NoMatch => {
                return;
            }
            QueryMatchLinkRes::Match(QueryMatch { new_ctx }) => {
                match_ctxs.push(new_ctx.into_owned());
            }
            QueryMatchLinkRes::RecurseIntoENodes {
                new_ctx,
                enode_matcher,
            } => {
                // we want to pass the new ctx to each enode in the eclass.
                //
                // but, if the returned new ctx is owned, we don't want to clone it for each enode.
                //
                // so, we grab a reference to it here, and construct a borrowed cow for it in every iteration, for every enode, thus
                // avoiding the clone, even in the case where the new ctx is owned.
                let new_ctx_ref = match &new_ctx {
                    Cow::Borrowed(borrowed) => *borrowed,
                    Cow::Owned(owned_ctx) => owned_ctx,
                };
                for (enode_id, enode) in self
                    .union_find()
                    .enumerate_enodes_in_effective_eclass(link_effective_eclass_id)
                {
                    self.match_enode(
                        enode_id,
                        enode,
                        &*enode_matcher,
                        // cloning this is zero cost since it is borrowed.
                        Cow::Borrowed(new_ctx_ref),
                        match_ctxs,
                    )
                }
            }
        }
    }

    fn match_enode_links<C: Clone>(
        &self,
        enode: &ENode,
        links_matcher: &dyn QueryLinksMatcher<C>,
        ctx: Cow<C>,
        match_ctxs: &mut Vec<C>,
    ) {
        let enode_links = enode.links();
        let links_amount = enode_links.len();

        let Some(links_amount_match) = links_matcher.match_links_amount(links_amount, ctx) else {
            // no match
            return;
        };
        let QueryMatch { new_ctx } = links_amount_match;

        if links_amount == 0 {
            // no links, so we got a match
            match_ctxs.push(new_ctx.into_owned());
            return;
        }

        // now match the links.
        let mut cur_match_ctxs: Vec<C> = vec![new_ctx.into_owned()];
        let mut new_match_ctxs: Vec<C> = Vec::new();
        for cur_link_idx in 0..links_amount {
            let link_matcher = links_matcher.get_link_matcher(cur_link_idx);

            // the eclass that the current enode link points to
            let link_eclass_id = *enode_links[cur_link_idx];
            let effective_eclass_id = self.eclass_id_to_effective(link_eclass_id);

            // we want a cartesian product over match contexts from previous links, so try matching the link for each previous match
            for cur_ctx in &cur_match_ctxs {
                self.match_enode_link(
                    effective_eclass_id,
                    &*link_matcher,
                    Cow::Borrowed(cur_ctx),
                    &mut new_match_ctxs,
                );
            }

            // new match contexts now contains the new cartesian product over the current link with all of its previous link.
            //
            // we want to use this new list of match contexts for matching the next link.
            //
            // so basically we want to set `cur_match_ctxs` to `new_match_ctxs`, and to clear `new_match_ctxs` in preparation for the
            // next iteration.
            //
            // but, doing this will lose the storage that was already allocated in the `cur_match_ctxs` vector, which we will then have
            // to re-allocate when re-building the `new_match_ctxs` list.
            //
            // so, instead, we perform a swap to keep both allocations.
            std::mem::swap(&mut cur_match_ctxs, &mut new_match_ctxs);

            // after the swap, `cur_match_ctxs` contains the value of `new_match_ctxs`, which is the list of match contexts that we
            // just generated.
            // and, `new_match_ctxs` now contains the value of `cur_match_ctxs`, which is the match contexts from the previous link.
            // we no longer need the match contexts from the previous link, and each iteration assumes that `new_match_ctxs` is empty
            // at the start of the iteration, so clear the vector.
            new_match_ctxs.clear();
        }

        // the final value of `cur_match_ctxs` contains the final cartesian product of match contexts, which is what we want to return.
        // so, copy it out.
        match_ctxs.append(&mut cur_match_ctxs);
    }

    fn match_enode<C: Clone>(
        &self,
        enode_id: ENodeId,
        enode: &ENode,
        matcher: &dyn QueryENodeMatcher<C>,
        ctx: Cow<C>,
        match_ctxs: &mut Vec<C>,
    ) {
        match matcher.match_enode(enode_id, enode, self, ctx) {
            QueryMatchENodeRes::NoMatch => {
                return;
            }
            QueryMatchENodeRes::Match(QueryMatch { new_ctx }) => {
                match_ctxs.push(new_ctx.into_owned());
            }
            QueryMatchENodeRes::RecurseIntoLinks {
                new_ctx,
                links_matcher,
            } => {
                self.match_enode_links(enode, &*links_matcher, new_ctx, match_ctxs);
            }
        }
    }
}

/// a match of a simple re-write rule.
#[derive(Debug, Clone)]
pub struct SimpleRewriteMatch<C> {
    /// the final ctx of this match.
    pub final_ctx: C,

    /// the effective eclass id that matched this rule.
    pub effective_eclass_id: EffectiveEClassId,
}
