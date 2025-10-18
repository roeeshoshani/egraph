use std::num::NonZeroUsize;

use crate::{did_anything::DidAnything, egraph::*, node::*, utils::CowBox};

/// a variable in a template enode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TemplateVar {
    pub id: NonZeroUsize,
}
impl TemplateVar {
    pub fn new(id: usize) -> Self {
        Self {
            id: NonZeroUsize::new(id).unwrap(),
        }
    }
    /// the index of the variable in a variables vector
    fn index(&self) -> usize {
        self.id.get() - 1
    }
}

/// a link in a template enode.
#[derive(Debug, Clone)]
pub enum TemplateLink {
    Specific(Box<ENodeTemplate>),
    Var(TemplateVar),
}
impl TemplateLink {
    fn max_template_var_id(&self) -> Option<NonZeroUsize> {
        match self {
            TemplateLink::Specific(generic_node) => generic_node.max_template_var_id(),
            TemplateLink::Var(template_var) => Some(template_var.id),
        }
    }
    fn does_use_template_var(&self, template_var: TemplateVar) -> bool {
        match self {
            TemplateLink::Specific(generic_node) => {
                generic_node.does_use_template_var(template_var)
            }
            TemplateLink::Var(cur_template_var) => *cur_template_var == template_var,
        }
    }
}
impl From<TemplateVar> for TemplateLink {
    fn from(x: TemplateVar) -> Self {
        Self::Var(x)
    }
}
impl<T> From<T> for TemplateLink
where
    ENodeTemplate: From<T>,
{
    fn from(x: T) -> Self {
        Self::Specific(Box::new(x.into()))
    }
}

pub type BinOpTemplate = BinOp<TemplateLink>;
pub type UnOpTemplate = UnOp<TemplateLink>;

/// an enode template.
pub type ENodeTemplate = GenericNode<TemplateLink>;
impl ENodeTemplate {
    fn max_template_var_id(&self) -> Option<NonZeroUsize> {
        self.links()
            .into_iter()
            .filter_map(|link| link.max_template_var_id())
            .max()
    }
    fn does_use_template_var(&self, template_var: TemplateVar) -> bool {
        self.links()
            .into_iter()
            .any(|link| link.does_use_template_var(template_var))
    }
}

#[derive(Debug, Clone)]
pub struct RewriteRule {
    pub query: ENodeTemplate,
    pub rewrite: TemplateLink,

    /// should we keep the original enode after the rule has been applied to it?
    pub keep_original: bool,
}
impl RewriteRule {
    pub fn new(query: ENodeTemplate, rewrite: TemplateLink, keep_original: bool) -> Self {
        let res = Self {
            query,
            rewrite,
            keep_original,
        };
        res.check();
        res
    }

    // checks that this re-write rule even makes sense to exist.
    fn check(&self) {
        match self.query.max_template_var_id() {
            Some(max_var_id) => {
                // the query uses some variables

                // make sure that there are no gaps in the variable ids
                for i in 1..=max_var_id.get() {
                    let does_use_var = self.query.does_use_template_var(TemplateVar {
                        id: {
                            // SAFETY: we start iterating from 1
                            unsafe { NonZeroUsize::new_unchecked(i) }
                        },
                    });
                    assert!(does_use_var);
                }

                // make sure that the re-write doesn't use variables that don't exist in the query
                if let Some(rewrite_max_var_id) = self.rewrite.max_template_var_id() {
                    assert!(rewrite_max_var_id <= max_var_id)
                }
            }
            None => {
                // the query doesn't use any values

                // make sure that the re-write also doesn't use any variables
                assert_eq!(self.rewrite.max_template_var_id(), None);
            }
        }
    }

    // checks that this re-write rule can swap directions (swap the query and the rewrite).
    //
    // this assumes that the rule was already checked using the basic sanity checks.
    fn check_can_swap_direction(&self) {
        // swapping direction without setting `keep_original` doesn't make sense, since not setting `keep_original` means that the
        // original representation is worthless, and now we suddenly want to swap direction to generate that original representation?
        //
        // additionally, in the context of bi-directional rules, swapping direction without setting `keep_original` doesn't make
        // sense, since it will just make the egraph go back and forth between two representations, which is completely pointless.
        assert!(self.keep_original);

        match self.query.max_template_var_id() {
            Some(max_var_id) => {
                // the query uses some variables

                // make sure that both the query and the rewrite use all template vars
                for i in 1..=max_var_id.get() {
                    let var = TemplateVar {
                        id: {
                            // SAFETY: we start iterating from 1
                            unsafe { NonZeroUsize::new_unchecked(i) }
                        },
                    };
                    assert!(self.query.does_use_template_var(var));
                    assert!(self.rewrite.does_use_template_var(var));
                }
            }
            None => {
                // the query doesn't use any values, so it can swap directions
            }
        }
    }

    pub fn swap_direction(self) -> Self {
        self.check_can_swap_direction();
        let rewrite_template = match self.rewrite {
            TemplateLink::Specific(x) => x,
            TemplateLink::Var(_) => {
                panic!("can't reverse a rule whose re-write is just a single template var")
            }
        };
        Self {
            query: *rewrite_template,
            rewrite: TemplateLink::Specific(self.query.into()),
            keep_original: self.keep_original,
        }
    }
}

fn instantiate_template_link(
    template_link: &TemplateLink,
    ctx: &TemplateRewriteCtx,
    egraph: &mut EGraph,
) -> AddENodeRes {
    match template_link {
        TemplateLink::Specific(inner_template) => {
            let enode = instantiate_template(inner_template, ctx, egraph);
            egraph.add_enode(enode)
        }
        TemplateLink::Var(template_var) => {
            let var_value = ctx.template_var_values.get(*template_var).unwrap();
            AddENodeRes {
                eclass_id: var_value.effective_eclass_id.to_eclass_id(),
                dedup_info: ENodeDedupInfo::Duplicate,
            }
        }
    }
}

fn instantiate_template(
    template: &ENodeTemplate,
    ctx: &TemplateRewriteCtx,
    egraph: &mut EGraph,
) -> ENode {
    template.convert_links(|template_link| {
        instantiate_template_link(template_link, ctx, egraph).eclass_id
    })
}

impl Rewrite for RewriteRule {
    type Ctx = TemplateRewriteCtx;

    fn create_initial_ctx(&self) -> Self::Ctx {
        TemplateRewriteCtx::new()
    }

    fn query(&self) -> CowBox<'_, dyn QueryENodeMatcher<Self::Ctx>> {
        CowBox::Borrowed(&self.query)
    }

    fn query_structural_hash(&self, egraph: &EGraph) -> Option<u64> {
        Some(egraph.node_hasher().hash_node(&self.query))
    }

    fn build_rewrite(&self, ctx: Self::Ctx, egraph: &mut EGraph) -> AddENodeRes {
        instantiate_template_link(&self.rewrite, &ctx, egraph)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TemplateVarValue {
    pub effective_eclass_id: EffectiveEClassId,
}

#[derive(Debug, Clone)]
pub struct TemplateVarValues(pub Vec<Option<TemplateVarValue>>);
impl TemplateVarValues {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn get(&self, var: TemplateVar) -> Option<TemplateVarValue> {
        self.0.get(var.index()).copied()?
    }
    pub fn set(&mut self, var: TemplateVar, value: TemplateVarValue) {
        let index = var.index();
        if !(index < self.0.len()) {
            // the vector is not large enough
            self.0.resize(index + 1, None);
        }
        self.0[index] = Some(value);
    }
}

#[derive(Debug, Clone)]
pub struct RewriteRuleStorage {
    pub template_var_values: TemplateVarValues,
}
impl RewriteRuleStorage {
    pub fn new() -> Self {
        Self {
            template_var_values: TemplateVarValues::new(),
        }
    }
}

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

#[derive(Debug, Clone)]
pub struct TemplateRewriteCtx {
    pub template_var_values: TemplateVarValues,
}
impl TemplateRewriteCtx {
    /// creates a new empty match context.
    pub fn new() -> Self {
        Self {
            template_var_values: TemplateVarValues::new(),
        }
    }
}

impl QueryENodeMatcher<TemplateRewriteCtx> for ENodeTemplate {
    fn match_enode(
        &self,
        _enode_id: ENodeId,
        enode: &ENode,
        _egraph: &EGraph,
        ctx: &TemplateRewriteCtx,
    ) -> QueryMatchENodeRes<'_, TemplateRewriteCtx> {
        if enode.convert_links(|_| ()) != self.convert_links(|_| ()) {
            // no match
            return QueryMatchENodeRes::NoMatch;
        }
        QueryMatchENodeRes::RecurseIntoLinks {
            new_ctx: ctx.clone(),
            links_matcher: CowBox::Borrowed(self),
        }
    }
}

impl QueryLinksMatcher<TemplateRewriteCtx> for ENodeTemplate {
    fn match_links_amount(
        &self,
        links_amount: usize,
        ctx: TemplateRewriteCtx,
    ) -> Option<QueryMatch<TemplateRewriteCtx>> {
        if links_amount != self.links().len() {
            return None;
        }
        Some(QueryMatch { new_ctx: ctx })
    }

    fn get_link_matcher(
        &self,
        link_index: usize,
    ) -> CowBox<'_, dyn QueryLinkMatcher<TemplateRewriteCtx>> {
        CowBox::Borrowed(self.links()[link_index])
    }
}

impl QueryLinkMatcher<TemplateRewriteCtx> for TemplateLink {
    fn match_link(
        &self,
        link_eclass_id: EClassId,
        egraph: &EGraph,
        ctx: &TemplateRewriteCtx,
    ) -> QueryMatchLinkRes<'_, TemplateRewriteCtx> {
        let effective_eclass_id = link_eclass_id.to_effective(egraph.enodes_union_find());
        match self {
            TemplateLink::Specific(enode_template) => QueryMatchLinkRes::RecurseIntoENodes {
                new_ctx: ctx.clone(),
                enode_matcher: CowBox::Borrowed(&**enode_template),
            },
            TemplateLink::Var(template_var) => {
                match ctx.template_var_values.get(*template_var) {
                    Some(existing_var_value) => {
                        if effective_eclass_id != existing_var_value.effective_eclass_id {
                            // no match
                            return QueryMatchLinkRes::NoMatch;
                        }

                        // we got a match, and we don't need to change the rule storage at all since we didn't bind any new template vars.
                        QueryMatchLinkRes::Match(QueryMatch {
                            new_ctx: ctx.clone(),
                        })
                    }
                    None => {
                        // the variable currently doesn't have any value, so we can bind it and consider it a match.
                        let mut new_ctx = ctx.clone();
                        new_ctx.template_var_values.set(
                            *template_var,
                            TemplateVarValue {
                                effective_eclass_id,
                            },
                        );
                        QueryMatchLinkRes::Match(QueryMatch { new_ctx })
                    }
                }
            }
        }
    }
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
macro_rules! rewrites_arr {
    () => { () };
    (...$rest:expr) => { $rest };
    ($a:expr) => { $crate::rewrites_arr![$a,] };
    ($a:expr, $($tok:tt)*) => {
        ($a, $crate::rewrites_arr![$($tok)*])
    };
}
