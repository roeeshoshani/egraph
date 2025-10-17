use std::num::NonZeroUsize;

use crate::{utils::CowBox, *};

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
pub struct RewriteRuleParams {
    pub query: ENodeTemplate,
    pub rewrite: TemplateLink,

    /// should we keep the original enode after the rule has been applied to it?
    pub keep_original: bool,

    /// is the rule bi-directional
    pub bi_directional: bool,
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

    fn swap_direction(self) -> Self {
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

#[derive(Debug, Clone)]
pub struct RewriteRuleSet {
    rules: Vec<RewriteRule>,
}
impl RewriteRuleSet {
    pub fn new() -> Self {
        Self { rules: Vec::new() }
    }

    pub fn add(&mut self, rule: RewriteRuleParams) {
        let basic_rule = RewriteRule::new(rule.query, rule.rewrite, rule.keep_original);

        self.rules.push(basic_rule.clone());

        if rule.bi_directional {
            self.rules.push(basic_rule.swap_direction());
        }
    }

    pub fn add_multiple<I>(&mut self, rules: I)
    where
        I: IntoIterator<Item = RewriteRuleParams>,
    {
        for rule in rules {
            self.add(rule);
        }
    }

    pub fn from_rules<I>(rules: I) -> Self
    where
        I: IntoIterator<Item = RewriteRuleParams>,
    {
        let mut res = Self::new();
        res.add_multiple(rules);
        res
    }

    pub fn rules(&self) -> &[RewriteRule] {
        &self.rules
    }
}

/// a re-write rule.
pub trait Rewrite {
    /// the context that is accumulated when matching the re-write rule's query and is used to build the final re-write result.
    type Ctx;
    type Query: QueryENodeMatcher<Self::Ctx>;
    fn build_rewrite();
    fn query_structural_hash(&self, egraph: &EGraph) -> Option<u64>;
}

pub trait QueryENodeMatcher<C> {
    fn match_enode(&self, enode_id: ENodeId, egraph: &EGraph, ctx: &C) -> QueryMatchENodeRes<C>;

    fn links_amount(&self) -> usize;

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

/// a partial match of a rewrite rule query. this is a match against some part of the query.
pub struct QueryPartialMatch<C> {
    /// the resulting context after matching the matched part.
    pub new_ctx: C,
}

/// the result of matching an enode against a re-write query.
pub enum QueryMatchENodeRes<C> {
    /// the enode did not match the query.
    NoMatch,

    /// the enode fully matched the query.
    Match(QueryPartialMatch<C>),

    /// the enode structually matched, but now we want to also match all of its links against the query.
    RecurseIntoLinks {
        /// the new ctx to use as a starting point when matching the links of this enode.
        // TODO: can we use `Cow` here instead of cloning it every single time?
        new_ctx: C,
    },
}

/// the result of matching an eclass against a re-write query.
pub enum QueryMatchLinkRes<'a, C> {
    /// the eclass did not match the query.
    NoMatch,

    /// the eclass fully matched the query
    Match(QueryPartialMatch<C>),

    /// the eclass may potentially match, so recurse into each of its enodes and try to match each of them.
    RecurseIntoENodes {
        /// the new ctx to use as a starting point when matching the enodes of this eclass.
        new_ctx: C,

        /// the enode matcher to use when matching the enodes.
        enode_matcher: CowBox<'a, dyn QueryENodeMatcher<C>>,
    },
}

#[derive(Debug, Clone)]
struct TemplateRewriteCtx {
    /// the rule storage which contains all the captured information while matching.
    pub rule_storage: RewriteRuleStorage,

    /// a list of eclass ids that we have already recursed along the current path that is being matched.
    pub recursed_eclasses: Vec<EffectiveEClassId>,
}
impl TemplateRewriteCtx {
    /// creates a new empty match context.
    pub fn new() -> Self {
        Self {
            rule_storage: RewriteRuleStorage::new(),
            recursed_eclasses: Vec::new(),
        }
    }
    /// checks if we have already recursed the given eclass along the current path that is being matched.
    pub fn has_recursed_eclass(&self, effective_eclass_id: EffectiveEClassId) -> bool {
        self.recursed_eclasses.contains(&effective_eclass_id)
    }

    /// creates a clone of this match context but with an added recursed eclass id.
    pub fn with_added_recursed_eclass(&self, effective_eclass_id: EffectiveEClassId) -> Self {
        let mut res = self.clone();
        res.recursed_eclasses.push(effective_eclass_id);
        res
    }
}

impl QueryENodeMatcher<TemplateRewriteCtx> for ENodeTemplate {
    fn match_enode(
        &self,
        enode_id: ENodeId,
        egraph: &EGraph,
        ctx: &TemplateRewriteCtx,
    ) -> QueryMatchENodeRes<TemplateRewriteCtx> {
        let enode = &egraph[enode_id];
        if enode.convert_links(|_| ()) != self.convert_links(|_| ()) {
            // no match
            return QueryMatchENodeRes::NoMatch;
        }
        QueryMatchENodeRes::RecurseIntoLinks {
            new_ctx: ctx.clone(),
        }
    }

    fn links_amount(&self) -> usize {
        self.links().len()
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
            TemplateLink::Specific(enode_template) => {
                // when matching an eclass against a specific template link, make sure that we haven't recursed this eclass already,
                // to prevent following eclass loops which will blow up the graph with redundant expressions.
                //
                // NOTE: this is only done in the case of matching against a specific template link, and not when matching against
                // a template variable. this is intentional, since we want to allow loops in our templates, for example, we want to
                // allow the following template `x & (1 + x)`.
                // additionally, note that in the case of matching against template variables, we don't need to worry about graph
                // blow up, since template variables don't cause expansions of eclasses, they only check for matching, unlike specific
                // template links, which cause expansion of an eclass to each of its enode forms.
                if ctx.has_recursed_eclass(effective_eclass_id) {
                    // don't loop
                    return QueryMatchLinkRes::NoMatch;
                }

                QueryMatchLinkRes::RecurseIntoENodes {
                    new_ctx: ctx.with_added_recursed_eclass(effective_eclass_id),
                    enode_matcher: CowBox::Borrowed(&**enode_template),
                }
            }
            TemplateLink::Var(template_var) => {
                match ctx.rule_storage.template_var_values.get(*template_var) {
                    Some(existing_var_value) => {
                        if effective_eclass_id != existing_var_value.effective_eclass_id {
                            // no match
                            return QueryMatchLinkRes::NoMatch;
                        }

                        // we got a match, and we don't need to change the rule storage at all since we didn't bind any new template vars.
                        QueryMatchLinkRes::Match(QueryPartialMatch {
                            new_ctx: ctx.clone(),
                        })
                    }
                    None => {
                        // the variable currently doesn't have any value, so we can bind it and consider it a match.
                        let mut new_ctx = ctx.clone();
                        new_ctx.rule_storage.template_var_values.set(
                            *template_var,
                            TemplateVarValue {
                                effective_eclass_id,
                            },
                        );
                        QueryMatchLinkRes::Match(QueryPartialMatch { new_ctx })
                    }
                }
            }
        }
    }
}
