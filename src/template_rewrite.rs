use std::num::NonZeroUsize;

use crate::{egraph::*, node::*, rewrite::*, utils::CowBox};

/// a template variable, used as a wildcard which matches everything when writing templates.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TemplateVar {
    pub id: NonZeroUsize,
}
impl TemplateVar {
    /// creates a new template variable with the given id. the id must be non-zero.
    pub fn new(id: usize) -> Self {
        Self {
            id: NonZeroUsize::new(id).unwrap(),
        }
    }

    /// the index of the variable in a variables array
    fn index(&self) -> usize {
        self.id.get() - 1
    }
}

/// a link in a template node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TemplateLink {
    /// this link matches another nested structure.
    Specific(Box<TemplateNode>),

    /// this link is a wildcard variable. it can match any value, and it will be substituted when instantiating the template.
    Var(TemplateVar),
}

impl TemplateLink {
    /// returns the id of the template var with the highest id that is contained in this template link.
    fn max_template_var_id(&self) -> Option<NonZeroUsize> {
        match self {
            TemplateLink::Specific(generic_node) => generic_node.max_template_var_id(),
            TemplateLink::Var(template_var) => Some(template_var.id),
        }
    }

    /// checks if this template link uses the given template var.
    fn does_use_template_var(&self, template_var: TemplateVar) -> bool {
        match self {
            TemplateLink::Specific(generic_node) => {
                generic_node.does_use_template_var(template_var)
            }
            TemplateLink::Var(cur_template_var) => *cur_template_var == template_var,
        }
    }
}

/// a template node which represents some node structure with wildcard variables.
pub type TemplateNode = GenericNode<TemplateLink>;

impl TemplateNode {
    /// returns the id of the template var with the highest id that is contained in this template node.
    fn max_template_var_id(&self) -> Option<NonZeroUsize> {
        self.links()
            .iter()
            .filter_map(|link| link.max_template_var_id())
            .max()
    }

    /// checks if this template node uses the given template var.
    fn does_use_template_var(&self, template_var: TemplateVar) -> bool {
        self.links()
            .iter()
            .any(|link| link.does_use_template_var(template_var))
    }
}

// convert a template var to a template link.
impl From<TemplateVar> for TemplateLink {
    fn from(x: TemplateVar) -> Self {
        Self::Var(x)
    }
}

// anything that can be converted to a template node should also be convertible to a template link.
impl<T> From<T> for TemplateLink
where
    TemplateNode: From<T>,
{
    fn from(x: T) -> Self {
        Self::Specific(Box::new(x.into()))
    }
}

/// a bin op which uses template nodes.
pub type TemplateBinOp = BinOp<TemplateLink>;

/// a un op which uses template nodes.
pub type TemplateUnOp = UnOp<TemplateLink>;

/// a template re-write.
///
/// this is a re-write rule that is specified by directly specifying the expected structure, and it allows specifying wildcard
/// variables in place of links in the expected structure, and using those variables to substitute values into the rewrite template.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TemplateRewrite {
    /// the structure that should be matched.
    pub query: TemplateNode,

    /// the template to instantiate to generate the re-write of the enode that matched the query.
    pub rewrite: TemplateLink,
}
impl TemplateRewrite {
    /// creates a rewrite for the commutativity of the bin op with the given kind (`a <op> b == b <op> a`).
    pub fn bin_op_commutativity(bin_op_kind: BinOpKind) -> Self {
        TemplateRewrite {
            query: TemplateBinOp {
                kind: bin_op_kind,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateVar::new(2).into(),
            }
            .into(),
            rewrite: TemplateBinOp {
                kind: bin_op_kind,
                lhs: TemplateVar::new(2).into(),
                rhs: TemplateVar::new(1).into(),
            }
            .into(),
        }
    }

    /// creates a rewrite for the associativity of the bin op with the given kind (`a <op> (b <op> c) == (a <op> b) <op> c`).
    pub fn bin_op_associativity(bin_op_kind: BinOpKind) -> Self {
        TemplateRewrite {
            query: TemplateBinOp {
                kind: bin_op_kind,
                lhs: TemplateVar::new(1).into(),
                rhs: TemplateBinOp {
                    kind: bin_op_kind,
                    lhs: TemplateVar::new(2).into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
            }
            .into(),
            rewrite: TemplateBinOp {
                kind: bin_op_kind,
                lhs: TemplateBinOp {
                    kind: bin_op_kind,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(2).into(),
                }
                .into(),
                rhs: TemplateVar::new(3).into(),
            }
            .into(),
        }
    }

    /// builds this template rewrite into a built template rewrite object, which can directly be applied to the egraph.
    pub fn build(self) -> BuiltTemplateRewrite {
        self.check();
        BuiltTemplateRewrite {
            query: self.query,
            rewrite: self.rewrite,
        }
    }

    // checks that this template rewrite even makes sense to exist.
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

    fn check_can_swap_direction(&self) {
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
        }
    }
}

/// a built template rewrite, which is a template rewrite which was verified to be valid, and can be directly applied to an egraph.
#[derive(Debug, Clone)]
pub struct BuiltTemplateRewrite {
    query: TemplateNode,
    rewrite: TemplateLink,
}

/// the value of a template variable that was captured while matching it to some eclass.
#[derive(Debug, Clone, Copy)]
struct TemplateVarValue {
    /// the effective eclass id that matched this variable.
    effective_eclass_id: EffectiveEClassId,
}

/// a data structure which maintains captured values of template variables when matching a template.
#[derive(Debug, Clone)]
struct TemplateVarValues(pub Vec<Option<TemplateVarValue>>);

impl TemplateVarValues {
    /// creates a new empty template var values which initially has no value for any of the variables.
    fn new() -> Self {
        Self(Vec::new())
    }

    /// returns the captured value of the given template variable.
    fn get(&self, var: TemplateVar) -> Option<TemplateVarValue> {
        self.0.get(var.index()).copied()?
    }

    /// sets the captured value of the given template variable.
    ///
    /// this function will panic if you set the value of the same variable twice.
    fn set(&mut self, var: TemplateVar, value: TemplateVarValue) {
        let index = var.index();
        if !(index < self.0.len()) {
            // the vector is not large enough
            self.0.resize(index + 1, None);
        }
        let slot = &mut self.0[index];
        assert!(slot.is_none());
        *slot = Some(value);
    }
}

/// the rewrite context of a template.
///
/// this is used to keep track of template variables that are matched during the matching process, and is used to instantiate the
/// re-write template with the right substitutions, according to the values matched to the variables during matching.
#[derive(Debug, Clone)]
pub struct TemplateRewriteCtx {
    template_var_values: TemplateVarValues,
}
impl TemplateRewriteCtx {
    /// creates a new empty context
    fn new() -> Self {
        Self {
            template_var_values: TemplateVarValues::new(),
        }
    }
}

impl Rewrite for BuiltTemplateRewrite {
    type Ctx = TemplateRewriteCtx;

    fn create_initial_ctx(&self) -> Self::Ctx {
        TemplateRewriteCtx::new()
    }

    fn query(&self) -> CowBox<'_, dyn QueryENodeMatcher<Self::Ctx>> {
        CowBox::Borrowed(&self.query)
    }

    fn build_rewrite(&self, ctx: Self::Ctx, egraph: &mut EGraph) -> AddENodeRes {
        instantiate_template_link(&self.rewrite, &ctx, egraph)
    }
}

impl QueryENodeMatcher<TemplateRewriteCtx> for TemplateNode {
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

impl QueryLinksMatcher<TemplateRewriteCtx> for TemplateNode {
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
        let effective_eclass_id = egraph.eclass_id_to_effective(link_eclass_id);
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

                        // we got a match, and we don't need to change the context at all since we didn't bind any new template vars.
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

/// instantiates the given template link by substituting variables according to the given context.
fn instantiate_template_link(
    template_link: &TemplateLink,
    ctx: &TemplateRewriteCtx,
    egraph: &mut EGraph,
) -> AddENodeRes {
    match template_link {
        TemplateLink::Specific(inner_template) => {
            let enode = instantiate_template_node(inner_template, ctx, egraph);
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

/// instantiates the given template node by substituting variables according to the given context.
fn instantiate_template_node(
    template: &TemplateNode,
    ctx: &TemplateRewriteCtx,
    egraph: &mut EGraph,
) -> ENode {
    template.convert_links(|template_link| {
        instantiate_template_link(template_link, ctx, egraph).eclass_id
    })
}
