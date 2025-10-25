use std::num::NonZeroUsize;

use hashbrown::HashMap;

use crate::{
    egraph::*,
    node::*,
    rewrite::*,
    utils::{CowBox, NonZeroUsizeAllocator},
};

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

/// a template var builder, which is just a wrapper around the variable's name. when built, it will be converted to
/// an actual template var.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TemplateVarBuilder {
    /// the variable name.
    pub name: String,
}
impl TemplateVarBuilder {
    /// creates a new template var builder which represents a template variable with the given name.
    pub fn new<T: Into<String>>(var_name: T) -> Self {
        Self {
            name: var_name.into(),
        }
    }
}

/// a shortcut for creating a template var builder as a template link builder. allows writing complex template
/// expressions without being very verbose.
pub fn tv<T: Into<String>>(var_name: T) -> TemplateLinkBuilder {
    TemplateVarBuilder::new(var_name).into()
}

/// a template link builder. this is used to represent links in the template re-write builder, and it will later
/// be built into an actual template link.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TemplateLinkBuilder {
    /// this link matches another nested structure.
    Specific(Box<TemplateNodeBuilder>),

    /// this link is a wildcard variable. it can match any value, and it will be substituted when instantiating the template.
    Var(TemplateVarBuilder),
}
impl TemplateLinkBuilder {
    /// applies the bin op with the given kind to this link as the lhs, and with the given rhs link as the rhs.
    pub fn apply_bin_op(self, kind: BinOpKind, rhs: Self) -> Self {
        Self::Specific(Box::new(GenericNode::BinOp(BinOp {
            kind,
            lhs: self,
            rhs: rhs,
        })))
    }

    /// applies the un op with the given kind to this link as the operand.
    pub fn apply_un_op(self, kind: UnOpKind) -> Self {
        Self::Specific(Box::new(GenericNode::UnOp(UnOp {
            kind,
            operand: self,
        })))
    }
}

macro_rules! impl_binop_for_template_link_builder {
    ($trait: ty, $trait_fn_name: ident, $bin_op_kind: expr) => {
        impl $trait for TemplateLinkBuilder {
            type Output = Self;

            fn $trait_fn_name(self, rhs: Self) -> Self::Output {
                self.apply_bin_op($bin_op_kind, rhs)
            }
        }
    };
}

impl_binop_for_template_link_builder! {std::ops::Add, add, BinOpKind::Add}
impl_binop_for_template_link_builder! {std::ops::Mul, mul, BinOpKind::Mul}
impl_binop_for_template_link_builder! {std::ops::BitAnd, bitand, BinOpKind::BitAnd}
impl_binop_for_template_link_builder! {std::ops::BitOr, bitor, BinOpKind::BitOr}

macro_rules! impl_unop_for_template_link_builder {
    ($trait: ty, $trait_fn_name: ident, $un_op_kind: expr) => {
        impl $trait for TemplateLinkBuilder {
            type Output = Self;

            fn $trait_fn_name(self) -> Self::Output {
                self.apply_un_op($un_op_kind)
            }
        }
    };
}

impl_unop_for_template_link_builder! {std::ops::Neg, neg,UnOpKind::Neg}
impl_unop_for_template_link_builder! {std::ops::Not, not,UnOpKind::BitNot}

/// a template node builder. this is used to represent nodes in the template re-write builder, and it will later
/// be built into an actual template node.
pub type TemplateNodeBuilder = GenericNode<TemplateLinkBuilder>;

// convert a template var builder to a template link builder.
impl From<TemplateVarBuilder> for TemplateLinkBuilder {
    fn from(x: TemplateVarBuilder) -> Self {
        Self::Var(x)
    }
}

// convert any string-like object to a template link which represent a template variable with the string as
// the variable name.
impl From<String> for TemplateLinkBuilder {
    fn from(x: String) -> Self {
        TemplateVarBuilder::new(x).into()
    }
}
impl<'a> From<&'a str> for TemplateLinkBuilder {
    fn from(x: &'a str) -> Self {
        TemplateVarBuilder::new(x).into()
    }
}

// anything that can be converted to a template node builder should also be convertible to a template link builder.
impl<T> From<T> for TemplateLinkBuilder
where
    TemplateNodeBuilder: From<T>,
{
    fn from(x: T) -> Self {
        Self::Specific(Box::new(x.into()))
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

/// a template node which represents some node structure with wildcard variables.
pub type TemplateNode = GenericNode<TemplateLink>;

/// a var translator, which translated string variables to template variables.
struct VarTranslator {
    name_to_var: HashMap<String, TemplateVar>,
    var_id_allocator: NonZeroUsizeAllocator,
}
impl VarTranslator {
    /// creates a new var translator.
    fn new() -> Self {
        Self {
            name_to_var: HashMap::new(),
            var_id_allocator: NonZeroUsizeAllocator::new(),
        }
    }

    /// builds the given query node by converting all var names to template variables.
    fn convert_query_node(&mut self, node: &TemplateNodeBuilder) -> TemplateNode {
        node.convert_links(move |link| self.convert_query_link(link))
    }

    /// builds the given query link by converting all var names to template variables.
    fn convert_query_link(&mut self, link: &TemplateLinkBuilder) -> TemplateLink {
        match link {
            TemplateLinkBuilder::Specific(node) => {
                TemplateLink::Specific(Box::new(self.convert_query_node(node)))
            }
            TemplateLinkBuilder::Var(var) => match self.name_to_var.entry(var.name.clone()) {
                hashbrown::hash_map::Entry::Occupied(occupied_entry) => {
                    TemplateLink::Var(*occupied_entry.get())
                }
                hashbrown::hash_map::Entry::Vacant(vacant_entry) => {
                    // allocate a new var
                    let var = TemplateVar {
                        id: self.var_id_allocator.alloc(),
                    };
                    vacant_entry.insert(var);
                    TemplateLink::Var(var)
                }
            },
        }
    }

    /// builds the given rewrite node by converting all var names to template variables.
    fn convert_rewrite_node(&self, node: &TemplateNodeBuilder) -> TemplateNode {
        node.convert_links(move |link| self.convert_rewrite_link(link))
    }

    /// builds the given rewrite link by converting all var names to template variables.
    fn convert_rewrite_link(&self, link: &TemplateLinkBuilder) -> TemplateLink {
        match link {
            TemplateLinkBuilder::Specific(node) => {
                TemplateLink::Specific(Box::new(self.convert_rewrite_node(node)))
            }
            TemplateLinkBuilder::Var(var) => TemplateLink::Var(self.name_to_var[&var.name]),
        }
    }
}

/// a template re-write.
///
/// this is a re-write rule that is specified by directly specifying the expected structure, and it allows specifying wildcard
/// variables in place of links in the expected structure, and using those variables to substitute values into the rewrite template.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TemplateRewriteBuilder {
    /// the structure that should be matched.
    pub query: TemplateLinkBuilder,

    /// the template to instantiate to generate the re-write of the enode that matched the query.
    pub rewrite: TemplateLinkBuilder,
}
impl TemplateRewriteBuilder {
    /// creates a rewrite for the commutativity of the bin op with the given kind (`a <op> b == b <op> a`).
    pub fn bin_op_commutativity(bin_op_kind: BinOpKind) -> Self {
        TemplateRewriteBuilder {
            query: tv("a").apply_bin_op(bin_op_kind, tv("b")),
            rewrite: tv("b").apply_bin_op(bin_op_kind, tv("a")),
        }
    }

    /// creates a rewrite for the associativity of the bin op with the given kind (`a <op> (b <op> c) == (a <op> b) <op> c`).
    pub fn bin_op_associativity(bin_op_kind: BinOpKind) -> Self {
        TemplateRewriteBuilder {
            query: tv("a").apply_bin_op(bin_op_kind, tv("b").apply_bin_op(bin_op_kind, tv("c"))),
            rewrite: (tv("a").apply_bin_op(bin_op_kind, tv("b")))
                .apply_bin_op(bin_op_kind, tv("c")),
        }
    }

    /// builds this into a template rewrite which can directly be applied to the egraph.
    pub fn build(self) -> TemplateRewrite {
        let mut translator = VarTranslator::new();

        TemplateRewrite {
            query: translator.convert_query_link(&self.query),
            rewrite: translator.convert_rewrite_link(&self.rewrite),
        }
    }
}

/// a template re-write.
///
/// this is a re-write rule that is specified by directly specifying the expected structure, and it allows specifying wildcard
/// variables in place of links in the expected structure, and using those variables to substitute values into the rewrite template.
#[derive(Debug, Clone)]
pub struct TemplateRewrite {
    query: TemplateLink,
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

impl SimpleRewrite for TemplateRewrite {
    type Ctx = TemplateRewriteCtx;

    fn create_initial_ctx(&self) -> Self::Ctx {
        TemplateRewriteCtx::new()
    }

    fn query(&self) -> CowBox<'_, dyn QueryLinkMatcher<Self::Ctx>> {
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
        link_effective_eclass_id: EffectiveEClassId,
        _egraph: &EGraph,
        ctx: &TemplateRewriteCtx,
    ) -> QueryMatchLinkRes<'_, TemplateRewriteCtx> {
        match self {
            TemplateLink::Specific(enode_template) => QueryMatchLinkRes::RecurseIntoENodes {
                new_ctx: ctx.clone(),
                enode_matcher: CowBox::Borrowed(&**enode_template),
            },
            TemplateLink::Var(template_var) => {
                match ctx.template_var_values.get(*template_var) {
                    Some(existing_var_value) => {
                        if link_effective_eclass_id != existing_var_value.effective_eclass_id {
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
                                effective_eclass_id: link_effective_eclass_id,
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
