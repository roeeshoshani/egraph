use std::num::NonZeroUsize;

use derivative::Derivative;

use crate::*;

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
pub enum TemplateLink<N: NodeProvider> {
    Specific(Box<ENodeTemplate<N>>),
    Var(TemplateVar),
}
impl<N: NodeProvider> From<TemplateVar> for TemplateLink<N> {
    fn from(x: TemplateVar) -> Self {
        Self::Var(x)
    }
}

/// an enode template.
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
pub struct ENodeTemplate<N: NodeProvider>(pub N::Node<TemplateLink<N>>);
fn enode_template_max_template_var_id<N: NodeProvider>(
    template: &ENodeTemplate<N>,
) -> Option<NonZeroUsize> {
    N::links(&template.0)
        .iter()
        .filter_map(|link| match link {
            TemplateLink::Specific(node) => enode_template_max_template_var_id::<N>(node),
            TemplateLink::Var(template_var) => Some(template_var.id),
        })
        .max()
}
fn enode_template_does_use_template_var<N: NodeProvider>(
    template: &ENodeTemplate<N>,
    template_var: TemplateVar,
) -> bool {
    N::links(&template.0).iter().any(|link| match link {
        TemplateLink::Specific(node) => {
            enode_template_does_use_template_var::<N>(node, template_var)
        }
        TemplateLink::Var(cur_template_var) => *cur_template_var == template_var,
    })
}

#[derive(Debug, Clone)]
pub struct RewriteRuleParams<N: NodeProvider> {
    pub query: ENodeTemplate<N>,
    pub rewrite: ENodeTemplate<N>,

    /// should we keep the original enode after the rule has been applied to it?
    pub keep_original: bool,

    /// is the rule bi-directional
    pub bi_directional: bool,
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
pub struct RewriteRule<N: NodeProvider> {
    pub query: ENodeTemplate<N>,
    pub rewrite: ENodeTemplate<N>,

    /// should we keep the original enode after the rule has been applied to it?
    pub keep_original: bool,
}
impl<N: NodeProvider> RewriteRule<N> {
    pub fn new(query: ENodeTemplate<N>, rewrite: ENodeTemplate<N>, keep_original: bool) -> Self {
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
        match enode_template_max_template_var_id::<N>(&self.query) {
            Some(max_var_id) => {
                // the query uses some variables

                // make sure that there are no gaps in the variable ids
                for i in 1..=max_var_id.get() {
                    let does_use_var = enode_template_does_use_template_var::<N>(
                        &self.query,
                        TemplateVar {
                            id: unsafe {
                                // SAFETY: we start iterating from 1
                                NonZeroUsize::new_unchecked(i)
                            },
                        },
                    );
                    assert!(does_use_var);
                }

                // make sure that the re-write doesn't use variables that don't exist in the query
                if let Some(rewrite_max_var_id) =
                    enode_template_max_template_var_id::<N>(&self.rewrite)
                {
                    assert!(rewrite_max_var_id <= max_var_id)
                }
            }
            None => {
                // the query doesn't use any values

                // make sure that the re-write also doesn't use any variables
                assert_eq!(enode_template_max_template_var_id::<N>(&self.rewrite), None);
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

        match enode_template_max_template_var_id::<N>(&self.query) {
            Some(max_var_id) => {
                // the query uses some variables

                // make sure that both the query and the rewrite use all template vars
                for i in 1..=max_var_id.get() {
                    let var = TemplateVar {
                        id: unsafe {
                            // SAFETY: we start iterating from 1
                            NonZeroUsize::new_unchecked(i)
                        },
                    };
                    assert!(enode_template_does_use_template_var::<N>(&self.query, var));
                    assert!(enode_template_does_use_template_var::<N>(
                        &self.rewrite,
                        var
                    ));
                }
            }
            None => {
                // the query doesn't use any values, so it can swap directions
            }
        }
    }

    fn swap_direction(self) -> Self {
        self.check_can_swap_direction();
        Self {
            query: self.rewrite,
            rewrite: self.query,
            keep_original: self.keep_original,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TemplateVarValue {
    pub eclass: EClassId,
    pub effective_eclass_id: EffectiveEClassId,
}

#[derive(Debug, Clone)]
pub struct TemplateVarValues(Vec<Option<TemplateVarValue>>);
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
pub struct RewriteRuleSet<N: NodeProvider> {
    rules: Vec<RewriteRule<N>>,
}
impl<N: NodeProvider> RewriteRuleSet<N> {
    pub fn new() -> Self {
        Self { rules: Vec::new() }
    }

    pub fn add(&mut self, rule: RewriteRuleParams<N>) {
        let basic_rule = RewriteRule::new(rule.query, rule.rewrite, rule.keep_original);

        self.rules.push(basic_rule.clone());

        if rule.bi_directional {
            self.rules.push(basic_rule.swap_direction());
        }
    }

    pub fn add_multiple<I>(&mut self, rules: I)
    where
        I: IntoIterator<Item = RewriteRuleParams<N>>,
    {
        for rule in rules {
            self.add(rule);
        }
    }

    pub fn from_rules<I>(rules: I) -> Self
    where
        I: IntoIterator<Item = RewriteRuleParams<N>>,
    {
        let mut res = Self::new();
        res.add_multiple(rules);
        res
    }

    pub fn rules(&self) -> &[RewriteRule<N>] {
        &self.rules
    }
}
