use std::num::NonZeroUsize;

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
pub enum TemplateLink {
    Specific(Box<ENodeTemplate>),
    Var(TemplateVar),
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
            .filter_map(|link| match link {
                TemplateLink::Specific(generic_node) => generic_node.max_template_var_id(),
                TemplateLink::Var(template_var) => Some(template_var.id),
            })
            .max()
    }
    fn does_use_template_var(&self, template_var: TemplateVar) -> bool {
        self.links().into_iter().any(|link| match link {
            TemplateLink::Specific(generic_node) => {
                generic_node.does_use_template_var(template_var)
            }
            TemplateLink::Var(cur_template_var) => *cur_template_var == template_var,
        })
    }
}

#[derive(Debug, Clone)]
pub struct RewriteRuleParams {
    pub query: ENodeTemplate,
    pub rewrite: ENodeTemplate,

    /// should we keep the original enode after the rule has been applied to it?
    pub keep_original: bool,
}
impl RewriteRuleParams {
    fn check(&self) {
        match self.query.max_template_var_id() {
            Some(max_var_id) => {
                // the query uses some variables

                // make sure that there are no gaps in the variable ids
                for i in 1..=max_var_id.get() {
                    let does_use_var = self.query.does_use_template_var(TemplateVar {
                        id: unsafe {
                            // SAFETY: we start iterating from 1
                            NonZeroUsize::new_unchecked(i)
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
}

#[derive(Debug, Clone)]
pub struct RewriteRule {
    params: RewriteRuleParams,
}
impl RewriteRule {
    pub fn new(params: RewriteRuleParams) -> Self {
        params.check();
        Self { params }
    }

    pub fn params(&self) -> &RewriteRuleParams {
        &self.params
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
