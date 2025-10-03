use std::num::NonZeroUsize;

use crate::*;

/// a variable in a template enode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TemplateVar {
    pub id: NonZeroUsize,
}

/// a link in a template enode.
#[derive(Debug)]
pub enum TemplateLink {
    Specific(Box<ENodeTemplate>),
    Var(TemplateVar),
}

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

pub struct RewriteRule {
    pub query: ENodeTemplate,
    pub rewrite: ENodeTemplate,
}

pub struct RewriteRuleRunner {
    pub query: ENodeTemplate,
    pub rewrite: ENodeTemplate,
    pub template_var_values: Vec<Option<EffectiveEClassId>>,
}
impl RewriteRuleRunner {
    pub fn new(params: RewriteRule) -> Self {
        let template_var_values = match params.query.max_template_var_id() {
            Some(max_var_id) => {
                // the query uses some variables

                // make sure that there are no gaps in the variable ids
                for i in 1..=max_var_id.get() {
                    let does_use_var = params.query.does_use_template_var(TemplateVar {
                        id: unsafe {
                            // SAFETY: we start iterating from 1
                            NonZeroUsize::new_unchecked(i)
                        },
                    });
                    assert!(does_use_var);
                }

                // make sure that the re-write doesn't use variables that don't exist in the query
                if let Some(rewrite_max_var_id) = params.rewrite.max_template_var_id() {
                    assert!(rewrite_max_var_id <= max_var_id)
                }

                // the max var id starts from 1 (since it is non-zero).
                // so, when indexing, we subtract one from the id to get the index.
                // so, the length of the vector which supports indexing all of those vars is just the max var id.
                vec![None; max_var_id.get()]
            }
            None => {
                // the query doesn't use any values

                // make sure that the re-write also doesn't use any variables
                assert_eq!(params.rewrite.max_template_var_id(), None);

                Vec::new()
            }
        };

        Self {
            template_var_values,
            query: params.query,
            rewrite: params.rewrite,
        }
    }
}
