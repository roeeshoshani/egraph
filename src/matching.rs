use crate::*;

/// a variable in a template enode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TemplateVar {
    pub id: usize,
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
    fn max_template_var_id(&self) -> Option<usize> {
        self.links()
            .into_iter()
            .filter_map(|link| match link {
                TemplateLink::Specific(generic_node) => generic_node.max_template_var_id(),
                TemplateLink::Var(template_var) => Some(template_var.id),
            })
            .max()
    }
}

pub struct RewriteRuleParams {
    pub query: ENodeTemplate,
    pub rewrite: ENodeTemplate,
}

pub struct RewriteRule {
    pub query: ENodeTemplate,
    pub rewrite: ENodeTemplate,
    pub max_query_var_id: Option<usize>,
}
impl RewriteRule {
    pub fn new(params: RewriteRuleParams) -> Self {
        Self {
            max_query_var_id: params.query.max_template_var_id(),
            query: params.query,
            rewrite: params.rewrite,
        }
    }
}
