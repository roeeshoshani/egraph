use crate::*;

/// a variable in a node query
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct QueryVariable {
    pub id: usize,
}

/// a link in a node query
#[derive(Debug)]
pub enum QueryLink {
    Specific(EClassId),
    Variable(QueryVariable),
}
impl QueryLink {
    pub fn from_eclass_id(eclass_id: &EClassId) -> Self {
        Self::Specific(*eclass_id)
    }
}

/// a node query.
pub type ENodeQuery = GenericNode<QueryLink>;
