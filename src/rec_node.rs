use derivative::Derivative;

use crate::NodeProvider;

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
#[derivative(Debug(bound = ""))]
#[derivative(PartialEq(bound = ""))]
#[derivative(Eq(bound = ""))]
#[derivative(Hash(bound = ""))]
pub struct RecNode<N: NodeProvider>(pub N::Node<Box<RecNode<N>>>);
