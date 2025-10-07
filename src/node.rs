use std::ops::Index;

pub trait NodeProvider {
    type Node<L>: Clone + std::fmt::Debug + std::hash::Hash + PartialEq + Eq;

    type NodeLinks<'a, L: 'a>: Iterator<Item = &'a L>
        + ExactSizeIterator
        + Index<usize, Output = &'a L>;

    fn convert_link<L1, L2, F>(node: &Self::Node<L1>, f: F) -> Self::Node<L2>
    where
        F: FnMut(&L1) -> L2,
        Self::Node<()>: std::hash::Hash;

    fn links<L>(node: &Self::Node<L>) -> Self::NodeLinks<'_, L>;

    fn dot_graph_label<L>(node: &Self::Node<L>) -> String;
}
