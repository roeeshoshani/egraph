pub trait NodeProvider {
    type Node<L: NodeRequiredTraits>: NodeRequiredTraits;

    type NodeLinks<'a, L: 'a>: NodeLinks<'a, L>;

    fn convert_link<L1, L2, F>(node: &Self::Node<L1>, f: F) -> Self::Node<L2>
    where
        F: FnMut(&L1) -> L2,
        Self::Node<()>: std::hash::Hash;

    fn links<L>(node: &Self::Node<L>) -> Self::NodeLinks<'_, L>;

    fn dot_graph_label<L>(node: &Self::Node<L>) -> String;
}

pub trait NodeLinks<'a, L: 'a> {
    fn len(&self) -> usize;
    fn iter(&self) -> impl Iterator<Item = &'a L>;
    fn get_index(&self, index: usize) -> &'a L;
}

pub trait NodeRequiredTraits: Clone + std::fmt::Debug + std::hash::Hash + PartialEq + Eq {}
impl<T: Clone + std::fmt::Debug + std::hash::Hash + PartialEq + Eq> NodeRequiredTraits for T {}
