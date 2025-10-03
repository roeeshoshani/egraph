use hashbrown::{Equivalent, HashMap, hash_map::RawEntryMut};

use crate::{union_find::*, *};

// NOTE: this should NOT implement `Hash`, `PartialEq` and `Eq` due to how it is implemented.
// we can have 2 instances of this type which point to different enodes, so the derived `Eq` implementation will say that they are not
// equal, but in practice the 2 enodes that they point to are part of the same eclass, so the 2 eclass ids should be equal.
//
// checking if 2 instances of this type are equal requires accessing the union find tree.
#[derive(Debug, Clone, Copy)]
pub struct EClassId {
    /// an id of some enode which is part of this eclass.
    /// this can be used to iterate over all
    pub enode_id: ENodeId,
}
impl EClassId {
    /// converts this eclass id to an effective eclass id which is correct for the given state of the union find tree.
    pub fn to_effective<T>(&self, union_find: &UnionFind<T>) -> EffectiveEClassId {
        EffectiveEClassId(union_find.root_of_item(self.enode_id.0))
    }
}

/// an effective eclass id.
///
/// usually, the eclass id is represented as an id to any enode in that eclass. this is problematic since it means that we can't
/// compare eclass ids, which means that we can't compare enodes.
///
/// this type represents an actual eclass id which can be compared to other eclass id. this is resolved by taking the root of the enode
/// id in the union find tree.
///
/// this id is only true for a snapshot of the union find tree. once the tree is modified, it is no longer up to date, since the root
/// may no longer be the real root, it may now have an ancestor (or even multiple ancestors).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EffectiveEClassId(pub UnionFindAnyId);

pub type ENode = GenericNode<EClassId>;

/// an eclass id which has no effect when hashed, such that its hash value is ignored when embedded into a struct.
///
/// this is needed because the eclass id contains information whose meaning may change depending on the state of the union find tree.
///
/// but, when the enode is used as a hashmap key, we need its value to be stable, so we want to ignore the eclass id when calculating
/// the hash.
#[derive(Debug, Clone, Copy)]
pub struct EClassIdIgnoreHash(pub EClassId);
impl std::hash::Hash for EClassIdIgnoreHash {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}
impl EClassIdIgnoreHash {
    pub fn from_eclass_id(eclass_id: &EClassId) -> Self {
        Self(*eclass_id)
    }
}

/// an enode which ignores the eclass id when hashed.
pub type ENodeIgnoreEClassIdHash = GenericNode<EClassIdIgnoreHash>;

/// an enode with an effective eclass id. this allows comparing the enode to other enodes.
pub type ENodeEffectiveEClassId = GenericNode<EffectiveEClassId>;

/// a query for an enode in the de-duplication hashmap.
///
/// due to how the enode is stored in memory, we can't just use it directly, since it contains information that is lazily resolved.
///
/// so, this query object allows querying the hashmap in a way that correctly hashes the enode, and correctly compares it to other
/// enodes after resolving the lazily resolved information.
struct ENodeHashMapQuery<'a> {
    /// used for calculating the hash for this enode query
    enode_ignore_eclass_id_hash: ENodeIgnoreEClassIdHash,

    /// used for comparing against other enodes
    enode_effective_eclass_id: ENodeEffectiveEClassId,

    union_find: &'a UnionFind<ENode>,
}
impl<'a> ENodeHashMapQuery<'a> {
    fn new(enode: &'a ENode, union_find: &'a UnionFind<ENode>) -> Self {
        Self {
            enode_ignore_eclass_id_hash: enode.convert_link(EClassIdIgnoreHash::from_eclass_id),
            enode_effective_eclass_id: enode
                .convert_link(|eclass_id| eclass_id.to_effective(union_find)),
            union_find,
        }
    }
}
impl<'a> std::hash::Hash for ENodeHashMapQuery<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.enode_ignore_eclass_id_hash.hash(state);
    }
}
impl<'a> Equivalent<ENodeIgnoreEClassIdHash> for ENodeHashMapQuery<'a> {
    fn equivalent(&self, key: &ENodeIgnoreEClassIdHash) -> bool {
        let converted_key = key.convert_link(|eclass_id| eclass_id.0.to_effective(self.union_find));
        converted_key == self.enode_effective_eclass_id
    }
}

/// the id of an enode.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ENodeId(pub UnionFindItemId);

#[derive(derive_debug::Dbg, Clone)]
pub struct EGraph {
    enodes: UnionFind<ENode>,

    /// a hashmap for de-duplication
    ///
    /// NOTE: we exclude the eclass id when hashing since the eclass id contains lazy data which needs to be resolved according
    /// to the state of the union find tree. and, when the tree changes, the meaning of the eclass id can change.
    ///
    /// hashmap keys need to be stable, and must not change, so we must exclude the eclass id from the hash.
    #[dbg(skip)]
    enode_to_id: HashMap<ENodeIgnoreEClassIdHash, ENodeId>,
}
impl EGraph {
    pub fn new() -> Self {
        Self {
            enodes: UnionFind::new(),
            enode_to_id: HashMap::new(),
        }
    }

    /// adds an enode to the egraph and returns the eclass id which contains it.
    pub fn add_enode(&mut self, enode: ENode) -> EClassId {
        let query = ENodeHashMapQuery::new(&enode, &self.enodes);
        let enode_id = match self.enode_to_id.raw_entry_mut().from_key(&query) {
            RawEntryMut::Occupied(entry) => *entry.get(),
            RawEntryMut::Vacant(entry) => {
                let enode_id = ENodeId(self.enodes.create_new_item(enode.clone()));
                entry.insert(
                    enode.convert_link(EClassIdIgnoreHash::from_eclass_id),
                    enode_id,
                );
                enode_id
            }
        };

        EClassId { enode_id }
    }

    pub fn add_rec_node(&mut self, rec_node: &RecNode) -> EClassId {
        // first, convert the recursive node into a graph node
        let graph_node = match &rec_node.0 {
            GenericNode::Imm(imm) => GenericNode::Imm(*imm),
            GenericNode::Var(var) => GenericNode::Var(*var),
            GenericNode::BinOp(bin_op) => GenericNode::BinOp(BinOp {
                kind: bin_op.kind,
                lhs: self.add_rec_node(&bin_op.lhs),
                rhs: self.add_rec_node(&bin_op.rhs),
            }),
            GenericNode::UnOp(un_op) => GenericNode::UnOp(UnOp {
                kind: un_op.kind,
                operand: self.add_rec_node(&un_op.operand),
            }),
        };
        self.add_enode(graph_node)
    }

    pub fn from_rec_node(rec_node: &RecNode) -> Self {
        let mut egraph = Self::new();
        egraph.add_rec_node(rec_node);
        egraph
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dedup_basic() {
        let mut egraph = EGraph::new();
        let enode = ENode::Var(Var(5));
        let id1 = egraph.add_enode(enode.clone());
        let id2 = egraph.add_enode(enode.clone());
        assert_eq!(id1.enode_id, id2.enode_id);
        assert_eq!(egraph.enodes.len(), 1);
    }

    #[test]
    fn test_dedup_nested() {
        let mut egraph = EGraph::new();
        let var1 = egraph.add_enode(ENode::Var(Var(1)));
        let var2 = egraph.add_enode(ENode::Var(Var(2)));

        let enode = ENode::BinOp(BinOp {
            kind: BinOpKind::Add,
            lhs: var1,
            rhs: var2,
        });

        let id1 = egraph.add_enode(enode.clone());

        // add something in between just to add some noise
        let var3 = egraph.add_enode(ENode::Var(Var(3)));

        // re-add the same enode
        let id2 = egraph.add_enode(enode.clone());

        // make sure that it got de-duplicated
        assert_eq!(id1.enode_id, id2.enode_id);
        assert_eq!(egraph.enodes.len(), 4);

        // now do some more
        let enode = ENode::UnOp(UnOp {
            kind: UnOpKind::Neg,
            operand: var1,
        });

        let id1 = egraph.add_enode(enode.clone());
        let id2 = egraph.add_enode(enode.clone());

        // make sure that it got de-duplicated
        assert_eq!(id1.enode_id, id2.enode_id);
        assert_eq!(egraph.enodes.len(), 5);

        // even more
        let enode = ENode::BinOp(BinOp {
            kind: BinOpKind::Mul,
            lhs: var3,
            rhs: var2,
        });

        let id1 = egraph.add_enode(enode.clone());
        let id2 = egraph.add_enode(enode.clone());

        // make sure that it got de-duplicated
        assert_eq!(id1.enode_id, id2.enode_id);
        assert_eq!(egraph.enodes.len(), 6);
    }

    #[test]
    fn test_basic() {
        // 0xff & ((x & 0xff00) || (x & 0xff0000))
        let rec_node: RecNode = RecBinOp {
            kind: BinOpKind::And,
            lhs: 0xff.into(),
            rhs: RecBinOp {
                kind: BinOpKind::Or,
                lhs: RecBinOp {
                    kind: BinOpKind::And,
                    lhs: Var(0).into(),
                    rhs: 0xff00.into(),
                }
                .into(),
                rhs: RecBinOp {
                    kind: BinOpKind::And,
                    lhs: Var(0).into(),
                    rhs: 0xff0000.into(),
                }
                .into(),
            }
            .into(),
        }
        .into();

        let egraph = EGraph::from_rec_node(&rec_node);
        dbg!(egraph);
        panic!();
    }
}
