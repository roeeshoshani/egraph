use hashbrown::{DefaultHashBuilder, HashTable, hash_table::Entry};
use std::hash::BuildHasher;

use crate::{union_find::*, *};

/// the id of an enode.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ENodeId(pub UnionFindItemId);

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
    pub fn to_effective(&self, union_find: &UnionFind<ENode>) -> EffectiveEClassId {
        EffectiveEClassId(union_find.root_of_item(self.enode_id.0))
    }
}

/// an enode.
pub type ENode = GenericNode<EClassId>;

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

/// an enode with an effective eclass id. this allows comparing the enode to other enodes.
pub type ENodeEffectiveEClassId = GenericNode<EffectiveEClassId>;

impl ENode {
    /// converts this enode to an enode with an effective eclass id which is correct for the given state of the union find tree.
    pub fn to_effective_eclass_id(&self, union_find: &UnionFind<ENode>) -> ENodeEffectiveEClassId {
        self.convert_link(|eclass_id| eclass_id.to_effective(union_find))
    }
}

#[derive(Debug, Clone)]
struct ENodeHashTableEntry {
    enode: ENode,
    id: ENodeId,
}

#[derive(Debug, Default, Clone)]
struct NodeHasher(DefaultHashBuilder);
impl NodeHasher {
    fn hash_node<L>(&self, node: &GenericNode<L>) -> u64 {
        // hash the enode, but ignore the links. only take its structure into account.
        self.0.hash_one(node.convert_link(|_| ()))
    }
}

#[derive(derive_debug::Dbg, Clone)]
pub struct EGraph {
    enodes_union_find: UnionFind<ENode>,

    /// a hashmap for de-duplication
    ///
    /// NOTE: we exclude the eclass id when hashing since the eclass id contains lazy data which needs to be resolved according
    /// to the state of the union find tree. and, when the tree changes, the meaning of the eclass id can change.
    ///
    /// hashmap keys need to be stable, and must not change, so we must exclude the eclass id from the hash.
    ///
    /// TODO: re-write the docs to explain that this is also used for querying.
    #[dbg(skip)]
    enodes_hash_table: HashTable<ENodeHashTableEntry>,

    /// a hasher for the hashmap
    hasher: NodeHasher,
}
impl EGraph {
    pub fn new() -> Self {
        Self {
            enodes_union_find: UnionFind::new(),
            enodes_hash_table: HashTable::new(),
            hasher: NodeHasher::default(),
        }
    }

    /// adds an enode to the egraph and returns the eclass id which contains it.
    pub fn add_enode(&mut self, enode: ENode) -> EClassId {
        let enode_effective_eclass_id = enode.to_effective_eclass_id(&self.enodes_union_find);
        let eq_fn = |entry: &ENodeHashTableEntry| {
            entry.enode.to_effective_eclass_id(&self.enodes_union_find) == enode_effective_eclass_id
        };

        let hash_fn = |entry: &ENodeHashTableEntry| self.hasher.hash_node(&entry.enode);

        let hash = self.hasher.hash_node(&enode);

        let entry = self.enodes_hash_table.entry(hash, eq_fn, hash_fn);

        let enode_id = match entry {
            Entry::Occupied(entry) => entry.get().id,
            Entry::Vacant(entry) => {
                let enode_id = ENodeId(self.enodes_union_find.create_new_item(enode.clone()));
                entry.insert(ENodeHashTableEntry {
                    enode,
                    id: enode_id,
                });
                enode_id
            }
        };

        EClassId { enode_id }
    }

    pub fn add_rec_node(&mut self, rec_node: &RecNode) -> EClassId {
        let graph_node = rec_node.0.convert_link(|link| self.add_rec_node(link));
        self.add_enode(graph_node)
    }

    pub fn apply_rule(&mut self, rule: &RewriteRule) {
        let hash = self.hasher.hash_node(&rule.query);
        let mut rule_storage = RewriteRuleStorage::new(rule);
        for entry in self.enodes_hash_table.iter_hash_mut(hash) {
            rule_storage.reset();
            apply_rule_match_enode_to_enode_template(
                &entry.enode,
                &rule.query,
                &mut rule_storage,
                &self.enodes_union_find,
            );
        }
    }

    pub fn from_rec_node(rec_node: &RecNode) -> Self {
        let mut egraph = Self::new();
        egraph.add_rec_node(rec_node);
        egraph
    }
}

struct Match {
    pub rule_storage: RewriteRuleStorage,
}

struct ENodeRuleMatcher<'a> {
    union_find: &'a UnionFind<ENode>,
    matched_enode_id: ENodeId,
}
impl<'a> ENodeRuleMatcher<'a> {
    fn match_enode_to_enode_template(
        &mut self,
        enode: &ENode,
        template: &ENodeTemplate,
        rule_storage: &RewriteRuleStorage,
        matches: &mut Vec<Match>,
    ) {
        // first, perform structural comparison on everything other than the link
        if enode.convert_link(|_| ()) != template.convert_link(|_| ()) {
            // no match
            return;
        }

        // now compare the links
        let enode_links = enode.links();
        let template_links = template.links();

        if enode_links.len() != template_links.len() {
            // no match
            return;
        }

        let links_amount = enode_links.len();

        if links_amount == 0 {
            // if there are no links, the structural comparison that we performed above is enough, so this enode is a match.
            matches.push(Match {
                rule_storage: rule_storage.clone(),
            });
            return;
        }

        // match the links
        let mut cur_matches: Vec<Match> = Vec::new();
        let mut new_matches: Vec<Match> = Vec::new();
        for cur_link_idx in 0..links_amount {
            let template_link = &template_links[cur_link_idx];

            new_matches.clear();
            for cur_match in &cur_matches {
                match template_link {
                    TemplateLink::Specific(generic_node) => {
                        self.match_enode_to_enode_template(
                            enode,
                            template,
                            &cur_match.rule_storage,
                            &mut new_matches,
                        );
                    }
                    TemplateLink::Var(template_var) => todo!(),
                }
            }
            std::mem::swap(&mut cur_matches, &mut new_matches);
        }
        todo!()
    }

    fn template_link_is_match_eclass(
        &mut self,
        template_link: &TemplateLink,
        eclass: EClassId,
        union_find: &UnionFind<ENode>,
    ) -> bool {
        // iterate all enodes in the eclass
        for enode_item_id in union_find.items_eq_to(eclass.enode_id.0) {
            let enode = &union_find[enode_item_id];
            if self.template_link_is_match_enode(template_link, eclass, enode) {
                todo!("collect the match")
            }
        }
        todo!()
    }

    fn template_link_is_match_enode(
        &mut self,
        template_link: &TemplateLink,
        eclass: EClassId,
        enode: &ENode,
    ) -> bool {
        match template_link {
            TemplateLink::Specific(generic_node) => todo!(),
            TemplateLink::Var(template_var) => todo!(),
        }
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
        assert_eq!(egraph.enodes_union_find.len(), 1);
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
        assert_eq!(egraph.enodes_union_find.len(), 4);

        // now do some more
        let enode = ENode::UnOp(UnOp {
            kind: UnOpKind::Neg,
            operand: var1,
        });

        let id1 = egraph.add_enode(enode.clone());
        let id2 = egraph.add_enode(enode.clone());

        // make sure that it got de-duplicated
        assert_eq!(id1.enode_id, id2.enode_id);
        assert_eq!(egraph.enodes_union_find.len(), 5);

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
        assert_eq!(egraph.enodes_union_find.len(), 6);
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
