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
        let matcher = Matcher {
            union_find: &self.enodes_union_find,
        };

        let mut matching_state = MatchingStateStorage::new();

        let hash = self.hasher.hash_node(&rule.params().query);
        for entry in self.enodes_hash_table.iter_hash_mut(hash) {
            matching_state.reset();
            matcher.match_enode_to_enode_template(
                &entry.enode,
                &rule.params().query,
                &mut matching_state.get_state(),
            );
            todo!(
                "do something with the matches: {:?}",
                matching_state.matches
            );
        }
    }

    pub fn from_rec_node(rec_node: &RecNode) -> Self {
        let mut egraph = Self::new();
        egraph.add_rec_node(rec_node);
        egraph
    }
}

#[derive(Debug, Clone)]
struct Match {
    pub rule_storage: RewriteRuleStorage,
}

struct MatchingStateStorage {
    rule_storage: RewriteRuleStorage,
    matches: Vec<Match>,
}
impl MatchingStateStorage {
    fn new() -> Self {
        Self {
            rule_storage: RewriteRuleStorage::new(),
            matches: Vec::new(),
        }
    }
    fn get_state(&mut self) -> MatchingState<'_> {
        MatchingState {
            rule_storage: &self.rule_storage,
            matches: &mut self.matches,
        }
    }
    fn reset(&mut self) {
        self.matches.clear();
    }
}

struct MatchingState<'a> {
    rule_storage: &'a RewriteRuleStorage,
    matches: &'a mut Vec<Match>,
}

struct Matcher<'a> {
    union_find: &'a UnionFind<ENode>,
}
impl<'a> Matcher<'a> {
    fn match_enode_to_enode_template(
        &self,
        enode: &ENode,
        template: &ENodeTemplate,
        state: &mut MatchingState,
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
            state.matches.push(Match {
                rule_storage: state.rule_storage.clone(),
            });
            return;
        }

        // now match the links.
        //
        // this part is a little complicated, i'll explain it as best as i can.
        //
        // whenever we encounter a link, due to links pointing to eclasses and not enodes, each link can match multiple enodes.
        // if we ignore template vars for a moment, this essentially means that we want a cartesian product over the matches of
        // each of the links to generate the final list of matches.
        //
        // for example, if the first link had matches [A, B], and the second link had matches [C, D], then in practice we can build
        // 4 different structures that match the rule: (A, C), (A, D), (B, C), (B, D).
        //
        // now let's consider what happens when template vars are added.
        // template vars require that when matching a link, we take into account the variable values inherited from the previous link.
        // but, the previous link could have had multiple matches, each with its own variable binding.
        // so, for each match in the previous link, we want to try to match the current link, but taking into account the variable
        // bindings of the match in the previous link.
        //
        // this will still result in somewhat of a cartesian product, but this time, some of the options will be omitted due to not
        // matching the variables.
        //
        // so, in each iteration, we first start with all matches from the previous link, which are by themselves a cartesian product
        // of the previous link's matches with the matches of the links before it.
        // then, we try to match each of them against each enode that matches the current link, which generates a new cartesian product
        // of the initial list of matches with the list of matches of the current link.
        // then we use the generated list for the next iteration.
        //
        // the initial list of matches, for the first link, is basically just our current initial state when starting to match the links.
        let mut cur_matches: Vec<Match> = vec![Match {
            rule_storage: state.rule_storage.clone(),
        }];
        let mut new_matches: Vec<Match> = Vec::new();
        for cur_link_idx in 0..links_amount {
            let template_link = template_links[cur_link_idx];
            let enode_link = *enode_links[cur_link_idx];

            // we want a cartesian product over matches from previous links, so try matching the link for each previous match
            for cur_match in &cur_matches {
                let mut new_matching_state = MatchingState {
                    rule_storage: &cur_match.rule_storage,
                    matches: &mut new_matches,
                };
                match template_link {
                    TemplateLink::Specific(enode_template) => {
                        self.match_specific_template_link(
                            enode_template,
                            enode_link,
                            &mut new_matching_state,
                        );
                    }
                    TemplateLink::Var(template_var) => {
                        self.match_template_var(*template_var, enode_link, &mut new_matching_state);
                    }
                }
            }

            // new matches now contains the new cartesian product over the current link with all of its previous link.
            //
            // we want to use this new list of matches for matching the next link.
            //
            // so basically we want to set `cur_matches` to `new_matches`, and to clear `new_matches` in preparation for the next
            // iteration.
            //
            // but, doing this will lose the storage that was already allocated in the `cur_matches` vector, which we will then have
            // to re-allocate when re-building the `new_matches` list.
            //
            // so, instead, we perform a swap to keep both allocations.
            std::mem::swap(&mut cur_matches, &mut new_matches);

            // after the swap, `cur_matches` contains the value of `new_matches`, which is the list of matches that we just generated.
            // and, `new_matches` now contains the value of `cur_matches`, which is the matches from the previous link.
            // we no longer need the matches from the previous link, and each iteration assumes that `new_matches` is empty at the start
            // of the iteration, so clear the vector.
            new_matches.clear();
        }
    }

    fn match_template_var(
        &self,
        template_var: TemplateVar,
        enode_link: EClassId,
        state: &mut MatchingState,
    ) {
        let effective_eclass_id = enode_link.to_effective(self.union_find);
        match state.rule_storage.template_var_values.get(template_var) {
            Some(existing_var_value) => {
                if effective_eclass_id != existing_var_value {
                    // no match
                    return;
                }

                // we got a match, and we don't need to change the rule storage at all since we didn't bind any new template vars.
                state.matches.push(Match {
                    rule_storage: state.rule_storage.clone(),
                });
            }
            None => {
                // the variable currently doesn't have any value, so we can bind it and consider it a match.
                let mut new_rule_storage = state.rule_storage.clone();
                new_rule_storage
                    .template_var_values
                    .set(template_var, effective_eclass_id);
                state.matches.push(Match {
                    rule_storage: new_rule_storage,
                });
            }
        }
    }

    fn match_specific_template_link(
        &self,
        template: &ENodeTemplate,
        eclass: EClassId,
        state: &mut MatchingState,
    ) {
        // iterate all enodes in the eclass
        for enode_item_id in self.union_find.items_eq_to(eclass.enode_id.0) {
            let enode = &self.union_find[enode_item_id];
            self.match_enode_to_enode_template(enode, template, state);
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

    // #[test]
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
    }

    #[test]
    fn test_basic_rewrite() {
        // 0xff & ((x & 0xff00) || (x & 0x0))
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
                    rhs: 0.into(),
                }
                .into(),
            }
            .into(),
        }
        .into();

        let mut egraph = EGraph::from_rec_node(&rec_node);
        egraph.apply_rule(&RewriteRule::new(RewriteRuleParams {
            query: BinOpTemplate {
                kind: BinOpKind::And,
                lhs: TemplateVar::new(1).into(),
                rhs: 0.into(),
            }
            .into(),
            rewrite: 0.into(),
        }));
    }
}
