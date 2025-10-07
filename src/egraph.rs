use hashbrown::{DefaultHashBuilder, HashSet, HashTable, hash_table::Entry};
use std::hash::BuildHasher;

use crate::{union_find::*, *};
use std::fmt::Write;

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
pub type ENodeWithEffectiveEClassId = GenericNode<EffectiveEClassId>;

impl ENode {
    /// converts this enode to an enode with an effective eclass id which is correct for the given state of the union find tree.
    pub fn to_enode_with_effective_eclass_id(
        &self,
        union_find: &UnionFind<ENode>,
    ) -> ENodeWithEffectiveEClassId {
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

#[derive(Clone)]
struct ENodeHashTable {
    table: HashTable<ENodeHashTableEntry>,
    hasher: NodeHasher,
}
impl ENodeHashTable {
    fn entry(
        &mut self,
        enode: &ENode,
        enodes_union_find: &UnionFind<ENode>,
    ) -> hashbrown::hash_table::Entry<'_, ENodeHashTableEntry> {
        let enode_with_effective_eclass_id =
            enode.to_enode_with_effective_eclass_id(enodes_union_find);
        let eq_fn = |entry: &ENodeHashTableEntry| {
            entry
                .enode
                .to_enode_with_effective_eclass_id(enodes_union_find)
                == enode_with_effective_eclass_id
        };

        let hash_fn = |entry: &ENodeHashTableEntry| self.hasher.hash_node(&entry.enode);

        let hash = self.hasher.hash_node(&enode);

        let entry = self.table.entry(hash, eq_fn, hash_fn);

        entry
    }
}

/// enode deduplication information when adding an enode
#[derive(Debug, PartialEq, Eq)]
pub enum ENodeDedupInfo {
    /// the added enode is a new enode that we previously didn't have in our egraph.
    New,

    /// the added enode is a duplicate of an existing enode that we already have in the egraph.
    Duplicate,
}

/// the result of adding an enode to the egraph
#[derive(Debug)]
pub struct AddENodeRes {
    /// the eclass id of the new enode
    pub eclass_id: EClassId,

    /// the deduplication info of the added enode
    pub dedup_info: ENodeDedupInfo,
}

#[derive(derive_debug::Dbg, Clone)]
pub struct EGraph {
    enodes_union_find: UnionFind<ENode>,

    #[dbg(skip)]
    enodes_hash_table: ENodeHashTable,
}
impl EGraph {
    pub fn new() -> Self {
        Self {
            enodes_union_find: UnionFind::new(),
            enodes_hash_table: ENodeHashTable {
                table: HashTable::new(),
                hasher: NodeHasher::default(),
            },
        }
    }

    /// adds an enode to the egraph and returns the eclass id which contains it.
    pub fn add_enode(&mut self, enode: ENode) -> AddENodeRes {
        let entry = self
            .enodes_hash_table
            .entry(&enode, &self.enodes_union_find);

        match entry {
            Entry::Occupied(entry) => AddENodeRes {
                eclass_id: EClassId {
                    enode_id: entry.get().id,
                },
                dedup_info: ENodeDedupInfo::Duplicate,
            },
            Entry::Vacant(entry) => {
                let enode_id = ENodeId(self.enodes_union_find.create_new_item(enode.clone()));
                entry.insert(ENodeHashTableEntry {
                    enode,
                    id: enode_id,
                });
                AddENodeRes {
                    eclass_id: EClassId { enode_id },
                    dedup_info: ENodeDedupInfo::New,
                }
            }
        }
    }

    /// replaces the enode with the given enode id with the given new enode.
    pub fn replace_enode(
        &mut self,
        enode_id_to_replace: ENodeId,
        replace_with: ENode,
    ) -> AddENodeRes {
        let entry = self
            .enodes_hash_table
            .entry(&replace_with, &self.enodes_union_find);

        match entry {
            Entry::Occupied(entry) => {
                // the new enode is a duplicate of an enode which already exists.
                // so, we just want to get rid of the original enode, both in the union find tree, and in the hash table.
                //
                // the union find tree doesn't support removing items, but it supports orphaning items.
                //
                // when we query enodes, we first start from the hash table, and then descend by looking at items in the same
                // eclass as the enode that each link points to.
                //
                // so, if we make sure that no existing node points to the original enode, and then we orphan it, then it will
                // become practically inaccessible, which is equivalent to removing it.
                let new_enode_id = entry.get().id;
                let new_eclass_id = EClassId {
                    enode_id: new_enode_id,
                };
                for item in self.enodes_union_find.items_mut() {
                    *item = item.convert_link(|link| {
                        if link.enode_id == enode_id_to_replace {
                            // if this link points to the enode that we want to replace, make it point to the new id instead
                            new_eclass_id
                        } else {
                            *link
                        }
                    })
                }
                self.enodes_union_find.orphan(enode_id_to_replace.0);

                // remove it from the hash table
                let orig_enode = &self.enodes_union_find[enode_id_to_replace.0];
                let Entry::Occupied(orig_entry) = self
                    .enodes_hash_table
                    .entry(&orig_enode, &self.enodes_union_find)
                else {
                    unreachable!();
                };
                orig_entry.remove();

                AddENodeRes {
                    eclass_id: new_eclass_id,
                    dedup_info: ENodeDedupInfo::Duplicate,
                }
            }
            Entry::Vacant(entry) => {
                // the new enode is not a duplicate of an existing node. in this case, we can just re-use the enode id of the original
                // node to store the new node, and we must make sure to also update the hash table.

                // first, insert the new enode into the hash table
                entry.insert(ENodeHashTableEntry {
                    enode: replace_with.clone(),
                    id: enode_id_to_replace,
                });

                // overwrite the original enode in the union find tree with the new enode
                let orig_enode = std::mem::replace(
                    &mut self.enodes_union_find[enode_id_to_replace.0],
                    replace_with,
                );

                // now remove the hash table entry of the old enode
                let Entry::Occupied(orig_entry) = self
                    .enodes_hash_table
                    .entry(&orig_enode, &self.enodes_union_find)
                else {
                    unreachable!();
                };
                orig_entry.remove();

                AddENodeRes {
                    eclass_id: EClassId {
                        enode_id: enode_id_to_replace,
                    },
                    dedup_info: ENodeDedupInfo::New,
                }
            }
        }
    }

    pub fn add_rec_node(&mut self, rec_node: &RecNode) -> AddENodeRes {
        let graph_node = rec_node
            .0
            .convert_link(|link| self.add_rec_node(link).eclass_id);
        self.add_enode(graph_node)
    }

    /// returns whether applying the rule actually added any new information to the egraph.
    pub fn apply_rule(&mut self, rule: &RewriteRule) -> bool {
        let hash = self.enodes_hash_table.hasher.hash_node(&rule.query);

        let mut enode_matches = Vec::new();

        // find the first enode that matches the rule.
        for entry in self.enodes_hash_table.table.iter_hash(hash) {
            let mut matching_state_storage = MatchingStateStorage::new();

            // match the current enode
            self.match_enode_to_enode_template(
                &entry.enode,
                &rule.query,
                &mut matching_state_storage.get_state(),
            );

            enode_matches.extend(matching_state_storage.matches.into_iter().map(|match_obj| {
                ENodeMatch {
                    match_obj,
                    enode_id: entry.id,
                }
            }));
        }

        self.handle_enode_matches(&enode_matches, rule)
    }

    fn handle_enode_matches(&mut self, enode_matches: &[ENodeMatch], rule: &RewriteRule) -> bool {
        let mut did_anything = false;

        // for each match, add the rerwrite result to the egraph
        for enode_match in enode_matches {
            let new_enode =
                self.instantiate_enode_template(&rule.rewrite, &enode_match.match_obj.rule_storage);

            // just add the new enode and union it with the original enode
            let add_res = self.add_enode(new_enode);
            self.enodes_union_find
                .union(add_res.eclass_id.enode_id.0, enode_match.enode_id.0);
            if add_res.dedup_info == ENodeDedupInfo::New {
                did_anything = true;
            }
        }

        did_anything
    }

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
            // the current template link
            let template_link = template_links[cur_link_idx];

            // the eclass that the current enode link points to
            let enode_link_eclass = *enode_links[cur_link_idx];

            // we want a cartesian product over matches from previous links, so try matching the link for each previous match
            for cur_match in &cur_matches {
                let mut new_matching_state = MatchingState {
                    rule_storage: &cur_match.rule_storage,
                    matches: &mut new_matches,
                };
                self.match_eclass_to_template_link(
                    enode_link_eclass,
                    template_link,
                    &mut new_matching_state,
                );
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

        // the final value of `cur_matches` contains the final cartesian product of matches, which is what we want to return.
        // so, copy it out.
        state.matches.append(&mut cur_matches);
    }

    fn match_eclass_to_template_link(
        &self,
        eclass: EClassId,
        template_link: &TemplateLink,
        state: &mut MatchingState,
    ) {
        match template_link {
            TemplateLink::Specific(enode_template) => {
                self.match_eclass_to_specific_template_link(eclass, enode_template, state);
            }
            TemplateLink::Var(template_var) => {
                self.match_eclass_to_template_var(eclass, *template_var, state);
            }
        }
    }

    fn match_eclass_to_template_var(
        &self,
        eclass: EClassId,
        template_var: TemplateVar,
        state: &mut MatchingState,
    ) {
        let effective_eclass_id = eclass.to_effective(&self.enodes_union_find);
        match state.rule_storage.template_var_values.get(template_var) {
            Some(existing_var_value) => {
                if effective_eclass_id != existing_var_value.effective_eclass_id {
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
                new_rule_storage.template_var_values.set(
                    template_var,
                    TemplateVarValue {
                        eclass,
                        effective_eclass_id,
                    },
                );
                state.matches.push(Match {
                    rule_storage: new_rule_storage,
                });
            }
        }
    }

    fn match_eclass_to_specific_template_link(
        &self,
        eclass: EClassId,
        template: &ENodeTemplate,
        state: &mut MatchingState,
    ) {
        // iterate all enodes in the eclass
        for enode_item_id in self
            .enodes_union_find
            .items_eq_to_including_self(eclass.enode_id.0)
        {
            let enode = &self.enodes_union_find[enode_item_id];
            self.match_enode_to_enode_template(enode, template, state);
        }
    }

    fn eclass_as_imm(&self, eclass_id: EClassId) -> Option<Imm> {
        self.enodes_union_find
            .items_eq_to_including_self(eclass_id.enode_id.0)
            .find_map(|item_id| {
                let enode = &self.enodes_union_find[item_id];
                match enode {
                    GenericNode::Imm(imm) => Some(imm),
                    _ => None,
                }
            })
            .copied()
    }

    /// returns `did_anything`
    pub fn perform_constant_folding(&mut self) -> bool {
        let mut did_anything = false;
        for enode_id in self.enodes_union_find.item_ids() {
            let GenericNode::BinOp(BinOp { kind, lhs, rhs }) = &self.enodes_union_find[enode_id]
            else {
                continue;
            };
            let Some(lhs_imm) = self.eclass_as_imm(*lhs) else {
                continue;
            };
            let Some(rhs_imm) = self.eclass_as_imm(*rhs) else {
                continue;
            };
            let res = kind.apply_to_imms(lhs_imm, rhs_imm);

            // add the imm and union it with the original enode
            let add_res = self.add_enode(GenericNode::Imm(Imm(res)));
            self.enodes_union_find
                .union(add_res.eclass_id.enode_id.0, enode_id);

            did_anything = true;
        }
        did_anything
    }

    pub fn apply_rule_set(&mut self, rule_set: &RewriteRuleSet) {
        loop {
            let mut did_anything = false;
            for rule in rule_set.rules() {
                did_anything |= self.perform_constant_folding();
                did_anything |= self.apply_rule(rule);
            }
            if !did_anything {
                break;
            }
        }
    }

    fn instantiate_enode_template(
        &mut self,
        template: &ENodeTemplate,
        rule_storage: &RewriteRuleStorage,
    ) -> ENode {
        template.convert_link(|template_link| match template_link {
            TemplateLink::Specific(inner_template) => {
                let enode = self.instantiate_enode_template(inner_template, rule_storage);
                self.add_enode(enode).eclass_id
            }
            TemplateLink::Var(template_var) => {
                let var_value = rule_storage.template_var_values.get(*template_var).unwrap();
                var_value.eclass
            }
        })
    }

    pub fn from_rec_node(rec_node: &RecNode) -> Self {
        let mut egraph = Self::new();
        egraph.add_rec_node(rec_node);
        egraph
    }

    pub fn to_dot(&self) -> String {
        let mut out = String::new();

        fn eclass_id_to_str(eclass_id: UnionFindAnyId) -> String {
            match eclass_id {
                UnionFindAnyId::Item(item_id) => format!("eclass_root_item_{}", item_id.0.get()),
                UnionFindAnyId::Parent(parent_id) => {
                    format!("eclass_root_parent_{}", parent_id.0.get())
                }
            }
        }

        let eclasses: HashSet<UnionFindAnyId> = self
            .enodes_union_find
            .item_ids()
            .map(|item_id| self.enodes_union_find.root_of_item(item_id))
            .collect();

        for &eclass in &eclasses {
            let eclass_id_str = eclass_id_to_str(eclass);
            writeln!(
                &mut out,
                "  subgraph cluster_{} {{\n    color=gray60; style=\"rounded\";",
                eclass_id_str
            )
            .unwrap();

            // one node per enode in the class
            for (i, enode_id) in self
                .enodes_union_find
                .items_eq_to_any_including_self(eclass)
                .enumerate()
            {
                let label = match &self.enodes_union_find[enode_id] {
                    GenericNode::Imm(imm) => format!("0x{:x}", imm.0),
                    GenericNode::Var(var) => format!("var{}", var.0),
                    GenericNode::BinOp(bin_op) => bin_op.kind.to_string(),
                    GenericNode::UnOp(un_op) => un_op.kind.to_string(),
                };
                writeln!(
                    &mut out,
                    "    {}_{} [label=\"{}\"];",
                    eclass_id_str, i, label
                )
                .unwrap();
            }

            out.push_str("  }\n");
        }

        // edges from each enode to target e-class clusters
        for eclass in eclasses {
            let eclass_id_str = eclass_id_to_str(eclass);
            for (i, enode_id) in self
                .enodes_union_find
                .items_eq_to_any_including_self(eclass)
                .enumerate()
            {
                let enode = &self.enodes_union_find[enode_id];
                for link in enode.links() {
                    // route to target cluster anchor; ltail/lhead draw the edge between clusters
                    let target_eclass_id = self.enodes_union_find.root_of_item(link.enode_id.0);
                    let target_eclass_id_str = eclass_id_to_str(target_eclass_id);
                    writeln!(
                        &mut out,
                        "  {}_{} -> {}_0 [lhead=cluster_{}];",
                        eclass_id_str, i, target_eclass_id_str, target_eclass_id_str
                    )
                    .unwrap();
                }
            }
        }

        return format!(
            r##"
            digraph egraph {{
                compound=true;
                rankdir=TB;
                bgcolor="#181818"
                node [
                    fontcolor = "#e6e6e6",
                    style = filled,
                    color = "#e6e6e6",
                    fillcolor = "#333333"
                ]
                edge [
                    color = "#e6e6e6",
                    fontcolor = "#e6e6e6"
                ]
                {}
            }}
            "##,
            out
        );
    }
}

#[derive(Debug, Clone)]
struct Match {
    pub rule_storage: RewriteRuleStorage,
}

#[derive(Debug, Clone)]
struct ENodeMatch {
    pub match_obj: Match,
    pub enode_id: ENodeId,
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
}

#[derive(Debug)]
struct MatchingState<'a> {
    rule_storage: &'a RewriteRuleStorage,
    matches: &'a mut Vec<Match>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dedup_basic() {
        let mut egraph = EGraph::new();
        let enode = ENode::Var(Var(5));
        let res1 = egraph.add_enode(enode.clone());
        let res2 = egraph.add_enode(enode.clone());
        assert_eq!(res1.dedup_info, ENodeDedupInfo::New);
        assert_eq!(res2.dedup_info, ENodeDedupInfo::Duplicate);
        assert_eq!(res1.eclass_id.enode_id, res2.eclass_id.enode_id);
        assert_eq!(egraph.enodes_union_find.len(), 1);
    }

    #[test]
    fn test_dedup_nested() {
        let mut egraph = EGraph::new();
        let var1_res = egraph.add_enode(ENode::Var(Var(1)));
        let var2_res = egraph.add_enode(ENode::Var(Var(2)));

        let enode = ENode::BinOp(BinOp {
            kind: BinOpKind::Add,
            lhs: var1_res.eclass_id,
            rhs: var2_res.eclass_id,
        });

        let res1 = egraph.add_enode(enode.clone());

        // add something in between just to add some noise
        let var3_res = egraph.add_enode(ENode::Var(Var(3)));

        // re-add the same enode
        let res2 = egraph.add_enode(enode.clone());

        // make sure that it got de-duplicated
        assert_eq!(res1.dedup_info, ENodeDedupInfo::New);
        assert_eq!(res2.dedup_info, ENodeDedupInfo::Duplicate);
        assert_eq!(res1.eclass_id.enode_id, res2.eclass_id.enode_id);
        assert_eq!(egraph.enodes_union_find.len(), 4);

        // now do some more
        let enode = ENode::UnOp(UnOp {
            kind: UnOpKind::Neg,
            operand: var1_res.eclass_id,
        });

        let res1 = egraph.add_enode(enode.clone());
        let res2 = egraph.add_enode(enode.clone());

        // make sure that it got de-duplicated
        assert_eq!(res1.dedup_info, ENodeDedupInfo::New);
        assert_eq!(res2.dedup_info, ENodeDedupInfo::Duplicate);
        assert_eq!(res1.eclass_id.enode_id, res2.eclass_id.enode_id);
        assert_eq!(egraph.enodes_union_find.len(), 5);

        // even more
        let enode = ENode::BinOp(BinOp {
            kind: BinOpKind::Mul,
            lhs: var3_res.eclass_id,
            rhs: var2_res.eclass_id,
        });

        let res1 = egraph.add_enode(enode.clone());
        let res2 = egraph.add_enode(enode.clone());

        // make sure that it got de-duplicated
        assert_eq!(res1.dedup_info, ENodeDedupInfo::New);
        assert_eq!(res2.dedup_info, ENodeDedupInfo::Duplicate);
        assert_eq!(res1.eclass_id.enode_id, res2.eclass_id.enode_id);
        assert_eq!(egraph.enodes_union_find.len(), 6);
    }

    #[test]
    fn test_basic_rewrite() {
        // 0xff & ((x & 0xff00) | (x & 0xff0000))
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

        let mut egraph = EGraph::from_rec_node(&rec_node);

        let rule_set = RewriteRuleSet::from_rules([
            // (x & 0) => 0
            RewriteRuleParams {
                query: BinOpTemplate {
                    kind: BinOpKind::And,
                    lhs: TemplateVar::new(1).into(),
                    rhs: 0.into(),
                }
                .into(),
                rewrite: 0.into(),
                keep_original: false,
                bi_directional: false,
            },
            // a & (b | c) => (a & b) | (a & c)
            RewriteRuleParams {
                query: BinOpTemplate {
                    kind: BinOpKind::And,
                    lhs: TemplateVar::new(1).into(),
                    rhs: BinOpTemplate {
                        kind: BinOpKind::Or,
                        lhs: TemplateVar::new(2).into(),
                        rhs: TemplateVar::new(3).into(),
                    }
                    .into(),
                }
                .into(),
                rewrite: BinOpTemplate {
                    kind: BinOpKind::Or,
                    lhs: BinOpTemplate {
                        kind: BinOpKind::And,
                        lhs: TemplateVar::new(1).into(),
                        rhs: TemplateVar::new(2).into(),
                    }
                    .into(),
                    rhs: BinOpTemplate {
                        kind: BinOpKind::And,
                        lhs: TemplateVar::new(1).into(),
                        rhs: TemplateVar::new(3).into(),
                    }
                    .into(),
                }
                .into(),
                keep_original: true,
                bi_directional: true,
            },
            // a & (b & c) => (a & b) & c
            RewriteRuleParams {
                query: BinOpTemplate {
                    kind: BinOpKind::And,
                    lhs: TemplateVar::new(1).into(),
                    rhs: BinOpTemplate {
                        kind: BinOpKind::And,
                        lhs: TemplateVar::new(2).into(),
                        rhs: TemplateVar::new(3).into(),
                    }
                    .into(),
                }
                .into(),
                rewrite: BinOpTemplate {
                    kind: BinOpKind::And,
                    lhs: BinOpTemplate {
                        kind: BinOpKind::And,
                        lhs: TemplateVar::new(1).into(),
                        rhs: TemplateVar::new(2).into(),
                    }
                    .into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
                keep_original: true,
                bi_directional: true,
            },
            // a & b => b & a
            RewriteRuleParams {
                query: BinOpTemplate {
                    kind: BinOpKind::And,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(2).into(),
                }
                .into(),
                rewrite: BinOpTemplate {
                    kind: BinOpKind::And,
                    lhs: TemplateVar::new(2).into(),
                    rhs: TemplateVar::new(1).into(),
                }
                .into(),
                keep_original: true,
                bi_directional: true,
            },
        ]);

        std::fs::write("./graph.dot", egraph.to_dot()).unwrap();

        egraph.apply_rule_set(&rule_set);

        std::fs::write("./graph2.dot", egraph.to_dot()).unwrap();

        panic!();
    }
}
