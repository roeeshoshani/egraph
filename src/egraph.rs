use duct::cmd;
use hashbrown::{DefaultHashBuilder, HashSet, HashTable, hash_table::Entry};
use std::{hash::BuildHasher, io::Write as _};
use tempfile::NamedTempFile;

use crate::{did_anything::DidAnything, union_find::*, *};
use std::fmt::Write as _;

/// the id of an enode.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ENodeId(pub UnionFindItemId);

/// the id of an eclass.
///
/// NOTE: this does not implement `Hash`, `PartialEq` and `Eq` due to how it is implemented.
/// we can have 2 instances of this type which point to different enodes, so the derived `Eq` implementation will say that they are not
/// equal, but in practice the 2 enodes that they point to are part of the same eclass, so the 2 eclass ids should be equal.
///
/// checking if 2 instances of this type are equal requires accessing the union find tree.
#[derive(Debug, Clone, Copy)]
pub struct EClassId {
    /// an id of some enode which is part of this eclass.
    /// this can be used to iterate over all enodes in the eclass.
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
/// compare eclass ids, which means that we can't compare enodes (since they contain eclass ids).
///
/// this type represents an eclass id which can actually be compared to other eclass id. this is resolved by taking the root node
/// of the enode id in the union find tree.
///
/// this id is only true for a given snapshot of the union find tree. once the tree is modified, it is no longer up to date, since
/// the root may no longer be the real root, it may now have an ancestor (or even multiple ancestors).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EffectiveEClassId(pub UnionFindItemId);

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

/// a hash table entry in our enode hash table.
#[derive(Debug, Clone)]
pub struct ENodeHashTableEntry {
    /// the enode.
    enode: ENode,

    /// the id of the enode.
    id: ENodeId,
}

/// a node hasher. used for implementing our enode hash table.
#[derive(Debug, Default, Clone)]
pub struct NodeHasher(DefaultHashBuilder);
impl NodeHasher {
    /// performs structural hashing on the given node, ignoring the link. the calculated hash then allows performing lookup
    /// in the hash table.
    pub fn hash_node<L>(&self, node: &GenericNode<L>) -> u64 {
        // hash the enode, but ignore the links. only take its structure into account.
        self.0.hash_one(node.convert_link(|_| ()))
    }
}

/// a hash table of enodes.
///
/// the enodes are hashed according to their structure, ignoring the links, but contain the fully detailed enode values including
/// the links, which allows for comparison against exact entries.
#[derive(Clone)]
pub struct ENodeHashTable {
    pub table: HashTable<ENodeHashTableEntry>,
    pub hasher: NodeHasher,
}
impl ENodeHashTable {
    /// finds the entry in the hash table for the given enode.
    ///
    /// if an exact enode already exists, returns an occupied entry. otherwise, returns a vacant entry.
    pub fn entry(
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

/// enode deduplication information when adding an enode.
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

/// an egraph.
#[derive(derive_debug::Dbg, Clone)]
pub struct EGraph {
    pub enodes_union_find: UnionFind<ENode>,

    #[dbg(skip)]
    pub enodes_hash_table: ENodeHashTable,
}
impl EGraph {
    /// returns a new empty egraph.
    pub fn new() -> Self {
        Self {
            enodes_union_find: UnionFind::new(),
            enodes_hash_table: ENodeHashTable {
                table: HashTable::new(),
                hasher: NodeHasher::default(),
            },
        }
    }

    /// adds an enode to the egraph, puts it in a new eclass which only contains that single enode, and returns the id of that eclass.
    ///
    /// if the exact enode already exists in the egraph, returns the id of the existing enode.
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

    pub fn are_eclass_ids_eq(&self, a: EClassId, b: EClassId) -> bool {
        self.enodes_union_find.are_eq(a.enode_id.0, b.enode_id.0)
    }

    /// adds a recursive node to the egraph, converting each node to an enode.
    pub fn add_rec_node(&mut self, rec_node: &RecNode) -> AddENodeRes {
        let graph_node = rec_node
            .0
            .convert_link(|link| self.add_rec_node(link).eclass_id);
        self.add_enode(graph_node)
    }

    pub fn match_rule(&self, rule: &RewriteRule) -> Vec<ENodeMatch> {
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

        enode_matches
    }

    pub fn apply_rule(&mut self, rule: &RewriteRule) -> DidAnything {
        let matches = self.match_rule(rule);
        self.handle_enode_matches(&matches, rule)
    }

    fn handle_enode_matches(
        &mut self,
        enode_matches: &[ENodeMatch],
        rule: &RewriteRule,
    ) -> DidAnything {
        let mut did_anything = DidAnything::False;

        // for each match, add the rerwrite result to the egraph
        for enode_match in enode_matches {
            let add_res = self.instantiate_enode_template_link(
                &rule.rewrite,
                &enode_match.match_obj.rule_storage,
            );

            let union_res = self
                .enodes_union_find
                .union(add_res.eclass_id.enode_id.0, enode_match.enode_id.0);
            if add_res.dedup_info == ENodeDedupInfo::New || union_res == UnionRes::New {
                did_anything = DidAnything::True;
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
            state.matches.push(state.cur_match.clone());
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
        let mut cur_matches: Vec<Match> = vec![state.cur_match.clone()];
        let mut new_matches: Vec<Match> = Vec::new();
        for cur_link_idx in 0..links_amount {
            // the current template link
            let template_link = template_links[cur_link_idx];

            // the eclass that the current enode link points to
            let enode_link_eclass = *enode_links[cur_link_idx];

            // we want a cartesian product over matches from previous links, so try matching the link for each previous match
            for cur_match in &cur_matches {
                let mut new_matching_state = MatchingState {
                    cur_match: cur_match,
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

        match state
            .cur_match
            .rule_storage
            .template_var_values
            .get(template_var)
        {
            Some(existing_var_value) => {
                if effective_eclass_id != existing_var_value.effective_eclass_id {
                    // no match
                    return;
                }

                // we got a match, and we don't need to change the rule storage at all since we didn't bind any new template vars.
                state.matches.push(state.cur_match.clone());
            }
            None => {
                // the variable currently doesn't have any value, so we can bind it and consider it a match.
                let mut new_match = state.cur_match.clone();
                new_match.rule_storage.template_var_values.set(
                    template_var,
                    TemplateVarValue {
                        eclass,
                        effective_eclass_id,
                    },
                );
                state.matches.push(new_match);
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
        for enode_item_id in self.enodes_union_find.items_eq_to(eclass.enode_id.0) {
            let enode = &self.enodes_union_find[enode_item_id];
            self.match_enode_to_enode_template(enode, template, state);
        }
    }

    fn eclass_as_imm(&self, eclass_id: EClassId) -> Option<Imm> {
        self.enodes_union_find
            .items_eq_to(eclass_id.enode_id.0)
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
    pub fn perform_constant_folding(&mut self) -> DidAnything {
        let mut did_anything = DidAnything::False;
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
            let union_res = self
                .enodes_union_find
                .union(add_res.eclass_id.enode_id.0, enode_id);

            did_anything = DidAnything::from(union_res == UnionRes::New);
        }
        did_anything
    }

    pub fn apply_rule_set(&mut self, rule_set: &RewriteRuleSet, max_iterations: Option<usize>) {
        let mut i = 0;
        loop {
            let mut did_anything = DidAnything::False;
            for rule in rule_set.rules() {
                did_anything |= self.perform_constant_folding();
                did_anything |= self.apply_rule(rule);
            }
            if !did_anything.as_bool() {
                break;
            }
            if let Some(max_iterations) = max_iterations {
                i += 1;
                if i == max_iterations {
                    break;
                }
            }
        }
    }

    fn instantiate_enode_template_link(
        &mut self,
        template_link: &TemplateLink,
        rule_storage: &RewriteRuleStorage,
    ) -> AddENodeRes {
        match template_link {
            TemplateLink::Specific(inner_template) => {
                let enode = self.instantiate_enode_template(inner_template, rule_storage);
                self.add_enode(enode)
            }
            TemplateLink::Var(template_var) => {
                let var_value = rule_storage.template_var_values.get(*template_var).unwrap();
                AddENodeRes {
                    eclass_id: var_value.eclass,
                    dedup_info: ENodeDedupInfo::Duplicate,
                }
            }
        }
    }

    fn instantiate_enode_template(
        &mut self,
        template: &ENodeTemplate,
        rule_storage: &RewriteRuleStorage,
    ) -> ENode {
        template.convert_link(|template_link| {
            self.instantiate_enode_template_link(template_link, rule_storage)
                .eclass_id
        })
    }

    pub fn from_rec_node(rec_node: &RecNode) -> (Self, EClassId) {
        let mut egraph = Self::new();
        let add_node_res = egraph.add_rec_node(rec_node);
        (egraph, add_node_res.eclass_id)
    }

    pub fn dump_dot_svg(&self, out_file_path: &str) {
        let mut tmpfile = NamedTempFile::new().unwrap();
        tmpfile.write_all(self.to_dot().as_bytes()).unwrap();
        cmd!("fdp", "-Tsvg", tmpfile.path(), "-o", out_file_path)
            .run()
            .unwrap();
    }

    pub fn enode_get_sample_rec_node(&self, enode_id: ENodeId) -> RecNode {
        RecNode(
            self.enodes_union_find[enode_id.0].convert_link(|link_eclass_id| {
                Box::new(self.eclass_get_sample_rec_node(*link_eclass_id))
            }),
        )
    }

    pub fn eclass_get_sample_rec_node(&self, eclass_id: EClassId) -> RecNode {
        self.enode_get_sample_rec_node(eclass_id.enode_id)
    }

    fn try_to_gexf(&self) -> gexf::Result<String> {
        let mut builder = gexf::GraphBuilder::new(gexf::EdgeType::Directed)
            .meta("egraph", "a visualization of an egraph");
        for enode_item_id in self.enodes_union_find.item_ids() {
            let enode_id = ENodeId(enode_item_id);
            let enode = &self.enodes_union_find[enode_item_id];

            let eclass_label = self
                .enodes_union_find
                .items_eq_to(enode_item_id)
                .min()
                .unwrap();

            let node_label = if enode.links().is_empty() {
                enode.structural_display()
            } else {
                format!(
                    "{}    {{ {} }}",
                    enode.structural_display(),
                    self.enode_get_sample_rec_node(enode_id).to_string()
                )
            };

            let node_id = enode_item_id.0.to_string();

            let node = gexf::Node::new(&node_id)
                .with_label(node_label)
                .with_attr("eclass", eclass_label.0.to_string());
            builder = builder.add_node(node)?;

            // edges
            for link in enode.links() {
                builder = builder.add_edge(&node_id, link.enode_id.0.0.to_string())?;
            }
        }
        builder.try_build()?.to_string()
    }

    pub fn to_gexf(&self) -> String {
        self.try_to_gexf().unwrap()
    }

    pub fn to_dot(&self) -> String {
        let mut out = String::new();

        fn eclass_id_to_str(eclass_root_item_id: UnionFindItemId) -> String {
            format!("eclass_item_{}", eclass_root_item_id.0.get())
        }

        let eclasses: HashSet<UnionFindItemId> = self
            .enodes_union_find
            .item_ids()
            .map(|item_id| self.enodes_union_find.root_of_item(item_id))
            .collect();

        for &eclass in &eclasses {
            let eclass_id_str = eclass_id_to_str(eclass);
            writeln!(
                &mut out,
                "  subgraph cluster_{} {{\n    color=gray60; style=\"rounded\"; fontcolor=\"white\"; label=\"{}\"",
                eclass_id_str,
                self.eclass_get_sample_rec_node(EClassId {
                    enode_id: ENodeId(
                        self.enodes_union_find.root_of_item(eclass)
                    )
                })
            )
            .unwrap();

            // one node per enode in the class
            for (i, enode_id) in self.enodes_union_find.items_eq_to(eclass).enumerate() {
                let label = self.enodes_union_find[enode_id].structural_display();
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
            for (i, enode_id) in self.enodes_union_find.items_eq_to(eclass).enumerate() {
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
pub struct Match {
    pub rule_storage: RewriteRuleStorage,
}
impl Match {
    pub fn new() -> Self {
        Self {
            rule_storage: RewriteRuleStorage::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ENodeMatch {
    pub match_obj: Match,
    pub enode_id: ENodeId,
}

struct MatchingStateStorage {
    initial_match: Match,
    matches: Vec<Match>,
}
impl MatchingStateStorage {
    fn new() -> Self {
        Self {
            initial_match: Match::new(),
            matches: Vec::new(),
        }
    }
    fn get_state(&mut self) -> MatchingState<'_> {
        MatchingState {
            cur_match: &self.initial_match,
            matches: &mut self.matches,
        }
    }
}

#[derive(Debug)]
struct MatchingState<'a> {
    cur_match: &'a Match,
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

        let (mut egraph, root_eclass) = EGraph::from_rec_node(&rec_node);

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
                bi_directional: false,
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
                bi_directional: false,
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
                bi_directional: false,
            },
            // a & a => a
            RewriteRuleParams {
                query: BinOpTemplate {
                    kind: BinOpKind::And,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(1).into(),
                }
                .into(),
                rewrite: TemplateVar::new(1).into(),
                keep_original: true,
                bi_directional: false,
            },
        ]);

        let zero_eclass = egraph.add_enode(0.into()).eclass_id;

        assert!(!egraph.are_eclass_ids_eq(zero_eclass, root_eclass));
        egraph.apply_rule_set(&rule_set, None);
        assert!(egraph.are_eclass_ids_eq(zero_eclass, root_eclass));
    }
}
