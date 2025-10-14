use derive_more::{Add, AddAssign};
use duct::cmd;
use hashbrown::{DefaultHashBuilder, HashMap, HashSet, HashTable, hash_table::Entry};
use std::{hash::BuildHasher, io::Write as _};
use tempfile::NamedTempFile;

use crate::{
    did_anything::DidAnything,
    graph::{Graph, GraphNodeId},
    union_find::*,
    *,
};
use std::fmt::Write as _;

/// the id of an enode.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ENodeId(pub UnionFindItemId);
impl ENodeId {
    /// returns an eclass id object representing the eclass which contains this enode id.
    pub fn eclass_id(&self) -> EClassId {
        EClassId { enode_id: *self }
    }
}

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
        EffectiveEClassId {
            eclass_root: ENodeId(union_find.root_of_item(self.enode_id.0)),
        }
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
pub struct EffectiveEClassId {
    /// the root node of the eclass in the union find tree.
    pub eclass_root: ENodeId,
}
impl EffectiveEClassId {
    /// converts this effective eclass id back to an eclass id.
    pub fn to_eclass_id(&self) -> EClassId {
        self.eclass_root.eclass_id()
    }
}

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
    pub fn entry_mut(
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
    enodes_union_find: UnionFind<ENode>,

    next_internal_var: InternalVar,

    #[dbg(skip)]
    enodes_hash_table: ENodeHashTable,
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
            next_internal_var: InternalVar(0),
        }
    }

    fn alloc_internal_var(&mut self) -> InternalVar {
        let res = self.next_internal_var;
        self.next_internal_var.0 += 1;
        res
    }

    /// union the given 2 enodes.
    pub fn union(&self, a: EClassId, b: EClassId) -> UnionRes {
        self.enodes_union_find.union(a.enode_id.0, b.enode_id.0)
    }

    /// adds an enode to the egraph, puts it in a new eclass which only contains that single enode, and returns the id of that eclass.
    ///
    /// if the exact enode already exists in the egraph, returns the id of the existing enode.
    pub fn add_enode(&mut self, enode: ENode) -> AddENodeRes {
        let entry = self
            .enodes_hash_table
            .entry_mut(&enode, &self.enodes_union_find);

        match entry {
            Entry::Occupied(entry) => AddENodeRes {
                eclass_id: entry.get().id.eclass_id(),
                dedup_info: ENodeDedupInfo::Duplicate,
            },
            Entry::Vacant(entry) => {
                let enode_id = ENodeId(self.enodes_union_find.create_new_item(enode.clone()));
                entry.insert(ENodeHashTableEntry {
                    enode,
                    id: enode_id,
                });
                AddENodeRes {
                    eclass_id: enode_id.eclass_id(),
                    dedup_info: ENodeDedupInfo::New,
                }
            }
        }
    }

    /// checks if the given two eclass ids are equal.
    ///
    /// eclass ids can't be compared directly since they contain lazily evaluated data.
    /// two eclass id instances may point to the same eclass even though structurally comparing the data stored in them will show
    /// that they are not equal.
    ///
    /// so, this function allows for truly comparing 2 eclass ids for a given state of the egraph.
    pub fn are_eq(&self, a: EClassId, b: EClassId) -> bool {
        self.enodes_union_find.are_eq(a.enode_id.0, b.enode_id.0)
    }

    /// adds a recursive node to the egraph, converting each node to an enode.
    pub fn add_rec_node(&mut self, rec_node: &RecNode) -> AddENodeRes {
        let graph_node = rec_node
            .0
            .convert_link(|link| self.add_rec_node(link).eclass_id);
        self.add_enode(graph_node)
    }

    /// match the given rule to any enodes in the egraph and return a list of matches.
    pub fn match_rule(&self, rule: &RewriteRule) -> Vec<ENodeMatch> {
        let hash = self.enodes_hash_table.hasher.hash_node(&rule.query);

        let mut enode_matches = Vec::new();

        // find the first enode that matches the rule.
        for entry in self.enodes_hash_table.table.iter_hash(hash) {
            let mut matching_state_storage = MatchingStateStorage::new();

            let eclass_id = entry.id.eclass_id();
            let effective_eclass_id = eclass_id.to_effective(&self.enodes_union_find);

            // match the current enode
            self.match_enode_to_enode_template(
                &entry.enode,
                effective_eclass_id,
                &rule.query,
                &mut matching_state_storage.get_state(),
            );

            enode_matches.extend(
                matching_state_storage
                    .match_ctxs
                    .into_iter()
                    .map(|match_ctx| ENodeMatch {
                        rule_storage: match_ctx.rule_storage,
                        enode_id: entry.id,
                    }),
            );
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
            let add_res =
                self.instantiate_enode_template_link(&rule.rewrite, &enode_match.rule_storage);

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
        effective_eclass_id: EffectiveEClassId,
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

        // sanity. the structural matching should already guarantee this.
        assert_eq!(enode_links.len(), template_links.len());

        // we defer adding the eclass if to the list of visited eclasses because we want to avoid redundant clones of the state,
        // but at this point we performed strctural comparison, so we are pretty sure that the clone will be required.
        let new_match = state
            .cur_match
            .with_added_visited_eclass(effective_eclass_id);

        let links_amount = enode_links.len();

        if links_amount == 0 {
            // if there are no links, the structural comparison that we performed above is enough, so this enode is a match.
            state.match_ctx.push(new_match);
            return;
        }

        // now match the links.
        //
        // this part is a little complicated, i'll explain it as best as i can.
        //
        // whenever we encounter a link, due to links pointing to eclasses and not enodes, each link can match multiple enodes.
        // if we ignore template vars for a moment, this essentially means that we want a cartesian product over the match contexts
        // of each of the links to generate the final list of match contexts.
        //
        // for example, if the first link had match contexts [A, B], and the second link had match contexts [C, D], then in practice
        // we can build 4 different structures that match the rule: (A, C), (A, D), (B, C), (B, D).
        //
        // now let's consider what happens when template vars are added.
        // template vars require that when matching a link, we take into account the variable values inherited from the previous link.
        // but, the previous link could have had multiple match contexts, each with its own variable binding.
        // so, for each match context in the previous link, we want to try to match the current link, but taking into account the
        // variable bindings of the match context in the previous link.
        //
        // this will still result in somewhat of a cartesian product, but this time, some of the options will be omitted due to not
        // matching the variables.
        //
        // so, in each iteration, we first start with all match contexts from the previous link, which are by themselves a cartesian
        // product of the previous link's match contexts with the match contexts of the links before it.
        // then, we try to match each of them against each enode that matches the current link, which generates a new cartesian product
        // of the initial list of match contexts with the list of match contexts of the current link.
        // then we use the generated list for the next iteration.
        //
        // the initial list of match contexts, for the first link, is basically just our current initial state when starting to
        // match the links.
        let mut cur_match_ctxs: Vec<MatchCtx> = vec![new_match];
        let mut new_match_ctxs: Vec<MatchCtx> = Vec::new();
        for cur_link_idx in 0..links_amount {
            // the current template link
            let template_link = template_links[cur_link_idx];

            // the eclass that the current enode link points to
            let enode_link_eclass = *enode_links[cur_link_idx];

            // we want a cartesian product over match contexts from previous links, so try matching the link for each previous match
            for cur_match in &cur_match_ctxs {
                let mut new_matching_state = MatchingState {
                    cur_match: cur_match,
                    match_ctx: &mut new_match_ctxs,
                };
                self.match_eclass_to_template_link(
                    enode_link_eclass,
                    template_link,
                    &mut new_matching_state,
                );
            }

            // new match contexts now contains the new cartesian product over the current link with all of its previous link.
            //
            // we want to use this new list of match contexts for matching the next link.
            //
            // so basically we want to set `cur_match_ctxs` to `new_match_ctxs`, and to clear `new_match_ctxs` in preparation for the
            // next iteration.
            //
            // but, doing this will lose the storage that was already allocated in the `cur_match_ctxs` vector, which we will then have
            // to re-allocate when re-building the `new_match_ctxs` list.
            //
            // so, instead, we perform a swap to keep both allocations.
            std::mem::swap(&mut cur_match_ctxs, &mut new_match_ctxs);

            // after the swap, `cur_match_ctxs` contains the value of `new_match_ctxs`, which is the list of match contexts that we
            // just generated.
            // and, `new_match_ctxs` now contains the value of `cur_match_ctxs`, which is the match contexts from the previous link.
            // we no longer need the match contexts from the previous link, and each iteration assumes that `new_match_ctxs` is empty
            // at the start of the iteration, so clear the vector.
            new_match_ctxs.clear();
        }

        // the final value of `cur_match_ctxs` contains the final cartesian product of match contexts, which is what we want to return.
        // so, copy it out.
        state.match_ctx.append(&mut cur_match_ctxs);
    }

    fn match_eclass_to_template_link(
        &self,
        eclass_id: EClassId,
        template_link: &TemplateLink,
        state: &mut MatchingState,
    ) {
        let effective_eclass_id = eclass_id.to_effective(&self.enodes_union_find);
        match template_link {
            TemplateLink::Specific(enode_template) => {
                self.match_eclass_to_specific_template_link(
                    effective_eclass_id,
                    enode_template,
                    state,
                );
            }
            TemplateLink::Var(template_var) => {
                self.match_eclass_to_template_var(effective_eclass_id, *template_var, state);
            }
        }
    }

    fn match_eclass_to_template_var(
        &self,
        effective_eclass_id: EffectiveEClassId,
        template_var: TemplateVar,
        state: &mut MatchingState,
    ) {
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
                //
                // we add the eclass id here since we defer it until we are sure we have a match.
                state.match_ctx.push(
                    state
                        .cur_match
                        .with_added_visited_eclass(effective_eclass_id),
                );
            }
            None => {
                // the variable currently doesn't have any value, so we can bind it and consider it a match.
                //
                // we add the eclass id here since we defer it until we are sure we have a match.
                let mut new_match = state
                    .cur_match
                    .with_added_visited_eclass(effective_eclass_id);
                new_match.rule_storage.template_var_values.set(
                    template_var,
                    TemplateVarValue {
                        effective_eclass_id,
                    },
                );
                state.match_ctx.push(new_match);
            }
        }
    }

    fn match_eclass_to_specific_template_link(
        &self,
        effective_eclass_id: EffectiveEClassId,
        template: &ENodeTemplate,
        state: &mut MatchingState,
    ) {
        // when matching an eclass against a specific template link, make sure that we haven't visited this eclass already, to prevent
        // following eclass loops which will blow up the graph with redundant expressions.
        //
        // NOTE: this is only done in the case of matching against a specific template link, and not when matching against a template
        // variable. this is intentional, since we want to allow loops in our templates, for example, we want to allow the following
        // template `x & (1 + x)`.
        // additionally, note that in the case of matching against template variables, we don't need to worry about graph blow up, since
        // template variables don't cause expansions of eclasses, they only check for matching, unlike specific template links, which
        // cause expansion of an eclass to each of its enode forms.
        if state.cur_match.has_visited_eclass(effective_eclass_id) {
            // don't loop
            return;
        }

        // iterate all enodes in the eclass
        for enode_item_id in self
            .enodes_union_find
            .items_eq_to(effective_eclass_id.eclass_root.0)
        {
            let enode = &self.enodes_union_find[enode_item_id];
            self.match_enode_to_enode_template(enode, effective_eclass_id, template, state);
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

    /// propegate all unions such that if `a == b`, `f(a) == f(b)`, which makes us uphold the egraph's congruence invariant.
    pub fn propegate_unions(&mut self) {
        loop {
            let mut did_anything = DidAnything::False;
            for enode_item_id in self.enodes_union_find.item_ids() {
                let enode_id = ENodeId(enode_item_id);
                let enode = &self.enodes_union_find[enode_item_id];
                let enode_with_effective_eclass_id =
                    enode.to_enode_with_effective_eclass_id(&self.enodes_union_find);
                let hash = self.enodes_hash_table.hasher.hash_node(enode);
                for hash_table_entry in self.enodes_hash_table.table.iter_hash(hash) {
                    if hash_table_entry
                        .enode
                        .to_enode_with_effective_eclass_id(&self.enodes_union_find)
                        == enode_with_effective_eclass_id
                    {
                        let union_res =
                            self.union(enode_id.eclass_id(), hash_table_entry.id.eclass_id());
                        if union_res == UnionRes::New {
                            did_anything = DidAnything::True;
                        }
                    }
                }
            }
            if !did_anything.as_bool() {
                break;
            }
        }
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
            self.propegate_unions();
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
                    eclass_id: var_value.effective_eclass_id.to_eclass_id(),
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

    pub fn from_graph(graph: &Graph) -> (Self, GraphToEgraphTranslationMap) {
        let mut egraph = Self::new();
        let translation_map = egraph.add_graph(graph);
        (egraph, translation_map)
    }

    /// adds a graph to the egraph, converting each graph node to an enode.
    pub fn add_graph(&mut self, graph: &Graph) -> GraphToEgraphTranslationMap {
        // first, represent each graph node as an internal var, and create a mapping from graph id to enode id.
        //
        // we do this since adding the graph nodes directly is not possible due to loops in the graph.
        let mut translation_map = GraphToEgraphTranslationMap::new();
        for graph_node_id in graph.valid_node_ids() {
            let internal_var = GenericNode::InternalVar(self.alloc_internal_var());
            let add_res = self.add_enode(internal_var);
            translation_map.0.insert(graph_node_id, add_res.eclass_id);
        }

        // now add each graphs node as a real node value, and union it with the original internal var that we created for it.
        for graph_node_id in graph.valid_node_ids() {
            let node = &graph[graph_node_id];

            // conver the graph node to an enode by converting the links according to the translation map
            let enode = node.convert_link(|link| translation_map.0[link]);

            // add the converted enode
            let add_res = self.add_enode(enode);

            // union the converted enode with the original internal var that was created for this graph node.
            self.enodes_union_find.union(
                add_res.eclass_id.enode_id.0,
                translation_map.0[&graph_node_id].enode_id.0,
            );
        }

        translation_map
    }

    pub fn dump_dot_svg(&self, out_file_path: &str) {
        let dot = self.to_dot();

        std::fs::write(format!("{}.dot", out_file_path), &dot).unwrap();

        let mut tmpfile = NamedTempFile::with_suffix(".dot").unwrap();
        tmpfile.write_all(dot.as_bytes()).unwrap();
        tmpfile.flush().unwrap();

        let tmp_path = tmpfile.into_temp_path();

        cmd!("fdp", "-Tsvg", &*tmp_path, "-o", out_file_path)
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
        // avoid choosing internal var nodes in sample representations, since they are just internal data which doesn't provide
        // any useful information.
        let chosen_enode = self
            .enodes_union_find
            .items_eq_to(eclass_id.enode_id.0)
            .find(|&enode_item_id| {
                let enode = &self.enodes_union_find[enode_item_id];
                !matches!(enode, GenericNode::InternalVar(_))
            })
            .unwrap();
        self.enode_get_sample_rec_node(ENodeId(chosen_enode))
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

        let find_eclass_label_of_node =
            |item_id: UnionFindItemId| self.enodes_union_find.items_eq_to(item_id).min().unwrap();

        fn eclass_dot_id(eclass_label: UnionFindItemId) -> String {
            format!("cluster_eclass_{}", eclass_label.0)
        }
        fn enode_dot_id(eclass_label: UnionFindItemId, index_in_eclass: usize) -> String {
            format!("eclass_{}_item_{}", eclass_label.0, index_in_eclass)
        }

        let eclass_labels: HashSet<UnionFindItemId> = self
            .enodes_union_find
            .item_ids()
            .map(|item_id| find_eclass_label_of_node(item_id))
            .collect();

        for &eclass_label in &eclass_labels {
            let eclass_id = ENodeId(eclass_label).eclass_id();

            writeln!(&mut out, "subgraph {} {{", eclass_dot_id(eclass_label),).unwrap();

            writeln!(
                &mut out,
                "color=gray60; style=\"rounded\"; fontcolor=\"white\"; label=\"{}\"",
                self.eclass_get_sample_rec_node(eclass_id)
            )
            .unwrap();

            // one node per enode in the class
            for (i, enode_id) in self.enodes_union_find.items_eq_to(eclass_label).enumerate() {
                let label = self.enodes_union_find[enode_id].structural_display();
                writeln!(
                    &mut out,
                    "{} [label=\"{}\"];",
                    enode_dot_id(eclass_label, i),
                    label
                )
                .unwrap();
            }

            out.push_str("}\n");
        }

        // edges from each enode to target e-class clusters
        for &eclass_label in &eclass_labels {
            for (i, enode_id) in self.enodes_union_find.items_eq_to(eclass_label).enumerate() {
                let enode = &self.enodes_union_find[enode_id];
                for link in enode.links() {
                    // route to target cluster anchor; ltail/lhead draw the edge between clusters
                    let target_eclass_label = find_eclass_label_of_node(link.enode_id.0);
                    writeln!(
                        &mut out,
                        "{} -> {} [lhead={}];",
                        enode_dot_id(eclass_label, i),
                        enode_dot_id(target_eclass_label, 0),
                        eclass_dot_id(target_eclass_label)
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

    fn extract_enode(&self, enode_id: ENodeId, ctx: &ExtractCtx) -> ScoredExtractionNode {
        let enode = &self.enodes_union_find[enode_id.0];
        let mut score = ExtractionScore {
            looping_score: 0,
            base_score: 1,
        };
        let extraction_node = enode.convert_link(|link_eclass_id| {
            let link_effective_eclass_id = link_eclass_id.to_effective(&self.enodes_union_find);
            let extract_link_res = self.extract_eclass_inner(link_effective_eclass_id, ctx);
            score += extract_link_res.score;
            extract_link_res.node
        });
        ScoredExtractionNode {
            node: ExtractionLink::Regular(Box::new(extraction_node)),
            score,
        }
    }

    fn extract_eclass_inner(
        &self,
        effective_eclass_id: EffectiveEClassId,
        ctx: &ExtractCtx,
    ) -> ScoredExtractionNode {
        if ctx.visited_eclasses.contains(&effective_eclass_id) {
            // we encountered an eclass that we have already visited.
            return ScoredExtractionNode {
                node: ExtractionLink::Loop(effective_eclass_id),
                score: ExtractionScore {
                    looping_score: 1,
                    base_score: 0,
                },
            };
        }

        let mut new_ctx = ctx.clone();
        new_ctx.visited_eclasses.insert(effective_eclass_id);

        self.enodes_union_find
            .items_eq_to(effective_eclass_id.eclass_root.0)
            .map(|enode_item_id| self.extract_enode(ENodeId(enode_item_id), &new_ctx))
            .min_by_key(|scored_enode| scored_enode.score)
            .unwrap()
    }

    pub fn extract_eclass(&self, eclass_id: EClassId) {
        let ctx = ExtractCtx::new();
        let extraction_node =
            self.extract_eclass_inner(eclass_id.to_effective(&self.enodes_union_find), &ctx);
        println!("{:#?}", extraction_node);
        todo!()
    }
}

/// a mapping from each graph node id to the eclass id of it in the egraph.
pub struct GraphToEgraphTranslationMap(pub HashMap<GraphNodeId, EClassId>);
impl GraphToEgraphTranslationMap {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}

#[derive(Debug, Clone)]
enum ExtractionLink {
    Regular(Box<ExtractionNode>),
    Loop(EffectiveEClassId),
}
type ExtractionNode = GenericNode<ExtractionLink>;

#[derive(Debug, Clone)]
struct ScoredExtractionNode {
    node: ExtractionLink,
    score: ExtractionScore,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Add, AddAssign)]
struct ExtractionScore {
    looping_score: usize,
    base_score: usize,
}

#[derive(Debug, Clone)]
struct ExtractCtx {
    visited_eclasses: HashSet<EffectiveEClassId>,
}
impl ExtractCtx {
    fn new() -> Self {
        Self {
            visited_eclasses: HashSet::new(),
        }
    }
}

/// match context. this struct contains context for a single path of the e-matching process.
#[derive(Debug, Clone)]
pub struct MatchCtx {
    /// the rule storage which contains all the captured information while matching.
    pub rule_storage: RewriteRuleStorage,

    /// a list of eclass ids that we have already visited along the current path that is being matched.
    pub visited_eclasses: Vec<EffectiveEClassId>,
}
impl MatchCtx {
    /// creates a new empty match context.
    pub fn new() -> Self {
        Self {
            rule_storage: RewriteRuleStorage::new(),
            visited_eclasses: Vec::new(),
        }
    }
    /// checks if we have already visited the given eclass along the current path that is being matched.
    pub fn has_visited_eclass(&self, effective_eclass_id: EffectiveEClassId) -> bool {
        self.visited_eclasses.contains(&effective_eclass_id)
    }

    /// creates a clone of this match context but with an added visited eclass id.
    pub fn with_added_visited_eclass(&self, effective_eclass_id: EffectiveEClassId) -> Self {
        let mut res = self.clone();
        res.visited_eclasses.push(effective_eclass_id);
        res
    }
}

/// a match of a rule to a specific enode.
#[derive(Debug, Clone)]
pub struct ENodeMatch {
    /// the rule storage for this match. this contains all the information captured while matching the rule for this specific
    /// enode match.
    pub rule_storage: RewriteRuleStorage,

    /// the enode id of the enode that matched the rule.
    pub enode_id: ENodeId,
}

/// storage for creating a matching state object, used to perform rule matching on the egraph.
struct MatchingStateStorage {
    /// the initial empty match context.
    initial_match: MatchCtx,

    /// a list of match contexts which will be filled by the matching process.
    match_ctxs: Vec<MatchCtx>,
}
impl MatchingStateStorage {
    /// creates a new empty matching state storage.
    fn new() -> Self {
        Self {
            initial_match: MatchCtx::new(),
            match_ctxs: Vec::new(),
        }
    }

    /// returns a matching state object which uses this matching state storage as backing storage.
    fn get_state(&mut self) -> MatchingState<'_> {
        MatchingState {
            cur_match: &self.initial_match,
            match_ctx: &mut self.match_ctxs,
        }
    }
}

/// a matching state object. passed around in all of the matching functions.
///
/// this object provides both access to the current match context, and access to an output list to store the newly
/// generated match contexts.
#[derive(Debug)]
struct MatchingState<'a> {
    /// the match context along the current path that is being matched.
    cur_match: &'a MatchCtx,

    /// an output list of match contexts where newly generated match contexts should be placed, after progressing another step
    /// in the e-matching process.
    match_ctx: &'a mut Vec<MatchCtx>,
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
        ]);

        let zero_eclass = egraph.add_enode(0.into()).eclass_id;

        assert!(!egraph.are_eq(zero_eclass, root_eclass));
        egraph.apply_rule_set(&rule_set, None);
        assert!(egraph.are_eq(zero_eclass, root_eclass));
    }

    #[test]
    fn test_propegate_union() {
        let mut egraph = EGraph::new();
        let var0 = egraph.add_enode(Var(0).into()).eclass_id;
        let var1 = egraph.add_enode(Var(1).into()).eclass_id;
        let un_op_var0 = egraph
            .add_enode(
                UnOp {
                    kind: UnOpKind::Neg,
                    operand: var0,
                }
                .into(),
            )
            .eclass_id;
        let un_op_var1 = egraph
            .add_enode(
                UnOp {
                    kind: UnOpKind::Neg,
                    operand: var1,
                }
                .into(),
            )
            .eclass_id;

        assert!(!egraph.are_eq(un_op_var0, un_op_var1));

        let union_res = egraph.union(var0, var1);
        assert_eq!(union_res, UnionRes::New);
        egraph.propegate_unions();

        assert!(egraph.are_eq(un_op_var0, un_op_var1));
    }

    #[test]
    fn test_propegate_union_multi_level() {
        let mut egraph = EGraph::new();
        let var0 = egraph.add_enode(Var(0).into()).eclass_id;
        let var1 = egraph.add_enode(Var(1).into()).eclass_id;
        let un_op_var0 = egraph
            .add_enode(
                UnOp {
                    kind: UnOpKind::Neg,
                    operand: var0,
                }
                .into(),
            )
            .eclass_id;
        let un_op_var1 = egraph
            .add_enode(
                UnOp {
                    kind: UnOpKind::Neg,
                    operand: var1,
                }
                .into(),
            )
            .eclass_id;

        let bin_op0 = egraph
            .add_enode(
                BinOp {
                    kind: BinOpKind::Add,
                    lhs: un_op_var0,
                    rhs: var1,
                }
                .into(),
            )
            .eclass_id;
        let bin_op1 = egraph
            .add_enode(
                BinOp {
                    kind: BinOpKind::Add,
                    lhs: un_op_var1,
                    rhs: var0,
                }
                .into(),
            )
            .eclass_id;

        // sanity
        assert!(!egraph.are_eq(bin_op0, bin_op1));

        let union_res = egraph.union(var0, var1);
        assert_eq!(union_res, UnionRes::New);
        egraph.propegate_unions();

        assert!(egraph.are_eq(un_op_var0, un_op_var1));
        assert!(egraph.are_eq(bin_op0, bin_op1));
    }
}
