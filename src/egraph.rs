use derive_more::{Add, AddAssign};
use duct::cmd;
use either::Either;
use hashbrown::{DefaultHashBuilder, HashMap, HashSet, HashTable, hash_table::Entry};
use std::{borrow::Cow, hash::BuildHasher, io::Write as _, ops::Index};
use tempfile::NamedTempFile;

use crate::{did_anything::*, graph::*, node::*, rec_node::*, rewrite::*, union_find::*};
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
        self.convert_links(|eclass_id| eclass_id.to_effective(union_find))
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
        self.0.hash_one(node.convert_links(|_| ()))
    }
}

/// a hash table of enodes.
///
/// the enodes are hashed according to their structure, ignoring the links, but contain the fully detailed enode values including
/// the links, which allows for comparison against exact entries.
#[derive(Clone)]
pub struct ENodeHashTable {
    table: HashTable<ENodeHashTableEntry>,
    hasher: NodeHasher,
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

    pub fn node_hasher(&self) -> &NodeHasher {
        &self.enodes_hash_table.hasher
    }

    pub fn enodes_union_find(&self) -> &UnionFind<ENode> {
        &self.enodes_union_find
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
        let graph_node = rec_node.convert_links(|link| self.add_rec_node_link(link).eclass_id);
        self.add_enode(graph_node)
    }

    /// adds a recursive node link to the egraph, converting each node to an enode.
    pub fn add_rec_node_link(&mut self, rec_node_link: &RecNodeLink) -> AddENodeRes {
        match rec_node_link {
            RecNodeLink::Regular(node) => self.add_rec_node(node),
            RecNodeLink::Loop => {
                // recursive nodes with loop links can't be added to the graph. they are only used to represent impossible rec nodes.
                unreachable!()
            }
        }
    }

    pub fn apply_rewrite<R: Rewrite>(&mut self, rewrite: &R) -> DidAnything {
        let matches = self.match_rewrite(rewrite);
        self.handle_rewrite_matches(matches, rewrite)
    }

    pub fn handle_rewrite_matches<R: Rewrite>(
        &mut self,
        enode_matches: Vec<ENodeMatch<R::Ctx>>,
        rewrite: &R,
    ) -> DidAnything {
        let mut did_anything = DidAnything::False;

        // for each match, add the rerwrite result to the egraph
        for enode_match in enode_matches {
            let add_res = rewrite.build_rewrite(enode_match.final_ctx, self);

            let union_res = self
                .enodes_union_find
                .union(add_res.eclass_id.enode_id.0, enode_match.enode_id.0);
            if add_res.dedup_info == ENodeDedupInfo::New || union_res == UnionRes::New {
                did_anything = DidAnything::True;
            }
        }

        did_anything
    }

    pub fn match_rewrite<R: Rewrite>(&self, rewrite: &R) -> Vec<ENodeMatch<R::Ctx>> {
        let mut enode_matches = Vec::new();

        let entries = match rewrite.query_structural_hash(self) {
            Some(hash) => Either::Left(self.enodes_hash_table.table.iter_hash(hash)),
            None => Either::Right(self.enodes_hash_table.table.iter()),
        };

        let initial_ctx = rewrite.create_initial_ctx();
        let query = rewrite.query();
        let recursed_eclasses = RecursedEClasses::new();

        // find the first enode that matches the rule.
        for entry in entries {
            let mut match_ctxs = Vec::new();

            // match the current enode
            self.match_enode(
                entry.id,
                &entry.enode,
                &*query,
                &initial_ctx,
                &recursed_eclasses,
                &mut match_ctxs,
            );

            enode_matches.extend(match_ctxs.into_iter().map(|ctx| ENodeMatch {
                final_ctx: ctx,
                enode_id: entry.id,
            }));
        }

        enode_matches
    }

    fn match_enode_link<C>(
        &self,
        link_eclass_id: EClassId,
        effective_eclass_id: EffectiveEClassId,
        link_matcher: &dyn QueryLinkMatcher<C>,
        ctx: &C,
        recursed_eclasses: &RecursedEClasses,
        match_ctxs: &mut Vec<C>,
    ) {
        match link_matcher.match_link(link_eclass_id, self, ctx) {
            QueryMatchLinkRes::NoMatch => {
                return;
            }
            QueryMatchLinkRes::Match(QueryMatch { new_ctx }) => {
                match_ctxs.push(new_ctx);
            }
            QueryMatchLinkRes::RecurseIntoENodes {
                new_ctx,
                enode_matcher,
            } => {
                // when matching recursing into the enodes of an eclass, make sure that we haven't recursed this eclass already,
                // to prevent following eclass loops which will blow up the graph with redundant expressions.
                if recursed_eclasses.has_recursed_eclass(effective_eclass_id) {
                    // don't loop
                    return;
                }

                // mark the eclass as recursed
                let new_recursed_eclasses =
                    recursed_eclasses.with_added_recursed_eclass(effective_eclass_id);

                for enode_item_id in self
                    .enodes_union_find
                    .items_eq_to(link_eclass_id.enode_id.0)
                {
                    let enode_id = ENodeId(enode_item_id);
                    let enode = &self.enodes_union_find[enode_item_id];
                    self.match_enode(
                        enode_id,
                        enode,
                        &*enode_matcher,
                        &new_ctx,
                        &new_recursed_eclasses,
                        match_ctxs,
                    )
                }
            }
        }
    }

    fn match_enode_links<C>(
        &self,
        enode: &ENode,
        links_matcher: &dyn QueryLinksMatcher<C>,
        ctx: C,
        recursed_eclasses: &RecursedEClasses,
        match_ctxs: &mut Vec<C>,
    ) {
        let enode_links = enode.links();
        let links_amount = enode_links.len();

        let Some(links_amount_match) = links_matcher.match_links_amount(links_amount, ctx) else {
            // no match
            return;
        };
        let QueryMatch { new_ctx } = links_amount_match;

        if links_amount == 0 {
            // no links, so we got a match
            match_ctxs.push(new_ctx);
            return;
        }

        // now match the links.
        let mut cur_match_ctxs: Vec<C> = vec![new_ctx];
        let mut new_match_ctxs: Vec<C> = Vec::new();
        for cur_link_idx in 0..links_amount {
            let link_matcher = links_matcher.get_link_matcher(cur_link_idx);

            // the eclass that the current enode link points to
            let link_eclass_id = *enode_links[cur_link_idx];
            let effective_eclass_id = link_eclass_id.to_effective(&self.enodes_union_find);

            // we want a cartesian product over match contexts from previous links, so try matching the link for each previous match
            for cur_ctx in &cur_match_ctxs {
                self.match_enode_link(
                    link_eclass_id,
                    effective_eclass_id,
                    &*link_matcher,
                    cur_ctx,
                    recursed_eclasses,
                    &mut new_match_ctxs,
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
        match_ctxs.append(&mut cur_match_ctxs);
    }

    fn match_enode<C>(
        &self,
        enode_id: ENodeId,
        enode: &ENode,
        matcher: &dyn QueryENodeMatcher<C>,
        ctx: &C,
        recursed_eclasses: &RecursedEClasses,
        match_ctxs: &mut Vec<C>,
    ) {
        match matcher.match_enode(enode_id, enode, self, ctx) {
            QueryMatchENodeRes::NoMatch => {
                return;
            }
            QueryMatchENodeRes::Match(QueryMatch { new_ctx }) => {
                match_ctxs.push(new_ctx);
            }
            QueryMatchENodeRes::RecurseIntoLinks {
                new_ctx,
                links_matcher,
            } => {
                self.match_enode_links(
                    enode,
                    &*links_matcher,
                    new_ctx,
                    recursed_eclasses,
                    match_ctxs,
                );
            }
        }
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

    pub fn apply_rewrites<R: Rewrites>(&mut self, rewrites: &R, max_iterations: Option<usize>) {
        let mut i = 0;
        loop {
            let mut did_anything = DidAnything::False;
            for i in 0..rewrites.len() {
                did_anything |= rewrites.apply_rewrite(i, self);
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
            let enode = node.convert_links(|&link| translation_map[link]);

            // add the converted enode
            let add_res = self.add_enode(enode);

            // union the converted enode with the original internal var that was created for this graph node.
            self.enodes_union_find.union(
                add_res.eclass_id.enode_id.0,
                translation_map[graph_node_id].enode_id.0,
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

    fn enode_get_sample_rec_node_inner(
        &self,
        enode_id: ENodeId,
        visited_eclasses: &mut HashSet<EffectiveEClassId>,
    ) -> RecNode {
        self.enodes_union_find[enode_id.0].convert_links(|link_eclass_id| {
            self.eclass_get_sample_rec_node_link_inner(*link_eclass_id, visited_eclasses)
        })
    }

    fn eclass_get_sample_rec_node_link_inner(
        &self,
        eclass_id: EClassId,
        visited_eclasses: &mut HashSet<EffectiveEClassId>,
    ) -> RecNodeLink {
        let effective_eclass_id = eclass_id.to_effective(&self.enodes_union_find);
        if !visited_eclasses.insert(effective_eclass_id) {
            return RecNodeLink::Loop;
        }

        // avoid choosing internal var nodes in sample representations, since they are just internal data which doesn't provide
        // any useful information.
        let chosen_enode = self
            .enodes_union_find
            .items_eq_to(eclass_id.enode_id.0)
            .find(|&enode_item_id| {
                let enode = &self.enodes_union_find[enode_item_id];
                !enode.is_internal_var()
            })
            .unwrap();
        self.enode_get_sample_rec_node_inner(ENodeId(chosen_enode), visited_eclasses)
            .into()
    }

    pub fn enode_get_sample_rec_node(&self, enode_id: ENodeId) -> RecNode {
        let mut visited_eclasses = HashSet::new();
        self.enode_get_sample_rec_node_inner(enode_id, &mut visited_eclasses)
    }

    pub fn eclass_get_sample_rec_node(&self, eclass_id: EClassId) -> RecNode {
        let mut visited_eclasses = HashSet::new();
        let link = self.eclass_get_sample_rec_node_link_inner(eclass_id, &mut visited_eclasses);
        match link {
            RecNodeLink::Regular(node) => *node,
            RecNodeLink::Loop => {
                // the root node link can't be a loop link
                unreachable!()
            }
        }
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

    fn extract_enode<'a>(&self, enode_id: ENodeId, ctx: &'a ExtractCtx) -> ExtractENodeRes<'a> {
        let enode = &self.enodes_union_find[enode_id.0];
        let mut score = ExtractionScore {
            looping_score: 0,
            base_score: 1,
        };

        // convert the enode to a graph node by extracting each link and advancing our ctx.
        let mut cur_ctx = Cow::Borrowed(ctx);
        let graph_node = enode.convert_links(|link_eclass_id| {
            let link_effective_eclass_id = link_eclass_id.to_effective(&self.enodes_union_find);

            // extract the best version of the link eclass id
            let ExtractEClassRes {
                res,
                eclass_graph_node_id: extracted_link_graph_node_id,
            } = self.extract_eclass_inner(link_effective_eclass_id, &cur_ctx);

            // update the socre according to the link's score
            score += res.score;

            // update the context to the context after extracting the link if any change was made.
            if let Cow::Owned(new_ctx) = res.ctx {
                cur_ctx = Cow::Owned(new_ctx);
            }

            // make us point to the graph node which represents that link eclass
            extracted_link_graph_node_id
        });

        ExtractENodeRes {
            res: ExtractRes {
                ctx: cur_ctx,
                score: score,
            },
            graph_node,
        }
    }

    fn extract_eclass_inner<'a>(
        &self,
        effective_eclass_id: EffectiveEClassId,
        ctx: &'a ExtractCtx,
    ) -> ExtractEClassRes<'a> {
        if let Some(graph_id) = ctx.eclass_to_graph_id.get(&effective_eclass_id) {
            // we encountered an eclass that we have already visited.
            return ExtractEClassRes {
                res: ExtractRes {
                    ctx: Cow::Borrowed(ctx),
                    score: ExtractionScore {
                        looping_score: 1,
                        base_score: 0,
                    },
                },
                eclass_graph_node_id: *graph_id,
            };
        };

        let mut new_ctx = ctx.clone();

        // for now, add a dummy node since we don't yet know which form will be chosen, but we must allocate a graph node for it
        // so that inner nodes can point back to it in case of loops.
        //
        // we will fill it in later.
        //
        // we can just always use the internal var 0 since unlike the egraph, the graph doesn't do any de-duplication.
        let internal_var = new_ctx.alloc_internal_var();
        let new_graph_id = new_ctx
            .graph
            .add_node(GenericNode::InternalVar(internal_var));
        new_ctx
            .eclass_to_graph_id
            .insert(effective_eclass_id, new_graph_id);

        let best_enode_res = self
            .enodes_union_find
            .items_eq_to(effective_eclass_id.eclass_root.0)
            .filter(|&enode_item_id| {
                // skip internal var nodes, they are of no use to us here.
                !self.enodes_union_find[enode_item_id].is_internal_var()
            })
            .map(|enode_item_id| self.extract_enode(ENodeId(enode_item_id), &new_ctx))
            .min_by_key(|extract_enode_res| extract_enode_res.res.score)
            .unwrap();

        // now fill in our actual graph node.
        let mut final_ctx = best_enode_res.res.ctx.into_owned();
        final_ctx.graph[new_graph_id] = best_enode_res.graph_node;

        ExtractEClassRes {
            res: ExtractRes {
                ctx: Cow::Owned(final_ctx),
                score: best_enode_res.res.score,
            },
            eclass_graph_node_id: new_graph_id,
        }
    }

    pub fn extract_eclass(&self, eclass_id: EClassId) -> (Graph, GraphNodeId) {
        let ctx = ExtractCtx::new();
        let extract_eclass_res =
            self.extract_eclass_inner(eclass_id.to_effective(&self.enodes_union_find), &ctx);
        (
            extract_eclass_res.res.ctx.into_owned().graph,
            extract_eclass_res.eclass_graph_node_id,
        )
    }
}
impl Index<ENodeId> for EGraph {
    type Output = ENode;

    fn index(&self, index: ENodeId) -> &Self::Output {
        &self.enodes_union_find[index.0]
    }
}

/// a mapping from each graph node id to the eclass id of it in the egraph.
pub struct GraphToEgraphTranslationMap(pub HashMap<GraphNodeId, EClassId>);
impl GraphToEgraphTranslationMap {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}
impl Index<GraphNodeId> for GraphToEgraphTranslationMap {
    type Output = EClassId;

    fn index(&self, index: GraphNodeId) -> &Self::Output {
        &self.0[&index]
    }
}

#[derive(Debug, Clone)]
struct ExtractRes<'a> {
    ctx: Cow<'a, ExtractCtx>,
    score: ExtractionScore,
}

#[derive(Debug, Clone)]
struct ExtractENodeRes<'a> {
    res: ExtractRes<'a>,
    graph_node: GraphNode,
}
#[derive(Debug, Clone)]
struct ExtractEClassRes<'a> {
    res: ExtractRes<'a>,
    eclass_graph_node_id: GraphNodeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Add, AddAssign)]
struct ExtractionScore {
    looping_score: usize,
    base_score: usize,
}

#[derive(Debug, Clone)]
struct ExtractCtx {
    graph: Graph,
    next_internal_var: InternalVar,
    eclass_to_graph_id: HashMap<EffectiveEClassId, GraphNodeId>,
}
impl ExtractCtx {
    fn new() -> Self {
        Self {
            graph: Graph::new(),
            eclass_to_graph_id: HashMap::new(),
            next_internal_var: InternalVar(0),
        }
    }

    fn alloc_internal_var(&mut self) -> InternalVar {
        let res = self.next_internal_var;
        self.next_internal_var.0 += 1;
        res
    }
}

/// a match of a rule to a specific enode.
#[derive(Debug, Clone)]
pub struct ENodeMatch<C> {
    /// the final ctx of this match.
    pub final_ctx: C,

    /// the enode id of the enode that matched the rule.
    pub enode_id: ENodeId,
}

/// a list of eclasses that we have already recursed into their enodes.
#[derive(Debug, Clone)]
struct RecursedEClasses(Vec<EffectiveEClassId>);
impl RecursedEClasses {
    /// creates a new empty recursed classes object
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// checks if we have already recursed the given eclass.
    pub fn has_recursed_eclass(&self, effective_eclass_id: EffectiveEClassId) -> bool {
        self.0.contains(&effective_eclass_id)
    }

    /// creates a clone of this object but with an added recursed eclass id.
    pub fn with_added_recursed_eclass(&self, effective_eclass_id: EffectiveEClassId) -> Self {
        let mut res = self.clone();
        res.0.push(effective_eclass_id);
        res
    }
}

#[cfg(test)]
mod tests {
    use crate::{rewrites, template_rewrite::*};

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
            kind: BinOpKind::BitAnd,
            lhs: 0xff.into(),
            rhs: RecBinOp {
                kind: BinOpKind::BitOr,
                lhs: RecBinOp {
                    kind: BinOpKind::BitAnd,
                    lhs: Var(0).into(),
                    rhs: 0xff00.into(),
                }
                .into(),
                rhs: RecBinOp {
                    kind: BinOpKind::BitAnd,
                    lhs: Var(0).into(),
                    rhs: 0xff0000.into(),
                }
                .into(),
            }
            .into(),
        }
        .into();

        let (mut egraph, root_eclass) = EGraph::from_rec_node(&rec_node);

        let rule_set = rewrites![
            // (x & 0) => 0
            RewriteRule {
                query: BinOpTemplate {
                    kind: BinOpKind::BitAnd,
                    lhs: TemplateVar::new(1).into(),
                    rhs: 0.into(),
                }
                .into(),
                rewrite: 0.into(),
                keep_original: false,
            },
            // a & (b | c) => (a & b) | (a & c)
            RewriteRule {
                query: BinOpTemplate {
                    kind: BinOpKind::BitAnd,
                    lhs: TemplateVar::new(1).into(),
                    rhs: BinOpTemplate {
                        kind: BinOpKind::BitOr,
                        lhs: TemplateVar::new(2).into(),
                        rhs: TemplateVar::new(3).into(),
                    }
                    .into(),
                }
                .into(),
                rewrite: BinOpTemplate {
                    kind: BinOpKind::BitOr,
                    lhs: BinOpTemplate {
                        kind: BinOpKind::BitAnd,
                        lhs: TemplateVar::new(1).into(),
                        rhs: TemplateVar::new(2).into(),
                    }
                    .into(),
                    rhs: BinOpTemplate {
                        kind: BinOpKind::BitAnd,
                        lhs: TemplateVar::new(1).into(),
                        rhs: TemplateVar::new(3).into(),
                    }
                    .into(),
                }
                .into(),
                keep_original: true,
            },
            // a & (b & c) => (a & b) & c
            RewriteRule {
                query: BinOpTemplate {
                    kind: BinOpKind::BitAnd,
                    lhs: TemplateVar::new(1).into(),
                    rhs: BinOpTemplate {
                        kind: BinOpKind::BitAnd,
                        lhs: TemplateVar::new(2).into(),
                        rhs: TemplateVar::new(3).into(),
                    }
                    .into(),
                }
                .into(),
                rewrite: BinOpTemplate {
                    kind: BinOpKind::BitAnd,
                    lhs: BinOpTemplate {
                        kind: BinOpKind::BitAnd,
                        lhs: TemplateVar::new(1).into(),
                        rhs: TemplateVar::new(2).into(),
                    }
                    .into(),
                    rhs: TemplateVar::new(3).into(),
                }
                .into(),
                keep_original: true,
            },
            // a & b => b & a
            RewriteRule {
                query: BinOpTemplate {
                    kind: BinOpKind::BitAnd,
                    lhs: TemplateVar::new(1).into(),
                    rhs: TemplateVar::new(2).into(),
                }
                .into(),
                rewrite: BinOpTemplate {
                    kind: BinOpKind::BitAnd,
                    lhs: TemplateVar::new(2).into(),
                    rhs: TemplateVar::new(1).into(),
                }
                .into(),
                keep_original: true,
            },
        ];

        let zero_eclass = egraph.add_enode(0.into()).eclass_id;

        assert!(!egraph.are_eq(zero_eclass, root_eclass));
        egraph.apply_rewrites(&rule_set, None);
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
