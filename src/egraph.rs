use derive_more::{Add, AddAssign};
use duct::cmd;
use enum_variant_accessors::EnumAsVariant;
use hashbrown::{HashMap, HashSet};
use std::{
    cell::RefCell,
    collections::BTreeMap,
    io::Write as _,
    ops::{Index, IndexMut},
};
use tempfile::NamedTempFile;

use crate::{did_anything::*, graph::*, node::*, rec_node::*, rewrite::*, union_find::*};
use std::fmt::Write as _;

/// the id of an enode.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
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
    pub fn to_effective(&self, union_find: &ENodesUnionFind) -> EffectiveEClassId {
        EffectiveEClassId {
            eclass_root: union_find.root_of_enode(self.enode_id),
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

/// an effective enode, which is an enode which uses effective eclass ids for its links. this allows comparing the enode to other enodes.
pub type EffectiveENode = GenericNode<EffectiveEClassId>;

impl ENode {
    /// converts this enode to an enode with an effective eclass id which is correct for the given state of the union find tree.
    fn to_effective(&self, union_find: &ENodesUnionFind) -> EffectiveENode {
        self.convert_links(|eclass_id| eclass_id.to_effective(union_find))
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

/// an item in the enodes union find.
#[derive(Debug, Clone, EnumAsVariant)]
pub enum ENodesUnionFindItem {
    /// a regular enode.
    ENode(ENode),

    /// a tombstone item. this is an item which represents an enode which was removed from the egraph, but we still want to keep
    /// its entry in the union find tree as a reference to the eclass from other enodes which point to it.
    Tombstone,
}

/// the union find tree of enodes.
#[derive(Debug, Clone)]
pub struct ENodesUnionFind(pub UnionFind<ENodesUnionFindItem>);
impl ENodesUnionFind {
    pub fn new() -> Self {
        Self(UnionFind::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn root_of_enode(&self, enode_id: ENodeId) -> ENodeId {
        ENodeId(self.0.root_of_item(enode_id.0))
    }

    pub fn enodes_eq_to(&self, enode_id: ENodeId) -> impl Iterator<Item = &ENode> + '_ {
        self.0
            .items_eq_to(enode_id.0)
            .map(ENodeId)
            .filter_map(|enode_id| self[enode_id].as_e_node())
    }

    pub fn enumerate_enodes_eq_to(
        &self,
        enode_id: ENodeId,
    ) -> impl Iterator<Item = (ENodeId, &ENode)> + '_ {
        self.0
            .items_eq_to(enode_id.0)
            .map(ENodeId)
            .filter_map(|enode_id| {
                let enode = self[enode_id].as_e_node()?;
                Some((enode_id, enode))
            })
    }

    pub fn enodes_in_eclass(&self, eclass_id: EClassId) -> impl Iterator<Item = &ENode> + '_ {
        self.enodes_eq_to(eclass_id.enode_id)
    }

    /// an internal recursive version of the cyclicity calculation.
    fn cyclicity_recursive(&self, node: ENodeId, node_marks: &mut [CyclicityMark]) -> Cyclicity {
        match node_marks[node.0.index()] {
            CyclicityMark::Unexplored => {
                // this node was unexplored, so we can just explore it now.
            }
            CyclicityMark::Acyclic => {
                // we have already visited this node and didn't detect any cycles, so this-far we are acyclic.
                return Cyclicity::Acyclic;
            }
            CyclicityMark::InRecursionStack => {
                // this node is already in our recursion stack, and we are now about to traverse it again, so we have a cycle.
                return Cyclicity::Cyclic;
            }
        }

        node_marks[node.0.index()] = CyclicityMark::InRecursionStack;

        match &self[node] {
            ENodesUnionFindItem::ENode(enode) => {
                for &link_eclass_id in enode.links() {
                    // poor man's enodes in eclass iteration, which avoids modifying the union find tree.
                    let eclass_root = self.0.root_of_item_no_update(link_eclass_id.enode_id.0);
                    for enode_id in self.enode_ids() {
                        if self.0.root_of_item_no_update(enode_id.0) != eclass_root {
                            // this enode is not part of the eclass
                            continue;
                        }
                        match self.cyclicity_recursive(enode_id, node_marks) {
                            Cyclicity::Cyclic => {
                                // the linked node is cyclic, so this node is also cyclic.
                                return Cyclicity::Cyclic;
                            }
                            Cyclicity::Acyclic => {
                                // the linked node is acyclic. keep scanning the other links.
                            }
                        }
                    }
                }
            }
            ENodesUnionFindItem::Tombstone => {
                // the node is a tombstone and doesn't have any links, so it can't cause a cycle.
            }
        }

        node_marks[node.0.index()] = CyclicityMark::Acyclic;

        Cyclicity::Acyclic
    }

    /// returns the cyclicity of this graph. this basically tells us if the graph is cyclic or acyclic.
    pub fn cyclicity(&self) -> Cyclicity {
        let mut marks: Vec<CyclicityMark> = vec![CyclicityMark::Unexplored; self.len()];

        for enode_id in self.enode_ids() {
            match marks[enode_id.0.index()] {
                CyclicityMark::Unexplored => {
                    match self.cyclicity_recursive(enode_id, &mut marks) {
                        Cyclicity::Cyclic => {
                            // this node is cyclic, so the entire graph is cyclic
                            return Cyclicity::Cyclic;
                        }
                        Cyclicity::Acyclic => {
                            // this node is acyclic, continue scanning other nodes to try to find cycles.
                        }
                    }
                }
                CyclicityMark::InRecursionStack => {
                    // we should never reach here, since we are not inside any recursion stack here
                    unreachable!()
                }
                CyclicityMark::Acyclic => {
                    // we already checked this node and found that it is acyclic, so we don't need to scan it again.
                    // continue to the next node.
                    continue;
                }
            }
        }

        // we checked all nodes, and didn't find any cyclic node, so the graph is acyclic.
        Cyclicity::Acyclic
    }

    pub fn enumerate_enodes_in_eclass(
        &self,
        eclass_id: EClassId,
    ) -> impl Iterator<Item = (ENodeId, &ENode)> + '_ {
        self.enumerate_enodes_eq_to(eclass_id.enode_id)
    }

    pub fn enodes_in_effective_eclass(
        &self,
        effective_eclass_id: EffectiveEClassId,
    ) -> impl Iterator<Item = &ENode> + '_ {
        self.enodes_eq_to(effective_eclass_id.eclass_root)
    }

    pub fn enumerate_enodes_in_effective_eclass(
        &self,
        effective_eclass_id: EffectiveEClassId,
    ) -> impl Iterator<Item = (ENodeId, &ENode)> + '_ {
        self.enumerate_enodes_eq_to(effective_eclass_id.eclass_root)
    }

    pub fn enode_ids(&self) -> impl Iterator<Item = ENodeId> + use<> {
        self.0.item_ids().map(ENodeId)
    }

    pub fn enode_entries(&self) -> impl Iterator<Item = (ENodeId, &ENodesUnionFindItem)> + use<'_> {
        self.enode_ids().map(|enode_id| (enode_id, &self[enode_id]))
    }

    pub fn enodes(&self) -> impl Iterator<Item = (ENodeId, &ENode)> + use<'_> {
        self.enode_entries().filter_map(|(id, item)| {
            let enode = item.as_e_node()?;
            Some((id, enode))
        })
    }

    pub fn eclass_ids(&self) -> impl Iterator<Item = EClassId> + use<'_> {
        self.0.root_item_ids().map(|item| ENodeId(item).eclass_id())
    }

    pub fn effective_eclass_ids(&self) -> impl Iterator<Item = EffectiveEClassId> + use<'_> {
        self.0.root_item_ids().map(|item| EffectiveEClassId {
            eclass_root: ENodeId(item),
        })
    }

    fn create_new_enode(&mut self, enode: ENode) -> ENodeId {
        ENodeId(self.0.create_new_item(ENodesUnionFindItem::ENode(enode)))
    }

    pub fn peek_next_enode_id(&self) -> ENodeId {
        ENodeId(self.0.peek_next_item_id())
    }

    /// checks if the given user enode uses the given used eclass.
    fn does_enode_use_eclass(&self, enode: &ENode, used_eclass: EffectiveEClassId) -> bool {
        enode.links().iter().any(|link| {
            let link_effective_eclass_id = link.to_effective(self);
            link_effective_eclass_id == used_eclass
                || self.does_eclass_use_eclass(link_effective_eclass_id, used_eclass)
        })
    }

    /// checks if the given user eclass uses the given used eclass.
    fn does_eclass_use_eclass(
        &self,
        user_eclass: EffectiveEClassId,
        used_eclass: EffectiveEClassId,
    ) -> bool {
        self.enodes_in_effective_eclass(user_eclass)
            .any(|enode| self.does_enode_use_eclass(enode, used_eclass))
    }

    /// kill all enodes in the user eclass which use the used eclass, by converting them to tombstones.
    fn kill_enodes_which_use_eclass(
        &mut self,
        user_eclass: EffectiveEClassId,
        used_eclass: EffectiveEClassId,
    ) {
        let mut looper_enode_ids = Vec::new();
        for (enode_id, enode) in self.enumerate_enodes_in_effective_eclass(user_eclass) {
            if self.does_enode_use_eclass(enode, used_eclass) {
                looper_enode_ids.push(enode_id);
            }
        }
        for enode_id in looper_enode_ids {
            // note that when we kill the nodes here, we only modify the union find tree, but not the hashmap.
            //
            // this is important, since it means that we will still be able to de-dup this node if it ever gets re-created due to some
            // re-write rule.
            //
            // this prevents a loop of thinking that we did anything by adding a node, and then removing it right after, when we
            // realize that it creates a loop.
            self[enode_id] = ENodesUnionFindItem::Tombstone;
        }
    }

    /// union the given two enodes, merging their eclasses into a single eclass.
    fn union_enodes(&mut self, a: ENodeId, b: ENodeId) -> UnionRes {
        let eclass_a = a.eclass_id().to_effective(self);
        let eclass_b = b.eclass_id().to_effective(self);
        if eclass_a == eclass_b {
            // the nodes are already unioned.
            return UnionRes::Existing;
        }

        // before we go straight to unioning the nodes in the union find tree, let's talk about loop detection.
        //
        // if the eclass of one item uses the eclass of the other, unioning them will create a loop.
        //
        // for example, if we have the expression x*0, and we are unioning 0 with x*0, we will create a loop, since x*0 and 0
        // will now be in the same eclass, and when x*0 points to its 0 child, it will now point to the eclass containing both 0 and x*0,
        // essentially making x*0 point to itself.
        //
        // this is problematic, since the egraph is not allowed to have loops. this is one of our invariants.
        //
        // this case happens if some enodes in the parent eclass uses the child eclass. let's call such enodes "looper enodes".
        //
        // and, by unioning the parent eclass with the child eclass, we are basically saying that those looper enodes are each equal
        // to the child eclass, which is used by those looper enodes.
        //
        // if a node is equal to an eclass which it uses, as is the case for our looper enodes, it means that this node is just overly
        // complicated, and is just adding bloat to our egraph, since we can just replace it with the eclass which it uses, which
        // by definition has a simpler structure than it, since this looper node just build more shit on top of that existing eclass.
        //
        // so, we can just get rid of such looper nodes altogether without losing any important information, and we can thus prevent the
        // loop from being created in the first place.
        //
        // note that this may make one of the eclasses temporarily empty of enodes, which doesn't make any sense, but after unioning
        // the enodes and merging the eclasses, it will no longer be empty, so we are fine.
        if self.does_eclass_use_eclass(eclass_a, eclass_b) {
            // eclass a uses eclass b. kill looper enodes in eclass a.
            self.kill_enodes_which_use_eclass(eclass_a, eclass_b);
        } else if self.does_eclass_use_eclass(eclass_b, eclass_a) {
            // eclass b uses eclass a. kill looper enodes in eclass b.
            self.kill_enodes_which_use_eclass(eclass_b, eclass_a);
        }

        // union the enodes in the union find tree.
        self.0.union(a.0, b.0)
    }

    /// union the given two eclasses, merging them into a single eclass.
    pub fn union_eclasses(&mut self, a: EClassId, b: EClassId) -> UnionRes {
        self.union_enodes(a.enode_id, b.enode_id)
    }

    /// checks if the given two eclass ids point to the same eclass.
    ///
    /// eclass ids can't be compared directly since they contain lazily evaluated data.
    /// two eclass id instances may point to the same eclass even though structurally comparing the data stored in them will show
    /// that they are not equal.
    ///
    /// so, this function allows for truly comparing 2 eclass ids for a given state of the egraph.
    pub fn are_eclasses_eq(&self, a: EClassId, b: EClassId) -> bool {
        self.0.are_eq(a.enode_id.0, b.enode_id.0)
    }

    /// checks if the given two enodes are equal according to the current state of the egraph.
    pub fn are_enodes_eq(&self, a: ENodeId, b: ENodeId) -> bool {
        self.are_eclasses_eq(a.eclass_id(), b.eclass_id())
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

    fn try_to_gexf(&self) -> gexf::Result<String> {
        let mut builder = gexf::GraphBuilder::new(gexf::EdgeType::Directed)
            .meta("egraph", "a visualization of an egraph");
        for (enode_id, enode) in self.enodes() {
            let eclass_label = self
                .enumerate_enodes_eq_to(enode_id)
                .map(|(enode_id, _)| enode_id)
                .min()
                .unwrap();

            let node_label = if enode.links().is_empty() {
                enode.structural_display()
            } else {
                format!(
                    "{}    {{ {} }}",
                    enode.structural_display(),
                    self.extract_enode(enode).to_string()
                )
            };

            let node_id = enode_id.0.0.to_string();

            let node = gexf::Node::new(&node_id)
                .with_label(node_label)
                .with_attr("eclass", eclass_label.0.0.to_string());
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

        let find_eclass_label_of_node = |enode_id: ENodeId| {
            self.enumerate_enodes_eq_to(enode_id)
                .map(|(enode_id, _)| enode_id)
                .min()
                .unwrap()
        };

        fn eclass_dot_id(eclass_label: ENodeId) -> String {
            format!("cluster_eclass_{}", eclass_label.0.0)
        }
        fn enode_dot_id(eclass_label: ENodeId, index_in_eclass: usize) -> String {
            format!("eclass_{}_item_{}", eclass_label.0.0, index_in_eclass)
        }

        let eclass_labels: HashSet<ENodeId> =
            self.enode_ids().map(find_eclass_label_of_node).collect();

        for &eclass_label in &eclass_labels {
            let eclass_id = eclass_label.eclass_id();

            writeln!(&mut out, "subgraph {} {{", eclass_dot_id(eclass_label),).unwrap();

            writeln!(
                &mut out,
                "color=gray60; style=\"rounded\"; fontcolor=\"white\"; label=\"{}\"",
                self.extract_eclass(eclass_id)
            )
            .unwrap();

            // one node per enode in the class
            for (i, cur_enode) in self.enodes_eq_to(eclass_label).enumerate() {
                let label = cur_enode.structural_display();
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
            for (i, cur_enode) in self.enodes_eq_to(eclass_label).enumerate() {
                for link in cur_enode.links() {
                    // route to target cluster anchor; ltail/lhead draw the edge between clusters
                    let target_eclass_label = find_eclass_label_of_node(link.enode_id);
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

    fn extract_enode_inner(&self, enode: &ENode, cache: &mut ExtractionCache) -> ExtractRes {
        // start with a base score of 1 for the node itself.
        // TODO: calculate a proper score based on the actual data of the node, not only the depth of the node.
        let mut score = ExtractionScore(1);

        // convert the enode to a rec node by extracting each link. also, while doing this, we update our score
        let rec_node = enode.convert_links(|link_eclass_id| {
            let link_effective_eclass_id = link_eclass_id.to_effective(self);

            // extract the best version of the link eclass id
            let extract_link_res = self.extract_eclass_inner(link_effective_eclass_id, cache);

            // update the socre according to the link's score
            score += extract_link_res.score;

            // use the extracted node in our new rec node
            RecNodeLink::from(extract_link_res.node)
        });

        ExtractRes {
            node: rec_node,
            score: score,
        }
    }

    fn extract_eclass_inner(
        &self,
        effective_eclass_id: EffectiveEClassId,
        cache: &mut ExtractionCache,
    ) -> ExtractRes {
        // check if we already have it in our cache.
        if let Some(existing_res) = cache.0.get(&effective_eclass_id) {
            return existing_res.clone();
        }

        // just choose the best result among the enodes in this eclass
        let res = self
            .enodes_in_effective_eclass(effective_eclass_id)
            .filter(|enode| {
                // skip internal var nodes, they are of no use to us here.
                !enode.is_internal_var()
            })
            .map(|enode| self.extract_enode_inner(enode, cache))
            .min_by_key(|extract_enode_res| extract_enode_res.score)
            .unwrap();

        cache.0.insert(effective_eclass_id, res.clone());

        res
    }

    pub fn extract_eclass(&self, eclass_id: EClassId) -> RecNode {
        let effective_eclass_id = eclass_id.to_effective(self);
        let mut cache = ExtractionCache::new();
        let res = self.extract_eclass_inner(effective_eclass_id, &mut cache);
        res.node
    }

    pub fn extract_enode(&self, enode: &ENode) -> RecNode {
        let mut cache = ExtractionCache::new();
        let res = self.extract_enode_inner(enode, &mut cache);
        res.node
    }
}
impl Index<ENodeId> for ENodesUnionFind {
    type Output = ENodesUnionFindItem;

    fn index(&self, index: ENodeId) -> &Self::Output {
        &self.0[index.0]
    }
}
impl IndexMut<ENodeId> for ENodesUnionFind {
    fn index_mut(&mut self, index: ENodeId) -> &mut Self::Output {
        &mut self.0[index.0]
    }
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
    union_find: ENodesUnionFind,

    next_internal_var: InternalVar,

    hashmap: RefCell<BTreeMap<EffectiveENode, ENodeId>>,
}
impl EGraph {
    /// returns a new empty egraph.
    pub fn new() -> Self {
        Self {
            union_find: ENodesUnionFind::new(),
            next_internal_var: InternalVar(0),
            hashmap: Default::default(),
        }
    }

    pub fn union_find(&self) -> &ENodesUnionFind {
        &self.union_find
    }

    /// converts this eclass id to an effective eclass id which is correct for the given state of the union find tree of the egraph.
    pub fn eclass_id_to_effective(&self, eclass_id: EClassId) -> EffectiveEClassId {
        eclass_id.to_effective(&self.union_find)
    }

    /// converts this enode to an enode with an effective eclass id which is correct for the given state of the union find tree of
    /// the graph.
    pub fn enode_to_effective(&self, enode: &ENode) -> EffectiveENode {
        enode.to_effective(&self.union_find)
    }

    fn alloc_internal_var(&mut self) -> InternalVar {
        let res = self.next_internal_var;
        self.next_internal_var.0 += 1;
        res
    }

    /// adds an enode to the egraph, puts it in a new eclass which only contains that single enode, and returns the id of that eclass.
    ///
    /// if the exact enode already exists in the egraph, returns the id of the existing enode.
    pub fn add_enode(&mut self, enode: ENode) -> AddENodeRes {
        let effective_enode = self.enode_to_effective(&enode);
        let mut hashmap = self.hashmap.borrow_mut();

        if let Some(existing_id) = hashmap.get(&effective_enode) {
            // the node already exists
            return AddENodeRes {
                eclass_id: existing_id.eclass_id(),
                dedup_info: ENodeDedupInfo::Duplicate,
            };
        }

        let enode_id = self.union_find.create_new_enode(enode);
        hashmap.insert(effective_enode, enode_id);
        AddENodeRes {
            eclass_id: enode_id.eclass_id(),
            dedup_info: ENodeDedupInfo::New,
        }
    }

    /// adds a recursive node to the egraph, converting each node to an enode.
    pub fn add_rec_node(&mut self, rec_node: &RecNode) -> AddENodeRes {
        let graph_node = rec_node.convert_links(|link| self.add_rec_node(&link.0).eclass_id);
        self.add_enode(graph_node)
    }

    pub fn apply_simple_rewrite<R: SimpleRewrite>(&mut self, rewrite: &R) -> DidAnything {
        let matches = self.match_simple_rewrite(rewrite);
        self.handle_simple_rewrite_matches(matches, rewrite)
    }

    pub fn handle_simple_rewrite_matches<R: SimpleRewrite>(
        &mut self,
        matches: Vec<SimpleRewriteMatch<R::Ctx>>,
        rewrite: &R,
    ) -> DidAnything {
        let mut did_anything = DidAnything::False;

        // for each match, add the rerwrite result to the egraph
        for match_obj in matches {
            let add_res = rewrite.build_rewrite(match_obj.final_ctx, self);

            let union_res = self.union_find.union_enodes(
                add_res.eclass_id.enode_id,
                match_obj.effective_eclass_id.eclass_root,
            );
            if add_res.dedup_info == ENodeDedupInfo::New || union_res == UnionRes::New {
                did_anything = DidAnything::True;
            }
        }

        did_anything
    }

    pub fn match_simple_rewrite<R: SimpleRewrite>(
        &self,
        rewrite: &R,
    ) -> Vec<SimpleRewriteMatch<R::Ctx>> {
        let mut matches = Vec::new();

        let initial_ctx = rewrite.create_initial_ctx();
        let query = rewrite.query();

        for effective_eclass_id in self.union_find.effective_eclass_ids() {
            let mut match_ctxs = Vec::new();

            // match the current enode
            self.match_enode_link(effective_eclass_id, &*query, &initial_ctx, &mut match_ctxs);

            matches.extend(match_ctxs.into_iter().map(|ctx| SimpleRewriteMatch {
                final_ctx: ctx,
                effective_eclass_id,
            }));
        }

        matches
    }

    fn match_enode_link<C>(
        &self,
        link_effective_eclass_id: EffectiveEClassId,
        link_matcher: &dyn QueryLinkMatcher<C>,
        ctx: &C,
        match_ctxs: &mut Vec<C>,
    ) {
        match link_matcher.match_link(link_effective_eclass_id, self, ctx) {
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
                for (enode_id, enode) in self
                    .union_find
                    .enumerate_enodes_in_effective_eclass(link_effective_eclass_id)
                {
                    self.match_enode(enode_id, enode, &*enode_matcher, &new_ctx, match_ctxs)
                }
            }
        }
    }

    fn match_enode_links<C>(
        &self,
        enode: &ENode,
        links_matcher: &dyn QueryLinksMatcher<C>,
        ctx: C,
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
            let effective_eclass_id = self.eclass_id_to_effective(link_eclass_id);

            // we want a cartesian product over match contexts from previous links, so try matching the link for each previous match
            for cur_ctx in &cur_match_ctxs {
                self.match_enode_link(
                    effective_eclass_id,
                    &*link_matcher,
                    cur_ctx,
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
                self.match_enode_links(enode, &*links_matcher, new_ctx, match_ctxs);
            }
        }
    }

    /// propegate all unions such that if `a == b`, `f(a) == f(b)`, which makes us uphold the egraph's congruence invariant.
    pub fn propegate_unions(&mut self) {
        struct Rehash {
            enode_id: ENodeId,
            prev_effective_enode: EffectiveENode,
            new_effective_enode: EffectiveENode,
        }

        let mut hashmap = self.hashmap.borrow_mut();

        loop {
            // first, collect all re-hashes that need to be performed
            let mut rehashes = Vec::new();
            for (prev_effective_enode, enode_id) in &*hashmap {
                // NOTE: we want to use the original effective enode from the hashmap, and not use the enode value in the union find
                // tree here.
                //
                // this is because the node may have became a tombstone due to loop prevention, in which case we still want to be able
                // to dedup its creation in the future, so we want to keep it in the hashmap.
                //
                // if we try to use the value from the union find, we will get a tombstone, which we can't really use here.
                //
                // so, we just use the previous effective node value and update it according to the new state of the union find tree.
                let new_effective_enode = prev_effective_enode
                    .convert_links(|link| self.eclass_id_to_effective(link.to_eclass_id()));

                if *prev_effective_enode != new_effective_enode {
                    rehashes.push(Rehash {
                        enode_id: *enode_id,
                        prev_effective_enode: prev_effective_enode.clone(),
                        new_effective_enode,
                    });
                }
            }

            let mut did_anything = DidAnything::False;

            for rehash in rehashes {
                // the links of this enode have changed due to the unioning operations. re-hash it.
                hashmap.remove(&rehash.prev_effective_enode);
                match hashmap.entry(rehash.new_effective_enode) {
                    std::collections::btree_map::Entry::Occupied(occupied_entry) => {
                        // after converting the node back to an effecitve node, it now collides with another existing node.
                        // the union made them now point to the same eclasses, so they are the same node now.
                        //
                        // so, first of all, we should union the two nodes that now collide, to combine their eclasses.
                        //
                        // additionally, since they are exactly the same effective node, we need to get rid of one of them, since
                        // we can't have 2 instances of the same effective enode in the hashmap, since keys must be unique.
                        //
                        // so, we should convert one of them to a tombstone.
                        let existing_enode_id = *occupied_entry.get();
                        if self
                            .union_find
                            .union_enodes(existing_enode_id, rehash.enode_id)
                            == UnionRes::New
                        {
                            did_anything = DidAnything::True;
                        }

                        // convert the node that was just re-hashed into a tombstone.
                        // the other node is already in the hashmap, so it is cheaper to delete the one we just removed from the hashmap.
                        self.union_find[rehash.enode_id] = ENodesUnionFindItem::Tombstone;
                    }
                    std::collections::btree_map::Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(rehash.enode_id);
                    }
                }
            }

            if !did_anything.as_bool() {
                break;
            }
        }
    }

    pub fn apply_rewrites<R: AsRef<dyn Rewrite>, T: AsRef<[R]> + ?Sized>(
        &mut self,
        rewrites: &T,
        max_iterations: Option<usize>,
    ) {
        let mut cur_iteration_index = 0;
        loop {
            let mut did_anything = DidAnything::False;
            for rewrite_index in 0..rewrites.as_ref().len() {
                did_anything |= rewrites.as_ref()[rewrite_index].as_ref().apply(self);
            }
            if !did_anything.as_bool() {
                break;
            }
            self.propegate_unions();
            if let Some(max_iterations) = max_iterations {
                cur_iteration_index += 1;
                if cur_iteration_index == max_iterations {
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
        // cycles are not allowed in the egraph!
        assert_eq!(graph.cyclicity(), Cyclicity::Acyclic);

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
            self.union_find
                .union_eclasses(add_res.eclass_id, translation_map[graph_node_id]);
        }

        translation_map
    }
}
impl Index<ENodeId> for EGraph {
    type Output = ENodesUnionFindItem;

    fn index(&self, index: ENodeId) -> &Self::Output {
        &self.union_find[index]
    }
}
impl IndexMut<ENodeId> for EGraph {
    fn index_mut(&mut self, index: ENodeId) -> &mut Self::Output {
        &mut self.union_find[index]
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
struct ExtractRes {
    node: RecNode,
    score: ExtractionScore,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Add, AddAssign)]
struct ExtractionScore(u64);

/// a cache used during extraction to speed up the process and to avoid repeating the same work multiple times.
struct ExtractionCache(HashMap<EffectiveEClassId, ExtractRes>);
impl ExtractionCache {
    /// create a new empty cache.
    fn new() -> Self {
        Self(HashMap::new())
    }
}

/// a match of a simple re-write rule.
#[derive(Debug, Clone)]
pub struct SimpleRewriteMatch<C> {
    /// the final ctx of this match.
    pub final_ctx: C,

    /// the effective eclass id that matched this rule.
    pub effective_eclass_id: EffectiveEClassId,
}

/// a mark of a graph node which calculating the cyclicity of the graph.
#[derive(Copy, Clone, PartialEq, Eq)]
enum CyclicityMark {
    /// this node is unexplored, and was never previously encountered.
    Unexplored,

    /// this node is in our current recursion stack. we are currently in the process of traversing this node.
    InRecursionStack,

    /// we have already finished checking this node and found it to be acyclic.
    Acyclic,
}

#[cfg(test)]
mod tests {
    use crate::{const_fold::BinOpConstFoldRewrite, rewrites_arr, template_rewrite::*};

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
        assert_eq!(egraph.union_find.len(), 1);
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
        assert_eq!(egraph.union_find.len(), 4);

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
        assert_eq!(egraph.union_find.len(), 5);

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
        assert_eq!(egraph.union_find.len(), 6);
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

        let rule_set = rewrites_arr![
            // (x & 0) => 0
            TemplateRewriteBuilder {
                query: tv("x") & 0.into(),
                rewrite: 0.into(),
            }
            .build(),
            // a & (b | c) => (a & b) | (a & c)
            TemplateRewriteBuilder {
                query: tv("a") & (tv("b") | tv("c")),
                rewrite: (tv("a") & tv("b")) | (tv("a") & tv("c")),
            }
            .build(),
            // a & (b & c) => (a & b) & c
            TemplateRewriteBuilder::bin_op_associativity(BinOpKind::BitAnd).build(),
            // a & b => b & a
            TemplateRewriteBuilder::bin_op_commutativity(BinOpKind::BitAnd).build(),
            BinOpConstFoldRewrite,
        ];

        let zero_eclass = egraph.add_enode(0.into()).eclass_id;

        assert!(!egraph.union_find.are_eclasses_eq(zero_eclass, root_eclass));
        egraph.apply_rewrites(rule_set.as_slice(), None);
        assert!(egraph.union_find.are_eclasses_eq(zero_eclass, root_eclass));
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

        assert!(!egraph.union_find.are_eclasses_eq(un_op_var0, un_op_var1));

        let union_res = egraph.union_find.union_eclasses(var0, var1);
        assert_eq!(union_res, UnionRes::New);
        egraph.propegate_unions();

        assert!(egraph.union_find.are_eclasses_eq(un_op_var0, un_op_var1));
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
        assert!(!egraph.union_find.are_eclasses_eq(bin_op0, bin_op1));

        let union_res = egraph.union_find.union_eclasses(var0, var1);
        assert_eq!(union_res, UnionRes::New);
        egraph.propegate_unions();

        assert!(egraph.union_find.are_eclasses_eq(un_op_var0, un_op_var1));
        assert!(egraph.union_find.are_eclasses_eq(bin_op0, bin_op1));
    }
}
