use derive_more::{Add, AddAssign};
use enum_variant_accessors::EnumAsVariant;
use hashbrown::HashMap;
use std::{
    borrow::Cow,
    cell::RefCell,
    ops::{Index, IndexMut},
};

use crate::{did_anything::*, graph::*, node::*, rec_node::*, rewrite::*, union_find::*};

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
/// its internal representation it just an enode if of some enode in this eclass.
///
/// NOTE: this does not implement `Hash`, `PartialEq` and `Eq` due to how it is implemented.
/// we can have 2 instances of this type which point to different enodes, so the derived `Eq` implementation will say that they are not
/// equal, but in practice the 2 enodes that they point to are part of the same eclass, so the 2 eclass ids should be equal.
///
/// checking if 2 instances of this type are equal requires accessing the union find tree.
#[derive(Debug, Clone, Copy)]
pub struct EClassId {
    /// an id of some enode which is part of this eclass.
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
/// usually, the eclass id is represented as an id to some enode in that eclass. this is problematic since it means that we can't
/// compare eclass ids, which means that we can't compare enodes (since they contain eclass ids).
///
/// this type represents an eclass id which can actually be compared to other eclass id. this is done by using the root node of the
/// eclass.
/// so, even eclass id object which point to different enodes will yield the same root, and thus the same effective eclass id object.
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
    ///
    /// there are 2 kinds of tombstone nodes: duplicate enodes, and looper enodes.
    ///
    /// a duplicate enode is an enode that was originally unique when added to the egraph, but became exactly equal to another
    /// enode during union propegation and was thus eliminated. such an enode is not present in the egraph's deduplication hashmap,
    /// and it is represented as a tombstone in the union find tree.
    ///
    /// for example, if we add the expressions `x * y` and `x * x` to the egraph, and then union `x` and `y`, the two expressions will
    /// becaume equal, and one of them will be killed and become a duplicate enode tombstone.
    ///
    /// a looper enode is an enode which was detected to cause a loop during a union operation, and was killed to avoid the creation of
    /// the loop, since the egraph is not allowed to have loops.
    ///
    /// for example, if we add the expression `x * 0` to the egraph, and then add the rewrite rule `x * 0 => 0`, it will cause a union
    /// between `x * 0` and `0`, but this union will create a loop, unless we get rid of the `x * 0` enode. so, in that case, this
    /// enode will be killed and become a looper enode tombstone.
    Tombstone,
}

/// the union find tree of enodes.
#[derive(Debug, Clone)]
pub struct ENodesUnionFind(pub UnionFind<ENodesUnionFindItem>);
impl ENodesUnionFind {
    /// returns a new empty union find tree.
    pub fn new() -> Self {
        Self(UnionFind::new())
    }

    /// returns the amount of enodes in the union find tree.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// finds the root enode of the given enode.
    pub fn root_of_enode(&self, enode_id: ENodeId) -> ENodeId {
        ENodeId(self.0.root_of_item(enode_id.0))
    }

    /// returns an iterator over all enodes equal to the given enode.
    pub fn enodes_eq_to(&self, enode_id: ENodeId) -> impl Iterator<Item = &ENode> + '_ {
        self.0
            .items_eq_to(enode_id.0)
            .map(ENodeId)
            .filter_map(|enode_id| self[enode_id].as_e_node())
    }

    /// returns an iterator over all enodes equal to the given enode, including enode ids.
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

    /// returns an iterator over all enodes in the given eclass.
    pub fn enodes_in_eclass(&self, eclass_id: EClassId) -> impl Iterator<Item = &ENode> + '_ {
        self.enodes_eq_to(eclass_id.enode_id)
    }

    /// returns an iterator over all enodes in the given eclass, including enode ids.
    pub fn enumerate_enodes_in_eclass(
        &self,
        eclass_id: EClassId,
    ) -> impl Iterator<Item = (ENodeId, &ENode)> + '_ {
        self.enumerate_enodes_eq_to(eclass_id.enode_id)
    }

    /// returns an iterator over all enodes in the given effective eclass.
    pub fn enodes_in_effective_eclass(
        &self,
        effective_eclass_id: EffectiveEClassId,
    ) -> impl Iterator<Item = &ENode> + '_ {
        self.enodes_eq_to(effective_eclass_id.eclass_root)
    }

    /// returns an iterator over all enodes in the given effective eclass, including enode ids.
    pub fn enumerate_enodes_in_effective_eclass(
        &self,
        effective_eclass_id: EffectiveEClassId,
    ) -> impl Iterator<Item = (ENodeId, &ENode)> + '_ {
        self.enumerate_enodes_eq_to(effective_eclass_id.eclass_root)
    }

    /// returns an iterator over the enode ids of all enodes in the union find tree.
    pub fn enode_ids(&self) -> impl Iterator<Item = ENodeId> + use<> {
        self.0.item_ids().map(ENodeId)
    }

    /// returns an iterator over all entries in the union find tree, including both enode entries and tombstone entries.
    pub fn entries(&self) -> impl Iterator<Item = (ENodeId, &ENodesUnionFindItem)> + use<'_> {
        self.enode_ids().map(|enode_id| (enode_id, &self[enode_id]))
    }

    /// returns an iterator over all enodes in the union find tree, excluding tombstone entries.
    pub fn enodes(&self) -> impl Iterator<Item = (ENodeId, &ENode)> + use<'_> {
        self.entries().filter_map(|(id, item)| {
            let enode = item.as_e_node()?;
            Some((id, enode))
        })
    }

    /// returns an iterator over the eclass ids of all eclasses in the union find tree.
    pub fn eclass_ids(&self) -> impl Iterator<Item = EClassId> + use<'_> {
        self.0.root_item_ids().map(|item| ENodeId(item).eclass_id())
    }

    /// returns an iterator over the effective eclass ids of all eclasses in the union find tree.
    pub fn effective_eclass_ids(&self) -> impl Iterator<Item = EffectiveEClassId> + use<'_> {
        self.0.root_item_ids().map(|item| EffectiveEClassId {
            eclass_root: ENodeId(item),
        })
    }

    /// creates a new enode in the union find tree. this must only be performed after properly deduping the enode in the egraph.
    fn create_new_enode(&mut self, enode: ENode) -> ENodeId {
        ENodeId(self.0.create_new_item(ENodesUnionFindItem::ENode(enode)))
    }

    /// peeks the next enode id that will be returned for the next enode created in the union find tree.
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
            RecLink::from(extract_link_res.node)
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

    /// extracts the given eclass.
    pub fn extract_eclass(&self, eclass_id: EClassId) -> RecNode {
        let effective_eclass_id = eclass_id.to_effective(self);
        let mut cache = ExtractionCache::new();
        let res = self.extract_eclass_inner(effective_eclass_id, &mut cache);
        res.node
    }

    /// extracts the given enode.
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

    hashmap: RefCell<HashMap<EffectiveENode, ENodeId>>,
}
impl EGraph {
    /// returns a new empty egraph.
    pub fn new() -> Self {
        Self {
            union_find: ENodesUnionFind::new(),
            next_internal_var: InternalVar(0),
            hashmap: RefCell::new(HashMap::new()),
        }
    }

    pub fn union_find(&self) -> &ENodesUnionFind {
        &self.union_find
    }

    pub fn union_find_mut(&mut self) -> &mut ENodesUnionFind {
        &mut self.union_find
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
            self.match_enode_link(
                effective_eclass_id,
                &*query,
                Cow::Borrowed(&initial_ctx),
                &mut match_ctxs,
            );

            matches.extend(match_ctxs.into_iter().map(|ctx| SimpleRewriteMatch {
                final_ctx: ctx,
                effective_eclass_id,
            }));
        }

        matches
    }

    fn match_enode_link<C: Clone>(
        &self,
        link_effective_eclass_id: EffectiveEClassId,
        link_matcher: &dyn QueryLinkMatcher<C>,
        ctx: Cow<C>,
        match_ctxs: &mut Vec<C>,
    ) {
        match link_matcher.match_link(link_effective_eclass_id, self, ctx) {
            QueryMatchLinkRes::NoMatch => {
                return;
            }
            QueryMatchLinkRes::Match(QueryMatch { new_ctx }) => {
                match_ctxs.push(new_ctx.into_owned());
            }
            QueryMatchLinkRes::RecurseIntoENodes {
                new_ctx,
                enode_matcher,
            } => {
                // we want to pass the new ctx to each enode in the eclass.
                //
                // but, if the returned new ctx is owned, we don't want to clone it for each enode.
                //
                // so, we grab a reference to it here, and construct a borrowed cow for it in every iteration, for every enode, thus
                // avoiding the clone, even in the case where the new ctx is owned.
                let new_ctx_ref = match &new_ctx {
                    Cow::Borrowed(borrowed) => *borrowed,
                    Cow::Owned(owned_ctx) => owned_ctx,
                };
                for (enode_id, enode) in self
                    .union_find
                    .enumerate_enodes_in_effective_eclass(link_effective_eclass_id)
                {
                    self.match_enode(
                        enode_id,
                        enode,
                        &*enode_matcher,
                        // cloning this is zero cost since it is borrowed.
                        Cow::Borrowed(new_ctx_ref),
                        match_ctxs,
                    )
                }
            }
        }
    }

    fn match_enode_links<C: Clone>(
        &self,
        enode: &ENode,
        links_matcher: &dyn QueryLinksMatcher<C>,
        ctx: Cow<C>,
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
            match_ctxs.push(new_ctx.into_owned());
            return;
        }

        // now match the links.
        let mut cur_match_ctxs: Vec<C> = vec![new_ctx.into_owned()];
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
                    Cow::Borrowed(cur_ctx),
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

    fn match_enode<C: Clone>(
        &self,
        enode_id: ENodeId,
        enode: &ENode,
        matcher: &dyn QueryENodeMatcher<C>,
        ctx: Cow<C>,
        match_ctxs: &mut Vec<C>,
    ) {
        match matcher.match_enode(enode_id, enode, self, ctx) {
            QueryMatchENodeRes::NoMatch => {
                return;
            }
            QueryMatchENodeRes::Match(QueryMatch { new_ctx }) => {
                match_ctxs.push(new_ctx.into_owned());
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
                    hashbrown::hash_map::Entry::Occupied(occupied_entry) => {
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
                    hashbrown::hash_map::Entry::Vacant(vacant_entry) => {
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
