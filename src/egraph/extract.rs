use derive_more::{Add, AddAssign};
use hashbrown::HashMap;

use crate::{egraph::*, rec_node::*};

impl ENodesUnionFind {
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
