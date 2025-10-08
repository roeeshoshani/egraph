use std::{
    num::NonZeroUsize,
    ops::{Index, IndexMut},
};

use either::Either;

/// a mapping between ids to their parent ids.
#[derive(Debug, Clone)]
struct IdToParentMap {
    next_id: NonZeroUsize,
    parent_of_id: Vec<Option<NonZeroUsize>>,
}
impl IdToParentMap {
    fn new() -> Self {
        Self {
            next_id: NonZeroUsize::new(1).unwrap(),
            parent_of_id: Vec::new(),
        }
    }
    fn id_to_index(id: NonZeroUsize) -> usize {
        id.get() - 1
    }
    #[must_use]
    fn alloc_id(&mut self) -> NonZeroUsize {
        let res = self.next_id;
        self.next_id = self.next_id.checked_add(1).unwrap();
        res
    }
    fn get_parent_of(&self, id: NonZeroUsize) -> Option<NonZeroUsize> {
        self.parent_of_id.get(Self::id_to_index(id)).copied()?
    }
    fn set_parent(&mut self, id: NonZeroUsize, new_parent: Option<NonZeroUsize>) {
        let index = Self::id_to_index(id);

        // the array is lazily extended. so, it is possible that an id exists even though the array is too small to hold its index.
        // in that case, resize the array.
        if !(index < self.parent_of_id.len()) {
            if new_parent.is_none() {
                // in this case, the parent is already none, so we don't need to do anything.
                return;
            }
            // resize the array such that it can hold the currently highest id, which is one less than than the next id.
            // note that the ids are 1-based and not 0-based, so we don't need an extra ` - 1` here, only one ` - 1` to get from the
            // next id to the currently highest id.
            self.parent_of_id.resize(self.next_id.get() - 1, None);
        }

        self.parent_of_id[index] = new_parent;
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct UnionFindItemId(pub NonZeroUsize);
impl UnionFindItemId {
    /// the index of the item in the items array
    fn index(&self) -> usize {
        self.0.get() - 1
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct UnionFindParentId(pub NonZeroUsize);

/// an id of any kind, either an item id or a parent id.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum UnionFindAnyId {
    Item(UnionFindItemId),
    Parent(UnionFindParentId),
}
impl UnionFindAnyId {
    pub fn as_item(&self) -> Option<UnionFindItemId> {
        match self {
            UnionFindAnyId::Item(x) => Some(*x),
            UnionFindAnyId::Parent(_) => None,
        }
    }
    pub fn as_parent(&self) -> Option<UnionFindParentId> {
        match self {
            UnionFindAnyId::Item(_) => None,
            UnionFindAnyId::Parent(x) => Some(*x),
        }
    }
}

/// the result of performing a union operation.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum UnionRes {
    /// the union that we performed is new, and resulted in unioning 2 items that previously weren't considered equal.
    New,

    /// the union that was performed is not new, and the 2 items that it unioned were already considered equal.
    Existing,
}

#[derive(Debug, Clone)]
pub struct UnionFind<T> {
    item_to_parent_map: IdToParentMap,
    parent_to_parent_map: IdToParentMap,
    items: Vec<T>,
}
impl<T> UnionFind<T> {
    pub fn new() -> Self {
        Self {
            item_to_parent_map: IdToParentMap::new(),
            parent_to_parent_map: IdToParentMap::new(),
            items: Vec::new(),
        }
    }
    pub fn len(&self) -> usize {
        self.items.len()
    }
    #[must_use]
    pub fn create_new_item(&mut self, item: T) -> UnionFindItemId {
        self.items.push(item);

        // allocate an id for it
        let id = UnionFindItemId(self.item_to_parent_map.alloc_id());

        // the id should be 1 more than the index in the items vec.
        // sanity check this assumption.
        debug_assert_eq!(id.0.get(), self.items.len());

        id
    }
    #[must_use]
    fn create_new_parent(&mut self) -> UnionFindParentId {
        UnionFindParentId(self.parent_to_parent_map.alloc_id())
    }
    fn get_parent_of_item(&self, item: UnionFindItemId) -> Option<UnionFindParentId> {
        self.item_to_parent_map
            .get_parent_of(item.0)
            .map(UnionFindParentId)
    }
    fn get_parent_of_parent(&self, id: UnionFindParentId) -> Option<UnionFindParentId> {
        self.parent_to_parent_map
            .get_parent_of(id.0)
            .map(UnionFindParentId)
    }
    fn set_parent_of_item(&mut self, item: UnionFindItemId, new_parent: Option<UnionFindParentId>) {
        self.item_to_parent_map
            .set_parent(item.0, new_parent.map(|x| x.0));
    }
    fn set_parent_of_parent(
        &mut self,
        id: UnionFindParentId,
        new_parent: Option<UnionFindParentId>,
    ) {
        self.parent_to_parent_map
            .set_parent(id.0, new_parent.map(|x| x.0));
    }
    fn get_parent_of_any(&self, id: UnionFindAnyId) -> Option<UnionFindParentId> {
        match id {
            UnionFindAnyId::Item(item_id) => self.get_parent_of_item(item_id),
            UnionFindAnyId::Parent(parent_id) => self.get_parent_of_parent(parent_id),
        }
    }
    pub fn union(&mut self, item_a: UnionFindItemId, item_b: UnionFindItemId) -> UnionRes {
        let root_a = self.root_of_item(item_a);
        let root_b = self.root_of_item(item_b);
        if root_a == root_b {
            // the items are already unioned.
            return UnionRes::Existing;
        }

        // the items are not unioned, we should union them by making sure that they both have the same root.
        //
        // this can be achieved by just setting the parent of one of the roots to the other root. but, this only works if
        // one of the roots is a parent and not an item, since items can not be parents of other nodes.
        //
        // if both nodes are items, we create a shared parent for them.
        match (root_a, root_b) {
            (UnionFindAnyId::Item(item_a), UnionFindAnyId::Item(item_b)) => {
                let new_parent = self.create_new_parent();
                self.set_parent_of_item(item_a, Some(new_parent));
                self.set_parent_of_item(item_b, Some(new_parent));
                UnionRes::New
            }
            (UnionFindAnyId::Item(item_a), UnionFindAnyId::Parent(parent_b)) => {
                self.set_parent_of_item(item_a, Some(parent_b));
                UnionRes::New
            }
            (UnionFindAnyId::Parent(parent_a), UnionFindAnyId::Item(item_b)) => {
                self.set_parent_of_item(item_b, Some(parent_a));
                UnionRes::New
            }
            (UnionFindAnyId::Parent(parent_a), UnionFindAnyId::Parent(parent_b)) => {
                // TODO: decide which one to use as the new parent base on the depth to balance the tree.
                self.set_parent_of_parent(parent_a, Some(parent_b));
                UnionRes::New
            }
        }
    }
    pub fn root_of_any(&self, id: UnionFindAnyId) -> UnionFindAnyId {
        let mut cur_item = id;
        loop {
            match self.get_parent_of_any(cur_item) {
                Some(parent) => {
                    // advance to the parent
                    cur_item = UnionFindAnyId::Parent(parent);
                }
                None => {
                    // no more parents, we reached the root
                    break cur_item;
                }
            }
        }
    }
    pub fn root_of_item(&self, item: UnionFindItemId) -> UnionFindAnyId {
        self.root_of_any(UnionFindAnyId::Item(item))
    }
    pub fn root_of_parent(&self, id: UnionFindParentId) -> UnionFindAnyId {
        self.root_of_any(UnionFindAnyId::Parent(id))
    }
    pub fn item_ids(&self) -> impl Iterator<Item = UnionFindItemId> + use<T> {
        (1..self.item_to_parent_map.next_id.get()).map(|i| {
            UnionFindItemId(
                // SAFETY: our iteration starts from 1, so the value can't be 0
                unsafe { NonZeroUsize::new_unchecked(i) },
            )
        })
    }
    /// returns an iterator over all items equal to the given item, excluding the item itself
    pub fn items_eq_to(&self, item: UnionFindItemId) -> impl Iterator<Item = UnionFindItemId> + '_ {
        // TODO: make this efficient if needed. we iterate over all of the nodes here, which may not be the most efficient thing.
        let root = self.root_of_item(item);
        self.item_ids()
            .filter(move |&cur_item| cur_item != item && self.root_of_item(cur_item) == root)
    }
    /// returns an iterator over all items equal to the given item, including the item itself
    pub fn items_eq_to_including_self(
        &self,
        item: UnionFindItemId,
    ) -> impl Iterator<Item = UnionFindItemId> + '_ {
        let root = self.root_of_item(item);
        self.item_ids()
            .filter(move |&cur_item| cur_item == item || self.root_of_item(cur_item) == root)
    }
    /// returns an iterator over all items equal to the given parent
    pub fn items_eq_to_parent(
        &self,
        id: UnionFindParentId,
    ) -> impl Iterator<Item = UnionFindItemId> + '_ {
        let root = self.root_of_parent(id);
        self.item_ids()
            .filter(move |&cur_item| self.root_of_item(cur_item) == root)
    }
    /// returns an iterator over all items equal to the given id. if the given id is an item id, the returned iterator excludes the
    /// item itself.
    pub fn items_eq_to_any(
        &self,
        id: UnionFindAnyId,
    ) -> Either<
        impl Iterator<Item = UnionFindItemId> + '_,
        impl Iterator<Item = UnionFindItemId> + '_,
    > {
        match id {
            UnionFindAnyId::Item(item_id) => Either::Left(self.items_eq_to(item_id)),
            UnionFindAnyId::Parent(parent_id) => Either::Right(self.items_eq_to_parent(parent_id)),
        }
    }
    /// returns an iterator over all items equal to the given id. if the given id is an item id, the returned iterator includes the
    /// item itself.
    pub fn items_eq_to_any_including_self(
        &self,
        id: UnionFindAnyId,
    ) -> Either<
        impl Iterator<Item = UnionFindItemId> + '_,
        impl Iterator<Item = UnionFindItemId> + '_,
    > {
        match id {
            UnionFindAnyId::Item(item_id) => Either::Left(self.items_eq_to_including_self(item_id)),
            UnionFindAnyId::Parent(parent_id) => Either::Right(self.items_eq_to_parent(parent_id)),
        }
    }
    pub fn are_eq(&self, item_a: UnionFindItemId, item_b: UnionFindItemId) -> bool {
        if item_a == item_b {
            return true;
        }
        self.root_of_item(item_a) == self.root_of_item(item_b)
    }
    pub fn flatten(&mut self) {
        for item in self.item_ids() {
            let root = self.root_of_item(item);
            // if the item has a root other than itself, then make it point to its root
            if let UnionFindAnyId::Parent(parent) = root {
                self.set_parent_of_item(item, Some(parent));
            }
        }
    }
    /// orphans the given item, detaching it from its parent.
    pub fn orphan(&mut self, item: UnionFindItemId) {
        self.set_parent_of_item(item, None);
    }
    pub fn items(&self) -> &[T] {
        &self.items
    }
    pub fn items_mut(&mut self) -> &mut [T] {
        &mut self.items
    }
}
impl<T> Index<UnionFindItemId> for UnionFind<T> {
    type Output = T;

    fn index(&self, id: UnionFindItemId) -> &Self::Output {
        &self.items[id.index()]
    }
}
impl<T> IndexMut<UnionFindItemId> for UnionFind<T> {
    fn index_mut(&mut self, id: UnionFindItemId) -> &mut Self::Output {
        &mut self.items[id.index()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect_to_vec<I, T>(iterator: I) -> Vec<T>
    where
        I: Iterator<Item = T>,
    {
        iterator.collect()
    }

    fn chk_groups<T>(
        union_find: &UnionFind<T>,
        all_items: &[UnionFindItemId],
        groups: &[&[UnionFindItemId]],
    ) {
        // make sure that the provided groups contain all items
        let groups_len_sum: usize = groups.iter().map(|group| group.len()).sum();
        assert_eq!(all_items.len(), groups_len_sum);

        for &group in groups {
            // all items in the group should be equal to each other
            for &item1 in group {
                for &item2 in group {
                    assert!(
                        union_find.are_eq(item1, item2),
                        "items {item1:?} and {item2:?} were expected to be eq, since they are in the same group of {group:?}, but they are not eq",
                    );
                }
            }

            // make sure that when iterating over each item in the group, we get all other items in the group, except for the item
            // itself.
            for &item in group {
                let group_items_other_than = {
                    let mut tmp = collect_to_vec(group.iter().copied());
                    // keep all items other than the current item
                    tmp.retain(|x| *x != item);
                    tmp
                };
                let items_eq_to_item = collect_to_vec(union_find.items_eq_to(item));
                assert_eq!(
                    group_items_other_than, items_eq_to_item,
                    "item {item:?} in group {group:?} was expected to have items eq to of {group_items_other_than:?} but instead has {items_eq_to_item:?}",
                );

                let items_eq_to_item_including_self =
                    collect_to_vec(union_find.items_eq_to_including_self(item));
                assert_eq!(
                    group, items_eq_to_item_including_self,
                    "item {item:?} in group {group:?} was expected to have items eq including self to of {group:?} but instead has {items_eq_to_item_including_self:?}",
                );
            }

            // make sure that each item that is not in the group is not equal to all of the items that are in the group,
            // since it is not with them in the group.
            for &item in all_items {
                // we only want to check items that are not in the group.
                if group.contains(&item) {
                    continue;
                }

                let items_eq_to_item = collect_to_vec(union_find.items_eq_to(item));
                let items_eq_to_item_including_self =
                    collect_to_vec(union_find.items_eq_to_including_self(item));

                for &group_item in group {
                    assert!(
                        !union_find.are_eq(item, group_item),
                        "expected item {item:?} to not be eq to group item {group_item:?} since item {item:?} is not part of the group {group:?}",
                    );
                    assert!(
                        !items_eq_to_item.contains(&group_item),
                        "expected group item {group:?} to NOT appear in the list of items eq to {item:?}, which contains items {items_eq_to_item:?}, since item {item:?} is not part of the group {group:?}",
                    );
                    assert!(
                        !items_eq_to_item_including_self.contains(&group_item),
                        "expected group item {group:?} to NOT appear in the list of items eq to {item:?} including self, which contains items {items_eq_to_item_including_self:?}, since item {item:?} is not part of the group {group:?}",
                    );
                }
            }
        }
    }

    #[test]
    fn test_eq_self_single_item() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item(());

        assert!(union_find.are_eq(a, a));
    }

    #[test]
    fn test_eq_self_multiple_item() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item(());

        // create some more items just to add some noise
        for _ in 0..20 {
            let _ = union_find.create_new_item(());
        }

        assert!(union_find.are_eq(a, a));
    }

    #[test]
    fn test_not_eq_before_union() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item(());
        let b = union_find.create_new_item(());

        assert!(!union_find.are_eq(a, b));
    }

    #[test]
    fn test_eq_after_union() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item(());
        let b = union_find.create_new_item(());

        union_find.union(a, b);

        assert!(union_find.are_eq(a, b));
    }

    #[test]
    fn test_union_transitive() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item(());
        let b = union_find.create_new_item(());
        let c = union_find.create_new_item(());

        let all_items = [a, b, c];

        chk_groups(&union_find, &all_items, &[&[a], &[b], &[c]]);

        union_find.union(a, b);

        chk_groups(&union_find, &all_items, &[&[a, b], &[c]]);

        union_find.union(b, c);

        chk_groups(&union_find, &all_items, &[&all_items]);
    }

    #[test]
    fn test_union_transitive_eq_multilayer() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item(());
        let b = union_find.create_new_item(());
        let c = union_find.create_new_item(());
        let d = union_find.create_new_item(());
        let e = union_find.create_new_item(());
        let f = union_find.create_new_item(());
        let g = union_find.create_new_item(());

        let all_items = [a, b, c, d, e, f, g];

        // create 3 groups of unionized items:
        // - a,b
        // - c,d
        // - e,f,g
        union_find.union(a, b);
        union_find.union(c, d);
        union_find.union(e, f);
        union_find.union(e, g);

        chk_groups(&union_find, &all_items, &[&[a, b], &[c, d], &[e, f, g]]);

        // now start unioning the groups.
        // first, union the first group (a,b) and the third group (e,f,g).
        union_find.union(b, g);

        chk_groups(&union_find, &all_items, &[&[a, b, e, f, g], &[c, d]]);

        // now merge all groups to a single group.
        union_find.union(a, c);

        chk_groups(&union_find, &all_items, &[&all_items]);
    }

    #[test]
    fn test_index_gives_correct_value() {
        let mut union_find = UnionFind::new();

        let a_val = 57;
        let b_val = 254;
        let c_val = 125125;
        let d_val = 929;
        let e_val = 4;
        let f_val = 0;
        let g_val = 14526;

        let all_vals = [a_val, b_val, c_val, d_val, e_val, f_val, g_val];

        let a = union_find.create_new_item(a_val);
        let b = union_find.create_new_item(b_val);
        let c = union_find.create_new_item(c_val);
        let d = union_find.create_new_item(d_val);
        let e = union_find.create_new_item(e_val);
        let f = union_find.create_new_item(f_val);
        let g = union_find.create_new_item(g_val);

        let all_items = [a, b, c, d, e, f, g];

        // make sure that we can fetch back the values and get the correct values.
        for (item, val) in all_items.iter().copied().zip(all_vals.iter().copied()) {
            assert_eq!(union_find[item], val)
        }

        // make sure that even after unioning, we still get the correct values
        union_find.union(a, b);
        union_find.union(c, d);
        union_find.union(e, f);
        union_find.union(e, g);

        for (item, val) in all_items.iter().copied().zip(all_vals.iter().copied()) {
            assert_eq!(union_find[item], val)
        }

        union_find.union(b, g);

        for (item, val) in all_items.iter().copied().zip(all_vals.iter().copied()) {
            assert_eq!(union_find[item], val)
        }

        union_find.union(a, c);

        for (item, val) in all_items.iter().copied().zip(all_vals.iter().copied()) {
            assert_eq!(union_find[item], val)
        }
    }

    #[test]
    fn test_flatten() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item(());
        let b = union_find.create_new_item(());
        let c = union_find.create_new_item(());
        let d = union_find.create_new_item(());
        let e = union_find.create_new_item(());
        let f = union_find.create_new_item(());
        let g = union_find.create_new_item(());

        let all_items = [a, b, c, d, e, f, g];

        union_find.union(a, b);
        union_find.union(c, d);
        union_find.union(e, f);
        union_find.union(e, g);

        union_find.union(b, g);

        // sanity check the groups
        chk_groups(&union_find, &all_items, &[&[a, b, e, f, g], &[c, d]]);

        // at this point, some items have different parents even though they are equal
        assert!(union_find.are_eq(a, f));
        assert_ne!(
            union_find.get_parent_of_item(a).unwrap(),
            union_find.get_parent_of_item(f).unwrap()
        );

        union_find.flatten();

        // now the equal items should have the same parent
        assert_eq!(
            union_find.get_parent_of_item(a).unwrap(),
            union_find.get_parent_of_item(f).unwrap()
        );

        // but items that are not equal should still have different parents
        assert_ne!(
            union_find.get_parent_of_item(a).unwrap(),
            union_find.get_parent_of_item(c).unwrap()
        );

        // make sure that flatenning didn't mess with the groups
        chk_groups(&union_find, &all_items, &[&[a, b, e, f, g], &[c, d]]);
    }
}
