use std::num::NonZeroUsize;

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
    #[must_use]
    fn alloc_id(&mut self) -> NonZeroUsize {
        let res = self.next_id;
        self.next_id = self.next_id.checked_add(1).unwrap();
        res
    }
    fn get_parent_of(&self, id: NonZeroUsize) -> Option<NonZeroUsize> {
        self.parent_of_id.get(id.get()).copied()?
    }
    fn set_parent(&mut self, id: NonZeroUsize, new_parent: Option<NonZeroUsize>) {
        let index = id.get();

        // the array is lazily extended. so, it is possible that an id exists even though the array is too small to hold its index.
        // in that case, resize the array.
        if !(index < self.parent_of_id.len()) {
            if new_parent.is_none() {
                // in this case, the parent is already none, so we don't need to do anything.
                return;
            }
            // resize the array such that it can hold the currently highest id, which is one less than than the next id.
            self.parent_of_id.resize(self.next_id.get(), None);
        }

        self.parent_of_id[index] = new_parent;
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ItemId(pub NonZeroUsize);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct ParentId(NonZeroUsize);

/// an id of any kind, either an item id or a parent id.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum AnyId {
    Item(ItemId),
    Parent(ParentId),
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
    #[must_use]
    pub fn create_new_item(&mut self, item: T) -> ItemId {
        self.items.push(item);

        // allocate an id for it
        let id = ItemId(self.item_to_parent_map.alloc_id());

        // the id should be 1 more than the index in the items vec.
        // sanity check this assumption.
        debug_assert_eq!(id.0.get(), self.items.len());

        id
    }
    #[must_use]
    fn create_new_parent(&mut self) -> ParentId {
        ParentId(self.parent_to_parent_map.alloc_id())
    }
    fn get_parent_of_item(&self, item: ItemId) -> Option<ParentId> {
        self.item_to_parent_map.get_parent_of(item.0).map(ParentId)
    }
    fn get_parent_of_parent(&self, id: ParentId) -> Option<ParentId> {
        self.parent_to_parent_map.get_parent_of(id.0).map(ParentId)
    }
    fn set_parent_of_item(&mut self, item: ItemId, new_parent: Option<ParentId>) {
        self.item_to_parent_map
            .set_parent(item.0, new_parent.map(|x| x.0));
    }
    fn set_parent_of_parent(&mut self, id: ParentId, new_parent: Option<ParentId>) {
        self.parent_to_parent_map
            .set_parent(id.0, new_parent.map(|x| x.0));
    }
    fn get_parent_of_any(&self, id: AnyId) -> Option<ParentId> {
        match id {
            AnyId::Item(item_id) => self.get_parent_of_item(item_id),
            AnyId::Parent(parent_id) => self.get_parent_of_parent(parent_id),
        }
    }
    fn set_parent_of_any(&mut self, id: AnyId, new_parent: Option<ParentId>) {
        match id {
            AnyId::Item(id) => self.set_parent_of_item(id, new_parent),
            AnyId::Parent(id) => self.set_parent_of_parent(id, new_parent),
        }
    }
    pub fn union(&mut self, item_a: ItemId, item_b: ItemId) {
        // we use a loop here since we may need to climb up the parents if both items already have a parent.
        let mut cur_item_a = AnyId::Item(item_a);
        let mut cur_item_b = AnyId::Item(item_b);
        loop {
            match (
                self.get_parent_of_any(cur_item_a),
                self.get_parent_of_any(cur_item_b),
            ) {
                (None, None) => {
                    // none of the items have a parent, create a new parent which is the union of both of them.
                    let parent = self.create_new_parent();
                    self.set_parent_of_any(cur_item_a, Some(parent));
                    self.set_parent_of_any(cur_item_b, Some(parent));

                    // we're done
                    break;
                }
                (None, Some(parent_b)) => {
                    // add item a to the group of b by settings its parent to parent b
                    self.set_parent_of_any(cur_item_a, Some(parent_b));

                    // we're done
                    break;
                }
                (Some(parent_a), None) => {
                    // add item b to the group of a by settings its parent to parent a
                    self.set_parent_of_any(cur_item_b, Some(parent_a));

                    // we're done
                    break;
                }
                (Some(parent_a), Some(parent_b)) => {
                    // both items already have a parent, union their parents.
                    cur_item_a = AnyId::Parent(parent_a);
                    cur_item_b = AnyId::Parent(parent_b);
                }
            }
        }
    }
    fn root_of_item(&self, item: ItemId) -> AnyId {
        let mut cur_item = AnyId::Item(item);
        loop {
            match self.get_parent_of_any(cur_item) {
                Some(parent) => {
                    // advance to the parent
                    cur_item = AnyId::Parent(parent);
                }
                None => {
                    // no more parents, we reached the root
                    break cur_item;
                }
            }
        }
    }
    pub fn items_eq_to(&self, item: ItemId) -> impl Iterator<Item = ItemId> + '_ {
        // TODO: make this efficient if needed
        let root = self.root_of_item(item);
        (1..self.item_to_parent_map.next_id.get())
            .map(|i| {
                ItemId(
                    // SAFETY: our iteration starts from 1, so the value can't be 0
                    unsafe { NonZeroUsize::new_unchecked(i) },
                )
            })
            .filter(move |&cur_item| cur_item != item && self.root_of_item(cur_item) == root)
    }
    pub fn are_eq(&self, item_a: ItemId, item_b: ItemId) -> bool {
        if item_a == item_b {
            return true;
        }
        self.root_of_item(item_a) == self.root_of_item(item_b)
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

    fn chk_groups(union_find: &UnionFind<()>, all_items: &[ItemId], groups: &[&[ItemId]]) {
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
            }

            // make sure that each item that is not in the group is not equal to all of the items that are in the group,
            // since it is not with them in the group.
            for &item in all_items {
                // we only want to check items that are not in the group.
                if group.contains(&item) {
                    continue;
                }

                let items_eq_to_item = collect_to_vec(union_find.items_eq_to(item));

                for &group_item in group {
                    assert!(
                        !union_find.are_eq(item, group_item),
                        "expected item {item:?} to not be eq to group item {group_item:?} since item {item:?} is not part of the group {group:?}",
                    );
                    assert!(
                        !items_eq_to_item.contains(&group_item),
                        "expected group item {group:?} to NOT appear in the list of items eq to {item:?}, which contains items {items_eq_to_item:?}, since item {item:?} is not part of the group {group:?}",
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
}
