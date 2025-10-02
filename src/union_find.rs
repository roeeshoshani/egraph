use std::num::NonZeroUsize;

#[must_use]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ItemId(pub NonZeroUsize);
impl ItemId {
    pub fn index(&self) -> usize {
        self.0.get()
    }
}

pub struct UnionFind {
    next_item_id: NonZeroUsize,
    parent_of_item: Vec<Option<ItemId>>,
}
impl UnionFind {
    pub fn new() -> Self {
        Self {
            next_item_id: unsafe { NonZeroUsize::new_unchecked(1) },
            parent_of_item: Vec::new(),
        }
    }
    pub fn create_new_item(&mut self) -> ItemId {
        let res = self.next_item_id;
        self.next_item_id.checked_add(1).unwrap();
        ItemId(res)
    }
    fn get_parent_of_item(&self, item: ItemId) -> Option<ItemId> {
        self.parent_of_item.get(item.0.get()).copied()?
    }
    fn set_parent_of_item(&mut self, item: ItemId, parent: Option<ItemId>) {
        let item_index = item.index();

        // the array is lazily extended. so, it is possible that an item exists even though the array is too small to hold its index.
        // in that case, resize the array.
        if !(item_index < self.parent_of_item.len()) {
            if parent.is_none() {
                // in this case, the parent is already none, so we don't need to do anything.
                return;
            }
            // resize the array such that it can hold the currently highest item id, which is one less than than the next item id.
            self.parent_of_item.resize(self.next_item_id.get(), None);
        }

        self.parent_of_item[item_index] = parent;
    }
    pub fn union(&mut self, item_a: ItemId, item_b: ItemId) {
        // we use a loop here since we may need to climb up the parents if both items already have a parent.
        let mut cur_item_a = item_a;
        let mut cur_item_b = item_b;
        loop {
            match (
                self.get_parent_of_item(cur_item_a),
                self.get_parent_of_item(cur_item_b),
            ) {
                (None, None) => {
                    // none of the items have a parent, create a new parent which is the union of both of them.
                    let parent = self.create_new_item();
                    self.set_parent_of_item(cur_item_a, Some(parent));
                    self.set_parent_of_item(cur_item_b, Some(parent));

                    // we're done
                    break;
                }
                (None, Some(parent_b)) => {
                    // add item a to the group of b by settings its parent to parent b
                    self.set_parent_of_item(cur_item_a, Some(parent_b));

                    // we're done
                    break;
                }
                (Some(parent_a), None) => {
                    // add item b to the group of a by settings its parent to parent a
                    self.set_parent_of_item(cur_item_b, Some(parent_a));

                    // we're done
                    break;
                }
                (Some(parent_a), Some(parent_b)) => {
                    // both items already have a parent, union their parents.
                    cur_item_a = parent_a;
                    cur_item_b = parent_b;
                }
            }
        }
    }
    fn root_of_item(&self, item: ItemId) -> ItemId {
        let mut cur_item = item;
        loop {
            match self.get_parent_of_item(item) {
                Some(parent) => {
                    // advance to the parent
                    cur_item = parent;
                }
                None => {
                    // no more parents, we reached the root
                    break cur_item;
                }
            }
        }
    }
    pub fn items_eq_to(&self, item: ItemId) -> impl Iterator<Item = ItemId> + '_ {
        todo!();
        std::iter::empty()
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

    #[test]
    fn test_eq_self_single_item() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item();

        assert!(union_find.are_eq(a, a));
    }

    #[test]
    fn test_eq_self_multiple_item() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item();

        // create some more items just to add some noise
        for _ in 0..20 {
            let _ = union_find.create_new_item();
        }

        assert!(union_find.are_eq(a, a));
    }

    #[test]
    fn test_not_eq_before_union() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item();
        let b = union_find.create_new_item();

        assert!(!union_find.are_eq(a, b));
    }

    #[test]
    fn test_eq_after_union() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item();
        let b = union_find.create_new_item();

        union_find.union(a, b);

        assert!(union_find.are_eq(a, b));
    }

    #[test]
    fn test_union_transitive() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item();
        let b = union_find.create_new_item();
        let c = union_find.create_new_item();

        union_find.union(a, b);
        union_find.union(b, c);

        assert!(union_find.are_eq(a, c));

        assert_eq!(collect_to_vec(union_find.items_eq_to(a)), &[b, c]);
    }

    fn chk_groups(union_find: &UnionFind, all_items: &[ItemId], groups: &[&[ItemId]]) {
        // make sure that the provided groups contain all items
        let groups_len_sum: usize = groups.iter().map(|group| group.len()).sum();
        assert_eq!(all_items.len(), groups_len_sum);

        for &group in groups {
            // all items in the group should be equal to each other
            for &item1 in group {
                for &item2 in group {
                    assert!(union_find.are_eq(item1, item2));
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
                assert_eq!(
                    group_items_other_than,
                    collect_to_vec(union_find.items_eq_to(item))
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
                    assert!(!union_find.are_eq(item, group_item));
                    assert!(!items_eq_to_item.contains(&group_item));
                }
            }
        }
    }

    #[test]
    fn test_union_transitive_eq_multilayer() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item();
        let b = union_find.create_new_item();
        let c = union_find.create_new_item();
        let d = union_find.create_new_item();
        let e = union_find.create_new_item();
        let f = union_find.create_new_item();
        let g = union_find.create_new_item();

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
