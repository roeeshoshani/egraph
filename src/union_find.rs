use std::{
    cell::{Cell, RefCell},
    num::NonZeroUsize,
    ops::{Index, IndexMut},
    sync::atomic::AtomicUsize,
};

pub static AVOID_UPDATE_PARENT_COUNT: AtomicUsize = AtomicUsize::new(0);

pub struct DontUpdateParentGuard(
    // dummy internal field, just to disallow construction without using the constructor.
    (),
);
impl DontUpdateParentGuard {
    pub fn new() -> Self {
        AVOID_UPDATE_PARENT_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        Self(())
    }
}
impl Drop for DontUpdateParentGuard {
    fn drop(&mut self) {
        AVOID_UPDATE_PARENT_COUNT.fetch_sub(1, std::sync::atomic::Ordering::Relaxed);
    }
}

/// the id of an item in the union find tree.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct UnionFindItemId(pub NonZeroUsize);
impl UnionFindItemId {
    /// the index of the item in the items array
    pub fn index(&self) -> usize {
        self.0.get() - 1
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

/// a union find tree, where each node contains an item of type `T`.
///
/// this type uses interior mutability for its internal storage. this is needed because the union find tree's lookup functions
/// may actually modify the structure of the tree, to optimize future lookups. for example, each root lookup makes the lookuped up
/// item point directly to its root, so that future root lookups on this item will be `O(1)`.
///
/// but, we still want the lookup functions to take a `&self`, and not a `&mut self`, for flexibility of usage. so, we use interior
/// mutability and enforce the borrowing rules at runtime.
#[derive(Debug, Clone)]
pub struct UnionFind<T> {
    /// an array of all items in the union find tree, indexed by the item id indexes.
    items: Vec<T>,

    /// an array of the parents of each item, indexed by the item id indexes.
    parent_of_item: Vec<Cell<UnionFindItemId>>,

    /// an array of the children of each item, indexed by the item id indexes.
    // TODO: is it better to use a hashset here?
    children_of_item: Vec<RefCell<Vec<UnionFindItemId>>>,
}
impl<T> UnionFind<T> {
    /// returns a new empty union find tree.
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            parent_of_item: Vec::new(),
            children_of_item: Vec::new(),
        }
    }

    /// returns the amount of items in the union find tree.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// creates a new item in the union find tree. the new item is placed in a new set containing only that item.
    #[must_use]
    pub fn create_new_item(&mut self, item: T) -> UnionFindItemId {
        self.items.push(item);

        let id = UnionFindItemId(
            // SAFETY: we just pushed to the vector so the length can't be 0.
            unsafe { NonZeroUsize::new_unchecked(self.items.len()) },
        );

        // initially, the item is a child of itself, and the parent of itself
        self.children_of_item.push(RefCell::new(vec![id]));
        self.parent_of_item.push(Cell::new(id));

        id
    }

    /// peeks the next item id without allocating it.
    pub fn peek_next_item_id(&self) -> UnionFindItemId {
        UnionFindItemId(
            // SAFETY: we add 1 so it can't be 0
            unsafe { NonZeroUsize::new_unchecked(self.items.len() + 1) },
        )
    }

    /// adds the given child to the list of children of the given parent.
    fn add_child(&self, parent: UnionFindItemId, child: UnionFindItemId) {
        self.children_of_item[parent.index()]
            .borrow_mut()
            .push(child);
    }

    /// removes the given child from the list of children of the given parent.
    fn remove_child(&self, parent: UnionFindItemId, child: UnionFindItemId) {
        let mut children = self.children_of_item[parent.index()].borrow_mut();
        let child_index = children.iter().position(|x| *x == child).unwrap();
        children.swap_remove(child_index);
    }

    /// returns the direct parent of the given item.
    pub fn get_parent_of_item(&self, item: UnionFindItemId) -> UnionFindItemId {
        self.parent_of_item[item.index()].get()
    }

    /// sets the direct parent of the given item, and correctly updates the children list of the old and new parents.
    ///
    /// returns the old parent.
    fn set_parent_of_item(
        &self,
        item: UnionFindItemId,
        new_parent: UnionFindItemId,
    ) -> UnionFindItemId {
        let old_parent = self.parent_of_item[item.index()].replace(new_parent);

        if old_parent == new_parent {
            // nothing changed
            return old_parent;
        }

        self.remove_child(old_parent, item);
        self.add_child(new_parent, item);

        old_parent
    }

    /// unions the given two items.
    ///
    /// this enslaves the root of item b to the root of item a.
    ///
    /// so, you can rely on the root of all items that were equal to a before the union call to remain the same root.]
    /// but, if the union result is new, then the root of all items that were equal to b before the union call has now changed,
    /// and their new root is now the root of item a.
    pub fn union(&self, item_a: UnionFindItemId, item_b: UnionFindItemId) -> UnionRes {
        // we use the "no update" version of the root lookup when looking up the root of item b, since making it point directly to
        // root b may not be optimal here, and we can potentially do even better.
        //
        // when we union the items, unless they are already unioned, we will enslave root b to root a.
        // this means that even if we use the "updating" version of the root lookup, after performing the union, item b will still
        // require 2 iterations to reach its root, since what was its root at the point of lookup, root b, is no longer the real root,
        // which is now root a, due to the enslaving that we performed.
        //
        // so, we can instead make item b point directly to root a, which is its real new root, so that future root lookups on item b
        // will only require one iteration.
        //
        // as for item a, it's root doesn't change due to the union, so we can use the regular "updating" version of the root lookup.
        let root_a = self.root_of_item(item_a);
        let root_b = self.root_of_item_no_update(item_b);
        if root_a == root_b {
            // the items are already unioned.
            //
            // make sure that we update item b to point directly to its root.
            self.set_parent_of_item(item_b, root_b);
            return UnionRes::Existing;
        }

        // union the items by enslaving root b to root a.
        self.set_parent_of_item(root_b, root_a);

        // make item b to point directly to its new root.
        self.set_parent_of_item(item_b, root_a);

        UnionRes::New
    }

    /// finds the root item of the given item, without updating the item to point directly to its root.
    pub fn root_of_item_no_update(&self, item: UnionFindItemId) -> UnionFindItemId {
        let mut cur_item = item;
        loop {
            let parent = self.get_parent_of_item(cur_item);
            if parent == cur_item {
                break cur_item;
            }
            cur_item = parent;
        }
    }

    /// finds the root item of the given item. this also updates the item to point directly to its root to make future lookups faster.
    pub fn root_of_item(&self, item: UnionFindItemId) -> UnionFindItemId {
        let root = self.root_of_item_no_update(item);

        if AVOID_UPDATE_PARENT_COUNT.load(std::sync::atomic::Ordering::Relaxed) == 0 {
            self.set_parent_of_item(item, root);
        }

        root
    }

    /// returns an iterator over all of the item ids in the tree.
    pub fn item_ids(&self) -> impl Iterator<Item = UnionFindItemId> + use<T> {
        (0..self.items.len()).map(|i| {
            UnionFindItemId(
                // SAFETY: we add 1 so it can't be 0
                unsafe { NonZeroUsize::new_unchecked(i + 1) },
            )
        })
    }

    /// returns an iterator over all of the root item ids in the tree.
    pub fn root_item_ids(&self) -> impl Iterator<Item = UnionFindItemId> + use<'_, T> {
        self.item_ids().filter(|item| self.is_root(*item))
    }

    /// checks if the given item is a root.
    pub fn is_root(&self, item: UnionFindItemId) -> bool {
        self.get_parent_of_item(item) == item
    }

    /// flattens all of the descendents of the given item to be direct children of it.
    fn flatten_descendents_of_item(&self, item: UnionFindItemId) {
        // we intentionally start with an immutable borrow instead of a mutable borrow, since in some cases we won't need to modify
        // the children array at all, for example when all of the descendents of this item are already direct children of it (the
        // item was already flattened).
        //
        // in those cases, we want to avoid taking a mutable borrow to the children array at all.
        // we only take the mutable borrow when we are certain that we need to modify the array.
        // this is important since it allows more flexible usage patterns when using the union tree data structure.
        //
        // as an example, let's consider the following code:
        // ```rust
        // for _ in union_find.items_eq_to(item_id) {
        //     for _ in union_find.items_eq_to(item_id) {
        //         ...
        //     }
        // }
        // ```
        // the first call to `items_eq_to` will first flatten all the descendents of the root item of `item_id`, and will then return
        // an iterator over them, which will immutably borrow the root item's children array.
        //
        // the second call will also try to flatten all the descendents of the root item item of `item_id`, but, they are already
        // flattened, so we don't really need to mutate the children array. if we were to take a mutable borrow to children anyway,
        // the above code will panic, because the iterator returned from the first `items_eq_to` call holds an immutable borrow to it.
        //
        // so, due to us only taking the mutable reference when it is actually needed, the above code is allowed.
        let children = self.children_of_item[item.index()].borrow();

        // generate an initial exploration queue made of our grand-children
        let mut exploration_queue = Vec::new();
        for &child in &*children {
            if child == item {
                // if the item is a root, it will be a child of itself, in which case, we don't want to explore its children once more,
                // we are already doing it.
                continue;
            }
            let mut grand_children = self.children_of_item[child.index()].borrow_mut();
            exploration_queue.append(&mut *grand_children);
        }

        if exploration_queue.is_empty() {
            // if there are no grand-children, we have nothing to do.
            //
            // NOTE: this early return is important since it entirely skips the mutable borrow of the children, which allows nested
            // iteration.
            return;
        }

        // collect new children which are currently not direct children of this item
        let mut new_children = Vec::new();
        while !exploration_queue.is_empty() {
            new_children.extend_from_slice(&exploration_queue);
            for child in std::mem::take(&mut exploration_queue) {
                let mut sub_children = self.children_of_item[child.index()].borrow_mut();
                exploration_queue.append(&mut *sub_children);
            }
        }

        // we expect to have at least one new child, since we previously already made sure that we have at least one grand-child.
        assert!(!new_children.is_empty());

        // make this item the parent of all new children
        for &child in &new_children {
            // make us the new parent of the child.
            //
            // don't try removing the child from its old parent's child list, since we emptied all child lists along the way.
            //
            // also, for now don't worry about adding the child to our children list, since we will later do it in a single batch
            // for all of the new children.
            self.parent_of_item[child.index()].replace(item);
        }

        // drop the borrow that we are currently holding to the item's children, since the logic below will append the new
        // children to it, which requires modifying it.
        drop(children);

        // add the new children to the children array.
        //
        // we need to reborrow children mutably since we previously only had an immutable borrow.
        let mut children = self.children_of_item[item.index()].borrow_mut();
        children.append(&mut new_children);
    }

    /// returns an iterator over all items equal to the given item, including the item itself
    pub fn items_eq_to(&self, item: UnionFindItemId) -> ItemsEqTo<'_> {
        let root = self.root_of_item(item);

        if root != item {
            self.flatten_descendents_of_item(root);
        }

        ItemsEqTo::new(self.children_of_item[root.index()].borrow())
    }

    /// checks if the given two items are equal.
    pub fn are_eq(&self, item_a: UnionFindItemId, item_b: UnionFindItemId) -> bool {
        if item_a == item_b {
            return true;
        }
        self.root_of_item(item_a) == self.root_of_item(item_b)
    }

    /// flattens the union find tree, causing future root lookups to be `O(1)`, until the next modification of the tree.
    pub fn flatten(&mut self) {
        for item in self.item_ids() {
            let root = self.root_of_item(item);
            self.set_parent_of_item(item, root);
        }
    }

    /// returns a reference to the values of all the items in the tree.
    pub fn items(&self) -> &[T] {
        &self.items
    }

    /// returns a mutable reference to the values of all the items in the tree.
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

/// an iterator over all items equal to some given item.
#[derive(Debug)]
pub struct ItemsEqTo<'a> {
    /// an immutable borrow of the list of items that are equal to the the chosen item.
    items: std::cell::Ref<'a, Vec<UnionFindItemId>>,

    /// returns the index of the next child to yield in the next iteration of this iterator.
    next_index: usize,
}
impl<'a> ItemsEqTo<'a> {
    /// creates a new iterator over items equal to some item. the provided argument should be a borrow to a list containing all of
    /// the items equal to the chosen item.
    fn new(items: std::cell::Ref<'a, Vec<UnionFindItemId>>) -> Self {
        Self {
            items,
            next_index: 0,
        }
    }
}
impl<'a> Iterator for ItemsEqTo<'a> {
    type Item = UnionFindItemId;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next_index >= self.items.len() {
            return None;
        }
        let res = self.items[self.next_index];
        self.next_index += 1;
        Some(res)
    }
}

#[cfg(test)]
mod tests {
    use assert_unordered::assert_eq_unordered;

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
                let items_eq_to_item_including_self = collect_to_vec(union_find.items_eq_to(item));
                assert_eq_unordered!(
                    group,
                    &items_eq_to_item_including_self,
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
                let items_eq_to_item_including_self = collect_to_vec(union_find.items_eq_to(item));

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

        dbg!(&union_find);

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
        let groups: &[&[UnionFindItemId]] = &[&[a, b, e, f, g], &[c, d]];
        chk_groups(&union_find, &all_items, groups);

        // at this point, some items can have different parents even though they are equal.
        // we can't check it since it might be by chance that all items in the group have the same parent.

        union_find.flatten();

        // now the equal items should have the same parent
        for &group in groups {
            for &item1 in group {
                for &item2 in group {
                    assert_eq!(
                        union_find.get_parent_of_item(item1),
                        union_find.get_parent_of_item(item2)
                    );
                }
            }
        }

        // but items that are not equal should still have different parents
        assert_ne!(
            union_find.get_parent_of_item(a),
            union_find.get_parent_of_item(c)
        );

        // make sure that flatenning didn't mess with the groups
        chk_groups(&union_find, &all_items, &[&[a, b, e, f, g], &[c, d]]);
    }

    /// a test which makes sure that we can nest union find operations that take a `&self` reference.
    ///
    /// such operations may be problematic due to the refcells that we use internally.
    ///
    /// this test makes sure that it works ok.
    #[test]
    fn test_operation_nesting() {
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
        let groups: &[&[UnionFindItemId]] = &[&[a, b, e, f, g], &[c, d]];
        chk_groups(&union_find, &all_items, groups);

        for item_eq_to_a in union_find.items_eq_to(a) {
            // perform "are equal" queries while iterating.
            for &x in groups[0] {
                assert!(union_find.are_eq(item_eq_to_a, x));
            }
            for &x in groups[1] {
                assert!(!union_find.are_eq(item_eq_to_a, x));
            }

            // now iterate over the same group once again, while still doing the previous iteration
            for item_eq_to_b in union_find.items_eq_to(b) {
                assert!(union_find.are_eq(item_eq_to_a, item_eq_to_b));
                for &x in groups[0] {
                    assert!(union_find.are_eq(item_eq_to_b, x));
                }
                for &x in groups[1] {
                    assert!(!union_find.are_eq(item_eq_to_b, x));
                }

                // now iterate over another group
                for item_eq_to_c in union_find.items_eq_to(c) {
                    assert!(!union_find.are_eq(item_eq_to_c, item_eq_to_a));
                    assert!(!union_find.are_eq(item_eq_to_c, item_eq_to_b));
                    for &x in groups[0] {
                        assert!(!union_find.are_eq(item_eq_to_c, x));
                    }
                    for &x in groups[1] {
                        assert!(union_find.are_eq(item_eq_to_c, x));
                    }

                    // iterate over the other group once more
                    for item_eq_to_d in union_find.items_eq_to(d) {
                        assert!(!union_find.are_eq(item_eq_to_d, item_eq_to_a));
                        assert!(!union_find.are_eq(item_eq_to_d, item_eq_to_b));
                        for &x in groups[0] {
                            assert!(!union_find.are_eq(item_eq_to_d, x));
                        }
                        for &x in groups[1] {
                            assert!(union_find.are_eq(item_eq_to_d, x));
                        }
                    }
                }
            }
        }
    }

    #[test]
    #[should_panic]
    fn test_union_node_while_iterating_over_it_not_allowed() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item(());
        let b = union_find.create_new_item(());
        let c = union_find.create_new_item(());
        let d = union_find.create_new_item(());
        let e = union_find.create_new_item(());
        let f = union_find.create_new_item(());
        let g = union_find.create_new_item(());

        union_find.union(a, b);
        union_find.union(c, d);
        union_find.union(e, f);
        union_find.union(e, g);

        union_find.union(b, g);

        for item_eq_to_a in union_find.items_eq_to(a) {
            union_find.union(item_eq_to_a, c);
        }
    }

    #[test]
    fn test_nop_union_node_while_iterating_over_it_is_allowed() {
        let mut union_find = UnionFind::new();

        let a = union_find.create_new_item(());
        let b = union_find.create_new_item(());
        let c = union_find.create_new_item(());
        let d = union_find.create_new_item(());
        let e = union_find.create_new_item(());
        let f = union_find.create_new_item(());
        let g = union_find.create_new_item(());

        union_find.union(a, b);
        union_find.union(c, d);
        union_find.union(e, f);
        union_find.union(e, g);

        union_find.union(b, g);

        for item_eq_to_a in union_find.items_eq_to(a) {
            // this union is redundant so it should be allowed
            union_find.union(item_eq_to_a, b);
        }
    }

    #[test]
    fn test_items_eq_to_doesnt_break_anything() {
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

        let groups: &[&[UnionFindItemId]] = &[&[a, b, e, f, g], &[c, d]];

        chk_groups(&union_find, &all_items, groups);

        assert_eq_unordered!(
            collect_to_vec(union_find.items_eq_to(e)).as_slice(),
            groups[0]
        );
        assert_eq_unordered!(
            collect_to_vec(union_find.items_eq_to(c)).as_slice(),
            groups[1]
        );
        assert_eq_unordered!(
            collect_to_vec(union_find.items_eq_to(a)).as_slice(),
            groups[0]
        );
        assert_eq_unordered!(
            collect_to_vec(union_find.items_eq_to(d)).as_slice(),
            groups[1]
        );

        chk_groups(&union_find, &all_items, groups);
    }
}
