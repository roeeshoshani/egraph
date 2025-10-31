use assert_unordered::assert_eq_unordered;

use egraph::union_find::*;

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
