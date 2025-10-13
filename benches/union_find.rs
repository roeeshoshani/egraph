use criterion::{BenchmarkId, Criterion, black_box, criterion_group, criterion_main};
use egraph::union_find::*;
use rand::seq::IndexedRandom;

const TREE_SIZE_OPTIONS: &[usize] = &[1_000, 4_000];
const NUM_UNION_OPS_OPTIONS: &[usize] = &[7_000, 20_000];

struct Pair {
    a: UnionFindItemId,
    b: UnionFindItemId,
}

struct Setup {
    tree: UnionFind<usize>,
    item_ids: Vec<UnionFindItemId>,
}
impl Setup {
    fn new(tree_size: usize) -> Self {
        // generate the tree
        let mut tree: UnionFind<usize> = UnionFind::new();
        for i in 0..tree_size {
            let _ = tree.create_new_item(black_box(i));
        }

        // collect all item ids
        let item_ids: Vec<_> = tree.item_ids().collect();

        Self { tree, item_ids }
    }
    fn gen_pairs(&self, num_pairs: usize) -> Vec<Pair> {
        let mut rng = rand::rng();
        (0..num_pairs)
            .map(|_| {
                let [id1, id2] = self
                    .item_ids
                    .choose_multiple_array::<_, 2>(&mut rng)
                    .unwrap();
                Pair { a: id1, b: id2 }
            })
            .collect()
    }
    fn gen_tree_with_unions(&self, num_union_ops: usize) -> UnionFind<usize> {
        let mut tree = self.tree.clone();
        let pairs = self.gen_pairs(num_union_ops);
        for pair in &pairs {
            tree.union(pair.a, pair.b);
        }
        tree
    }
}

struct TestId {
    size: usize,
    num_union_ops: usize,
}
impl std::fmt::Display for TestId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "size={},num_union_ops={}", self.size, self.num_union_ops)
    }
}

fn test_union(c: &mut Criterion) {
    let mut group = c.benchmark_group("union");

    for &size in TREE_SIZE_OPTIONS {
        for &num_ops in NUM_UNION_OPS_OPTIONS {
            let setup = Setup::new(size);
            let pairs = setup.gen_pairs(num_ops);

            group.bench_with_input(
                BenchmarkId::from_parameter(TestId {
                    size,
                    num_union_ops: num_ops,
                }),
                &(&setup, &pairs),
                |b, &(setup, pairs)| {
                    b.iter(|| {
                        let mut tree = setup.tree.clone();
                        for pair in pairs {
                            tree.union(black_box(pair.a), black_box(pair.b));
                        }
                    })
                },
            );
        }
    }
    group.finish();
}

fn test_are_eq(c: &mut Criterion) {
    let mut group = c.benchmark_group("are_eq");

    for &size in TREE_SIZE_OPTIONS {
        for &num_union_ops in NUM_UNION_OPS_OPTIONS {
            let setup = Setup::new(size);
            let tree = setup.gen_tree_with_unions(num_union_ops);

            group.bench_with_input(
                BenchmarkId::from_parameter(TestId {
                    size,
                    num_union_ops,
                }),
                &(&tree, &setup.item_ids),
                |b, &(tree, item_ids)| {
                    b.iter(|| {
                        for &item_id_1 in item_ids {
                            for &item_id_2 in item_ids {
                                black_box(tree.are_eq(black_box(item_id_1), black_box(item_id_2)));
                            }
                        }
                    })
                },
            );
        }
    }
    group.finish();
}

fn test_items_eq_to(c: &mut Criterion) {
    let mut group = c.benchmark_group("items_eq_to");

    for &size in TREE_SIZE_OPTIONS {
        for &num_union_ops in NUM_UNION_OPS_OPTIONS {
            let setup = Setup::new(size);
            let base_tree = setup.gen_tree_with_unions(num_union_ops);

            group.bench_with_input(
                BenchmarkId::from_parameter(TestId {
                    size,
                    num_union_ops,
                }),
                &(&setup, &base_tree),
                |b, &(setup, base_tree)| {
                    b.iter(|| {
                        let tree = base_tree;
                        for item_id in &setup.item_ids {
                            black_box(tree.items_eq_to(black_box(*item_id)).count());
                        }
                    })
                },
            );
        }
    }
    group.finish();
}

criterion_group!(benches, test_union, test_are_eq, test_items_eq_to);
criterion_main!(benches);
