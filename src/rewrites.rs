use crate::{did_anything::DidAnything, egraph::EGraph, rewrite_rules::Rewrite};

pub trait Rewrites: Sized {
    const LEN: usize;

    fn apply_rewrite(&self, rewrite_index: usize, egraph: &mut EGraph) -> DidAnything;

    fn push<R: Rewrite>(self, rewrite: R) -> (R, Self) {
        (rewrite, self)
    }

    fn len(&self) -> usize {
        Self::LEN
    }
}

impl Rewrites for () {
    const LEN: usize = 1;

    fn apply_rewrite(&self, _rewrite_index: usize, _egraph: &mut EGraph) -> DidAnything {
        unreachable!()
    }
}
impl<R: Rewrite> Rewrites for R {
    const LEN: usize = 1;

    fn apply_rewrite(&self, rewrite_index: usize, egraph: &mut EGraph) -> DidAnything {
        assert_eq!(rewrite_index, 0);
        egraph.apply_rewrite(self)
    }
}

impl<A: Rewrite, B: Rewrites> Rewrites for (A, B) {
    const LEN: usize = 1 + B::LEN;

    fn apply_rewrite(&self, rewrite_index: usize, egraph: &mut EGraph) -> DidAnything {
        assert!(rewrite_index <= Self::LEN);
        if rewrite_index + 1 == Self::LEN {
            // apply the current rule
            egraph.apply_rewrite(&self.0)
        } else {
            // apply some inner rule, recurse
            self.1.apply_rewrite(rewrite_index, egraph)
        }
    }
}

#[macro_export]
macro_rules! rewrites_arr {
    () => { () };
    (...$rest:expr) => { $rest };
    ($a:expr) => { $crate::rewrites_arr![$a,] };
    ($a:expr, $($tok:tt)*) => {
        ($a, $crate::rewrites_arr![$($tok)*])
    };
}
