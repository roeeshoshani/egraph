use std::{cell::Cell, num::NonZeroUsize, ops::Deref};

/// creates an array vec containing the arguments.
#[macro_export]
macro_rules! array_vec {
    () => (
        ::arrayvec::ArrayVec::new()
    );
    ($($x:expr),+ $(,)?) => (
        {
            let mut result = ::arrayvec::ArrayVec::new();
            $(
                result.push($x);
            )+
            result
        }
    );
}

pub enum CowBox<'a, T: ?Sized> {
    Borrowed(&'a T),
    Owned(Box<T>),
}
impl<'a, T: ?Sized> Deref for CowBox<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            CowBox::Borrowed(borrowed) => borrowed,
            CowBox::Owned(owned) => owned,
        }
    }
}
impl<'a, T: ?Sized> AsRef<T> for CowBox<'a, T> {
    fn as_ref(&self) -> &T {
        match self {
            CowBox::Borrowed(borrowed) => borrowed,
            CowBox::Owned(owned) => owned,
        }
    }
}

/// a non zero usize allocator. allocates incrementing non-zero usize values.
pub struct NonZeroUsizeAllocator {
    next: Cell<NonZeroUsize>,
}
impl NonZeroUsizeAllocator {
    /// creates a new non zero usize allocator which starts from the initial value of 1.
    pub fn new() -> Self {
        Self {
            next: Cell::new(unsafe { NonZeroUsize::new_unchecked(1) }),
        }
    }

    /// allocates the next non zero usize value.
    ///
    /// this allows allocating even when using a `&self` and not a `&mut self` by using a cell. this allows greater
    /// flexibility when using this data structure.
    pub fn alloc(&self) -> NonZeroUsize {
        let res = self.next.get();
        self.next.set(res.checked_add(1).unwrap());
        res
    }
}
