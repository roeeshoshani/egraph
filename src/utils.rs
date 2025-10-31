use std::{cell::Cell, num::NonZeroUsize, ops::Deref};

use thiserror::Error;

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

/// indexes the given array twice mutable at the given two indexes, assuming that you know which index is higher than the other.
///
/// returns a tuple of the form `(low_element, high_element)`.
///
/// this function is just an internal helper for the nicer index mut twice utility.
fn index_mut_twice_low_high<T>(
    arr: &mut [T],
    low: usize,
    high: usize,
) -> (Option<&mut T>, Option<&mut T>) {
    if high >= arr.len() {
        // `high` is out of bounds, do a best effort to at least get `low`
        return (arr.get_mut(low), None);
    }

    let (start, end) = arr.split_at_mut_checked(high).unwrap();

    let low_elem = start.get_mut(low);
    let high_elem = end.get_mut(0);

    (low_elem, high_elem)
}

/// an error which occurs when trying to index an array twice mutably, while checking the validity of each index separately.
#[derive(Debug, Error, PartialEq, Eq, Clone, Copy)]
pub enum IndexMutTwiceChkEachErr {
    /// tried to mutably reference the same index twice
    #[error("tried to mutably reference the same index twice")]
    SameIndex,
}

/// indexes the given array twice mutably at the given two indexes, while checking the validity of each index separately.
///
/// panics if both indexes are equal.
pub fn index_mut_twice_chk_each<T>(
    arr: &mut [T],
    first_index: usize,
    second_index: usize,
) -> Result<(Option<&mut T>, Option<&mut T>), IndexMutTwiceChkEachErr> {
    if first_index < second_index {
        Ok(index_mut_twice_low_high(arr, first_index, second_index))
    } else if second_index < first_index {
        // in this case, we need to swap the order of the elements
        let (second_elem, first_elem) = index_mut_twice_low_high(arr, second_index, first_index);
        Ok((first_elem, second_elem))
    } else {
        // in this case the indexes are the same. this is not allowed, since you can't get 2 mutable refs to the same element.
        Err(IndexMutTwiceChkEachErr::SameIndex)
    }
}

/// an error which occurs when trying to index an array twice mutably.
#[derive(Debug, Error, PartialEq, Eq, Clone, Copy)]
pub enum IndexMutTwiceErr {
    /// tried to mutably reference the same index twice
    #[error("tried to mutably reference the same index twice")]
    SameIndex,

    /// the first index is invalid
    #[error("the first index ({0}) is invalid")]
    InvalidFirstIndex(usize),

    /// the second index is invalid
    #[error("the second index ({0}) is invalid")]
    InvalidSecondIndex(usize),
}
impl From<IndexMutTwiceChkEachErr> for IndexMutTwiceErr {
    fn from(value: IndexMutTwiceChkEachErr) -> Self {
        match value {
            IndexMutTwiceChkEachErr::SameIndex => Self::SameIndex,
        }
    }
}

/// indexes the given array twice mutably at the given two indexes.
///
/// panics if both indexes are equal.
pub fn index_mut_twice<T>(
    arr: &mut [T],
    first_index: usize,
    second_index: usize,
) -> Result<(&mut T, &mut T), IndexMutTwiceErr> {
    let (maybe_first, maybe_second) = index_mut_twice_chk_each(arr, first_index, second_index)?;

    let first = maybe_first.ok_or(IndexMutTwiceErr::InvalidFirstIndex(first_index))?;
    let second = maybe_second.ok_or(IndexMutTwiceErr::InvalidSecondIndex(second_index))?;

    Ok((first, second))
}
