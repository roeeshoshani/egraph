use std::{cell::Cell, num::NonZeroUsize};

use thiserror::Error;

/// a mapping between ids to their parent ids.
///
/// this data structure allows allocating ids, and getting and setting their parent ids.
#[derive(Debug, Clone)]
pub struct IdToParentMap {
    next_id: NonZeroUsize,
    parent_of_id: Vec<Cell<Option<NonZeroUsize>>>,
}
impl IdToParentMap {
    /// creates a new empty mapping.
    pub fn new() -> Self {
        Self {
            next_id: NonZeroUsize::new(1).unwrap(),
            parent_of_id: Vec::new(),
        }
    }

    /// converts an item id to its index in an array of items.
    fn id_to_index(id: NonZeroUsize) -> usize {
        id.get() - 1
    }

    /// allocates a new item id. initially, the parent of the item is itself.
    #[must_use]
    pub fn alloc_id(&mut self) -> NonZeroUsize {
        let res = self.next_id;
        self.next_id = self.next_id.checked_add(1).unwrap();
        res
    }

    /// returns the optional parent of the item with the given id. the parent can be none for 2 possible reasons:
    /// 1. the lazily allocated vector is too small to contain a slot for this item.
    /// 2. the slot allocated for this item contains none.
    /// both cases are treated equally, as if the item has no parent, in which case it is considered that it is the parent of itself.
    fn get_opt_parent_of(&self, id: NonZeroUsize) -> Option<NonZeroUsize> {
        self.parent_of_id.get(Self::id_to_index(id))?.get()
    }

    /// returns the parent of the given item.
    pub fn get_parent_of(&self, id: NonZeroUsize) -> NonZeroUsize {
        // if the node doesn't have a parent, it is considered to be the parent of itself
        self.get_opt_parent_of(id).unwrap_or(id)
    }

    /// sets the parent of the given item.
    pub fn set_parent(&mut self, id: NonZeroUsize, new_parent: NonZeroUsize) {
        let index = Self::id_to_index(id);

        // the array is lazily extended. so, it is possible that an id exists even though the array is too small to hold its index.
        // in that case, resize the array.
        if !(index < self.parent_of_id.len()) {
            if id == new_parent {
                // in this case, the parent is already none, which means that the item already points to itself, so we don't need
                // to do anything.
                return;
            }
            // resize the array such that it can hold the currently highest id, which is one less than than the next id.
            // note that the ids are 1-based and not 0-based, so we don't need an extra ` - 1` here, only one ` - 1` to get from the
            // next id to the currently highest id.
            self.parent_of_id
                .resize(self.next_id.get() - 1, Cell::new(None));
        }

        self.parent_of_id[index].set(Some(new_parent));
    }

    /// tries to set the parent of the item with the given id.
    ///
    /// this function has the benefit of not requiring a mutable ref to self while still allowing modification.
    ///
    /// this may fail if the parent of this node hasn't been set before, due to the lazily allocated array of parent ids.
    pub fn try_set_parent(
        &self,
        id: NonZeroUsize,
        new_parent: NonZeroUsize,
    ) -> Result<(), TrySetParentOfItemErr> {
        let index = Self::id_to_index(id);

        if index < self.parent_of_id.len() {
            // the index fits in the array, just set it.
            self.parent_of_id[index].set(Some(new_parent));
            Ok(())
        } else {
            // in this case, the index doesn't fit in the array.
            if id == new_parent {
                // in this case, the item already points to itself, so there's nothing for us to do here
                return Ok(());
            }

            // the item was requested to point to some parent, but the array is currently not large enough to set the parent of the
            // item, and we're not allowed to modify the array in this function, due to it taking `&self` and not `&mut self`.
            //
            // so we, can't do it, return an error.
            Err(TrySetParentOfItemErr { id, new_parent })
        }
    }

    /// returns all of the allocated item ids.
    pub fn item_ids(&self) -> std::ops::Range<usize> {
        1..self.next_id.get()
    }
}

/// an error while trying to set the parent of an item without re-allocating the vector.
#[derive(Debug, Error)]
#[error("failed to set parent of item with id {id:?} to {new_parent:?}")]
pub struct TrySetParentOfItemErr {
    pub id: NonZeroUsize,
    pub new_parent: NonZeroUsize,
}
