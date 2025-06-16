use crate::Cell;
use std::{hash::Hash, marker::PhantomData};
use super::{OutputType, StorageFor};

/// Helper to store a simple computation type which has no fields and thus
/// does not require a HashMap to cache each possible value.
///
/// Examples include `struct SourceFile;` or `struct Time;`
pub type SingletonStorage<K, V> = (Option<Cell>, Option<V>, PhantomData<K>);

impl<K> StorageFor<K> for SingletonStorage<K, K::Output> where
    K: Clone + Eq + Hash + OutputType,
    K::Output: Eq
{
    fn get_cell_for_computation(&self, _: &K) -> Option<Cell> {
        self.0
    }

    fn insert_new_cell(&mut self, cell: Cell, _: K) {
        assert!(self.0.is_none(), "Overwriting previous singleton value - are you using SingleStorage<{}> with a non-singleton type?", std::any::type_name::<K>());
        self.0 = Some(cell);
    }

    fn get_output(&self, _: Cell) -> Option<&K::Output> {
        self.1.as_ref()
    }

    fn update_output(&mut self, _: Cell, new_value: K::Output) -> bool {
        let changed = self.1.as_ref().is_none_or(|value| *value != new_value);
        self.1 = Some(new_value);
        changed
    }
}
