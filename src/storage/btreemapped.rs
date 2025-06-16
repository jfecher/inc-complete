use std::collections::BTreeMap;

use crate::{storage::StorageFor, Cell};

use super::OutputType;

pub type BTreeMapStorage<K, Output> = (
    BTreeMap<K, Cell>,
    BTreeMap<Cell, (K, Option<Output>)>,
);

impl<K> StorageFor<K> for BTreeMapStorage<K, K::Output> where
    K: Clone + Ord + OutputType,
    K::Output: Eq
{
    fn get_cell_for_computation(&self, key: &K) -> Option<Cell> {
        self.0.get(key).copied()
    }

    fn insert_new_cell(&mut self, cell: Cell, key: K) {
        self.0.insert(key.clone(), cell);
        self.1.insert(cell, (key, None));
    }

    fn get_output(&self, cell: Cell) -> Option<&K::Output> {
        self.1[&cell].1.as_ref()
    }

    fn update_output(&mut self, cell: Cell, new_value: K::Output) -> bool {
        let previous_output = &mut self.1.get_mut(&cell).unwrap().1;
        let changed = previous_output.as_ref().is_none_or(|value| *value != new_value);
        *previous_output = Some(new_value);
        changed
    }
}
