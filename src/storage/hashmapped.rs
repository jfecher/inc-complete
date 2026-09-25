use dashmap::{DashMap, mapref::entry::Entry};
use serde::ser::SerializeSeq;

use crate::{Cell, storage::StorageFor};
use std::hash::{BuildHasher, Hash};

use super::Computation;

pub struct HashMapStorage<K, Hasher = rustc_hash::FxBuildHasher>
where
    K: Computation + Eq + Hash,
    Hasher: BuildHasher,
{
    key_to_cell: DashMap<K, Cell, Hasher>,
    cell_to_key: DashMap<Cell, (K, Option<K::Output>), Hasher>,
}

impl<K, H> Default for HashMapStorage<K, H>
where
    K: Computation + Eq + Hash,
    H: Default + BuildHasher + Clone,
{
    fn default() -> Self {
        Self {
            key_to_cell: Default::default(),
            cell_to_key: Default::default(),
        }
    }
}

impl<K, H> StorageFor<K> for HashMapStorage<K, H>
where
    K: Clone + Eq + Hash + Computation,
    K::Output: Eq + Clone,
    H: BuildHasher + Clone,
{
    fn get_cell_for_computation(&self, key: &K) -> Option<Cell> {
        self.key_to_cell.get(key).map(|value| *value)
    }

    fn insert_new_cell(&self, cell: Cell, key: K) {
        // key_to_cell must be written last to avoid data races
        self.cell_to_key.insert(cell, (key.clone(), None));
        self.key_to_cell.insert(key, cell);
    }

    fn get_or_insert_cell(&self, key: K, new_cell: impl FnOnce() -> Cell) -> Cell {
        if let Some(cell) = self.get_cell_for_computation(&key) {
            return cell;
        }

        // The entry holds this key's shard lock so only one thread can create its cell
        match self.key_to_cell.entry(key) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let cell = new_cell();
                self.cell_to_key.insert(cell, (entry.key().clone(), None));
                entry.insert(cell);
                cell
            }
        }
    }

    fn try_get_input(&self, cell: Cell) -> Option<K> {
        let key_ref = self.cell_to_key.get(&cell)?;
        Some(key_ref.0.clone())
    }

    fn get_input(&self, cell: Cell) -> K {
        self.cell_to_key.get(&cell).unwrap().0.clone()
    }

    fn get_output(&self, cell: Cell) -> Option<K::Output> {
        self.cell_to_key.get(&cell).unwrap().1.clone()
    }

    fn update_output(&self, cell: Cell, new_value: K::Output) -> bool {
        let mut previous_output = self.cell_to_key.get_mut(&cell).unwrap();
        let changed = K::ASSUME_CHANGED
            || previous_output
                .1
                .as_ref()
                .is_none_or(|value| *value != new_value);
        previous_output.1 = Some(new_value);
        changed
    }

    fn gc(&mut self, used_cells: &std::collections::HashSet<Cell>) {
        // Remove cells that are not in the used set
        self.cell_to_key.retain(|cell, _| used_cells.contains(cell));
        self.key_to_cell.retain(|_, cell| used_cells.contains(cell));
    }
}

impl<K, H> serde::Serialize for HashMapStorage<K, H>
where
    K: serde::Serialize + Computation + Eq + Hash + Clone,
    K::Output: serde::Serialize + Clone,
    H: BuildHasher + Clone,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.cell_to_key.len()))?;
        for kv in self.cell_to_key.iter() {
            seq.serialize_element(&(kv.key(), kv.value()))?;
        }
        seq.end()
    }
}

impl<'de, K, H> serde::Deserialize<'de> for HashMapStorage<K, H>
where
    K: serde::Deserialize<'de> + Hash + Eq + Computation + Clone,
    K::Output: serde::Deserialize<'de>,
    H: Default + BuildHasher + Clone,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let cell_to_key_vec: Vec<(Cell, (K, Option<K::Output>))> =
            serde::Deserialize::deserialize(deserializer)?;

        let key_to_cell = DashMap::default();
        let cell_to_key = DashMap::default();

        for (cell, (key, value)) in cell_to_key_vec {
            key_to_cell.insert(key.clone(), cell);
            cell_to_key.insert(cell, (key, value));
        }

        Ok(HashMapStorage {
            cell_to_key,
            key_to_cell,
        })
    }
}
