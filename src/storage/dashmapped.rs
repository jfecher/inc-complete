use dashmap::DashMap;

use crate::{Cell, storage::StorageFor};
use std::hash::Hash;

use super::OutputType;

pub struct DashMapStorage<K: OutputType + Eq + Hash> {
    key_to_cell: DashMap<K, Cell>,
    cell_to_key: DashMap<Cell, (K, Option<K::Output>)>,
}

impl<K: OutputType + Eq + Hash> Default for DashMapStorage<K> {
    fn default() -> Self {
        Self {
            key_to_cell: Default::default(),
            cell_to_key: Default::default(),
        }
    }
}

impl<K> StorageFor<K> for DashMapStorage<K>
where
    K: Clone + Eq + Hash + OutputType,
    K::Output: Eq + Clone,
{
    fn get_cell_for_computation(&self, key: &K) -> Option<Cell> {
        self.key_to_cell.get(key).map(|value| *value)
    }

    fn insert_new_cell(&self, cell: Cell, key: K) {
        self.key_to_cell.insert(key.clone(), cell);
        self.cell_to_key.insert(cell, (key, None));
    }

    fn get_input(&self, cell: Cell) -> K {
        self.cell_to_key.get(&cell).unwrap().0.clone()
    }

    fn get_output(&self, cell: Cell) -> Option<K::Output> {
        self.cell_to_key.get(&cell).unwrap().1.clone()
    }

    fn update_output(&self, cell: Cell, new_value: K::Output) -> bool {
        let mut previous_output = self.cell_to_key.get_mut(&cell).unwrap();
        let changed = previous_output
            .1
            .as_ref()
            .is_none_or(|value| *value != new_value);
        previous_output.1 = Some(new_value);
        changed
    }
}

impl<K> serde::Serialize for DashMapStorage<K>
where
    K: serde::Serialize + OutputType + Eq + Hash,
    K::Output: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.cell_to_key.serialize(serializer)
    }
}

impl<'de, K> serde::Deserialize<'de> for DashMapStorage<K>
where
    K: serde::Deserialize<'de> + Hash + Eq + OutputType + Clone,
    K::Output: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let cell_to_key: DashMap<Cell, (K, Option<K::Output>)> =
            serde::Deserialize::deserialize(deserializer)?;

        let key_to_cell = cell_to_key
            .iter()
            .map(|entry| (entry.value().0.clone(), *entry.key()))
            .collect();
        Ok(DashMapStorage {
            cell_to_key,
            key_to_cell,
        })
    }
}
