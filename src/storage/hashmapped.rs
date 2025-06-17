use std::collections::HashMap;

use crate::{Cell, storage::StorageFor};
use std::hash::Hash;

use super::OutputType;

#[derive(Clone)]
pub struct HashMapStorage<K: OutputType> {
    key_to_cell: HashMap<K, Cell>,
    cell_to_key: HashMap<Cell, (K, Option<K::Output>)>,
}

impl<K: OutputType> Default for HashMapStorage<K> {
    fn default() -> Self {
        Self {
            key_to_cell: Default::default(),
            cell_to_key: Default::default(),
        }
    }
}

impl<K> StorageFor<K> for HashMapStorage<K>
where
    K: Clone + Eq + Hash + OutputType,
    K::Output: Eq,
{
    fn get_cell_for_computation(&self, key: &K) -> Option<Cell> {
        self.key_to_cell.get(key).copied()
    }

    fn insert_new_cell(&mut self, cell: Cell, key: K) {
        self.key_to_cell.insert(key.clone(), cell);
        self.cell_to_key.insert(cell, (key, None));
    }

    fn get_input(&self, cell: Cell) -> &K {
        &self.cell_to_key[&cell].0
    }

    fn get_output(&self, cell: Cell) -> Option<&K::Output> {
        self.cell_to_key[&cell].1.as_ref()
    }

    fn update_output(&mut self, cell: Cell, new_value: K::Output) -> bool {
        let previous_output = &mut self.cell_to_key.get_mut(&cell).unwrap().1;
        let changed = previous_output
            .as_ref()
            .is_none_or(|value| *value != new_value);
        *previous_output = Some(new_value);
        changed
    }
}

impl<K> serde::Serialize for HashMapStorage<K>
where
    K: serde::Serialize + OutputType,
    K::Output: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.cell_to_key.serialize(serializer)
    }
}

impl<'de, K> serde::Deserialize<'de> for HashMapStorage<K>
where
    K: serde::Deserialize<'de> + Hash + Eq + OutputType + Clone,
    K::Output: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let cell_to_key: HashMap<Cell, (K, Option<K::Output>)> =
            serde::Deserialize::deserialize(deserializer)?;
        let key_to_cell = cell_to_key
            .iter()
            .map(|(cell, (key, _))| (key.clone(), *cell))
            .collect();
        Ok(HashMapStorage {
            cell_to_key,
            key_to_cell,
        })
    }
}
