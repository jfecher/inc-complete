use scc::HashMap;

use crate::{Cell, storage::StorageFor};
use std::hash::Hash;

use super::OutputType;

pub struct HashMapStorage<K: OutputType + Eq + Hash> {
    key_to_cell: HashMap<K, Cell>,
    cell_to_key: HashMap<Cell, (K, Option<K::Output>)>,
}

impl<K: OutputType + Eq + Hash> Default for HashMapStorage<K> {
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
    K::Output: Eq + Clone,
{
    fn get_cell_for_computation(&self, key: &K) -> Option<Cell> {
        self.key_to_cell.get(key).map(|value| *value)
    }

    fn insert_new_cell(&self, cell: Cell, key: K) {
        self.key_to_cell.insert(key.clone(), cell).ok();
        self.cell_to_key.insert(cell, (key, None)).ok();
    }

    fn get_input(&self, cell: Cell) -> K {
        self.cell_to_key.get(&cell).unwrap().0.clone()
    }

    fn get_output(&self, cell: Cell) -> Option<K::Output> {
        self.cell_to_key.get(&cell).unwrap().1.clone()
    }

    fn update_output(&self, cell: Cell, new_value: K::Output) -> bool {
        let mut previous_output = self.cell_to_key.get(&cell).unwrap();
        let changed = previous_output
            .1
            .as_ref()
            .is_none_or(|value| *value != new_value);
        previous_output.1 = Some(new_value);
        changed
    }
}

impl<K> serde::Serialize for HashMapStorage<K>
where
    K: serde::Serialize + OutputType + Eq + Hash + Clone,
    K::Output: serde::Serialize + Clone,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut cell_to_key_vec: Vec<(Cell, (K, Option<K::Output>))> = Vec::with_capacity(self.cell_to_key.len());

        self.cell_to_key.scan(|cell, (key, value)| {
            cell_to_key_vec.push((*cell, (key.clone(), value.clone())));
        });

        cell_to_key_vec.serialize(serializer)
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
        let cell_to_key_vec: Vec<(Cell, (K, Option<K::Output>))> = serde::Deserialize::deserialize(deserializer)?;

        let key_to_cell = HashMap::new();
        let cell_to_key = HashMap::new();

        for (cell, (key, value)) in cell_to_key_vec {
            key_to_cell.insert(key.clone(), cell).ok();
            cell_to_key.insert(cell, (key, value)).ok();
        }

        Ok(HashMapStorage { cell_to_key, key_to_cell })
    }
}
