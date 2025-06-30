use scc::HashMap;

use crate::{Cell, storage::StorageFor};
use std::hash::{BuildHasher, Hash};

use super::OutputType;

pub struct HashMapStorage<K, Hasher = rustc_hash::FxBuildHasher> where
    K: OutputType + Eq + Hash,
    Hasher: BuildHasher,
{
    key_to_cell: HashMap<K, Cell, Hasher>,
    cell_to_key: HashMap<Cell, (K, Option<K::Output>), Hasher>,
}

impl<K, H> Default for HashMapStorage<K, H> where
    K: OutputType + Eq + Hash,
    H: Default + BuildHasher,
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
    K: Clone + Eq + Hash + OutputType,
    K::Output: Eq + Clone,
    H: BuildHasher,
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

impl<K, H> serde::Serialize for HashMapStorage<K, H>
where
    K: serde::Serialize + OutputType + Eq + Hash + Clone,
    K::Output: serde::Serialize + Clone,
    H: BuildHasher,
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

impl<'de, K, H> serde::Deserialize<'de> for HashMapStorage<K, H>
where
    K: serde::Deserialize<'de> + Hash + Eq + OutputType + Clone,
    K::Output: serde::Deserialize<'de>,
    H: Default + BuildHasher,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let cell_to_key_vec: Vec<(Cell, (K, Option<K::Output>))> = serde::Deserialize::deserialize(deserializer)?;

        let key_to_cell = HashMap::default();
        let cell_to_key = HashMap::default();

        for (cell, (key, value)) in cell_to_key_vec {
            key_to_cell.insert(key.clone(), cell).ok();
            cell_to_key.insert(cell, (key, value)).ok();
        }

        Ok(HashMapStorage { cell_to_key, key_to_cell })
    }
}
