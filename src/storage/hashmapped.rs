use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{storage::StorageFor, Cell};
use std::hash::Hash;

use super::OutputType;

pub type HashMapStorage<K, Output> = (
    HashMapSerializeWrapper<K, Cell>,
    HashMapSerializeWrapper<Cell, (K, Option<Output>)>,
);

#[derive(Debug, Clone)]
pub struct HashMapSerializeWrapper<K, V>(HashMap<K, V>);

impl<K, V> Default for HashMapSerializeWrapper<K, V> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<K: Clone + Serialize, V: Clone + Serialize> Serialize for HashMapSerializeWrapper<K, V> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer
    {
        let vec: Vec<(K, V)> = self.0.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
        vec.serialize(serializer)
    }
}

impl<'de, K, V> Deserialize<'de> for HashMapSerializeWrapper<K, V> where
    K: Clone + Deserialize<'de> + Eq + Hash,
    V: Clone + Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>
    {
        let vec: Vec<(K, V)> = Vec::deserialize(deserializer)?;
        Ok(Self(vec.into_iter().collect()))
    }
}

impl<K> StorageFor<K> for HashMapStorage<K, K::Output> where
    K: Clone + Eq + Hash + OutputType,
    K::Output: Eq
{
    fn get_cell_for_computation(&self, key: &K) -> Option<Cell> {
        self.0.0.get(key).copied()
    }

    fn insert_new_cell(&mut self, cell: Cell, key: K) {
        self.0.0.insert(key.clone(), cell);
        self.1.0.insert(cell, (key, None));
    }

    fn get_output(&self, cell: Cell) -> Option<&K::Output> {
        self.1.0[&cell].1.as_ref()
    }

    fn update_output(&mut self, cell: Cell, new_value: K::Output) -> bool {
        let previous_output = &mut self.1.0.get_mut(&cell).unwrap().1;
        let changed = previous_output.as_ref().is_none_or(|value| *value != new_value);
        *previous_output = Some(new_value);
        changed
    }
}
