use serde::{Deserialize, ser::SerializeTuple};

use super::{OutputType, StorageFor};
use crate::Cell;

/// Helper to store a simple computation type which has no fields and thus
/// does not require a HashMap to cache each possible value.
///
/// Examples include `struct SourceFile;` or `struct Time;`
#[derive(Clone)]
pub struct SingletonStorage<K: OutputType> {
    cell: Option<Cell>,
    key: Option<K>,
    value: Option<K::Output>,
}

impl<K: OutputType> Default for SingletonStorage<K> {
    fn default() -> Self {
        Self {
            cell: None,
            value: None,
            key: None,
        }
    }
}

impl<K> StorageFor<K> for SingletonStorage<K>
where
    K: OutputType,
    K::Output: Eq,
{
    fn get_cell_for_computation(&self, _: &K) -> Option<Cell> {
        self.cell
    }

    fn insert_new_cell(&mut self, cell: Cell, key: K) {
        assert!(
            self.cell.is_none(),
            "Overwriting previous singleton value - are you using SingleStorage<{}> with a non-singleton type?",
            std::any::type_name::<K>()
        );
        self.cell = Some(cell);
        self.key = Some(key);
    }

    fn get_input(&self, _: Cell) -> &K {
        self.key.as_ref().unwrap()
    }

    fn get_output(&self, _: Cell) -> Option<&K::Output> {
        self.value.as_ref()
    }

    fn update_output(&mut self, _: Cell, new_value: K::Output) -> bool {
        let changed = self.value.as_ref().is_none_or(|value| *value != new_value);
        self.value = Some(new_value);
        changed
    }
}

impl<K> serde::Serialize for SingletonStorage<K>
where
    K: serde::Serialize + OutputType,
    K::Output: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut s = serializer.serialize_tuple(3)?;
        s.serialize_element(&self.cell)?;
        s.serialize_element(&self.key)?;
        s.serialize_element(&self.value)?;
        s.end()
    }
}

impl<'de, K> serde::Deserialize<'de> for SingletonStorage<K>
where
    K: serde::Deserialize<'de> + OutputType,
    K::Output: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let (cell, key, value) = Deserialize::deserialize(deserializer)?;
        Ok(SingletonStorage { cell, key, value })
    }
}
