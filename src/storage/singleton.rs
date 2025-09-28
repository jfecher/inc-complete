use serde::{Deserialize, ser::SerializeStruct};

use super::{Computation, StorageFor};
use crate::Cell;

/// Helper to store a simple computation type which has no fields and thus
/// does not require a map to cache each possible value.
///
/// Examples include `struct SourceFile;` or `struct Time;`
pub struct SingletonStorage<K: Computation> {
    cell: std::sync::OnceLock<Cell>,
    key: std::sync::OnceLock<K>,
    value: std::sync::Mutex<Option<K::Output>>,
}

impl<K: Computation> Default for SingletonStorage<K> {
    fn default() -> Self {
        Self {
            cell: Default::default(),
            value: Default::default(),
            key: Default::default(),
        }
    }
}

impl<K> StorageFor<K> for SingletonStorage<K>
where
    K: Computation + Clone,
    K::Output: Eq + Clone,
{
    fn get_cell_for_computation(&self, _: &K) -> Option<Cell> {
        self.cell.get().copied()
    }

    fn insert_new_cell(&self, cell: Cell, key: K) {
        assert!(
            self.cell.set(cell).is_ok(),
            "Overwriting previous singleton value - are you using SingleStorage<{}> with a non-singleton type?",
            std::any::type_name::<K>()
        );
        self.key
            .set(key)
            .unwrap_or_else(|_| panic!("insert_new_cell: cell already initialized"));
    }

    fn try_get_input(&self, _: Cell) -> Option<K> {
        self.key.get().cloned()
    }

    fn get_output(&self, _: Cell) -> Option<K::Output> {
        self.value.lock().unwrap().clone()
    }

    fn update_output(&self, _: Cell, new_value: K::Output) -> bool {
        let mut guard = self.value.lock().unwrap();
        let changed = K::ASSUME_CHANGED || guard.as_ref().is_none_or(|value| *value != new_value);
        *guard = Some(new_value);
        changed
    }

    fn gc(&mut self, used_cells: &std::collections::HashSet<Cell>) {
        if let Some(this_cell) = self.cell.get() {
            if !used_cells.contains(this_cell) {
                if let Ok(val) = self.value.get_mut() {
                    *val = None;
                }
            }
        }
    }
}

impl<K> serde::Serialize for SingletonStorage<K>
where
    K: serde::Serialize + Computation,
    K::Output: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut s = serializer.serialize_struct("SingletonStorage", 3)?;
        if let Some(cell) = self.cell.get() {
            s.serialize_field("cell", cell)?;
        }
        if let Some(key) = self.key.get() {
            s.serialize_field("key", key)?;
        }
        if let Ok(lock) = self.value.lock() {
            if let Some(value) = &*lock {
                s.serialize_field("value", value)?;
            }
        }
        s.end()
    }
}

impl<'de, K> serde::Deserialize<'de> for SingletonStorage<K>
where
    K: serde::Deserialize<'de> + Computation,
    K::Output: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wrapper: SerializeWrapper<K> = Deserialize::deserialize(deserializer)?;
        Ok(wrapper.into_storage())
    }
}

#[derive(Deserialize)]
struct SerializeWrapper<K: Computation> {
    #[serde(default)]
    cell: Option<Cell>,

    // Serde complains we need a `K: Default` without this, but that shouldn't be necessary.
    #[serde(default = "none")]
    key: Option<K>,

    #[serde(default)]
    #[serde(bound = "K::Output: Deserialize<'de>")]
    value: Option<K::Output>,
}

fn none<T>() -> Option<T> {
    None
}

impl<K: Computation> SerializeWrapper<K> {
    fn into_storage(self) -> SingletonStorage<K> {
        let cell = match self.cell {
            Some(cell) => std::sync::OnceLock::from(cell),
            None => std::sync::OnceLock::new(),
        };
        let key = match self.key {
            Some(key) => std::sync::OnceLock::from(key),
            None => std::sync::OnceLock::new(),
        };
        let value = std::sync::Mutex::new(self.value);
        SingletonStorage { cell, key, value }
    }
}
