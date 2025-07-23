use scc::{TreeIndex, ebr::Guard};

use crate::{Cell, storage::StorageFor};

use super::OutputType;

/// TreeIndexStorage is backed internally by a `scc::TreeIndex`,
/// a type optimized for read-heavy workloads. See [scc's documentation](https://docs.rs/scc/2.3.4/scc/#treeindex)
/// for performance details.
pub struct TreeIndexStorage<K: OutputType> {
    key_to_cell: TreeIndex<K, Cell>,
    cell_to_key: TreeIndex<Cell, (K, Option<K::Output>)>,
}

impl<K: OutputType> Default for TreeIndexStorage<K> {
    fn default() -> Self {
        Self {
            key_to_cell: Default::default(),
            cell_to_key: Default::default(),
        }
    }
}

impl<K> StorageFor<K> for TreeIndexStorage<K>
where
    K: Clone + Ord + OutputType + 'static,
    K::Output: Clone + Eq,
{
    fn get_cell_for_computation(&self, key: &K) -> Option<Cell> {
        self.key_to_cell.peek_with(key, |_, v| *v)
    }

    fn insert_new_cell(&self, cell: Cell, key: K) {
        self.key_to_cell.insert(key.clone(), cell).ok();
        self.cell_to_key.insert(cell, (key, None)).ok();
    }

    fn get_input(&self, cell: Cell) -> K {
        self.cell_to_key
            .peek_with(&cell, |_, (k, _)| k.clone())
            .unwrap()
    }

    fn get_output(&self, cell: Cell) -> Option<K::Output> {
        self.cell_to_key
            .peek_with(&cell, |_, (_, v)| v.clone())
            .unwrap()
    }

    fn update_output(&self, cell: Cell, new_value: K::Output) -> bool {
        let changed = self
            .cell_to_key
            .peek_with(&cell, |_, old_value| {
                old_value.1.as_ref().is_none_or(|value| *value != new_value)
            })
            .unwrap();

        // TreeIndex is read-optimized so this write will be slow
        if changed {
            let key = self
                .cell_to_key
                .peek_with(&cell, |_, (k, _)| k.clone())
                .unwrap();
            self.cell_to_key.remove(&cell);
            self.cell_to_key.insert(cell, (key, Some(new_value))).ok();
        }

        changed
    }

    fn gc(&mut self, used_cells: &std::collections::HashSet<Cell>) {
        let guard = Guard::new();
        for cell in self.cell_to_key.iter(&guard) {
            if !used_cells.contains(cell.0) {
                self.cell_to_key.remove(cell.0);
            }
        }
        for cell in self.key_to_cell.iter(&guard) {
            if !used_cells.contains(cell.1) {
                self.key_to_cell.remove(cell.0);
            }
        }
    }
}

impl<K> serde::Serialize for TreeIndexStorage<K>
where
    K: serde::Serialize + OutputType + Eq + Clone + 'static,
    K::Output: serde::Serialize + Clone + 'static,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut cell_to_key_vec: Vec<(Cell, (K, Option<K::Output>))> =
            Vec::with_capacity(self.cell_to_key.len());

        let guard = Guard::new();
        for (cell, (key, value)) in self.cell_to_key.iter(&guard) {
            cell_to_key_vec.push((*cell, (key.clone(), value.clone())));
        }

        cell_to_key_vec.serialize(serializer)
    }
}

impl<'de, K> serde::Deserialize<'de> for TreeIndexStorage<K>
where
    K: serde::Deserialize<'de> + Ord + OutputType + Clone + 'static,
    K::Output: serde::Deserialize<'de> + Clone,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let cell_to_key_vec: Vec<(Cell, (K, Option<K::Output>))> =
            serde::Deserialize::deserialize(deserializer)?;

        let key_to_cell = TreeIndex::new();
        let cell_to_key = TreeIndex::new();

        for (cell, (key, value)) in cell_to_key_vec {
            key_to_cell.insert(key.clone(), cell).ok();
            cell_to_key.insert(cell, (key, value)).ok();
        }

        Ok(TreeIndexStorage {
            cell_to_key,
            key_to_cell,
        })
    }
}
