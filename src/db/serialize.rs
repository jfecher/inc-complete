use std::sync::{
    Arc,
    atomic::{AtomicU32, Ordering},
};

use parking_lot::Mutex;
use serde::{Deserialize, Serialize, ser::SerializeStruct};

use crate::Cell;

use super::Db;

impl<Storage> serde::Serialize for Db<Storage>
where
    Storage: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut cells = Vec::with_capacity(self.cells.len());

        for item in self.cells.iter() {
            let value = item.value();
            let input_dependencies: Vec<_> = value.input_dependencies.iter().copied().collect();

            cells.push((
                *item.key(),
                CellDataDeserialize {
                    computation_id: value.computation_id,
                    last_updated_version: value.last_updated_version,
                    last_verified_version: value.last_verified_version,
                    dependencies: value.dependencies.clone(),
                    input_dependencies,
                },
            ));
        }

        let version = self.version.load(Ordering::SeqCst);
        let next_cell = self.next_cell.load(Ordering::SeqCst);

        let mut s = serializer.serialize_struct("Db", 4)?;
        s.serialize_field("version", &version)?;
        s.serialize_field("next_cell", &next_cell)?;
        s.serialize_field("cells", &cells)?;
        s.serialize_field("storage", &self.storage)?;
        s.end()
    }
}

#[derive(Deserialize)]
#[serde(rename = "Db")]
struct DbDeserialize<Storage> {
    version: u32,
    next_cell: u32,
    cells: Vec<(Cell, CellDataDeserialize)>,
    storage: Storage,
}

#[derive(Serialize, Deserialize)]
#[serde(rename = "CellData")]
struct CellDataDeserialize {
    computation_id: u32,
    last_updated_version: u32,
    last_verified_version: u32,
    dependencies: Vec<Cell>,
    input_dependencies: Vec<Cell>,
}

impl<'de, Storage> serde::Deserialize<'de> for Db<Storage>
where
    Storage: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let db = DbDeserialize::deserialize(deserializer)?;

        let cells = dashmap::DashMap::with_capacity(db.cells.len());

        for (cell, data) in db.cells {
            cells.insert(
                cell,
                crate::cell::CellData {
                    computation_id: data.computation_id,
                    last_updated_version: data.last_updated_version,
                    last_verified_version: data.last_verified_version,
                    dependencies: data.dependencies,
                    input_dependencies: data.input_dependencies.into_iter().collect(),
                    lock: Arc::new(Mutex::new(())),
                },
            );
        }

        Ok(Db {
            cells,
            version: AtomicU32::new(db.version),
            next_cell: AtomicU32::new(db.next_cell),
            storage: db.storage,
        })
    }
}
