use std::sync::atomic::{AtomicU32, Ordering};

use serde::{ser::SerializeStruct, Deserialize};

use crate::{cell::CellData, Cell};

use super::Db;

impl<Storage> serde::Serialize for Db<Storage> where
    Storage: serde::Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer
    {
        let mut cells = Vec::with_capacity(self.cells.len());

        self.cells.scan(|key, value| {
            cells.push((key.clone(), value.clone()));
        });

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
    cells: Vec<(Cell, CellData)>,
    storage: Storage,
}

impl<'de, Storage> serde::Deserialize<'de> for Db<Storage> where
    Storage: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>
    {
        let db = DbDeserialize::deserialize(deserializer)?;

        let cells = scc::HashMap::with_capacity(db.cells.len());
        for (cell, data) in db.cells {
            cells.insert(cell, data).ok();
        }

        Ok(Db {
            cells,
            version: AtomicU32::new(db.version),
            next_cell: AtomicU32::new(db.next_cell),
            storage: db.storage,
        })
    }
}
