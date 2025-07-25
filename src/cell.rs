use std::{collections::HashSet, sync::Arc};

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
#[serde(transparent)]
pub struct Cell(u32);

impl Cell {
    pub(crate) fn new(index: u32) -> Self {
        Self(index)
    }
}

pub(crate) struct CellData {
    pub(crate) computation_id: u32,
    pub(crate) last_updated_version: u32,
    pub(crate) last_verified_version: u32,
    pub(crate) dependencies: Vec<Cell>,
    pub(crate) input_dependencies: HashSet<Cell, rustc_hash::FxBuildHasher>,
    pub(crate) lock: Arc<parking_lot::Mutex<()>>,
}

impl CellData {
    pub(crate) fn new(computation_id: u32) -> Self {
        Self {
            computation_id,
            last_updated_version: 0,
            last_verified_version: 0,
            dependencies: Vec::new(),
            input_dependencies: Default::default(),
            lock: Arc::new(parking_lot::Mutex::new(())),
        }
    }
}
