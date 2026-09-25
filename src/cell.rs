use std::sync::OnceLock;
use std::sync::atomic::{AtomicU32, Ordering};

use rustc_hash::FxHashSet;

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
#[serde(transparent)]
pub struct Cell(u32);

impl Cell {
    pub(crate) fn new(index: u32) -> Self {
        Self(index)
    }

    pub(crate) fn index(self) -> u32 {
        self.0
    }
}

pub(crate) struct CellData {
    pub(crate) computation_id: u32,
    last_updated_version: AtomicU32,
    last_run_version: AtomicU32,
    last_verified_version: AtomicU32,
    dependencies: parking_lot::Mutex<Dependencies>,

    /// Held while this cell's computation is running
    pub(crate) lock: parking_lot::Mutex<()>,
}

#[derive(Default)]
struct Dependencies {
    /// Each dependency in the order they were requested
    ordered: Vec<Cell>,

    /// Used for faster contains checks. TODO: Re-check if this is still beneficial
    set: FxHashSet<Cell>,
}

impl Dependencies {
    fn new(ordered: Vec<Cell>) -> Self {
        let set = ordered.iter().copied().collect();
        Self { ordered, set }
    }

    fn clear(&mut self) {
        self.ordered.clear();
        self.set.clear();
    }

    fn insert(&mut self, cell: Cell) {
        if self.set.insert(cell) {
            self.ordered.push(cell);
        }
    }
}

impl CellData {
    pub(crate) fn new(computation_id: u32) -> Self {
        Self::with_versions(computation_id, 0, 0, 0, Vec::new())
    }

    pub(crate) fn with_versions(
        computation_id: u32,
        last_updated_version: u32,
        last_run_version: u32,
        last_verified_version: u32,
        dependencies: Vec<Cell>,
    ) -> Self {
        Self {
            computation_id,
            last_updated_version: AtomicU32::new(last_updated_version),
            last_run_version: AtomicU32::new(last_run_version),
            last_verified_version: AtomicU32::new(last_verified_version),
            dependencies: parking_lot::Mutex::new(Dependencies::new(dependencies)),
            lock: parking_lot::Mutex::new(()),
        }
    }

    // Acquire/Release so observing a new version also observes the output stored before it
    pub(crate) fn last_updated_version(&self) -> u32 {
        self.last_updated_version.load(Ordering::Acquire)
    }

    pub(crate) fn last_run_version(&self) -> u32 {
        self.last_run_version.load(Ordering::Acquire)
    }

    pub(crate) fn last_verified_version(&self) -> u32 {
        self.last_verified_version.load(Ordering::Acquire)
    }

    pub(crate) fn set_last_updated_version(&self, version: u32) {
        self.last_updated_version.store(version, Ordering::Release);
    }

    pub(crate) fn set_last_run_version(&self, version: u32) {
        self.last_run_version.store(version, Ordering::Release);
    }

    pub(crate) fn set_last_verified_version(&self, version: u32) {
        self.last_verified_version.store(version, Ordering::Release);
    }

    pub(crate) fn dependencies(&self) -> Vec<Cell> {
        self.dependencies.lock().ordered.clone()
    }

    pub(crate) fn has_dependencies(&self) -> bool {
        !self.dependencies.lock().ordered.is_empty()
    }

    pub(crate) fn for_each_dependency(&self, f: impl FnMut(&Cell)) {
        self.dependencies.lock().ordered.iter().for_each(f);
    }

    pub(crate) fn add_dependency(&self, dependency: Cell) {
        self.dependencies.lock().insert(dependency);
    }

    pub(crate) fn clear_dependencies(&self) {
        self.dependencies.lock().clear();
    }
}

/// Size of the first chunk in a [CellTable]. Each following chunk doubles in size.
const FIRST_CHUNK_SIZE: usize = 1024;

/// Enough chunks to hold every `u32` cell index
const CHUNK_COUNT: usize = 23;

/// Append-only table of [CellData] indexed by [Cell]s.
pub(crate) struct CellTable {
    chunks: [OnceLock<Box<[OnceLock<CellData>]>>; CHUNK_COUNT],
}

impl Default for CellTable {
    fn default() -> Self {
        Self {
            chunks: std::array::from_fn(|_| OnceLock::new()),
        }
    }
}

impl CellTable {
    /// Returns (chunk index, index within that chunk)
    fn position(cell: Cell) -> (usize, usize) {
        let index = cell.index() as usize;
        let chunk = (index / FIRST_CHUNK_SIZE + 1).ilog2() as usize;
        let chunk_start = FIRST_CHUNK_SIZE * ((1 << chunk) - 1);
        (chunk, index - chunk_start)
    }

    pub(crate) fn get(&self, cell: Cell) -> Option<&CellData> {
        let (chunk, offset) = Self::position(cell);
        self.chunks[chunk].get()?[offset].get()
    }

    fn chunk(&self, chunk: usize) -> &[OnceLock<CellData>] {
        self.chunks[chunk].get_or_init(|| {
            let size = FIRST_CHUNK_SIZE << chunk;
            (0..size).map(|_| OnceLock::new()).collect()
        })
    }

    /// Allocate the chunk holding `cell` ahead of time so `insert` doesn't have to
    pub(crate) fn reserve(&self, cell: Cell) {
        self.chunk(Self::position(cell).0);
    }

    /// Panics if `cell` was already inserted
    pub(crate) fn insert(&self, cell: Cell, data: CellData) {
        let (chunk, offset) = Self::position(cell);
        if self.chunk(chunk)[offset].set(data).is_err() {
            panic!("inc-complete internal error: {cell:?} was inserted twice");
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (Cell, &CellData)> {
        self.chunks
            .iter()
            .enumerate()
            .flat_map(|(chunk_index, chunk)| {
                let chunk_start = FIRST_CHUNK_SIZE * ((1 << chunk_index) - 1);
                let slots = chunk.get().map(|chunk| chunk.iter()).into_iter().flatten();
                slots.enumerate().filter_map(move |(offset, slot)| {
                    let cell = Cell::new((chunk_start + offset) as u32);
                    slot.get().map(|data| (cell, data))
                })
            })
    }
}

#[cfg(test)]
mod tests {
    use super::{CHUNK_COUNT, Cell, CellData, CellTable, FIRST_CHUNK_SIZE};

    #[test]
    fn positions_are_contiguous() {
        let mut expected = (0, 0);
        for index in 0..(FIRST_CHUNK_SIZE * 16) as u32 {
            let position = CellTable::position(Cell::new(index));
            if position != expected {
                assert_eq!(position, (expected.0 + 1, 0), "at index {index}");
            }
            expected = (position.0, position.1 + 1);
        }
        assert_eq!(CellTable::position(Cell::new(u32::MAX)).0, CHUNK_COUNT - 1);
    }

    #[test]
    fn insert_and_iterate() {
        let table = CellTable::default();
        let cells = [0, 5, 1023, 1024, 3071, 3072, 100_000];
        for index in cells {
            table.insert(Cell::new(index), CellData::new(index));
        }
        for index in cells {
            assert_eq!(table.get(Cell::new(index)).unwrap().computation_id, index);
        }
        assert!(table.get(Cell::new(2)).is_none());
        let found: Vec<_> = table.iter().map(|(cell, _)| cell.index()).collect();
        assert_eq!(found, cells);
    }
}
