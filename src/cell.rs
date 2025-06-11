#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Cell(petgraph::graph::NodeIndex);

impl Cell {
    pub(crate) fn new(index: petgraph::graph::NodeIndex) -> Self {
        Self(index)
    }

    pub(crate) fn index(self) -> petgraph::graph::NodeIndex {
        self.0
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) struct CellData {
    pub(crate) computation_id: u32,
    pub(crate) last_updated_version: u32,
    pub(crate) last_verified_version: u32,
}

impl CellData {
    pub(crate) fn new(computation_id: u32) -> Self {
        Self {
            computation_id,
            last_updated_version: 0,
            last_verified_version: 0,
        }
    }
}
