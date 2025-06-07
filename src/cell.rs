use std::rc::Rc;

use crate::Value;

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

pub(crate) struct CellValue<F> {
    pub(crate) input: Rc<F>,
    pub(crate) result: Option<(u64, Value)>,

    pub(crate) last_updated_version: u32,
    pub(crate) last_verified_version: u32,
}

impl<F> CellValue<F> {
    pub(crate) fn new(input: Rc<F>) -> Self {
        Self {
            input,
            result: None,
            last_updated_version: 0,
            last_verified_version: 0,
        }
    }

    pub(crate) fn result(&self) -> Option<&Value> {
        self.result.as_ref().map(|(_, value)| value)
    }
}

