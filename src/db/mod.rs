use std::{collections::HashMap, hash::Hash};
use crate::{value::HashEqObj, Cell, Run, Value};
use petgraph::graph::DiGraph;
use crate::cell::CellValue;

mod handle;
mod tests;

pub use handle::DbHandle;

const START_VERSION: u32 = 1;

pub struct Db<F> {
    cells: DiGraph<CellValue<F>, ()>,
    version: u32,

    input_to_cell: HashMap<F, Cell>,
}

impl<F> Db<F> {
    pub fn new() -> Self {
        Self {
            cells: DiGraph::default(),
            input_to_cell: HashMap::default(),
            version: START_VERSION,
        }
    }
}

impl<F: Run + Copy + Eq + Hash + Clone> Db<F> {
    /// Retrieves the up to date value for the given computation, re-running any dependencies as
    /// necessary.
    ///
    /// This function can panic if the dynamic type of the value returned by `compute.run(..)` is not `T`.
    pub fn get<'a, T: 'static>(&'a mut self, compute: F) -> &'a T
    where
        F: std::fmt::Debug,
    {
        let cell_id = self.get_or_insert_cell(compute);
        self.update_cell(cell_id);

        let cell = &self.cells[cell_id.index()];
        let result = cell.result().expect("cell result should have been computed already");

        result.downcast_obj_ref()
            .expect("Output type to `Db::get` does not match the type of the value returned by the `Run::run` function")
    }

    /// Trigger an update of the given cell, recursively checking and re-running any out of date
    /// dependencies.
    fn update_cell(&mut self, cell_id: Cell)
    where
        F: std::fmt::Debug,
    {
        let cell = &self.cells[cell_id.index()];

        if cell.last_verified_version != self.version {
            if cell.result.is_some() {
                // if any dependency may have changed, update
                if self.is_stale_cell(cell_id) {
                    self.run_compute_function(cell_id);
                } else {
                    let cell = &mut self.cells[cell_id.index()];
                    cell.last_verified_version = self.version;
                }
            } else {
                // cell.result is None, initialize it
                self.run_compute_function(cell_id);
            }
        }
    }

    /// True if a given input is stale and needs to be re-computed.
    /// Inputs which have never been computed are also considered stale.
    ///
    /// This does not actually re-compute the input.
    pub fn is_stale(&self, input: F) -> bool {
        // If the cell doesn't exist, it is definitely stale
        let Some(cell) = self.get_cell(input) else {
            return true;
        };
        self.is_stale_cell(cell)
    }

    /// True if a given cell is stale and needs to be re-computed.
    /// This does not actually re-compute the input.
    pub fn is_stale_cell(&self, cell: Cell) -> bool {
        let neighbors = self.cells.neighbors(cell.index()).collect::<Vec<_>>();

        // if any dependency may have changed, this cell is stale
        neighbors.into_iter().any(|dependency_id| {
            let dependency = &self.cells[dependency_id];
            let cell = &self.cells[cell.index()];

            dependency.last_verified_version != self.version
                || dependency.last_updated_version > cell.last_verified_version
        })
    }

    pub fn get_cell(&self, input: F) -> Option<Cell> {
        self.input_to_cell.get(&input).copied()
    }

    pub fn get_or_insert_cell(&mut self, input: F) -> Cell {
        if let Some(cell_id) = self.input_to_cell.get(&input) {
            *cell_id
        } else {
            let new_id = self.cells.add_node(CellValue::new(input.clone()));
            let cell = Cell::new(new_id);
            self.input_to_cell.insert(input, cell);
            cell
        }
    }

    /// Updates an input with a new value
    ///
    /// May panic in Debug mode if the input is not an input - ie. it has at least 1 dependency.
    /// Note that this step is skipped when compiling in Release mode.
    pub fn update_input(&mut self, input: F, new_value: Value)
    where
        F: std::fmt::Debug,
    {
        let cell_id = self.get_or_insert_cell(input);
        debug_assert!(
            self.is_input(cell_id),
            "`{input:?}` is not an input - inputs must have 0 dependencies"
        );

        let cell = &mut self.cells[cell_id.index()];
        let new_hash = new_value.get_hash();

        if let Some((old_hash, _)) = cell.result.as_ref() {
            if new_hash == *old_hash {
                cell.last_verified_version = self.version;
                return;
            }
        }

        self.version += 1;
        cell.result = Some((new_hash, new_value));
        cell.last_updated_version = self.version;
        cell.last_verified_version = self.version;
    }

    fn is_input(&self, cell: Cell) -> bool {
        self.cells.neighbors(cell.index()).count() == 0
    }

    #[cfg(test)]
    pub(crate) fn get_cell_value(&self, input: F) -> &CellValue<F> where F: std::fmt::Debug {
        let cell = self.get_cell(input).unwrap_or_else(|| {
            panic!("get_cell_value: Expected cell for `{input:?}` to exist")
        });
        &self.cells[cell.index()]
    }

    /// Similar to `update_input` but runs the compute function
    /// instead of accepting a given value. This also will not update
    /// `self.version`
    fn run_compute_function(&mut self, cell_id: Cell) {
        let cell = &self.cells[cell_id.index()];
        let result = cell.compute.run(&mut self.handle(cell_id));
        let new_hash = result.get_hash();
        let cell = &mut self.cells[cell_id.index()];

        if let Some((old_hash, _)) = cell.result.as_ref() {
            if new_hash == *old_hash {
                cell.last_verified_version = self.version;
                return;
            }
        }

        cell.result = Some((new_hash, result));
        cell.last_verified_version = self.version;
        cell.last_updated_version = self.version;
    }

    fn handle(&mut self, cell: Cell) -> DbHandle<F> {
        DbHandle::new(self, cell)
    }
}

