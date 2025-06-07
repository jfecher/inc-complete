use std::{any::{Any, TypeId}, rc::Rc};
use crate::{value::HashEqObj, Cell, Computation, Value};
use petgraph::graph::DiGraph;
use crate::cell::CellData;

mod handle;
mod tests;

pub use handle::DbHandle;

const START_VERSION: u32 = 1;

pub struct Db<C: Computation> {
    cells: DiGraph<CellData, ()>,
    version: u32,
    input_to_cell: C::InputToCell,
    cell_to_output: C::CellToOutput,
}

impl<C: Computation> Db<C> where 
    C::InputToCell: Default,
    C::CellToOutput: Default,
{
    pub fn new() -> Self {
        Self {
            cells: DiGraph::default(),
            version: START_VERSION,
            input_to_cell: Default::default(),
            cell_to_output: Default::default(),
        }
    }
}

impl<C: Computation> Db<C> where C::Output: Eq {
    /// Retrieves the up to date value for the given computation, re-running any dependencies as
    /// necessary.
    ///
    /// This function can panic if the dynamic type of the value returned by `compute.run(..)` is not `T`.
    pub fn get<ConcreteC: Computation>(&mut self, compute: ConcreteC) -> &ConcreteC::Output
    where
        C: std::fmt::Debug,
    {
        let cell_id = self.get_or_insert_cell(compute);
        self.get_with_cell(cell_id)
    }

    /// Retrieves the up to date value for the given cell, re-running any dependencies as
    /// necessary.
    ///
    /// This function can panic if the dynamic type of the value returned by `compute.run(..)` is not `T`.
    pub fn get_with_cell<'a, T: 'static>(&'a mut self, cell_id: Cell) -> &'a T {
        self.update_cell(cell_id);

        let computation_id = self[cell_id].computation_id;
        let container = C::get_c2o_container_mut(computation_id, self.storage_mut());
        let output = C::get_function_and_output(cell_id, container).1
            .expect("cell result should have been computed already");

        (output as &dyn Any).downcast_ref().unwrap_or_else(|| {
            panic!("Output type to `Db::get_with_cell` does not match the type of the value returned by the `Computation::run` function")
        })
    }

    /// Trigger an update of the given cell, recursively checking and re-running any out of date
    /// dependencies.
    fn update_cell(&mut self, cell_id: Cell) {
        let cell = &self.cells[cell_id.index()];

        if cell.last_verified_version != self.version {
            // if any dependency may have changed, update
            if self.is_stale_cell(cell_id) {
                self.run_compute_function(cell_id);
            } else {
                let cell = &mut self.cells[cell_id.index()];
                cell.last_verified_version = self.version;
            }
        }
    }

    /// True if a given input is stale and needs to be re-computed.
    /// Inputs which have never been computed are also considered stale.
    ///
    /// This does not actually re-compute the input.
    pub fn is_stale(&self, input: &C) -> bool {
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

    /// Return the corresponding Cell for a given input, if it exists.
    ///
    /// This will not update any values.
    pub fn get_cell<ConcreteC: Computation>(&self, input: &ConcreteC) -> Option<Cell> {
        C::dispatch_input_to_cell(input, &self.input_to_cell)
    }

    pub fn get_or_insert_cell<ConcreteC>(&mut self, input: ConcreteC) -> Cell 
        where ConcreteC: Computation
    {
        if let Some(cell) = C::dispatch_input_to_cell(&input, &self.input_to_cell) {
            cell
        } else {
            let computation_id = C::computation_id_of::<ConcreteC>();

            let new_id = self.cells.add_node(CellData::new(computation_id));
            let cell = Cell::new(new_id);
            C::dispatch_insert_new_cell(cell, input, &mut self.input_to_cell, &mut self.cell_to_output);
            cell
        }
    }

    /// Updates an input with a new value
    ///
    /// May panic in Debug mode if the input is not an input - ie. it has at least 1 dependency.
    /// Note that this step is skipped when compiling in Release mode.
    pub fn update_input(&mut self, input: C, new_value: Value)
    where
        C: std::fmt::Debug,
    {
        // need to split dispatch_run into dispatch_run and dispatch_update
        let cell = &self.cells[cell_id.index()];
        let computation_id = cell.computation_id;

        let changed = C::dispatch_run(cell_id, computation_id, self);

        C::dispatch

        let cell = &mut self.cells[cell_id.index()];

        if changed {
            self.version += 1;
            cell.last_updated_version = self.version;
            cell.last_verified_version = self.version;
        } else {
            cell.last_verified_version = self.version;
        }




        let cell_id = self.get_or_insert_cell(input);
        debug_assert!(
            self.is_input(cell_id),
            "`{:?}` is not an input - inputs must have 0 dependencies",
            self.cells[cell_id.index()].input,
        );

        let cell = &mut self.cells[cell_id.index()];
        let new_hash = new_value.get_hash();

        if let Some((old_hash, _)) = cell.last_result_hash.as_ref() {
            if new_hash == *old_hash {
                cell.last_verified_version = self.version;
                return;
            }
        }

        self.version += 1;
        cell.last_result_hash = Some((new_hash, new_value));
        cell.last_updated_version = self.version;
        cell.last_verified_version = self.version;
    }

    fn is_input(&self, cell: Cell) -> bool {
        self.cells.neighbors(cell.index()).count() == 0
    }

    #[cfg(test)]
    pub(crate) fn get_cell_value(&self, input: &C) -> &CellData<C> where C: std::fmt::Debug {
        let cell = self.get_cell(input).unwrap_or_else(|| {
            panic!("get_cell_value: Expected cell for `{input:?}` to exist")
        });
        &self.cells[cell.index()]
    }

    /// Similar to `update_input` but runs the compute function
    /// instead of accepting a given value. This also will not update
    /// `self.version`
    fn run_compute_function(&mut self, cell_id: Cell)
        where C::Output: Eq
    {
        let cell = &self.cells[cell_id.index()];
        let computation_id = cell.computation_id;

        let changed = C::dispatch_run(cell_id, computation_id, self);

        let cell = &mut self.cells[cell_id.index()];
        cell.last_verified_version = self.version;

        if changed {
            cell.last_updated_version = self.version;
        }
    }

    pub(crate) fn handle(&mut self, cell: Cell) -> DbHandle<C> {
        DbHandle::new(self, cell)
    }

    pub fn storage_mut(&mut self) -> &mut C::InputToOutput {
        &mut self.storage
    }
}

impl<T: Computation> std::ops::Index<Cell> for Db<T> {
    type Output = CellData<T>;

    fn index(&self, index: Cell) -> &Self::Output {
        &self.cells[index.index()]
    }
}
