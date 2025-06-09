use crate::{Cell, Computation};
use petgraph::graph::DiGraph;
use crate::cell::CellData;

mod handle;
mod tests;

pub use handle::DbHandle;

const START_VERSION: u32 = 1;

pub struct Db<C: Computation> {
    cells: DiGraph<CellData, ()>,
    version: u32,
    storage: C::Storage,
}

impl<C: Computation> Db<C> where 
    C::Storage: Default,
{
    pub fn new() -> Self {
        Self {
            cells: DiGraph::default(),
            version: START_VERSION,
            storage: Default::default(),
        }
    }
}

impl<C: Computation> Db<C> {
    /// True if a given input is stale and needs to be re-computed.
    /// Inputs which have never been computed are also considered stale.
    ///
    /// This does not actually re-compute the input.
    pub fn is_stale<Concrete: Computation>(&self, input: &Concrete) -> bool {
        // If the cell doesn't exist, it is definitely stale
        let Some(cell) = self.get_cell(input) else {
            return true;
        };
        self.is_stale_cell(cell)
    }

    /// True if a given cell is stale and needs to be re-computed.
    /// This does not actually re-compute the input.
    pub fn is_stale_cell(&self, cell: Cell) -> bool {
        let computation_id = self.cells[cell.index()].computation_id;
        if C::output_is_unset::<C>(cell, computation_id, computation_id, self) {
            return true;
        }

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
        C::dispatch_input_to_cell(input, &self.storage)
    }

    pub fn get_or_insert_cell<ConcreteC>(&mut self, input: ConcreteC) -> Cell 
        where ConcreteC: Computation
    {
        if let Some(cell) = C::dispatch_input_to_cell(&input, &self.storage) {
            cell
        } else {
            let computation_id = C::computation_id_of::<ConcreteC>();

            let new_id = self.cells.add_node(CellData::new(computation_id));
            let cell = Cell::new(new_id);
            C::dispatch_insert_new_cell(cell, input, &mut self.storage);
            cell
        }
    }

    /// Updates an input with a new value
    ///
    /// May panic in Debug mode if the input is not an input - ie. it has at least 1 dependency.
    /// Note that this step is skipped when compiling in Release mode.
    pub fn update_input<ConcreteC: Computation>(&mut self, input: ConcreteC, new_value: ConcreteC::Output)
    where
        ConcreteC: std::fmt::Debug,
        C::Output: Eq,
    {
        let debug = format!("{input:?}");
        let cell_id = self.get_or_insert_cell(input);
        debug_assert!(
            self.is_input(cell_id),
            "`{debug:?}` is not an input - inputs must have 0 dependencies",
        );

        // need to split dispatch_run into dispatch_run and dispatch_update
        let cell = &self.cells[cell_id.index()];
        let computation_id = cell.computation_id;

        let changed = C::dispatch_update_output::<ConcreteC, C>(cell_id, computation_id, computation_id, new_value, self);
        let cell = &mut self.cells[cell_id.index()];

        if changed {
            self.version += 1;
            cell.last_updated_version = self.version;
            cell.last_verified_version = self.version;
        } else {
            cell.last_verified_version = self.version;
        }
    }

    fn is_input(&self, cell: Cell) -> bool {
        self.cells.neighbors(cell.index()).count() == 0
    }

    pub(crate) fn handle(&mut self, cell: Cell) -> DbHandle<C> {
        DbHandle::new(self, cell)
    }

    pub fn storage(&self) -> &C::Storage {
        &self.storage
    }

    pub fn storage_mut(&mut self) -> &mut C::Storage {
        &mut self.storage
    }

    #[cfg(test)]
    pub(crate) fn unwrap_cell_value<Concrete: Computation>(&self, input: &Concrete) -> &CellData where Concrete: std::fmt::Debug {
        let cell = self.get_cell(input).unwrap_or_else(|| {
            panic!("unwrap_cell_value: Expected cell for `{input:?}` to exist")
        });
        &self.cells[cell.index()]
    }
}

impl<C: Computation + Clone> Db<C> where C::Output: Eq {
    /// Similar to `update_input` but runs the compute function
    /// instead of accepting a given value. This also will not update
    /// `self.version`
    fn run_compute_function(&mut self, cell_id: Cell)
        where C::Output: Eq
    {
        let cell = &self.cells[cell_id.index()];
        let computation_id = cell.computation_id;

        let changed = C::dispatch_run::<C>(cell_id, computation_id, computation_id, self);

        let cell = &mut self.cells[cell_id.index()];
        cell.last_verified_version = self.version;

        if changed {
            cell.last_updated_version = self.version;
        }
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

    /// Retrieves the up to date value for the given computation, re-running any dependencies as
    /// necessary.
    ///
    /// This function can panic if the dynamic type of the value returned by `compute.run(..)` is not `T`.
    pub fn get<Concrete: Computation>(&mut self, compute: Concrete) -> &Concrete::Output {
        let cell_id = self.get_or_insert_cell(compute);
        self.get_with_cell::<Concrete>(cell_id)
    }

    /// Retrieves the up to date value for the given cell, re-running any dependencies as
    /// necessary.
    ///
    /// This function can panic if the dynamic type of the value returned by `compute.run(..)` is not `T`.
    pub fn get_with_cell<Concrete: Computation>(&mut self, cell_id: Cell) -> &Concrete::Output {
        self.update_cell(cell_id);

        let computation_id = self.cells[cell_id.index()].computation_id;
        let container = C::get_storage_mut::<Concrete>(computation_id, self.storage_mut());
        Concrete::get_function_and_output(cell_id, container).1
            .expect("cell result should have been computed already")
    }
}
