use crate::cell::CellData;
use crate::storage::{ComputationId, StorageFor};
use crate::{Cell, OutputType, Storage};

mod handle;
mod tests;

pub use handle::DbHandle;

const START_VERSION: u32 = 1;

/// The central database object to manage and cache incremental computations.
///
/// To use this, a type implementing `Storage` is required to be provided.
/// See the documentation for `impl_storage!`.
#[derive(serde::Serialize, serde::Deserialize)]
pub struct Db<Storage> {
    cells: dashmap::DashMap<Cell, CellData>,
    version: u32,
    storage: Storage,
}

impl<Storage: Default> Db<Storage> {
    /// Construct a new `Db` object using `Default::default()` for the initial storage.
    pub fn new() -> Self {
        Self::with_storage(Storage::default())
    }
}

impl<S: Default> Default for Db<S> {
    fn default() -> Self {
        Self::new()
    }
}

impl<S> Db<S> {
    /// Construct a new `Db` object with the given initial storage.
    pub fn with_storage(storage: S) -> Self {
        Self {
            cells: Default::default(),
            version: START_VERSION,
            storage,
        }
    }

    /// Retrieve an immutable reference to this `Db`'s storage
    pub fn storage(&self) -> &S {
        &self.storage
    }

    /// Retrieve a mutable reference to this `Db`'s storage.
    ///
    /// Note that any mutations made to the storage using this are _not_ tracked by the `Db`!
    /// Using this incorrectly may break correctness!
    pub fn storage_mut(&mut self) -> &mut S {
        &mut self.storage
    }
}

impl<S: Storage> Db<S> {
    /// True if a given input is stale and needs to be re-computed.
    /// Inputs which have never been computed are also considered stale.
    ///
    /// This does not actually re-compute the input.
    pub fn is_stale<C: OutputType>(&mut self, input: &C) -> bool
    where
        S: StorageFor<C>,
    {
        // If the cell doesn't exist, it is definitely stale
        let Some(cell) = self.get_cell(input) else {
            return true;
        };
        self.is_stale_cell(cell)
    }

    /// True if a given cell is stale and needs to be re-computed.
    /// This does not actually re-compute the input.
    fn is_stale_cell(&self, cell: Cell) -> bool {
        let computation_id = self.cells.get(&cell).unwrap().computation_id;

        if self.storage.output_is_unset(cell, computation_id) {
            return true;
        }

        // if any dependency may have changed, this cell is stale
        let cell = self.cells.get(&cell).unwrap();
        let last_verified = cell.last_verified_version;

        // Dependencies need to be iterated in the order they were computed.
        // Otherwise we may re-run a computation which does not need to be re-run.
        // In the worst case this could even lead to panics - see the div0 test.
        cell.dependencies.iter().any(|dependency_id| {
            self.update_cell(*dependency_id);

            // This cell is stale if the dependency has been updated since
            // we last verified this cell
            let dependency = self.cells.get(&dependency_id).unwrap();
            dependency.last_updated_version > last_verified
        })
    }

    /// Return the corresponding Cell for a given computation, if it exists.
    ///
    /// This will not update any values.
    fn get_cell<C: OutputType>(&self, computation: &C) -> Option<Cell>
    where
        S: StorageFor<C>,
    {
        self.storage.get_cell_for_computation(computation)
    }

    pub(crate) fn get_or_insert_cell<C>(&self, input: C) -> Cell
    where
        C: OutputType + ComputationId,
        S: StorageFor<C>,
    {
        if let Some(cell) = self.get_cell(&input) {
            cell
        } else {
            let computation_id = C::computation_id();

            let new_cell = Cell::new(self.cells.len() as u32);
            self.cells.insert(new_cell, CellData::new(computation_id));
            self.storage.insert_new_cell(new_cell, input);
            new_cell
        }
    }

    /// Updates an input with a new value
    ///
    /// Panics in debug mode if the input is not an input - ie. it has at least 1 dependency.
    /// Note that this check is skipped when compiling in Release mode.
    pub fn update_input<C: OutputType>(&mut self, input: C, new_value: C::Output)
    where
        C: ComputationId,
        S: StorageFor<C>,
    {
        let cell_id = self.get_or_insert_cell(input);
        debug_assert!(
            self.is_input(cell_id),
            "`update_input` given a non-input value. Inputs must have 0 dependencies",
        );

        let changed = self.storage.update_output(cell_id, new_value);
        let mut cell = self.cells.get_mut(&cell_id).unwrap();

        if changed {
            self.version += 1;
            cell.last_updated_version = self.version;
            cell.last_verified_version = self.version;
        } else {
            cell.last_verified_version = self.version;
        }
    }

    fn is_input(&self, cell: Cell) -> bool {
        self.cells.get(&cell).unwrap().dependencies.len() == 0
    }

    fn handle(&self, cell: Cell) -> DbHandle<S> {
        DbHandle::new(self, cell)
    }

    #[cfg(test)]
    pub(crate) fn unwrap_cell_value<C: OutputType>(&self, input: &C) -> CellData
    where
        S: StorageFor<C>,
    {
        let cell = self
            .get_cell(input)
            .unwrap_or_else(|| panic!("unwrap_cell_value: Expected cell to exist"));

        self.cells.get(&cell).unwrap().clone()
    }

    /// Similar to `update_input` but runs the compute function
    /// instead of accepting a given value. This also will not update
    /// `self.version`
    fn run_compute_function(&self, cell_id: Cell) {
        let computation_id = self.cells.get(&cell_id).unwrap().computation_id;

        let handle = self.handle(cell_id);
        let changed = S::run_computation(&handle, cell_id, computation_id);

        let mut cell = self.cells.get_mut(&cell_id).unwrap();
        cell.last_verified_version = self.version;

        if changed {
            cell.last_updated_version = self.version;
        }
    }

    /// Trigger an update of the given cell, recursively checking and re-running any out of date
    /// dependencies.
    fn update_cell(&self, cell_id: Cell) {
        let last_verified_version = self.cells.get(&cell_id).unwrap().last_verified_version;

        if last_verified_version != self.version {
            // if any dependency may have changed, update
            if self.is_stale_cell(cell_id) {
                self.run_compute_function(cell_id);
            } else {
                let mut cell = self.cells.get_mut(&cell_id).unwrap();
                cell.last_verified_version = self.version;
            }
        }
    }

    /// Retrieves the up to date value for the given computation, re-running any dependencies as
    /// necessary.
    ///
    /// This function can panic if the dynamic type of the value returned by `compute.run(..)` is not `T`.
    pub fn get<C: OutputType + ComputationId>(&mut self, compute: C) -> C::Output
    where
        S: StorageFor<C>,
    {
        let cell_id = self.get_or_insert_cell(compute);
        self.get_with_cell::<C>(cell_id)
    }

    /// Retrieves the up to date value for the given cell, re-running any dependencies as
    /// necessary.
    ///
    /// This function can panic if the dynamic type of the value returned by `compute.run(..)` is not `T`.
    pub(crate) fn get_with_cell<Concrete: OutputType>(&self, cell_id: Cell) -> Concrete::Output
    where
        S: StorageFor<Concrete>,
    {
        self.update_cell(cell_id);

        self.storage
            .get_output(cell_id)
            .expect("cell result should have been computed already")
    }
}
