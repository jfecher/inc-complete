use std::sync::atomic::{AtomicU32, Ordering};

use crate::accumulate::Accumulate;
use crate::cell::CellData;
use crate::storage::{ComputationId, StorageFor};
use crate::{Cell, OutputType, Storage};

mod handle;
mod serialize;
mod tests;

pub use handle::DbHandle;

const START_VERSION: u32 = 1;

/// The central database object to manage and cache incremental computations.
///
/// To use this, a type implementing `Storage` is required to be provided.
/// See the documentation for `impl_storage!`.
pub struct Db<Storage> {
    cells: dashmap::DashMap<Cell, CellData>,
    version: AtomicU32,
    next_cell: AtomicU32,
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

/// Abstracts over the `get` function provided by `Db<S>` and `DbHandle<S>` to avoid
/// providing `get` and `get_db` variants for each function.
pub trait DbGet<C: OutputType> {
    /// Run an incremental computation `C` and return its output.
    /// If `C` is already cached, no computation will be performed.
    fn get(&self, key: C) -> C::Output;
}

impl<S, C> DbGet<C> for Db<S>
where
    C: OutputType + ComputationId,
    S: Storage + StorageFor<C>,
{
    fn get(&self, key: C) -> C::Output {
        self.get(key)
    }
}

impl<S> Db<S> {
    /// Construct a new `Db` object with the given initial storage.
    pub fn with_storage(storage: S) -> Self {
        Self {
            cells: Default::default(),
            version: AtomicU32::new(START_VERSION),
            next_cell: AtomicU32::new(0),
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

            // We just need a unique ID here, we don't care about ordering between
            // threads, so we're using Ordering::Relaxed.
            let cell_id = self.next_cell.fetch_add(1, Ordering::Relaxed);
            let new_cell = Cell::new(cell_id);

            self.cells.insert(new_cell, CellData::new(computation_id));
            self.storage.insert_new_cell(new_cell, input);
            new_cell
        }
    }

    fn handle(&self, cell: Cell) -> DbHandle<S> {
        DbHandle::new(self, cell)
    }

    #[cfg(test)]
    #[allow(unused)]
    pub(crate) fn with_cell_data<C: OutputType>(&self, input: &C, f: impl FnOnce(&CellData))
    where
        S: StorageFor<C>,
    {
        let cell = self
            .get_cell(input)
            .unwrap_or_else(|| panic!("unwrap_cell_value: Expected cell to exist"));

        self.cells.get(&cell).map(|value| f(&value)).unwrap()
    }

    pub fn version(&self) -> u32 {
        self.version.load(Ordering::SeqCst)
    }

    
    pub fn gc(&mut self, version: u32) {
        let used_cells: std::collections::HashSet<Cell> = self
            .cells
            .iter()
            .filter_map(|entry| {
                if entry.value().last_verified_version >= version {
                    Some(entry.key().clone())
                } else {
                    None
                }
            })
            .collect();

        self.storage.gc(&used_cells);
    } 
}

impl<S: Storage> Db<S> {
    /// Updates an input with a new value
    ///
    /// This requires an exclusive reference to self to ensure that there are no currently
    /// running queries. Updating an input while an incremental computation is occurring
    /// can break soundness for dependency tracking.
    ///
    /// Panics if the given computation is not an input - ie. panics if it has at least 1 dependency.
    pub fn update_input<C>(&mut self, input: C, new_value: C::Output)
    where
        C: OutputType + ComputationId,
        S: StorageFor<C>,
    {
        let cell_id = self.get_or_insert_cell(input);
        assert!(
            self.is_input(cell_id),
            "`update_input` given a non-input value. Inputs must have 0 dependencies",
        );

        let changed = self.storage.update_output(cell_id, new_value);
        let mut cell = self.cells.get_mut(&cell_id).unwrap();

        if changed {
            let version = self.version.fetch_add(1, Ordering::SeqCst) + 1;
            cell.last_updated_version = version;
            cell.last_verified_version = version;
        } else {
            cell.last_verified_version = self.version.load(Ordering::SeqCst);
        }
    }

    fn is_input(&self, cell: Cell) -> bool {
        self.with_cell(cell, |cell| {
            cell.dependencies.is_empty() && cell.input_dependencies.is_empty()
        })
    }

    /// True if a given computation is stale and needs to be re-computed.
    /// Computations which have never been computed are also considered stale.
    ///
    /// Note that this may re-compute dependencies of the given computation.
    pub fn is_stale<C: OutputType>(&self, input: &C) -> bool
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
    ///
    /// Note that this may re-compute some input
    fn is_stale_cell(&self, cell: Cell) -> bool {
        let computation_id = self.with_cell(cell, |data| data.computation_id);

        if self.storage.output_is_unset(cell, computation_id) {
            return true;
        }

        // if any input dependency has changed, this cell is stale
        let (last_verified, inputs, dependencies) = self.with_cell(cell, |data| {
            (
                data.last_verified_version,
                data.input_dependencies.clone(),
                data.dependencies.clone(),
            )
        });

        // Optimization: only recursively check all dependencies if any
        // of the inputs this cell depends on have changed
        let inputs_changed = inputs.into_iter().any(|input_id| {
            // This cell is stale if the dependency has been updated since
            // we last verified this cell
            self.with_cell(input_id, |input| input.last_updated_version > last_verified)
        });

        // Dependencies need to be iterated in the order they were computed.
        // Otherwise we may re-run a computation which does not need to be re-run.
        // In the worst case this could even lead to panics - see the div0 test.
        inputs_changed
            && dependencies.into_iter().any(|dependency_id| {
                self.update_cell(dependency_id);
                self.with_cell(dependency_id, |dependency| {
                    dependency.last_updated_version > last_verified
                })
            })
    }

    /// Similar to `update_input` but runs the compute function
    /// instead of accepting a given value. This also will not update
    /// `self.version`
    fn run_compute_function(&self, cell_id: Cell) {
        let computation_id = self.with_cell(cell_id, |data| data.computation_id);

        let handle = self.handle(cell_id);
        let changed = S::run_computation(&handle, cell_id, computation_id);

        let version = self.version.load(Ordering::SeqCst);
        let mut cell = self.cells.get_mut(&cell_id).unwrap();
        cell.last_verified_version = version;

        if changed {
            cell.last_updated_version = version;
        }
    }

    /// Trigger an update of the given cell, recursively checking and re-running any out of date
    /// dependencies.
    fn update_cell(&self, cell_id: Cell) {
        let last_verified_version = self.with_cell(cell_id, |data| data.last_verified_version);
        let version = self.version.load(Ordering::SeqCst);

        if last_verified_version != version {
            // if any dependency may have changed, update
            if self.is_stale_cell(cell_id) {
                let lock = self.with_cell(cell_id, |cell| cell.lock.clone());

                match lock.try_lock() {
                    Some(guard) => {
                        self.run_compute_function(cell_id);
                        drop(guard);
                    }
                    None => {
                        // This computation is already being run in another thread.
                        // Block until it finishes and return the result
                        drop(lock.lock());
                    }
                }
            } else {
                let mut cell = self.cells.get_mut(&cell_id).unwrap();
                cell.last_verified_version = version;
            }
        }
    }

    /// Retrieves the up to date value for the given computation, re-running any dependencies as
    /// necessary.
    ///
    /// This function can panic if the dynamic type of the value returned by `compute.run(..)` is not `T`.
    ///
    /// Locking behavior: This function locks the cell corresponding to the given computation. This
    /// can cause a deadlock if the computation recursively depends on itself.
    pub fn get<C: OutputType + ComputationId>(&self, compute: C) -> C::Output
    where
        S: StorageFor<C>,
    {
        let cell_id = self.get_or_insert_cell(compute);
        self.get_with_cell::<C>(cell_id)
    }

    pub(crate) fn get_with_cell<Concrete: OutputType>(&self, cell_id: Cell) -> Concrete::Output
    where
        S: StorageFor<Concrete>,
    {
        self.update_cell(cell_id);

        self.storage
            .get_output(cell_id)
            .expect("cell result should have been computed already")
    }

    fn with_cell<R>(&self, cell: Cell, f: impl FnOnce(&CellData) -> R) -> R {
        f(&self.cells.get(&cell).unwrap())
    }

    /// Fill the given set with all cells directly or indirectly used as dependencies
    /// for the given cell. To ensure dependencies are up to date, this may run
    /// any stale dependencies.
    fn collect_all_dependencies(&self, operation: Cell) -> Vec<Cell> {
        // Ensure all dependencies are up to date
        self.update_cell(operation);

        let mut queue: Vec<_> = self.with_cell(operation, |cell| {
            cell.dependencies.iter().copied().collect()
        });

        let mut cells = Vec::new();
        cells.push(operation);

        // We must be careful to preserve dependency order in `cells`.
        // This is easiest if we push from last-to-first and reverse `cells` later.
        while let Some(dependency) = queue.pop() {
            cells.push(dependency);

            self.with_cell(dependency, |cell| {
                for dependency in cell.dependencies.iter() {
                    queue.push(*dependency);
                }
            });
        }

        cells.reverse();
        cells
    }

    /// Retrieve an accumulated value in a container of the user's choice.
    /// This will return all the accumulated items after the given computation.
    ///
    /// Note that although this method will not re-perform the given computation,
    /// it will re-collect all the required accumulated items each time it is called,
    /// which may be costly for large dependency trees.
    ///
    /// This is most often used for operations like retrieving diagnostics or logs.
    pub fn get_accumulated<Container, Item, C>(&self, compute: C) -> Container where
        Container: FromIterator<Item>,
        S: Accumulate<Item> + StorageFor<C>,
        C: OutputType + ComputationId
    {
        let cell_id = self.get_or_insert_cell(compute);

        let cells = self.collect_all_dependencies(cell_id);
        self.storage.get_accumulated(&cells)
    }
}
