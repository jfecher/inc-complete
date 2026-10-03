use std::collections::BTreeSet;
use std::sync::atomic::{AtomicU32, Ordering};

use crate::accumulate::{ACCUMULATED_COMPUTATION_ID, Accumulate, Accumulated};
use crate::cell::{CellData, CellTable};
use crate::storage::StorageFor;
use crate::{Cell, Computation, Storage};

pub mod debug_with_db;
mod handle;
mod serialize;
mod tests;

pub use handle::DbHandle;
use rustc_hash::FxHashSet;

const START_VERSION: u32 = 1;

/// How far past the next cell id to grow the cell table before creating a new cell
const RESERVE_AHEAD: u32 = 256;

/// The central database object to manage and cache incremental computations.
///
/// To use this, a type implementing `Storage` is required to be provided.
/// See the documentation for `impl_storage!`.
pub struct Db<Storage> {
    cells: CellTable,
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
pub trait DbGet<C: Computation> {
    /// Run an incremental computation `C` and return its output.
    /// If `C` is already cached, no computation will be performed.
    fn get(&self, key: C) -> C::Output;
}

impl<S, C> DbGet<C> for Db<S>
where
    C: Computation,
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

    pub(crate) fn cell(&self, cell: Cell) -> &CellData {
        self.cells
            .get(cell)
            .unwrap_or_else(|| panic!("inc-complete internal error: {cell:?} has no data"))
    }
}

impl<S: Storage> Db<S> {
    /// Return the corresponding Cell for a given computation, if it exists.
    ///
    /// This will not update any values.
    fn get_cell<C: Computation>(&self, computation: &C) -> Option<Cell>
    where
        S: StorageFor<C>,
    {
        self.storage.get_cell_for_computation(computation)
    }

    pub(crate) fn get_or_insert_cell<C>(&self, input: C) -> Cell
    where
        C: Computation,
        S: StorageFor<C>,
    {
        if let Some(cell) = self.get_cell(&input) {
            return cell;
        }

        // Growing the table is slow, so do it before the storage takes any locks
        let upcoming = self
            .next_cell
            .load(Ordering::Relaxed)
            .saturating_add(RESERVE_AHEAD);
        self.cells.reserve(Cell::new(upcoming));

        self.storage.get_or_insert_cell(input, || {
            // We just need a unique ID here, ordering between threads doesn't matter
            let cell = Cell::new(self.next_cell.fetch_add(1, Ordering::Relaxed));
            self.cells.insert(cell, CellData::new(C::computation_id()));
            cell
        })
    }

    fn handle(&self, cell: Cell) -> DbHandle<'_, S> {
        DbHandle::new(self, cell)
    }

    #[cfg(test)]
    #[allow(unused)]
    pub(crate) fn with_cell_data<C: Computation>(&self, input: &C, f: impl FnOnce(&CellData))
    where
        S: StorageFor<C>,
    {
        let cell = self
            .get_cell(input)
            .unwrap_or_else(|| panic!("unwrap_cell_value: Expected cell to exist"));

        f(self.cell(cell))
    }

    pub fn version(&self) -> u32 {
        self.version.load(Ordering::SeqCst)
    }

    pub fn gc(&mut self, version: u32) {
        let used_cells: std::collections::HashSet<Cell> = self
            .cells
            .iter()
            .filter(|(_, data)| data.last_verified_version() >= version)
            .map(|(cell, _)| cell)
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
        C: Computation,
        S: StorageFor<C>,
    {
        let cell_id = self.get_or_insert_cell(input);
        assert!(
            self.is_input(cell_id),
            "`update_input` given a non-input value. Inputs must have 0 dependencies",
        );

        let changed = self.storage.update_output(cell_id, new_value);
        let cell = self.cell(cell_id);

        if changed {
            let version = self.version.fetch_add(1, Ordering::SeqCst) + 1;
            cell.set_last_updated_version(version);
            cell.set_last_verified_version(version);
        } else {
            cell.set_last_verified_version(self.version.load(Ordering::SeqCst));
        }
    }

    fn is_input(&self, cell: Cell) -> bool {
        !self.cell(cell).has_dependencies()
    }

    /// True if a given computation is stale and needs to be re-computed.
    /// Computations which have never been computed are also considered stale.
    ///
    /// Note that this may re-compute dependencies of the given computation.
    pub fn is_stale<C: Computation>(&self, input: &C) -> bool
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
        let data = self.cell(cell);
        let computation_id = data.computation_id;
        if self.storage.output_is_unset(cell, computation_id) {
            return true;
        }

        let last_verified = data.last_verified_version();
        let dependencies = data.dependencies();

        // Reporting this stale makes `update_cell` wait for an existing run instead
        if data.lock.is_locked() {
            return true;
        }
        std::sync::atomic::fence(Ordering::Acquire);

        // Dependencies need to be iterated in the order they were computed.
        // Otherwise we may re-run a computation which does not need to be re-run.
        // In the worst case this could even lead to panics - see the div0 test.
        dependencies.into_iter().any(|dependency_id| {
            self.update_cell(dependency_id);
            let dependency = self.cell(dependency_id);
            if computation_id == ACCUMULATED_COMPUTATION_ID {
                dependency.last_run_version() > last_verified
            } else {
                dependency.last_updated_version() > last_verified
            }
        })
    }

    /// Similar to `update_input` but runs the compute function
    /// instead of accepting a given value. This also will not update
    /// `self.version`
    fn run_compute_function(&self, cell_id: Cell) {
        let cell = self.cell(cell_id);
        self.storage.clear_accumulated_for_cell(cell_id);
        let handle = self.handle(cell_id);
        let changed = S::run_computation(&handle, cell_id, cell.computation_id);

        let version = self.version.load(Ordering::SeqCst);
        cell.set_last_run_version(version);

        if changed {
            cell.set_last_updated_version(version);
        }

        // Written last since other threads check it before reading anything else
        cell.set_last_verified_version(version);
    }

    /// Trigger an update of the given cell, recursively checking and re-running any out of date
    /// dependencies.
    fn update_cell(&self, cell_id: Cell) {
        let cell = self.cell(cell_id);
        let version = self.version.load(Ordering::SeqCst);

        while cell.last_verified_version() != version {
            // if any dependency may have changed, update
            if !self.is_stale_cell(cell_id) {
                cell.set_last_verified_version(version);
                return;
            }

            match cell.lock.try_lock() {
                Some(guard) => {
                    // Another thread may have finished running this cell before we took the lock
                    if cell.last_verified_version() != version {
                        self.run_compute_function(cell_id);
                    }
                    drop(guard);
                    return;
                }
                None => {
                    // This computation is already being run in another thread.
                    // Before blocking and waiting, since we have time, check for a cycle and
                    // issue and panic if found.
                    self.check_for_cycle(cell_id);

                    // Block until it finishes, then loop in case that thread panicked before finishing
                    drop(cell.lock.lock());
                }
            }
        }
    }

    /// Perform a DFS to check for a cycle, panicking if found
    fn check_for_cycle(&self, starting_cell: Cell) {
        let mut visited = FxHashSet::default();
        let mut path = Vec::new();

        // We're going to push actions to this stack. Most actions will be pushing
        // a dependency cell to track as the next node in the graph, but some will be
        // pop actions for popping the top node off the current path. If we encounter
        // a node which is already in the current path, we have found a cycle.
        let mut stack = Vec::new();
        stack.push(Action::Traverse(starting_cell));

        enum Action {
            Traverse(Cell),
            Pop(Cell),
        }

        while let Some(action) = stack.pop() {
            match action {
                // This assert_eq is never expected to fail
                Action::Pop(expected) => assert_eq!(path.pop(), Some(expected)),
                Action::Traverse(cell) => {
                    if path.contains(&cell) {
                        // Include the same cell twice so the cycle is more clear to users
                        path.push(cell);
                        self.cycle_error(&path);
                    }

                    if visited.insert(cell) {
                        path.push(cell);
                        stack.push(Action::Pop(cell));
                        self.cell(cell).for_each_dependency(|dependency| {
                            stack.push(Action::Traverse(*dependency));
                        });
                    }
                }
            }
        }
    }

    /// Issue an error with the given cycle
    fn cycle_error(&self, cycle: &[Cell]) {
        let mut error = String::new();
        for (i, cell) in cycle.iter().enumerate() {
            error += &format!(
                "\n  {}. {}",
                i + 1,
                self.storage.input_debug_string(self, *cell)
            );
        }
        panic!("inc-complete: Cycle Detected!\n\nCycle:{error}")
    }

    /// Retrieves the up to date value for the given computation, re-running any dependencies as
    /// necessary.
    ///
    /// This function can panic if the dynamic type of the value returned by `compute.run(..)` is not `T`.
    ///
    /// Locking behavior: This function locks the cell corresponding to the given computation. This
    /// can cause a deadlock if the computation recursively depends on itself.
    pub fn get<C: Computation>(&self, compute: C) -> C::Output
    where
        S: StorageFor<C>,
    {
        let cell_id = self.get_or_insert_cell(compute);
        self.get_with_cell::<C>(cell_id)
    }

    pub(crate) fn get_with_cell<Concrete: Computation>(&self, cell_id: Cell) -> Concrete::Output
    where
        S: StorageFor<Concrete>,
    {
        self.update_cell(cell_id);

        self.storage
            .get_output(cell_id)
            .expect("cell result should have been computed already")
    }

    /// Retrieve each accumulated value of the given type after the given computation is run.
    ///
    /// This is most often used for operations like retrieving diagnostics or logs.
    ///
    /// Compared to [Db::get_accumulated_uncached], this version reuses the normal flow for
    /// queries and thus saves accumulated values for each intermediate query. This involves
    /// more synching and data duplication but can be beneficial if intermediate results
    /// ever need to be reused, e.g. if you call [Db::get_accumulated] in a loop where each
    /// call may share dependencies. If you already have a single query which emits all the
    /// accumulated values you need, [Db::get_accumulated_uncached] is likely faster, but
    /// requires a `&mut Db`.
    pub fn get_accumulated<Item, C>(&self, compute: C) -> BTreeSet<Item>
    where
        S: StorageFor<C> + StorageFor<Accumulated<Item>>,
        C: Computation,
        Item: 'static,
    {
        let cell_id = self.get_or_insert_cell(compute);
        self.update_cell(cell_id);
        self.get(Accumulated::<Item>::new(cell_id))
    }

    /// Retrieve each accumulated value of the given type after the given computation is run.
    ///
    /// This is most often used for operations like retrieving diagnostics or logs.
    ///
    /// This is a faster version of [Db::get_accumulated] for some use-cases. This version tends to be
    /// more efficient when you already have a single query which emits all the accumulated values
    /// you need, while the original [Db::get_accumulated] is more efficient when you have many
    /// smaller calls since it avoids duplicated work and is safe to call with only a [DbHandle].
    pub fn get_accumulated_uncached<Item, C>(&mut self, compute: C) -> BTreeSet<Item>
    where
        S: StorageFor<C> + StorageFor<Accumulated<Item>> + Accumulate<Item>,
        C: Computation,
        Item: 'static + Ord,
    {
        let cell_id = self.get_or_insert_cell(compute);
        self.update_cell(cell_id);

        let mut items = BTreeSet::new();
        let mut queue = vec![cell_id];

        let mut visited = vec![false; self.next_cell.load(Ordering::Relaxed) as usize];
        visited[cell_id.index() as usize] = true;

        while let Some(cell) = queue.pop() {
            self.cell(cell).for_each_dependency(|dependency| {
                let seen = &mut visited[dependency.index() as usize];
                if !*seen {
                    *seen = true;
                    queue.push(*dependency);
                }
            });
            items.extend(self.storage().get_accumulated::<Vec<Item>>(cell));
        }

        items
    }
}
