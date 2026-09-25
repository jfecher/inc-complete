use std::collections::BTreeSet;

use crate::{
    Cell, Computation, Db, Storage,
    accumulate::{Accumulate, Accumulated},
    storage::StorageFor,
};

use super::DbGet;

/// A handle to the database during some operation.
///
/// This wraps calls to the Db so that any `get` calls
/// will be automatically registered as dependencies of
/// the current operation.
pub struct DbHandle<'db, S> {
    db: &'db Db<S>,
    current_operation: Cell,
}

impl<'db, S> DbHandle<'db, S> {
    pub(crate) fn new(db: &'db Db<S>, current_operation: Cell) -> Self {
        // We're re-running a cell so remove any past dependencies
        db.cell(current_operation).clear_dependencies();

        Self {
            db,
            current_operation,
        }
    }

    /// Retrieve an immutable reference to this `Db`'s storage
    ///
    /// Note that any mutations made to the storage using this are _not_ tracked by the database!
    /// Using this incorrectly may break correctness!
    pub fn storage(&self) -> &S {
        self.db.storage()
    }
}

impl<S: Storage> DbHandle<'_, S> {
    /// Locking behavior: This function locks the cell corresponding to the given computation. This
    /// can cause a deadlock if the computation recursively depends on itself.
    pub fn get<C: Computation>(&self, compute: C) -> C::Output
    where
        S: StorageFor<C>,
    {
        // Register the dependency, running it if out of date
        let dependency = self.db.get_or_insert_cell(compute);
        self.update_and_register_dependency_inner(dependency);

        self.storage()
            .get_output(dependency)
            .expect("cell result should have been computed already")
    }

    /// Registers the given cell as a dependency, running it and updating any required metadata
    fn update_and_register_dependency_inner(&self, dependency: Cell) {
        self.db
            .cell(self.current_operation)
            .add_dependency(dependency);

        // Run the computation to update its dependencies before we query them afterward
        self.db.update_cell(dependency);
    }

    /// Accumulate an item in the current computation. This item can be retrieved along
    /// with all other accumulated items in this computation and its dependencies via
    /// a call to `get_accumulated`.
    ///
    /// This is most often used for operations like pushing diagnostics or logs.
    pub fn accumulate<Item>(&self, item: Item)
    where
        S: Accumulate<Item>,
    {
        self.storage().accumulate(self.current_operation, item);
    }

    /// Retrieve an accumulated value in a container of the user's choice.
    /// This will return all the accumulated items after the given computation.
    ///
    /// This is most often used for operations like retrieving diagnostics or logs.
    pub fn get_accumulated<Item, C>(&self, compute: C) -> BTreeSet<Item>
    where
        C: Computation,
        Item: 'static + Ord,
        S: StorageFor<Accumulated<Item>> + StorageFor<C> + Accumulate<Item>,
    {
        let dependency = self.db.get_or_insert_cell(compute);
        self.get_accumulated_with_cell::<Item>(dependency)
    }

    /// Retrieve an accumulated value in a container of the user's choice.
    /// This will return all the accumulated items after the given computation.
    ///
    /// This is the implementation of the publically accessible `db.get(Accumulated::<Item>(MyComputation))`.
    ///
    /// This is most often used for operations like retrieving diagnostics or logs.
    pub(crate) fn get_accumulated_with_cell<Item>(&self, cell_id: Cell) -> BTreeSet<Item>
    where
        Item: 'static + Ord,
        S: StorageFor<Accumulated<Item>> + Accumulate<Item>,
    {
        self.update_and_register_dependency_inner(cell_id);
        let dependencies = self.db.cell(cell_id).dependencies();

        // Collect `Accumulator` results from each dependency. This should also ensure we
        // rerun this if any dependency changes, even if `cell_id` is updated such that it
        // uses different dependencies but its output remains the same.
        let computation_id = Accumulated::<Item>::computation_id();
        let mut result: BTreeSet<Item> = dependencies
            .into_iter()
            // Filter out `Accumulated<Item>` cells from the dep list. They exist for staleness
            // tracking only and must not be traversed for value collection, or we'd get duplicates.
            .filter(|&dep| self.db.cell(dep).computation_id != computation_id)
            .flat_map(|dependency| self.get(Accumulated::<Item>::new(dependency)))
            .collect();

        result.extend(self.storage().get_accumulated::<Vec<Item>>(cell_id));
        result
    }
}

impl<'db, S, C> DbGet<C> for DbHandle<'db, S>
where
    C: Computation,
    S: Storage + StorageFor<C>,
{
    fn get(&self, key: C) -> C::Output {
        self.get(key)
    }
}
