use crate::{
    accumulate::Accumulate, storage::{ComputationId, StorageFor}, Cell, Db, OutputType, Storage
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
        let mut cell = db.cells.get_mut(&current_operation).unwrap();

        cell.dependencies.clear();
        cell.input_dependencies.clear();

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
    pub fn get<C: OutputType + ComputationId>(&self, compute: C) -> C::Output
    where
        S: StorageFor<C>,
    {
        // Register the dependency
        let dependency = self.db.get_or_insert_cell(compute);
        self.update_and_register_dependency(dependency);

        // Fetch the current value of the dependency, running it if out of date
        self.db.get_with_cell(dependency)
    }

    /// Registers the given cell as a dependency, running it and updating any required metadata
    fn update_and_register_dependency<C: OutputType + ComputationId>(&self, dependency: Cell)
    where
        S: StorageFor<C>,
    {
        let mut cell = self.db.cells.get_mut(&self.current_operation).unwrap();

        // If `dependency` is an input it must be remembered both as a dependency
        // and as an input dependency. Otherwise we cannot differentiate between
        // computations which directly depend on inputs and those that only indirectly
        // depend on them.
        cell.dependencies.push(dependency);
        if C::IS_INPUT {
            cell.input_dependencies.insert(dependency);
        }

        drop(cell);

        // Run the computation to update its dependencies before we query them afterward
        self.db.update_cell(dependency);

        let dependency = self.db.cells.get(&dependency).unwrap();
        let dependency_inputs = dependency.input_dependencies.clone();
        drop(dependency);

        let mut cell = self.db.cells.get_mut(&self.current_operation).unwrap();
        for input in dependency_inputs {
            cell.input_dependencies.insert(input);
        }
    }

    /// Accumulate an item in the current computation. This item can be retrieved along
    /// with all other accumulated items in this computation and its dependencies via
    /// a call to `get_accumulated`.
    ///
    /// This is most often used for operations like pushing diagnostics or logs.
    pub fn accumulate<Item>(&self, item: Item) where
        S: Accumulate<Item>
    {
        self.storage().accumulate(self.current_operation, item);
    }

    /// Retrieve an accumulated value in a container of the user's choice.
    /// This will return all the accumulated items after the given computation.
    ///
    /// Note that although this method will not re-perform the given computation,
    /// it will re-collect all the required accumulated items each time it is called,
    /// which may be costly for large dependency trees.
    ///
    /// This is most often used for operations like retrieving diagnostics or logs.
    ///
    /// FIXME: This method is private to the crate until the bug in tracking
    /// accumulated values is fixed (see src/db/tests/accumulated.rs). Use the
    /// version on a full `Db` in the meantime which does not require dependency tracking.
    #[allow(unused)]
    pub(crate) fn get_accumulated<Container, Item, C>(&self, compute: C) -> Container where
        Container: FromIterator<Item>,
        S: Accumulate<Item> + StorageFor<C>,
        C: OutputType + ComputationId
    {
        // Ensure the dependency is registered.
        let cell_id = self.db.get_or_insert_cell(compute);
        let _ = self.update_and_register_dependency(cell_id);

        let cells = self.db.collect_all_dependencies(cell_id);
        self.storage().get_accumulated(&cells)
    }
}

impl<'db, S, C> DbGet<C> for DbHandle<'db, S>
where
    C: OutputType + ComputationId,
    S: Storage + StorageFor<C>,
{
    fn get(&self, key: C) -> C::Output {
        self.get(key)
    }
}
