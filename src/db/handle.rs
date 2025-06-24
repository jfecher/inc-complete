use crate::{
    Cell, Db, OutputType, Storage,
    storage::{ComputationId, StorageFor},
};

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
        db.cells
            .get(&current_operation)
            .unwrap()
            .dependencies
            .clear();

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

impl<'db, S: Storage> DbHandle<'db, S> {
    #[cfg(not(feature = "async"))]
    pub fn get<C: OutputType + ComputationId>(&self, compute: C) -> C::Output
    where
        S: StorageFor<C>,
    {
        // Register the dependency
        let dependency = self.db.get_or_insert_cell(compute);
        let mut cell = self.db.cells.get(&self.current_operation).unwrap();
        cell.dependencies.push(dependency);
        drop(cell);

        // Fetch the current value of the dependency
        self.db.get_with_cell(dependency)
    }

    #[cfg(feature = "async")]
    pub fn get<C: OutputType + ComputationId>(&self, compute: C) -> impl Future<Output = C::Output> + Send
    where
        S: StorageFor<C> + Sync,
    {
        // Register the dependency
        let dependency = self.db.get_or_insert_cell(compute);
        let mut cell = self.db.cells.get_mut(&self.current_operation).unwrap();
        cell.dependencies.push(dependency);
        drop(cell);

        // Fetch the current value of the dependency
        self.db.get_with_cell(dependency)
    }
}
