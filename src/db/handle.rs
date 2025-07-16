use crate::{
    Cell, Db, OutputType, Storage,
    storage::{ComputationId, StorageFor},
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
        let mut cell = db.cells
            .get_mut(&current_operation)
            .unwrap();
        
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
    #[cfg(not(feature = "async"))]
    pub fn get<C: OutputType + ComputationId>(&self, compute: C) -> C::Output
    where
        S: StorageFor<C>,
    {
        // Register the dependency
        let dependency = self.db.get_or_insert_cell(compute);
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

        // Fetch the current value of the dependency
        let output = self.db.get_with_cell(dependency);

        let dependency = self.db.cells.get(&dependency).unwrap();
        let dependency_inputs = dependency.input_dependencies.clone();
        drop(dependency);

        let mut cell = self.db.cells.get_mut(&self.current_operation).unwrap();
        for input in dependency_inputs {
            cell.input_dependencies.insert(input);
        }

        output
    }

    #[cfg(feature = "async")]
    pub fn get<C: OutputType + ComputationId>(
        &self,
        compute: C,
    ) -> impl Future<Output = C::Output> + Send
    where
        S: StorageFor<C> + Sync,
    {
        // Register the dependency
        let dependency = self.db.get_or_insert_cell(compute);
        let mut cell = self.db.cells.get(&self.current_operation).unwrap();
        cell.dependencies.push(dependency);
        drop(cell);

        // Fetch the current value of the dependency
        self.db.get_with_cell(dependency)
    }
}

#[cfg(not(feature = "async"))]
impl<'db, S, C> DbGet<C> for DbHandle<'db, S>
where
    C: OutputType + ComputationId,
    S: Storage + StorageFor<C>,
{
    fn get(&self, key: C) -> C::Output {
        self.get(key)
    }
}

#[cfg(feature = "async")]
impl<'db, S, C> DbGet<C> for DbHandle<'db, S>
where
    C: OutputType + ComputationId,
    S: Storage + StorageFor<C> + Sync,
{
    fn get(&self, key: C) -> impl Future<Output = C::Output> + Send {
        self.get(key)
    }
}
