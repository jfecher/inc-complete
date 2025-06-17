use petgraph::visit::EdgeRef;

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
    db: &'db mut Db<S>,
    current_operation: Cell,
}

impl<'db, S> DbHandle<'db, S> {
    pub(crate) fn new(db: &'db mut Db<S>, current_operation: Cell) -> Self {
        // We're re-running a cell so remove any past dependencies
        let edges = db
            .cells
            .edges(current_operation.index())
            .map(|edge| edge.id())
            .collect::<Vec<_>>();

        for edge in edges {
            db.cells.remove_edge(edge);
        }
        Self {
            db,
            current_operation,
        }
    }

    /// Retrieve an immutable reference to this `Db`'s storage
    pub fn storage(&self) -> &S {
        self.db.storage()
    }

    /// Retrieve a mutable reference to the internal database's storage.
    ///
    /// Note that any mutations made to the storage using this are _not_ tracked by the database!
    /// Using this incorrectly may break correctness!
    pub fn storage_mut(&mut self) -> &mut S {
        self.db.storage_mut()
    }
}

impl<'db, S: Storage> DbHandle<'db, S> {
    pub fn get<C: OutputType + ComputationId>(&mut self, compute: C) -> &C::Output
    where
        S: StorageFor<C>,
    {
        // Register the dependency
        let dependency = self.db.get_or_insert_cell(compute);
        self.db
            .cells
            .update_edge(self.current_operation.index(), dependency.index(), ());

        // Fetch the current value of the dependency
        self.db.get_with_cell(dependency)
    }
}
