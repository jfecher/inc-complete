use crate::{Cell, Db, Run};
use std::hash::Hash;

/// A handle to the database during some operation.
///
/// This wraps calls to the Db so that any `get` calls
/// will be automatically registered as dependencies of
/// the current operation.
pub struct DbHandle<'db, F> {
    db: &'db mut Db<F>,
    current_operation: Cell,
}

impl<'db, F: Run + Copy + Eq + Hash + Clone> DbHandle<'db, F> {
    pub(crate) fn new(db: &'db mut Db<F>, current_operation: Cell) -> Self {
        Self { db, current_operation }
    }

    pub fn get<T: 'static>(&mut self, compute: F) -> &T
        where 
            F: std::fmt::Debug
    {
        // Register the dependency
        let dependency = self.db.cell(compute);
        self.db.cells.update_edge(self.current_operation.0, dependency.0, ());

        // Fetch the current value of the dependency
        self.db.get(compute)
    }
}
