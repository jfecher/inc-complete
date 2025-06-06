use petgraph::visit::EdgeRef;

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

impl<'db, F: Run + Eq + Hash> DbHandle<'db, F> {
    pub(crate) fn new(db: &'db mut Db<F>, current_operation: Cell) -> Self {
        // We're re-running a cell so remove any past dependencies
        let edges = db.cells.edges(current_operation.index())
            .map(|edge| edge.id())
            .collect::<Vec<_>>();

        for edge in edges {
            db.cells.remove_edge(edge);
        }
        Self { db, current_operation }
    }

    pub fn get<T: 'static>(&mut self, compute: F) -> &T
        where 
            F: std::fmt::Debug
    {
        // Register the dependency
        let dependency = self.db.get_or_insert_cell(compute);
        self.db.cells.update_edge(self.current_operation.index(), dependency.index(), ());

        // Fetch the current value of the dependency
        self.db.get_with_cell(dependency)
    }
}
