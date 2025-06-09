use petgraph::visit::EdgeRef;

use crate::{Cell, Computation, Db};

/// A handle to the database during some operation.
///
/// This wraps calls to the Db so that any `get` calls
/// will be automatically registered as dependencies of
/// the current operation.
pub struct DbHandle<'db, C: Computation> {
    db: &'db mut Db<C>,
    current_operation: Cell,
}

impl<'db, C: Computation> DbHandle<'db, C> {
    pub(crate) fn new(db: &'db mut Db<C>, current_operation: Cell) -> Self {
        // We're re-running a cell so remove any past dependencies
        let edges = db.cells.edges(current_operation.index())
            .map(|edge| edge.id())
            .collect::<Vec<_>>();

        for edge in edges {
            db.cells.remove_edge(edge);
        }
        Self { db, current_operation }
    }

    pub fn get<Concrete: Computation>(&mut self, compute: Concrete) -> &Concrete::Output
        where C::Output: Eq, C: Clone
    {
        // Register the dependency
        let dependency = self.db.get_or_insert_cell(compute);
        self.db.cells.update_edge(self.current_operation.index(), dependency.index(), ());

        // Fetch the current value of the dependency
        self.db.get_with_cell::<Concrete>(dependency)
    }
}
