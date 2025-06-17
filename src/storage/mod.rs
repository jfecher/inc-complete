use crate::{Cell, DbHandle};

mod btreemapped;
mod hashmapped;
mod singleton;
mod macros;
mod tuple_impls;

pub use btreemapped::BTreeMapStorage;
pub use hashmapped::HashMapStorage;
pub use singleton::SingletonStorage;

pub trait Storage: Sized {
    fn output_is_unset(&self, cell: Cell, computation_id: u32) -> bool;

    /// Run a computation, returning true if the result changed from its previous value.
    fn run_computation(db: &mut DbHandle<Self>, cell: Cell, computation_id: u32) -> bool;
}

pub trait StorageFor<C: OutputType> {
    /// Given a computation key, return the cell associated with it, if it exists.
    fn get_cell_for_computation(&self, key: &C) -> Option<Cell>;

    /// Insert a new Cell with the given computation that has yet to be run
    fn insert_new_cell(&mut self, cell: Cell, key: C);

    /// Retrieve the input for this computation.
    /// The input is expected to already be inserted into this storage.
    fn get_input(&self, cell: Cell) -> &C;

    /// Retrieve the output for the given cell, if it exists
    fn get_output(&self, cell: Cell) -> Option<&C::Output>;

    /// `C` has been re-run and has returned the output `new_value`, return `true`
    /// if `new_value` has changed from its previous value, and cache the new value
    /// if needed.
    fn update_output(&mut self, cell: Cell, new_value: C::Output) -> bool;
}

pub trait ComputationId {
    fn computation_id() -> u32;
}

pub trait OutputType {
    type Output;
}

pub trait Run<Storage>: OutputType {
    fn run(&self, db: &mut DbHandle<Storage>) -> Self::Output;
}
