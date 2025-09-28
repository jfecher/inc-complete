use crate::{Cell, DbHandle};

mod hashmapped;
mod indexmapped;
mod macros;
mod singleton;

pub use hashmapped::HashMapStorage;
pub use indexmapped::TreeIndexStorage;
pub use singleton::SingletonStorage;

/// The Storage trait is implemented on a type which can cache all of the computations
/// used in the program (or a subset of it). These types are typically composed of
/// several fields with each field implementing `StorageFor<T>` where `T` is one
/// computation type. Note that each computation type must be unique within a `Storage` type.
///
/// This trait is most often automatically implemented by `impl_storage!`, see the documentation
/// on that macro for usage details.
///
/// Note that during serialization, the entire Storage is serialized along with the `Db` object.
/// To achieve backwards-compatible serialization even when new fields for new computation types
/// are added, it is recommended to use `#[serde(default)]` on any newly-added fields to still
/// be able to deserialize from older versions without that field.
pub trait Storage: Sized {
    /// For the computation type with the given computation id, return true if the
    /// output with the given Cell has not yet been set.
    fn output_is_unset(&self, cell: Cell, computation_id: u32) -> bool;

    /// For the computation type with the given computation id, run the computation
    /// with the corresponding Cell, returning true if the result changed from its previous value.
    fn run_computation(db: &DbHandle<Self>, cell: Cell, computation_id: u32) -> bool;

    /// Shrink the storage by removing any data not in use by the given cells
    fn gc(&mut self, used_cells: &std::collections::HashSet<Cell>);

    /// Return a Debug String representation of the Computation referred to by the given cell.
    ///
    /// Used by inc-complete to label components of a cycle when a cycle is detected.
    fn input_debug_string(&self, cell: Cell) -> String;
}

/// This trait is implemented by a type storing a single computation type `C`.
/// Examples include `HashMapStorage<C>`, `SingletonStorage<C>`, and `BTreeMapStorage<C>`.
///
/// To implement this efficiently, most types implementing this are two-way maps
/// from `C` to `Cell` and from `Cell` to `(C, Option<C::Output>)`.
pub trait StorageFor<C: Computation> {
    /// Given a computation key, return the cell associated with it, if it exists.
    fn get_cell_for_computation(&self, key: &C) -> Option<Cell>;

    /// Insert a new Cell with the given computation that has yet to be run
    fn insert_new_cell(&self, cell: Cell, key: C);

    /// Retrieve the input for this computation, returning `None` if not found.
    fn try_get_input(&self, cell: Cell) -> Option<C>;

    /// Retrieve the input for this computation.
    /// The input is expected to already be inserted into this storage.
    fn get_input(&self, cell: Cell) -> C {
        self.try_get_input(cell).expect("inc-complete internal error: get_input: expected input to be set")
    }

    /// Retrieve the output for the given cell, if it exists
    fn get_output(&self, cell: Cell) -> Option<C::Output>;

    /// `C` has been re-run and has returned the output `new_value`, return `true`
    /// if `new_value` has changed from its previous value, and cache the new value
    /// if needed. If `C::ASSUME_CHANGED` is true, skip the comparison and assume the value changed.
    fn update_output(&self, cell: Cell, new_value: C::Output) -> bool;

    fn gc(&mut self, used_cells: &std::collections::HashSet<Cell>);
}

pub trait Computation: std::fmt::Debug {
    type Output;
    const IS_INPUT: bool;
    const ASSUME_CHANGED: bool;

    fn computation_id() -> u32;
}

pub trait Run<Storage>: Computation {
    fn run(&self, db: &DbHandle<Storage>) -> Self::Output;
}
