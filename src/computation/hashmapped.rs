use std::collections::HashMap;

use crate::{Cell, DbHandle};
use std::hash::Hash;

use super::Computation;

/// A helper type for defining Computations with HashMap-backed storage
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HashMapStorage<T>(T);

impl<T> HashMapStorage<T> {
    pub fn new(value: T) -> Self {
        Self(value)
    }
}

impl<T> Computation for HashMapStorage<T>
where
    T: Computation + Eq + Hash,
{
    type Output = <T as Computation>::Output;
    type Storage = (
        HashMap<Self, Cell>,
        HashMap<Cell, (Self, Option<Self::Output>)>,
    );

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
        self.0.run(handle)
    }

    fn input_to_cell(input: &Self, (self_to_cell, _): &Self::Storage) -> Option<Cell> {
        self_to_cell.get(input).copied()
    }

    fn get_function_and_output(
        cell: Cell,
        (_, cell_to_output): &Self::Storage,
    ) -> (&Self, Option<&Self::Output>) {
        let (this, output) = &cell_to_output[&cell];
        (this, output.as_ref())
    }

    fn set_output(cell: Cell, new_output: Self::Output, (_, cell_to_output): &mut Self::Storage) {
        cell_to_output.entry(cell).and_modify(|(_, output)| {
            *output = Some(new_output);
        });
    }

    fn insert_new_cell(cell: Cell, function: Self, storage: &mut Self::Storage) {
        storage.0.insert(function.clone(), cell);
        storage.1.insert(cell, (function, None));
    }
}
