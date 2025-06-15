use crate::{Cell, DbHandle};

use super::Computation;

/// A helper type for defining intermediate Computations.
/// This will consist of any non-input in a program. These are
/// always functions which are cached and derive their value from
/// other functions or inputs.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Intermediate<T>(T);

impl<T> Intermediate<T> {
    pub const fn new(x: T) -> Self {
        Self(x)
    }
}

pub trait Run {
    type Output: Eq;

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output;
}

impl<T> Computation for Intermediate<T>
where
    T: Run + Clone + 'static,
{
    type Output = <T as Run>::Output;
    type Storage = ();

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
        self.0.run(handle)
    }

    fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
        panic!(
            "Intermediate used without a storage type wrapper - try wrapping your type with `HashMapStorage<Intermediate<T>>`"
        )
    }

    fn get_function_and_output(_: Cell, _: &Self::Storage) -> (&Self, Option<&Self::Output>) {
        panic!(
            "Intermediate used without a storage type wrapper - try wrapping your type with `HashMapStorage<Intermediate<T>>`"
        )
    }

    fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
        panic!(
            "Intermediate used without a storage type wrapper - try wrapping your type with `HashMapStorage<Intermediate<T>>`"
        )
    }

    fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
        panic!(
            "Intermediate used without a storage type wrapper - try wrapping your type with `HashMapStorage<Intermediate<T>>`"
        )
    }
}
