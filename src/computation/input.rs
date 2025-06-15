use crate::{Cell, DbHandle};

use super::Computation;

/// Helper to define a Computation for an input type which has no dependencies
/// and thus requires an explicit update from `db.update_input` to be initialized
/// instead of a `run` function.
///
/// When using an input in a Computation tuple, you still need to decide on a storage
/// type wrapper. An example would be `HashMapStorage<Input<T>>`.
///
/// Examples include `struct FileContents { text: String };` or `struct Time;`
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Input<T>(T);

impl<T> Input<T> {
    pub const fn new(value: T) -> Self {
        Self(value)
    }
}

pub trait OutputTypeForInput: Clone {
    type Output: Eq;
}

impl<T: OutputTypeForInput + 'static> Computation for Input<T> {
    type Output = <T as OutputTypeForInput>::Output;
    type Storage = (Option<Cell>, Option<Self::Output>);

    fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
        panic!(
            "Input `{}` queried before `db.update_input(..)` called",
            std::any::type_name::<T>()
        )
    }

    fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
        panic!(
            "Input used without a storage type wrapper - try wrapping your type with `HashMapStorage<Input<T>>`"
        )
    }

    fn get_function_and_output(_: Cell, _: &Self::Storage) -> (&Self, Option<&Self::Output>) {
        panic!(
            "Input used without a storage type wrapper - try wrapping your type with `HashMapStorage<Input<T>>`"
        )
    }

    fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
        panic!(
            "Input used without a storage type wrapper - try wrapping your type with `HashMapStorage<Input<T>>`"
        )
    }

    fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
        panic!(
            "Input used without a storage type wrapper - try wrapping your type with `HashMapStorage<Input<T>>`"
        )
    }
}
