use crate::{Cell, DbHandle};

use super::Computation;

/// Helper to define a Computation for a simple input type which has no fields and thus
/// does not require a HashMap to cache each possible value.
///
/// Examples include `struct SourceFile;` or `struct Time;`
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SingletonStorage<T>(T);

impl<T> SingletonStorage<T> {
    pub fn new(value: T) -> Self {
        let bytes = std::mem::size_of::<T>();
        assert_eq!(
            bytes,
            0,
            "SingletonStorage only supports 0-sized types but `{}` is `{}` bytes large",
            std::any::type_name::<T>(),
            bytes
        );
        Self(value)
    }
}

impl<T: Computation> Computation for SingletonStorage<T> {
    type Output = <T as Computation>::Output;
    type Storage = (
        Option<Cell>,
        // Self is required here to return a reference to it in `get_function_and_output`
        Self,
        Option<Self::Output>,
    );

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
        self.0.run(handle)
    }

    fn input_to_cell(_: &Self, storage: &Self::Storage) -> Option<Cell> {
        storage.0
    }

    fn get_function_and_output(_: Cell, storage: &Self::Storage) -> (&Self, Option<&Self::Output>) {
        (&storage.1, storage.2.as_ref())
    }

    fn set_output(_: Cell, new_output: Self::Output, storage: &mut Self::Storage) {
        storage.2 = Some(new_output);
    }

    fn insert_new_cell(cell: Cell, this: Self, storage: &mut Self::Storage) {
        *storage = (Some(cell), this, None);
    }
}
