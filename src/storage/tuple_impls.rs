use crate::Cell;

use super::{OutputType, StorageFor};

impl<A, SA, SB> StorageFor<A> for (SA, SB)
    where SA: StorageFor<A>, A: OutputType
{
    fn get_cell_for_computation(&self, key: &A) -> Option<Cell> {
        self.0.get_cell_for_computation(key)
    }

    fn insert_new_cell(&mut self, cell: Cell, key: A) {
        self.0.insert_new_cell(cell, key);
    }

    fn get_output(&self, cell: Cell) -> Option<&<A>::Output> {
        self.0.get_output(cell)
    }

    fn update_output(&mut self, cell: Cell, new_value: <A>::Output) -> bool {
        self.0.update_output(cell, new_value)
    }
}

impl<B, SA, SB> StorageFor<B> for (SA, SB)
    where SB: StorageFor<B>, B: OutputType
{
    fn get_cell_for_computation(&self, key: &B) -> Option<Cell> {
        self.1.get_cell_for_computation(key)
    }

    fn insert_new_cell(&mut self, cell: Cell, key: B) {
        self.1.insert_new_cell(cell, key);
    }

    fn get_output(&self, cell: Cell) -> Option<&<B>::Output> {
        self.1.get_output(cell)
    }

    fn update_output(&mut self, cell: Cell, new_value: <B>::Output) -> bool {
        self.1.update_output(cell, new_value)
    }
}
