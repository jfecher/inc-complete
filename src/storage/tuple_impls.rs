#[macro_export]
macro_rules! impl_storage_for_field {
    ($typ:ty, $field:ident, $computation_type:ty) => {
        impl $crate::StorageFor<$computation_type> for $typ {
            fn get_cell_for_computation(&self, key: &$computation_type) -> Option<$crate::Cell> {
                self.$field.get_cell_for_computation(key)
            }

            fn insert_new_cell(&mut self, cell: $crate::Cell, key: $computation_type) {
                self.$field.insert_new_cell(cell, key)
            }

            fn get_input(&self, cell: $crate::Cell) -> &$computation_type {
                self.$field.get_input(cell)
            }

            fn get_output(&self, cell: $crate::Cell) -> Option<&<$computation_type as $crate::OutputType>::Output> {
                self.$field.get_output(cell)
            }

            fn update_output(&mut self, cell: $crate::Cell, new_value: <$computation_type as $crate::OutputType>::Output) -> bool {
                self.$field.update_output(cell, new_value)
            }
        }
    };
}
