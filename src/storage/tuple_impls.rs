#[macro_export]
macro_rules! impl_storage {
    ($typ:ty, $( $field:ident : $computation_type:ty ),* $(, )? ) => {
        impl $crate::Storage for $typ {
            fn output_is_unset(&self, cell: $crate::Cell, computation_id: u32) -> bool {
                use $crate::StorageFor;
                match computation_id {
                    $(
                        x if x == <$computation_type as $crate::ComputationId>::computation_id() => {
                            self.$field.get_output(cell).is_none()
                        },
                    )*
                    id => panic!("Unknown computation id: {id}"),
                }
            }

            fn run_computation(db: &mut DbHandle<Self>, cell: $crate::Cell, computation_id: u32) -> bool {
                use $crate::StorageFor;
                match computation_id {
                    $(
                        x if x == <$computation_type as $crate::ComputationId>::computation_id() => {
                            let new_value = db.storage().$field.get_input(cell).clone().run(db);
                            db.storage_mut().$field.update_output(cell, new_value)
                        }
                    )*
                    id => panic!("Unknown computation id: {id}"),
                }
            }
        }

        $(
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
        })*
    };
}
