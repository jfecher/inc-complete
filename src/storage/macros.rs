/// Helper macro to define an intermediate computation type. This will
/// define the type for you along with a `new` method, `Run` impl and
/// a function wrapper for `db.get(ComputationType::new())`.
///
/// Example usage:
#[macro_export]
macro_rules! define_intermediate {
    ($type_name:ident, $output_type:ty, $id:tt, $storage_type:ty, $run_function:expr) => {
        impl $crate::OutputType for $type_name {
            type Output = $output_type;
        }

        impl $crate::ComputationId for $type_name {
            fn computation_id() -> u32 {
                $id
            }
        }

        impl $crate::Run<$storage_type> for $type_name {
            fn run(&self, db: &mut $crate::DbHandle<$storage_type>) -> $output_type {
                ($run_function)(self, db)
            }
        }
    };
}

/// Helper macro to define an input type. This will define the type for you along with
/// an `OutputTypeForInput` impl and a function wrapper for `db.get(ComputationType::new())`.
///
/// Note that inputs must be set via `db.update_input(MyInputType, value)` before they are used.
///
/// Example usage:
/// ```
#[macro_export]
macro_rules! define_input {
    ( $type_name:ident, $output_type:ty, $id:tt ) => {
        impl $crate::OutputType for $type_name {
            type Output = $output_type;
        }

        impl $crate::ComputationId for $type_name {
            fn computation_id() -> u32 {
                $id
            }
        }
    };
}
