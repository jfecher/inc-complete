/// Helper macro to define an intermediate computation type.
/// This will implement `OutputType`, `ComputationId`, and `Run`.
///
/// This macro supports multiple `Storage` types used, separated by `|`,
/// in case your program uses multiple databases with differing storage types.
///
/// Signature:
/// `define_intermediate!(computation_id, ComputationType -> OutputType, StorageType ( | MoreStorageTypes)*, run_function)`
///
/// Example usage:
/// ```
/// # use inc_complete::{ define_intermediate, define_input, storage::SingletonStorage, impl_storage, DbHandle };
/// # struct MyStorageType { input: SingletonStorage<MyInput>, double: SingletonStorage<Double> }
/// # #[derive(Clone)]
/// # struct MyInput;
/// # define_input!(0, MyInput -> i32, MyStorageType);
/// # impl_storage!(MyStorageType, input:MyInput, double:Double);
/// ##[derive(Clone)]
/// struct Double;
///
/// // Define `Double` as a computation with id 1 and the given run function which returns an `i32`
/// // to be used with a `Db<MyStorageType>` or `DbHandle<MyStorageType>`.
/// // The type annotations on the closure are unnecessary.
/// // We also may provide an existing function instead of a closure.
/// define_intermediate!(1, Double -> i32, MyStorageType, |_: &Double, db: &DbHandle<MyStorageType>| {
///     db.get(MyInput) * 2
/// });
/// ```
#[cfg(not(feature = "async"))]
#[macro_export]
macro_rules! define_intermediate {
    ( $id:tt, $type_name:ident -> $output_type:ty, $( $storage_type:ty )|+, $run_function:expr) => {
        impl $crate::OutputType for $type_name {
            type Output = $output_type;
        }

        impl $crate::ComputationId for $type_name {
            fn computation_id() -> u32 {
                $id
            }
        }

        $(
        impl $crate::Run<$storage_type> for $type_name {
            fn run(&self, db: &$crate::DbHandle<$storage_type>) -> $output_type {
                // The type annotation here makes it so that users don't have to annotate
                // the arguments of `run_function`.
                let f: fn(&Self, &$crate::DbHandle<$storage_type>) -> $output_type =
                    $run_function;
                f(self, db)
            }
        }
        )+
    };
}

#[cfg(feature = "async")]
#[macro_export]
macro_rules! define_intermediate {
    ( $id:tt, $type_name:ident -> $output_type:ty, $( $storage_type:ty )|+, $run_function:expr) => {
        impl $crate::OutputType for $type_name {
            type Output = $output_type;
        }

        impl $crate::ComputationId for $type_name {
            fn computation_id() -> u32 {
                $id
            }
        }

        $(
        impl $crate::Run<$storage_type> for $type_name {
            fn run<'db>(&self, db: &$crate::DbHandle<'db, $storage_type>) -> impl Future<Output = $output_type> {
                $run_function(self, db)
            }
        }
        )+
    };
}

/// Helper macro to define an input computation type.
/// This will implement `OutputType`, `ComputationId`, and `Run`.
/// Note that the `Run` implementation will panic by default with a message that
/// `update_input` should have been called beforehand.
///
/// This macro supports multiple `Storage` types used, separated by `|`,
/// in case your program uses multiple databases with differing storage types.
///
/// Signature:
/// `define_input!(computation_id, ComputationType -> OutputType, StorageType ( | MoreStorageTypes)* )`
///
/// Example usage:
/// ```
/// # use inc_complete::{ define_intermediate, define_input, storage::SingletonStorage, impl_storage, DbHandle };
/// # struct MyStorageType { input: SingletonStorage<MyInput>, double: SingletonStorage<Double> }
/// # impl_storage!(MyStorageType, input:MyInput,double:Double);
/// # #[derive(Clone)]
/// # struct Double;
/// # define_intermediate!(1, Double -> i32, MyStorageType, |_: &Double, db: &DbHandle<MyStorageType>| {
/// #     db.get(MyInput) * 2
/// # });
/// ##[derive(Clone)]
/// struct MyInput;
///
/// // Define `MyInput` as an input computation with id 0 and an `i32` value
/// // which can be used with a `Db<MyStorageType>`.
/// define_input!(0, MyInput -> i32, MyStorageType);
/// ```
#[macro_export]
macro_rules! define_input {
    ( $id:tt, $type_name:ident -> $output_type:ty, $( $storage_type:ty )|+ ) => {
        impl $crate::OutputType for $type_name {
            type Output = $output_type;
        }

        impl $crate::ComputationId for $type_name {
            fn computation_id() -> u32 {
                $id
            }
        }

        $(
        impl $crate::Run<$storage_type> for $type_name {
            fn run(&self, _: &$crate::DbHandle<$storage_type>) -> $output_type {
                panic!("Attempted to call `run` function on input {}, did you forget to call `update_input`?",
                    stringify!($type_name))
            }
        }
        )+
    };
}

/// Implements `Storage` for a struct type. This enables the given struct type `S` to be used
/// as a generic on `Db<S>` to store _all_ computations cached by the program.
///
/// This will also create forwarding impls for `StorageFor<ComputationType>` for each field,
/// computation type pair used.
///
/// Example usage:
/// ```
/// use inc_complete::{ impl_storage, define_input, define_intermediate };
/// use inc_complete::storage::{ SingletonStorage, HashMapStorage };
///
/// ##[derive(Default)]
/// struct MyStorage {
///     foos: SingletonStorage<Foo>,
///     bars: HashMapStorage<Bar>,
/// }
///
/// impl_storage!(MyStorage,
///     foos: Foo,
///     bars: Bar,
/// );
///
/// // Each input & intermediate computation should implement Clone
/// ##[derive(Clone)]
/// struct Foo;
/// define_input!(0, Foo -> usize, MyStorage);
///
/// // HashMapStorage requires Eq and Hash
/// ##[derive(Clone, PartialEq, Eq, Hash)]
/// struct Bar(std::rc::Rc<String>);
/// define_intermediate!(1, Bar -> usize, MyStorage, |bar, db| {
///     bar.0.len() + db.get(Foo)
/// });
/// ```
///
/// Note that using this macro requires each computation type to implement `Clone`.
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

            $crate::run_computation!( $($field: $computation_type),* );
        }

        $(
        impl $crate::StorageFor<$computation_type> for $typ {
            fn get_cell_for_computation(&self, key: &$computation_type) -> Option<$crate::Cell> {
                self.$field.get_cell_for_computation(key)
            }

            fn insert_new_cell(&self, cell: $crate::Cell, key: $computation_type) {
                self.$field.insert_new_cell(cell, key)
            }

            fn get_input(&self, cell: $crate::Cell) -> $computation_type {
                self.$field.get_input(cell)
            }

            fn get_output(&self, cell: $crate::Cell) -> Option<<$computation_type as $crate::OutputType>::Output> {
                self.$field.get_output(cell)
            }

            fn update_output(&self, cell: $crate::Cell, new_value: <$computation_type as $crate::OutputType>::Output) -> bool {
                self.$field.update_output(cell, new_value)
            }
        })*
    };
}

#[cfg(not(feature = "async"))]
#[doc(hidden)]
#[macro_export]
macro_rules! run_computation {
    ( $($field:ident: $computation_type:ty),* ) => {
        fn run_computation(db: &$crate::DbHandle<Self>, cell: $crate::Cell, computation_id: u32) -> bool {
            use $crate::{ StorageFor, Run };
            match computation_id {
                $(
                    x if x == <$computation_type as $crate::ComputationId>::computation_id() => {
                        let new_value = db.storage().$field.get_input(cell).run(db);
                        db.storage().$field.update_output(cell, new_value)
                    }
                )*
                id => panic!("Unknown computation id: {id}"),
            }
        }
    }
}

#[cfg(feature = "async")]
#[doc(hidden)]
#[macro_export]
macro_rules! run_computation {
    ( $($field:ident: $computation_type:ty),* ) => {
        async fn run_computation<'db>(db: &$crate::DbHandle<'db, Self>, cell: $crate::Cell, computation_id: u32) -> bool {
            use $crate::{ StorageFor, Run };
            match computation_id {
                $(
                    x if x == <$computation_type as $crate::ComputationId>::computation_id() => {
                        let new_value = db.storage().$field.get_input(cell).run(db).await;
                        db.storage().$field.update_output(cell, new_value)
                    }
                )*
                id => panic!("Unknown computation id: {id}"),
            }
        }
    }
}
