/// Helper macro to define an intermediate computation type. This will
/// define the type for you along with a `new` method, `Run` impl and
/// a function wrapper for `db.get(ComputationType::new())`.
#[macro_export]
macro_rules! define_intermediate {
    // No `cloned` on output_type
    ( $type_name:ident $( { $($fields:tt)* } )? , 
      $get_function_name:ident, $output_type:ty, $impl_function:expr) => {
        define_intermediate!(@inner $type_name $( { $( $fields )* } )?, $get_function_name, $output_type, $impl_function; (|x|x); &'db);
    };
    // `cloned` on output_type
    ( $type_name:ident $( { $($fields:tt)* } )? , 
      $get_function_name:ident, cloned $output_type:ty, $impl_function:expr) => {
        define_intermediate!(@inner $type_name $( { $($fields)* } )?, $get_function_name, $output_type, $impl_function; Clone::clone;);
    };
    (@inner $type_name:ident $( { $( $($(@$if_ref:tt)? &)? $field_name:ident : $field_type:ty),* } )? , 
      $get_function_name:ident, $output_type:ty, $impl_function:expr; $clone_function:expr ; $($output_ref:tt)* ) => {
        
        paste::paste! {
            #[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
            struct [<$type_name Internal>] { $( $( $field_name : $field_type ),* )? }

            type $type_name = $crate::HashMapStorage<$crate::Intermediate<[<$type_name Internal>]>>;

            // Constructor function
            #[allow(non_snake_case)]
            #[allow(unused)]
            fn $type_name($( $( $field_name : $field_type, )* )?) -> $type_name {
                $crate::HashMapStorage::new($crate::Intermediate::new([<$type_name Internal>] { $( $( $field_name ),* )? }))
            }

            // Query function with DbHandle
            #[allow(unused)]
            fn $get_function_name<'db>($( $( $field_name : $field_type, )* )? db: &'db mut $crate::DbHandle<impl $crate::Computation>) -> $($output_ref)? $output_type {
                $clone_function(db.get($type_name ( $( $( $field_name ),* )? )))
            }

            // TODO: Should create a trait to abstract over DbHandle and Db
            // Query function with Db
            #[allow(unused)]
            fn [<$get_function_name _db>]<'db>($( $( $field_name : $field_type, )* )? db: &'db mut $crate::Db<impl $crate::Computation>) -> $($output_ref)? $output_type {
                $clone_function(db.get($type_name ( $( $( $field_name ),* )? )))
            }

            impl $crate::Run for [<$type_name Internal>] {
                type Output = $output_type;

                fn run(&self, handle: &mut $crate::DbHandle<impl $crate::Computation>) -> Self::Output {
                    $impl_function($( $( $($($if_ref)? & )? self. $field_name, )*)? handle)
                }
            }
        }
    };
}

/// Helper macro to define an input type. This will define the type for you along with
/// an `OutputTypeForInput` impl and a function wrapper for `db.get(ComputationType::new())`.
#[macro_export]
macro_rules! define_input {
    // No `cloned` on output_type
    ( $type_name:ident $( { $($fields:tt)* } )? , 
      $get_function_name:ident, $output_type:ty) => {
        define_input!($type_name $( { $($fields)* } )?, $get_function_name, $output_type; (|x|x); &'db);
    };
    // `cloned` on output_type
    ( $type_name:ident $( { $($fields:tt)* } )? , 
      $get_function_name:ident, cloned $output_type:ty) => {
        define_input!($type_name $( { $($fields)* } )?, $get_function_name, $output_type; Clone::clone;);
    };

    ( $type_name:ident $( { $( $field_name:ident : $field_type:ty),* } )? , 
      $get_function_name:ident, $output_type:ty ; $clone_function:expr ; $($output_ref:tt)* ) => {
        
        paste::paste! {
            #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
            pub struct [<$type_name Internal>] { $( $( $field_name : $field_type ),* )? }

            pub type $type_name = $crate::HashMapStorage<$crate::Input<[<$type_name Internal>]>>;

            // Constructor function
            #[allow(non_snake_case)]
            #[allow(unused)]
            fn $type_name($( $( $field_name : $field_type, )* )?) -> $type_name {
                $crate::HashMapStorage::new($crate::Input::new([<$type_name Internal>] { $( $( $field_name ),* )? }))
            }

            // Query function with DbHandle
            #[allow(unused)]
            fn $get_function_name<'db>($( $( $field_name : $field_type, )* )? db: &'db mut $crate::DbHandle<impl $crate::Computation>) -> $($output_ref)* $output_type {
                let result = db.get($type_name ($( $( $field_name ),* )? ));
                $clone_function(result)
            }

            // TODO: Should create a trait to abstract over DbHandle and Db
            // Query function with Db
            #[allow(unused)]
            fn [<$get_function_name _db>]<'db>($( $( $field_name : $field_type, )* )? db: &'db mut $crate::Db<impl $crate::Computation>) -> $($output_ref)? $output_type {
                let result = db.get($type_name ($( $( $field_name ),* )? ));
                $clone_function(result)
            }

            impl $crate::OutputTypeForInput for [<$type_name Internal>] {
                type Output = $output_type;
            }
        }
    };
}
