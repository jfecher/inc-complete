/// Helper macro to define an intermediate computation type. This will
/// define the type for you along with a `new` method, `Run` impl and
/// a function wrapper for `db.get(ComputationType::new())`.
///
/// Example usage:
/// ```
/// # use inc_complete::{ define_intermediate, DbHandle, Computation };
/// # use std::rc::Rc;
/// # #[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
/// # struct Expr;
/// # #[derive(Debug, PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
/// # struct Type;
/// # #[derive(Debug, PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
/// # struct Error;
/// // Defines the `Parse` type, a `parse` function to get the current parse
/// // or re-parse if it is out of date, the output type String, and parse_impl
/// // which actually performs the parse and returns a String.
/// define_intermediate!(Parse, parse, String, parse_impl);
///
/// fn parse_impl(db: &mut DbHandle<impl Computation>) -> String { todo!() }
///
/// // You can also specify arguments to the type, prefixing each with `&` if it
/// // is to be passed by reference to `parse_impl`.
/// // The output type can also be prefixed with `cloned` if the getter should
/// // return it cloned instead of a reference
/// define_intermediate!(TypeCheck { &expr: Rc<Expr> }, type_check, cloned Result<Type, Error>, type_check_impl);
///
/// fn type_check_impl(expr: &Rc<Expr>, db: &mut DbHandle<impl Computation>) -> Result<Type, Error> { todo!() }
/// ```
#[macro_export]
macro_rules! define_intermediate {
    // No `cloned` on output_type
    ( $( #$attrs:tt )?
      $type_name:ident $( { $($fields:tt)* } )? ,
      $get_function_name:ident, $output_type:ty, $impl_function:expr) => {
        define_intermediate!(@inner $(#$attrs)? $type_name $( { $( $fields )* } )?, $get_function_name, $output_type, $impl_function; (|x|x); &'db);
    };
    // `cloned` on output_type
    ( $( #$attrs:tt )?
      $type_name:ident $( { $($fields:tt)* } )? ,
      $get_function_name:ident, cloned $output_type:ty, $impl_function:expr) => {
        define_intermediate!(@inner $(#$attrs)? $type_name $( { $($fields)* } )?, $get_function_name, $output_type, $impl_function; Clone::clone;);
    };
    (@inner $( #$attrs:tt )?
      $type_name:ident $( { $( $($(@$if_ref:tt)? &)? $field_name:ident : $field_type:ty),* } )? ,
      $get_function_name:ident, $output_type:ty, $impl_function:expr; $clone_function:expr ; $($output_ref:tt)* ) => {

        $crate::paste::paste! {
            #[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
            $(#$attrs)?
            pub(crate) struct [<$type_name Internal>] { $( $( $field_name : $field_type ),* )? }

            pub(crate) type $type_name = $crate::HashMapStorage<$crate::Intermediate<[<$type_name Internal>]>>;

            // Constructor function
            #[allow(non_snake_case)]
            #[allow(unused)]
            pub(crate) fn $type_name($( $( $field_name : $field_type, )* )?) -> $type_name {
                $crate::HashMapStorage::new($crate::Intermediate::new([<$type_name Internal>] { $( $( $field_name ),* )? }))
            }

            // Query function with DbHandle
            #[allow(unused)]
            pub(crate) fn $get_function_name<'db>($( $( $field_name : $field_type, )* )? db: &'db mut $crate::DbHandle<impl $crate::Computation>) -> $($output_ref)? $output_type {
                $clone_function(db.get($type_name ( $( $( $field_name ),* )? )))
            }

            // TODO: Should create a trait to abstract over DbHandle and Db
            // Query function with Db
            #[allow(unused)]
            pub(crate) fn [<$get_function_name _db>]<'db>($( $( $field_name : $field_type, )* )? db: &'db mut $crate::Db<impl $crate::Computation>) -> $($output_ref)? $output_type {
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
///
/// Note that inputs must be set via `db.update_input(MyInputType, value)` before they are used.
///
/// Example usage:
/// ```
/// # use inc_complete::define_input;
/// # #[derive(Debug, PartialEq, Eq, Hash, Clone)]
/// # struct Expr;
/// # #[derive(Debug, PartialEq, Eq, Hash, Clone)]
/// # struct Type;
/// # #[derive(Debug, PartialEq, Eq, Hash, Clone)]
/// # struct Error;
/// // Defines the `SourceFile` type, a `get_source` function to get the current
/// // source file, and the output type String.
/// define_input!(SourceFile, get_source, String);
///
/// // Inputs can have arguments on their type as well.
/// // The output type can also be prefixed with `cloned` if the getter should
/// // return it cloned instead of a reference
/// define_input!(SourceFile2 { file_id: u32 }, get_source2, cloned String);
/// ```
#[macro_export]
macro_rules! define_input {
    // No `cloned` on output_type
    ( $( #$attrs:tt )?
      $type_name:ident $( { $($fields:tt)* } )? ,
      $get_function_name:ident, $output_type:ty) => {
        define_input!(@inner $(#$attrs)? $type_name $( { $($fields)* } )?, $get_function_name, $output_type; (|x|x); &'db);
    };
    // `cloned` on output_type
    ( $( #$attrs:tt )?
      $type_name:ident $( { $($fields:tt)* } )? ,
      $get_function_name:ident, cloned $output_type:ty) => {
        define_input!(@inner $(#$attrs)? $type_name $( { $($fields)* } )?, $get_function_name, $output_type; Clone::clone;);
    };

    ( @inner $( #$attrs:tt )?
      $type_name:ident $( { $( $field_name:ident : $field_type:ty),* } )? ,
      $get_function_name:ident, $output_type:ty ; $clone_function:expr ; $($output_ref:tt)* ) => {

        $crate::paste::paste! {
            #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
            $( #$attrs )?
            pub(crate) struct [<$type_name Internal>] { $( $( $field_name : $field_type ),* )? }

            pub(crate) type $type_name = $crate::HashMapStorage<$crate::Input<[<$type_name Internal>]>>;

            // Constructor function
            #[allow(non_snake_case)]
            #[allow(unused)]
            pub(crate) fn $type_name($( $( $field_name : $field_type, )* )?) -> $type_name {
                $crate::HashMapStorage::new($crate::Input::new([<$type_name Internal>] { $( $( $field_name ),* )? }))
            }

            // Query function with DbHandle
            #[allow(unused)]
            pub(crate) fn $get_function_name<'db>($( $( $field_name : $field_type, )* )? db: &'db mut $crate::DbHandle<impl $crate::Computation>) -> $($output_ref)* $output_type {
                let result = db.get($type_name ($( $( $field_name ),* )? ));
                $clone_function(result)
            }

            // TODO: Should create a trait to abstract over DbHandle and Db
            // Query function with Db
            #[allow(unused)]
            pub(crate) fn [<$get_function_name _db>]<'db>($( $( $field_name : $field_type, )* )? db: &'db mut $crate::Db<impl $crate::Computation>) -> $($output_ref)? $output_type {
                let result = db.get($type_name ($( $( $field_name ),* )? ));
                $clone_function(result)
            }

            impl $crate::OutputTypeForInput for [<$type_name Internal>] {
                type Output = $output_type;
            }
        }
    };
}
