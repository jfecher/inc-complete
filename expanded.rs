#![feature(prelude_import)]
//! # Inc-Complete
//!
//! inc-complete is a library for incremental compilation supporting serialization from the ground
//! up.
//!
//! In inc-complete, a central `Db` object is used to query and cache the result of _pure_
//! functions. The functions being pure is key. If there are side-effects performed then
//! they will not be re-performed when the computation's result is later cached and returned again.
//!
//! Before we create the `Db` object however, we need to define a tuple of all the computations
//! we want to cache. In inc-complete, each computation is its own type and is either an input
//! (if it has no dependencies) or an intermediate computation. For this example we're going to
//! model the following spreadsheet:
//!
//! ```text
//!       [  A  ] [     B    ]
//! [ 1 ] [ 12  ] [ =A1 + 8  ]
//! [ 2 ] [  4  ] [ =B1 + A2 ]
//! ```
//!
//! We will have two inputs: `A1` and `A2`, and two intermediates: `B1` and `B2` where
//! `B1` depends on `A1` and `B2` depends on `B1` and `A2` directly, and `A1` transitively.
//! Let's start by defining these types:
//!
//! ```
//! #[derive(Clone, Debug, Default)]
//! struct A1;
//!
//! #[derive(Clone, Debug, Default)]
//! struct A2;
//!
//! #[derive(Clone, PartialEq, Eq, Hash, Default)]
//! struct B1;
//!
//! #[derive(Clone, PartialEq, Eq, Hash, Default)]
//! struct B2;
//! ```
//!
//! The derives are all necessary for some traits we'll implement later.
//!
//! Now we can define a type alias for the tuple containing all our computation types:
//!
//! ```
//! # #[derive(Clone, Debug, Default)]
//! # struct A1;
//! # #[derive(Clone, Debug, Default)]
//! # struct A2;
//! # #[derive(Clone, PartialEq, Eq, Hash, Default)]
//! # struct B1;
//! # #[derive(Clone, PartialEq, Eq, Hash, Default)]
//! # struct B2;
//! use inc_complete::{ Input, Intermediate, SingletonStorage };
//!
//! type Spreadsheet = (
//!     SingletonStorage<Input<A1>>,
//!     SingletonStorage<Input<A2>>,
//!     SingletonStorage<Intermediate<B1>>,
//!     SingletonStorage<Intermediate<B2>>,
//! );
//! ```
//!
//! Note that we have to tell inc-complete both how we want to store our computation cache and
//! whether the computation itself is an input or an intermediate computation derived from inputs or
//! other intermediates. In this example, we're using `SingletonStorage` for all of our
//! computations because all of `A1`, `A2`, `B1`, and `B2` are singleton values like `()` with
//! only a single value in their type. This lets us store them with an `Option<T>` instead of a
//! `HashMap<K, V>`. If you are unsure which storage type to choose, `HashMapStorage<T>` or
//! `BTreeMapStorage<T>` are good defaults. Even if used on singletons they will give you correct
//! behavior, just with slightly less performance than `SingletonStorage<T>`.
//!
//! Also note that the storage type wrapper goes on the outside of the `Input`/`Intermediate` type,
//! you'll get trait errors if you try to define them the other way around.
//!
//! Let's also take the time now to create some `new` functions so we don't have to construct these
//! wrappers each time:
//!
//! ```
//! # #[derive(Clone, Debug, Default)]
//! # struct A1;
//! # #[derive(Clone, Debug, Default)]
//! # struct A2;
//! # #[derive(Clone, PartialEq, Eq, Hash, Default)]
//! # struct B1;
//! # #[derive(Clone, PartialEq, Eq, Hash, Default)]
//! # struct B2;
//! # use inc_complete::{ Input, Intermediate, SingletonStorage };
//! impl A1 {
//!     fn new() -> SingletonStorage<Input<A1>> {
//!         Default::default()
//!     }
//! }
//!
//! impl A2 {
//!     fn new() -> SingletonStorage<Input<A2>> {
//!         Default::default()
//!     }
//! }
//!
//! impl B1 {
//!     fn new() -> SingletonStorage<Intermediate<B1>> {
//!         Default::default()
//!     }
//! }
//!
//! impl B2 {
//!     fn new() -> SingletonStorage<Intermediate<B2>> {
//!         Default::default()
//!     }
//! }
//! ```
//!
//! It's true that we can just call `Default::default` in each, but having the output type
//! be known helps for calls to `DbHandle::get::<T>(&mut self, computation: T)` which we'll see later.
//!
//! Next, for `Input` types we now need to define what type the input is. For this spreadsheet example
//! all our types are `i64`:
//!
//! ```
//! # #[derive(Clone, Debug, Default)]
//! # struct A1;
//! # #[derive(Clone, Debug, Default)]
//! # struct A2;
//! use inc_complete::OutputTypeForInput;
//!
//! impl OutputTypeForInput for A1 {
//!     type Output = i64;
//! }
//!
//! impl OutputTypeForInput for A2 {
//!     type Output = i64;
//! }
//! ```
//!
//! For `Intermediate` types we need to provide a `run` function to compute their result. This function
//! will have access to the computation type itself (which often store parameters as data) and
//! a `DbHandle` object to query sub-computations with:
//!
//! ```
//! # #[derive(Clone, Debug, Default)]
//! # struct A1;
//! # #[derive(Clone, Debug, Default)]
//! # struct A2;
//! # #[derive(Clone, PartialEq, Eq, Hash, Default)]
//! # struct B1;
//! # #[derive(Clone, PartialEq, Eq, Hash, Default)]
//! # struct B2;
//! # impl inc_complete::OutputTypeForInput for A1 {
//! #     type Output = i64;
//! # }
//! # impl inc_complete::OutputTypeForInput for A2 {
//! #     type Output = i64;
//! # }
//! # use inc_complete::{ Input, Intermediate, SingletonStorage };
//! # impl A1 {
//! #     fn new() -> SingletonStorage<Input<A1>> {
//! #         Default::default()
//! #     }
//! # }
//! # impl A2 {
//! #     fn new() -> SingletonStorage<Input<A2>> {
//! #         Default::default()
//! #     }
//! # }
//! # impl B1 {
//! #     fn new() -> SingletonStorage<Intermediate<B1>> {
//! #         Default::default()
//! #     }
//! # }
//! # impl B2 {
//! #     fn new() -> SingletonStorage<Intermediate<B2>> {
//! #         Default::default()
//! #     }
//! # }
//! use inc_complete::{ Run, DbHandle, Computation };
//!
//! impl Run for B1 {
//!     type Output = i64;
//!
//!     fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
//!         // These functions should be pure but we're going to cheat here to
//!         // make it obvious when a function is recomputed
//!         println!("Computing B1!");
//!         *handle.get(A1::new()) + 8
//!     }
//! }
//!
//! impl Run for B2 {
//!     type Output = i64;
//!
//!     fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
//!         println!("Computing B2!");
//!         *handle.get(B1::new()) + *handle.get(A2::new())
//!     }
//! }
//! ```
//!
//! Ceremony aside - this code should be relatively straight-forward. We `get` the value of
//! any sub-computations we need and the `DbHandle` object automatically gives us the most
//! up to date version of those computations - we'll examine this claim a bit closer later.
//!
//! Those `new` functions are also coming in handy now. Another approach would have been to make
//! a wrapper function accepting a `DbHandle`:
//!
//! ```
//! # #[derive(Clone, PartialEq, Eq, Hash, Default)]
//! # struct B1;
//! # use inc_complete::{ Input, Run, Intermediate, SingletonStorage, DbHandle, Computation };
//! # #[derive(Clone, Debug, Default)]
//! # struct A1;
//! # impl inc_complete::OutputTypeForInput for A1 {
//! #     type Output = i64;
//! # }
//! # impl A1 {
//! #     fn new() -> SingletonStorage<Input<A1>> {
//! #         Default::default()
//! #     }
//! # }
//! # impl B1 {
//! #     fn new() -> SingletonStorage<Intermediate<B1>> {
//! #         Default::default()
//! #     }
//! # }
//! # impl Run for B1 {
//! #     type Output = i64;
//! #     fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
//! #         println!("Computing B1!");
//! #         *handle.get(A1::new()) + 8
//! #     }
//! # }
//! fn b1(handle: &mut DbHandle<impl Computation>) -> i64 {
//!     // Assuming we didn't have `B1::new()`
//!     *handle.get(SingletonStorage::new(Intermediate::new(B1)))
//!     // Now we can use `b1(handle)`
//! }
//! ```
//!
//! With that out of the way though, we can finally create our `Db`, set the initial values for our
//! inputs, and run our program:
//!
//! ```
//! # #[derive(Clone, Debug, Default)]
//! # struct A1;
//! # #[derive(Clone, Debug, Default)]
//! # struct A2;
//! # #[derive(Clone, PartialEq, Eq, Hash, Default)]
//! # struct B1;
//! # #[derive(Clone, PartialEq, Eq, Hash, Default)]
//! # struct B2;
//! # impl inc_complete::OutputTypeForInput for A1 {
//! #     type Output = i64;
//! # }
//! # impl inc_complete::OutputTypeForInput for A2 {
//! #     type Output = i64;
//! # }
//! # use inc_complete::{ Input, Intermediate, SingletonStorage };
//! # impl A1 {
//! #     fn new() -> SingletonStorage<Input<A1>> {
//! #         Default::default()
//! #     }
//! # }
//! # impl A2 {
//! #     fn new() -> SingletonStorage<Input<A2>> {
//! #         Default::default()
//! #     }
//! # }
//! # impl B1 {
//! #     fn new() -> SingletonStorage<Intermediate<B1>> {
//! #         Default::default()
//! #     }
//! # }
//! # impl B2 {
//! #     fn new() -> SingletonStorage<Intermediate<B2>> {
//! #         Default::default()
//! #     }
//! # }
//! # impl inc_complete::Run for B1 {
//! #     type Output = i64;
//! #     fn run(&self, handle: &mut inc_complete::DbHandle<impl inc_complete::Computation>) -> Self::Output {
//! #         println!("Computing B1!");
//! #         *handle.get(A1::new()) + 8
//! #     }
//! # }
//! # impl inc_complete::Run for B2 {
//! #     type Output = i64;
//! #     fn run(&self, handle: &mut inc_complete::DbHandle<impl inc_complete::Computation>) -> Self::Output {
//! #         println!("Computing B2!");
//! #         *handle.get(B1::new()) + *handle.get(A2::new())
//! #     }
//! # }
//! # type Spreadsheet = (
//! #     SingletonStorage<Input<A1>>,
//! #     SingletonStorage<Input<A2>>,
//! #     SingletonStorage<Intermediate<B1>>,
//! #     SingletonStorage<Intermediate<B2>>,
//! # );
//! use inc_complete::Db;
//! type SpreadsheetDb = Db<Spreadsheet>;
//!
//! fn main() {
//!     let mut db = SpreadsheetDb::new();
//!     db.update_input(A1::new(), 12);
//!     db.update_input(A2::new(), 4);
//!
//!     // Output:
//!     // Computing B2!
//!     // Computing B1!
//!     let b2 = *db.get(B2::new());
//!     assert_eq!(b2, 24);
//!
//!     // No output, result of B2 is cached
//!     let b2 = *db.get(B2::new());
//!     assert_eq!(b2, 24);
//!
//!     // Now lets update an input
//!     db.update_input(A2::new(), 10);
//!
//!     // B2 is now stale and gets recomputed, but crucially B1
//!     // does not depend on A2 and does not get recomputed.
//!     // Output:
//!     // Computing B2!
//!     let b2 = *db.get(B2::new());
//!     assert_eq!(b2, 30);
//! }
//! ```
//!
//! ...And that's it for basic usage! If you want to delve deeper you can implement
//! your own `Input`, `Intermediate`, or storage type wrapper to have more control over how your
//! type is cached by implementing the `Computation` trait.
//!
//! This example did not show it but you can also use structs with fields in your computations, e.g:
//!
//! ```
//! use inc_complete::{ Intermediate, Run, DbHandle, Computation, HashMapStorage };
//!
//! // a fibonacci function with cached sub-results
//! #[derive(Clone, PartialEq, Eq, Hash)]
//! struct Fibonacci { x: u32 }
//!
//! impl Fibonacci {
//!     fn new(x: u32) -> HashMapStorage<Intermediate<Fibonacci>> {
//!         HashMapStorage::new(Intermediate::new(Fibonacci { x }))
//!     }
//! }
//!
//! impl Run for Fibonacci {
//!     type Output = u32;
//!
//!     fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
//!         let x = self.x;
//!         if x <= 1 {
//!             x
//!         } else {
//!             // Not exponential time since each sub-computation will be cached!
//!             *handle.get(Fibonacci::new(x - 1)) + handle.get(Fibonacci::new(x - 2))
//!         }
//!     }
//! }
//! ```
//!
//! These fields often correspond to parameters of the function being modeled, in
//! this case the integer input to `fibonacci`.
#[prelude_import]
use std::prelude::rust_2024::*;
#[macro_use]
extern crate std;
mod cell {
    pub struct Cell(petgraph::graph::NodeIndex);
    #[automatically_derived]
    impl ::core::fmt::Debug for Cell {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Cell", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for Cell {}
    #[automatically_derived]
    impl ::core::clone::Clone for Cell {
        #[inline]
        fn clone(&self) -> Cell {
            let _: ::core::clone::AssertParamIsClone<petgraph::graph::NodeIndex>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Cell {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Cell {
        #[inline]
        fn eq(&self, other: &Cell) -> bool {
            self.0 == other.0
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for Cell {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<petgraph::graph::NodeIndex>;
        }
    }
    #[automatically_derived]
    impl ::core::hash::Hash for Cell {
        #[inline]
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            ::core::hash::Hash::hash(&self.0, state)
        }
    }
    #[automatically_derived]
    impl ::core::cmp::PartialOrd for Cell {
        #[inline]
        fn partial_cmp(
            &self,
            other: &Cell,
        ) -> ::core::option::Option<::core::cmp::Ordering> {
            ::core::cmp::PartialOrd::partial_cmp(&self.0, &other.0)
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Ord for Cell {
        #[inline]
        fn cmp(&self, other: &Cell) -> ::core::cmp::Ordering {
            ::core::cmp::Ord::cmp(&self.0, &other.0)
        }
    }
    impl Cell {
        pub(crate) fn new(index: petgraph::graph::NodeIndex) -> Self {
            Self(index)
        }
        pub(crate) fn index(self) -> petgraph::graph::NodeIndex {
            self.0
        }
    }
    pub(crate) struct CellData {
        pub(crate) computation_id: u32,
        pub(crate) last_updated_version: u32,
        pub(crate) last_verified_version: u32,
    }
    impl CellData {
        pub(crate) fn new(computation_id: u32) -> Self {
            Self {
                computation_id,
                last_updated_version: 0,
                last_verified_version: 0,
            }
        }
    }
}
#[macro_use]
mod computation {
    use crate::{Cell, Db, DbHandle};
    use std::any::{Any, TypeId};
    mod tuple_impls {
        use std::any::{Any, TypeId};
        use crate::{Cell, Computation, Db, DbHandle};
        impl<A, Z> Computation for (A, Z)
        where
            A: Computation,
            Z: Computation,
        {
            type Storage = (A::Storage, Z::Storage);
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, Z> Computation for (A, B, Z)
        where
            A: Computation,
            B: Computation,
            Z: Computation,
        {
            type Storage = (A::Storage, B::Storage, Z::Storage);
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, Z> Computation for (A, B, C, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            Z: Computation,
        {
            type Storage = (A::Storage, B::Storage, C::Storage, Z::Storage);
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, Z> Computation for (A, B, C, D, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            Z: Computation,
        {
            type Storage = (A::Storage, B::Storage, C::Storage, D::Storage, Z::Storage);
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, Z> Computation for (A, B, C, D, E, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, Z> Computation for (A, B, C, D, E, F, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, G, Z> Computation for (A, B, C, D, E, F, G, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, G, H, Z> Computation for (A, B, C, D, E, F, G, H, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, G, H, I, Z> Computation for (A, B, C, D, E, F, G, H, I, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, G, H, I, J, Z> Computation
        for (A, B, C, D, E, F, G, H, I, J, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, G, H, I, J, K, Z> Computation
        for (A, B, C, D, E, F, G, H, I, J, K, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, G, H, I, J, K, L, Z> Computation
        for (A, B, C, D, E, F, G, H, I, J, K, L, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            L: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                L::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage::<Concrete>(computation_id - i, L);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage_mut::<Concrete>(computation_id - i, L);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_input_to_cell::<Concrete>(input, L);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_insert_new_cell::<Concrete>(cell, input, L);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, G, H, I, J, K, L, M, Z> Computation
        for (A, B, C, D, E, F, G, H, I, J, K, L, M, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            L: Computation,
            M: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                L::Storage,
                M::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage::<Concrete>(computation_id - i, M);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage_mut::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage_mut::<Concrete>(computation_id - i, M);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_input_to_cell::<Concrete>(input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_input_to_cell::<Concrete>(input, M);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_insert_new_cell::<Concrete>(cell, input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_insert_new_cell::<Concrete>(cell, input, M);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z> Computation
        for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            L: Computation,
            M: Computation,
            N: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                L::Storage,
                M::Storage,
                N::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage::<Concrete>(computation_id - i, N);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage_mut::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage_mut::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage_mut::<Concrete>(computation_id - i, N);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_input_to_cell::<Concrete>(input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_input_to_cell::<Concrete>(input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_input_to_cell::<Concrete>(input, N);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_insert_new_cell::<Concrete>(cell, input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_insert_new_cell::<Concrete>(cell, input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_insert_new_cell::<Concrete>(cell, input, N);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z> Computation
        for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            L: Computation,
            M: Computation,
            N: Computation,
            O: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                L::Storage,
                M::Storage,
                N::Storage,
                O::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage::<Concrete>(computation_id - i, O);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage_mut::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage_mut::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage_mut::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage_mut::<Concrete>(computation_id - i, O);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_input_to_cell::<Concrete>(input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_input_to_cell::<Concrete>(input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_input_to_cell::<Concrete>(input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_input_to_cell::<Concrete>(input, O);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_insert_new_cell::<Concrete>(cell, input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_insert_new_cell::<Concrete>(cell, input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_insert_new_cell::<Concrete>(cell, input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_insert_new_cell::<Concrete>(cell, input, O);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z> Computation
        for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            L: Computation,
            M: Computation,
            N: Computation,
            O: Computation,
            P: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                L::Storage,
                M::Storage,
                N::Storage,
                O::Storage,
                P::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage::<Concrete>(computation_id - i, P);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage_mut::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage_mut::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage_mut::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage_mut::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage_mut::<Concrete>(computation_id - i, P);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_input_to_cell::<Concrete>(input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_input_to_cell::<Concrete>(input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_input_to_cell::<Concrete>(input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_input_to_cell::<Concrete>(input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_input_to_cell::<Concrete>(input, P);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_insert_new_cell::<Concrete>(cell, input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_insert_new_cell::<Concrete>(cell, input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_insert_new_cell::<Concrete>(cell, input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_insert_new_cell::<Concrete>(cell, input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_insert_new_cell::<Concrete>(cell, input, P);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z> Computation
        for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            L: Computation,
            M: Computation,
            N: Computation,
            O: Computation,
            P: Computation,
            Q: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                L::Storage,
                M::Storage,
                N::Storage,
                O::Storage,
                P::Storage,
                Q::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage_mut::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage_mut::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage_mut::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage_mut::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage_mut::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage_mut::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_input_to_cell::<Concrete>(input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_input_to_cell::<Concrete>(input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_input_to_cell::<Concrete>(input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_input_to_cell::<Concrete>(input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_input_to_cell::<Concrete>(input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_input_to_cell::<Concrete>(input, Q);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_insert_new_cell::<Concrete>(cell, input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_insert_new_cell::<Concrete>(cell, input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_insert_new_cell::<Concrete>(cell, input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_insert_new_cell::<Concrete>(cell, input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_insert_new_cell::<Concrete>(cell, input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_insert_new_cell::<Concrete>(cell, input, Q);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z> Computation
        for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            L: Computation,
            M: Computation,
            N: Computation,
            O: Computation,
            P: Computation,
            Q: Computation,
            R: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                L::Storage,
                M::Storage,
                N::Storage,
                O::Storage,
                P::Storage,
                Q::Storage,
                R::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage::<Concrete>(computation_id - i, R);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage_mut::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage_mut::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage_mut::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage_mut::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage_mut::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage_mut::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage_mut::<Concrete>(computation_id - i, R);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_input_to_cell::<Concrete>(input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_input_to_cell::<Concrete>(input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_input_to_cell::<Concrete>(input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_input_to_cell::<Concrete>(input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_input_to_cell::<Concrete>(input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_input_to_cell::<Concrete>(input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_input_to_cell::<Concrete>(input, R);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_insert_new_cell::<Concrete>(cell, input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_insert_new_cell::<Concrete>(cell, input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_insert_new_cell::<Concrete>(cell, input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_insert_new_cell::<Concrete>(cell, input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_insert_new_cell::<Concrete>(cell, input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_insert_new_cell::<Concrete>(cell, input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_insert_new_cell::<Concrete>(cell, input, R);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z> Computation
        for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            L: Computation,
            M: Computation,
            N: Computation,
            O: Computation,
            P: Computation,
            Q: Computation,
            R: Computation,
            S: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                L::Storage,
                M::Storage,
                N::Storage,
                O::Storage,
                P::Storage,
                Q::Storage,
                R::Storage,
                S::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage::<Concrete>(computation_id - i, R);
                }
                i += 1;
                if computation_id == i {
                    return S::get_storage::<Concrete>(computation_id - i, S);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage_mut::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage_mut::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage_mut::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage_mut::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage_mut::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage_mut::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage_mut::<Concrete>(computation_id - i, R);
                }
                i += 1;
                if computation_id == i {
                    return S::get_storage_mut::<Concrete>(computation_id - i, S);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return S::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return S::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return S::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_input_to_cell::<Concrete>(input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_input_to_cell::<Concrete>(input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_input_to_cell::<Concrete>(input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_input_to_cell::<Concrete>(input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_input_to_cell::<Concrete>(input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_input_to_cell::<Concrete>(input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_input_to_cell::<Concrete>(input, R);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return S::dispatch_input_to_cell::<Concrete>(input, S);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_insert_new_cell::<Concrete>(cell, input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_insert_new_cell::<Concrete>(cell, input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_insert_new_cell::<Concrete>(cell, input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_insert_new_cell::<Concrete>(cell, input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_insert_new_cell::<Concrete>(cell, input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_insert_new_cell::<Concrete>(cell, input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_insert_new_cell::<Concrete>(cell, input, R);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return S::dispatch_insert_new_cell::<Concrete>(cell, input, S);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z> Computation
        for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            L: Computation,
            M: Computation,
            N: Computation,
            O: Computation,
            P: Computation,
            Q: Computation,
            R: Computation,
            S: Computation,
            T: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                L::Storage,
                M::Storage,
                N::Storage,
                O::Storage,
                P::Storage,
                Q::Storage,
                R::Storage,
                S::Storage,
                T::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage::<Concrete>(computation_id - i, R);
                }
                i += 1;
                if computation_id == i {
                    return S::get_storage::<Concrete>(computation_id - i, S);
                }
                i += 1;
                if computation_id == i {
                    return T::get_storage::<Concrete>(computation_id - i, T);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage_mut::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage_mut::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage_mut::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage_mut::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage_mut::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage_mut::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage_mut::<Concrete>(computation_id - i, R);
                }
                i += 1;
                if computation_id == i {
                    return S::get_storage_mut::<Concrete>(computation_id - i, S);
                }
                i += 1;
                if computation_id == i {
                    return T::get_storage_mut::<Concrete>(computation_id - i, T);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return S::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return T::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return S::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return T::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return S::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return T::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_input_to_cell::<Concrete>(input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_input_to_cell::<Concrete>(input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_input_to_cell::<Concrete>(input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_input_to_cell::<Concrete>(input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_input_to_cell::<Concrete>(input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_input_to_cell::<Concrete>(input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_input_to_cell::<Concrete>(input, R);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return S::dispatch_input_to_cell::<Concrete>(input, S);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return T::dispatch_input_to_cell::<Concrete>(input, T);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_insert_new_cell::<Concrete>(cell, input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_insert_new_cell::<Concrete>(cell, input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_insert_new_cell::<Concrete>(cell, input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_insert_new_cell::<Concrete>(cell, input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_insert_new_cell::<Concrete>(cell, input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_insert_new_cell::<Concrete>(cell, input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_insert_new_cell::<Concrete>(cell, input, R);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return S::dispatch_insert_new_cell::<Concrete>(cell, input, S);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return T::dispatch_insert_new_cell::<Concrete>(cell, input, T);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<
            A,
            B,
            C,
            D,
            E,
            F,
            G,
            H,
            I,
            J,
            K,
            L,
            M,
            N,
            O,
            P,
            Q,
            R,
            S,
            T,
            U,
            Z,
        > Computation
        for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            L: Computation,
            M: Computation,
            N: Computation,
            O: Computation,
            P: Computation,
            Q: Computation,
            R: Computation,
            S: Computation,
            T: Computation,
            U: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                L::Storage,
                M::Storage,
                N::Storage,
                O::Storage,
                P::Storage,
                Q::Storage,
                R::Storage,
                S::Storage,
                T::Storage,
                U::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage::<Concrete>(computation_id - i, R);
                }
                i += 1;
                if computation_id == i {
                    return S::get_storage::<Concrete>(computation_id - i, S);
                }
                i += 1;
                if computation_id == i {
                    return T::get_storage::<Concrete>(computation_id - i, T);
                }
                i += 1;
                if computation_id == i {
                    return U::get_storage::<Concrete>(computation_id - i, U);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage_mut::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage_mut::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage_mut::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage_mut::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage_mut::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage_mut::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage_mut::<Concrete>(computation_id - i, R);
                }
                i += 1;
                if computation_id == i {
                    return S::get_storage_mut::<Concrete>(computation_id - i, S);
                }
                i += 1;
                if computation_id == i {
                    return T::get_storage_mut::<Concrete>(computation_id - i, T);
                }
                i += 1;
                if computation_id == i {
                    return U::get_storage_mut::<Concrete>(computation_id - i, U);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return S::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return T::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return U::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return S::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return T::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return U::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return S::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return T::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return U::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_input_to_cell::<Concrete>(input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_input_to_cell::<Concrete>(input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_input_to_cell::<Concrete>(input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_input_to_cell::<Concrete>(input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_input_to_cell::<Concrete>(input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_input_to_cell::<Concrete>(input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_input_to_cell::<Concrete>(input, R);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return S::dispatch_input_to_cell::<Concrete>(input, S);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return T::dispatch_input_to_cell::<Concrete>(input, T);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return U::dispatch_input_to_cell::<Concrete>(input, U);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_insert_new_cell::<Concrete>(cell, input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_insert_new_cell::<Concrete>(cell, input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_insert_new_cell::<Concrete>(cell, input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_insert_new_cell::<Concrete>(cell, input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_insert_new_cell::<Concrete>(cell, input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_insert_new_cell::<Concrete>(cell, input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_insert_new_cell::<Concrete>(cell, input, R);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return S::dispatch_insert_new_cell::<Concrete>(cell, input, S);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return T::dispatch_insert_new_cell::<Concrete>(cell, input, T);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return U::dispatch_insert_new_cell::<Concrete>(cell, input, U);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<
            A,
            B,
            C,
            D,
            E,
            F,
            G,
            H,
            I,
            J,
            K,
            L,
            M,
            N,
            O,
            P,
            Q,
            R,
            S,
            T,
            U,
            V,
            Z,
        > Computation
        for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            L: Computation,
            M: Computation,
            N: Computation,
            O: Computation,
            P: Computation,
            Q: Computation,
            R: Computation,
            S: Computation,
            T: Computation,
            U: Computation,
            V: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                L::Storage,
                M::Storage,
                N::Storage,
                O::Storage,
                P::Storage,
                Q::Storage,
                R::Storage,
                S::Storage,
                T::Storage,
                U::Storage,
                V::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<V>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    Z,
                ) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage::<Concrete>(computation_id - i, R);
                }
                i += 1;
                if computation_id == i {
                    return S::get_storage::<Concrete>(computation_id - i, S);
                }
                i += 1;
                if computation_id == i {
                    return T::get_storage::<Concrete>(computation_id - i, T);
                }
                i += 1;
                if computation_id == i {
                    return U::get_storage::<Concrete>(computation_id - i, U);
                }
                i += 1;
                if computation_id == i {
                    return V::get_storage::<Concrete>(computation_id - i, V);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    Z,
                ) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage_mut::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage_mut::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage_mut::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage_mut::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage_mut::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage_mut::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage_mut::<Concrete>(computation_id - i, R);
                }
                i += 1;
                if computation_id == i {
                    return S::get_storage_mut::<Concrete>(computation_id - i, S);
                }
                i += 1;
                if computation_id == i {
                    return T::get_storage_mut::<Concrete>(computation_id - i, T);
                }
                i += 1;
                if computation_id == i {
                    return U::get_storage_mut::<Concrete>(computation_id - i, U);
                }
                i += 1;
                if computation_id == i {
                    return V::get_storage_mut::<Concrete>(computation_id - i, V);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return S::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return T::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return U::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return V::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return S::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return T::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return U::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return V::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return S::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return T::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return U::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return V::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    Z,
                ) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_input_to_cell::<Concrete>(input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_input_to_cell::<Concrete>(input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_input_to_cell::<Concrete>(input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_input_to_cell::<Concrete>(input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_input_to_cell::<Concrete>(input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_input_to_cell::<Concrete>(input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_input_to_cell::<Concrete>(input, R);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return S::dispatch_input_to_cell::<Concrete>(input, S);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return T::dispatch_input_to_cell::<Concrete>(input, T);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return U::dispatch_input_to_cell::<Concrete>(input, U);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<V>() {
                    return V::dispatch_input_to_cell::<Concrete>(input, V);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    Z,
                ) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_insert_new_cell::<Concrete>(cell, input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_insert_new_cell::<Concrete>(cell, input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_insert_new_cell::<Concrete>(cell, input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_insert_new_cell::<Concrete>(cell, input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_insert_new_cell::<Concrete>(cell, input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_insert_new_cell::<Concrete>(cell, input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_insert_new_cell::<Concrete>(cell, input, R);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return S::dispatch_insert_new_cell::<Concrete>(cell, input, S);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return T::dispatch_insert_new_cell::<Concrete>(cell, input, T);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return U::dispatch_insert_new_cell::<Concrete>(cell, input, U);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<V>() {
                    return V::dispatch_insert_new_cell::<Concrete>(cell, input, V);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<
            A,
            B,
            C,
            D,
            E,
            F,
            G,
            H,
            I,
            J,
            K,
            L,
            M,
            N,
            O,
            P,
            Q,
            R,
            S,
            T,
            U,
            V,
            W,
            Z,
        > Computation
        for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            L: Computation,
            M: Computation,
            N: Computation,
            O: Computation,
            P: Computation,
            Q: Computation,
            R: Computation,
            S: Computation,
            T: Computation,
            U: Computation,
            V: Computation,
            W: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                L::Storage,
                M::Storage,
                N::Storage,
                O::Storage,
                P::Storage,
                Q::Storage,
                R::Storage,
                S::Storage,
                T::Storage,
                U::Storage,
                V::Storage,
                W::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<V>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<W>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    W,
                    Z,
                ) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage::<Concrete>(computation_id - i, R);
                }
                i += 1;
                if computation_id == i {
                    return S::get_storage::<Concrete>(computation_id - i, S);
                }
                i += 1;
                if computation_id == i {
                    return T::get_storage::<Concrete>(computation_id - i, T);
                }
                i += 1;
                if computation_id == i {
                    return U::get_storage::<Concrete>(computation_id - i, U);
                }
                i += 1;
                if computation_id == i {
                    return V::get_storage::<Concrete>(computation_id - i, V);
                }
                i += 1;
                if computation_id == i {
                    return W::get_storage::<Concrete>(computation_id - i, W);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    W,
                    Z,
                ) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage_mut::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage_mut::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage_mut::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage_mut::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage_mut::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage_mut::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage_mut::<Concrete>(computation_id - i, R);
                }
                i += 1;
                if computation_id == i {
                    return S::get_storage_mut::<Concrete>(computation_id - i, S);
                }
                i += 1;
                if computation_id == i {
                    return T::get_storage_mut::<Concrete>(computation_id - i, T);
                }
                i += 1;
                if computation_id == i {
                    return U::get_storage_mut::<Concrete>(computation_id - i, U);
                }
                i += 1;
                if computation_id == i {
                    return V::get_storage_mut::<Concrete>(computation_id - i, V);
                }
                i += 1;
                if computation_id == i {
                    return W::get_storage_mut::<Concrete>(computation_id - i, W);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return S::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return T::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return U::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return V::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return W::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return S::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return T::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return U::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return V::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return W::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return S::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return T::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return U::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return V::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return W::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    W,
                    Z,
                ) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_input_to_cell::<Concrete>(input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_input_to_cell::<Concrete>(input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_input_to_cell::<Concrete>(input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_input_to_cell::<Concrete>(input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_input_to_cell::<Concrete>(input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_input_to_cell::<Concrete>(input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_input_to_cell::<Concrete>(input, R);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return S::dispatch_input_to_cell::<Concrete>(input, S);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return T::dispatch_input_to_cell::<Concrete>(input, T);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return U::dispatch_input_to_cell::<Concrete>(input, U);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<V>() {
                    return V::dispatch_input_to_cell::<Concrete>(input, V);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<W>() {
                    return W::dispatch_input_to_cell::<Concrete>(input, W);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    W,
                    Z,
                ) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_insert_new_cell::<Concrete>(cell, input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_insert_new_cell::<Concrete>(cell, input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_insert_new_cell::<Concrete>(cell, input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_insert_new_cell::<Concrete>(cell, input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_insert_new_cell::<Concrete>(cell, input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_insert_new_cell::<Concrete>(cell, input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_insert_new_cell::<Concrete>(cell, input, R);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return S::dispatch_insert_new_cell::<Concrete>(cell, input, S);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return T::dispatch_insert_new_cell::<Concrete>(cell, input, T);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return U::dispatch_insert_new_cell::<Concrete>(cell, input, U);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<V>() {
                    return V::dispatch_insert_new_cell::<Concrete>(cell, input, V);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<W>() {
                    return W::dispatch_insert_new_cell::<Concrete>(cell, input, W);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<
            A,
            B,
            C,
            D,
            E,
            F,
            G,
            H,
            I,
            J,
            K,
            L,
            M,
            N,
            O,
            P,
            Q,
            R,
            S,
            T,
            U,
            V,
            W,
            X,
            Z,
        > Computation
        for (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Z)
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            L: Computation,
            M: Computation,
            N: Computation,
            O: Computation,
            P: Computation,
            Q: Computation,
            R: Computation,
            S: Computation,
            T: Computation,
            U: Computation,
            V: Computation,
            W: Computation,
            X: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                L::Storage,
                M::Storage,
                N::Storage,
                O::Storage,
                P::Storage,
                Q::Storage,
                R::Storage,
                S::Storage,
                T::Storage,
                U::Storage,
                V::Storage,
                W::Storage,
                X::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<V>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<W>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<X>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    W,
                    X,
                    Z,
                ) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage::<Concrete>(computation_id - i, R);
                }
                i += 1;
                if computation_id == i {
                    return S::get_storage::<Concrete>(computation_id - i, S);
                }
                i += 1;
                if computation_id == i {
                    return T::get_storage::<Concrete>(computation_id - i, T);
                }
                i += 1;
                if computation_id == i {
                    return U::get_storage::<Concrete>(computation_id - i, U);
                }
                i += 1;
                if computation_id == i {
                    return V::get_storage::<Concrete>(computation_id - i, V);
                }
                i += 1;
                if computation_id == i {
                    return W::get_storage::<Concrete>(computation_id - i, W);
                }
                i += 1;
                if computation_id == i {
                    return X::get_storage::<Concrete>(computation_id - i, X);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    W,
                    X,
                    Z,
                ) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage_mut::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage_mut::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage_mut::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage_mut::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage_mut::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage_mut::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage_mut::<Concrete>(computation_id - i, R);
                }
                i += 1;
                if computation_id == i {
                    return S::get_storage_mut::<Concrete>(computation_id - i, S);
                }
                i += 1;
                if computation_id == i {
                    return T::get_storage_mut::<Concrete>(computation_id - i, T);
                }
                i += 1;
                if computation_id == i {
                    return U::get_storage_mut::<Concrete>(computation_id - i, U);
                }
                i += 1;
                if computation_id == i {
                    return V::get_storage_mut::<Concrete>(computation_id - i, V);
                }
                i += 1;
                if computation_id == i {
                    return W::get_storage_mut::<Concrete>(computation_id - i, W);
                }
                i += 1;
                if computation_id == i {
                    return X::get_storage_mut::<Concrete>(computation_id - i, X);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return S::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return T::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return U::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return V::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return W::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return X::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return S::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return T::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return U::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return V::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return W::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return X::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return S::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return T::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return U::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return V::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return W::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return X::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    W,
                    X,
                    Z,
                ) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_input_to_cell::<Concrete>(input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_input_to_cell::<Concrete>(input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_input_to_cell::<Concrete>(input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_input_to_cell::<Concrete>(input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_input_to_cell::<Concrete>(input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_input_to_cell::<Concrete>(input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_input_to_cell::<Concrete>(input, R);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return S::dispatch_input_to_cell::<Concrete>(input, S);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return T::dispatch_input_to_cell::<Concrete>(input, T);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return U::dispatch_input_to_cell::<Concrete>(input, U);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<V>() {
                    return V::dispatch_input_to_cell::<Concrete>(input, V);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<W>() {
                    return W::dispatch_input_to_cell::<Concrete>(input, W);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<X>() {
                    return X::dispatch_input_to_cell::<Concrete>(input, X);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    W,
                    X,
                    Z,
                ) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_insert_new_cell::<Concrete>(cell, input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_insert_new_cell::<Concrete>(cell, input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_insert_new_cell::<Concrete>(cell, input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_insert_new_cell::<Concrete>(cell, input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_insert_new_cell::<Concrete>(cell, input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_insert_new_cell::<Concrete>(cell, input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_insert_new_cell::<Concrete>(cell, input, R);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return S::dispatch_insert_new_cell::<Concrete>(cell, input, S);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return T::dispatch_insert_new_cell::<Concrete>(cell, input, T);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return U::dispatch_insert_new_cell::<Concrete>(cell, input, U);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<V>() {
                    return V::dispatch_insert_new_cell::<Concrete>(cell, input, V);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<W>() {
                    return W::dispatch_insert_new_cell::<Concrete>(cell, input, W);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<X>() {
                    return X::dispatch_insert_new_cell::<Concrete>(cell, input, X);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
        impl<
            A,
            B,
            C,
            D,
            E,
            F,
            G,
            H,
            I,
            J,
            K,
            L,
            M,
            N,
            O,
            P,
            Q,
            R,
            S,
            T,
            U,
            V,
            W,
            X,
            Y,
            Z,
        > Computation
        for (
            A,
            B,
            C,
            D,
            E,
            F,
            G,
            H,
            I,
            J,
            K,
            L,
            M,
            N,
            O,
            P,
            Q,
            R,
            S,
            T,
            U,
            V,
            W,
            X,
            Y,
            Z,
        )
        where
            A: Computation,
            B: Computation,
            C: Computation,
            D: Computation,
            E: Computation,
            F: Computation,
            G: Computation,
            H: Computation,
            I: Computation,
            J: Computation,
            K: Computation,
            L: Computation,
            M: Computation,
            N: Computation,
            O: Computation,
            P: Computation,
            Q: Computation,
            R: Computation,
            S: Computation,
            T: Computation,
            U: Computation,
            V: Computation,
            W: Computation,
            X: Computation,
            Y: Computation,
            Z: Computation,
        {
            type Storage = (
                A::Storage,
                B::Storage,
                C::Storage,
                D::Storage,
                E::Storage,
                F::Storage,
                G::Storage,
                H::Storage,
                I::Storage,
                J::Storage,
                K::Storage,
                L::Storage,
                M::Storage,
                N::Storage,
                O::Storage,
                P::Storage,
                Q::Storage,
                R::Storage,
                S::Storage,
                T::Storage,
                U::Storage,
                V::Storage,
                W::Storage,
                X::Storage,
                Y::Storage,
                Z::Storage,
            );
            type Output = ();
            fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `run`"),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `input_to_cell`"),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `insert_new_cell`"),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `get_function_and_output`"),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("Type dispatch failed in `set_output`"),
                    );
                }
            }
            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<V>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<W>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<X>() {
                    return i;
                }
                i += 1;
                if TypeId::of::<Concrete>() == TypeId::of::<Y>() {
                    return i;
                }
                i += 1;
                i + Z::computation_id_of::<Concrete>()
            }
            fn get_storage<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &Self::Storage,
            ) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    W,
                    X,
                    Y,
                    Z,
                ) = storage;
                if computation_id == i {
                    return A::get_storage::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage::<Concrete>(computation_id - i, R);
                }
                i += 1;
                if computation_id == i {
                    return S::get_storage::<Concrete>(computation_id - i, S);
                }
                i += 1;
                if computation_id == i {
                    return T::get_storage::<Concrete>(computation_id - i, T);
                }
                i += 1;
                if computation_id == i {
                    return U::get_storage::<Concrete>(computation_id - i, U);
                }
                i += 1;
                if computation_id == i {
                    return V::get_storage::<Concrete>(computation_id - i, V);
                }
                i += 1;
                if computation_id == i {
                    return W::get_storage::<Concrete>(computation_id - i, W);
                }
                i += 1;
                if computation_id == i {
                    return X::get_storage::<Concrete>(computation_id - i, X);
                }
                i += 1;
                if computation_id == i {
                    return Y::get_storage::<Concrete>(computation_id - i, Y);
                }
                i += 1;
                Z::get_storage::<Concrete>(computation_id - i, Z)
            }
            fn get_storage_mut<Concrete: Computation + 'static>(
                computation_id: u32,
                storage: &mut Self::Storage,
            ) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    W,
                    X,
                    Y,
                    Z,
                ) = storage;
                if computation_id == i {
                    return A::get_storage_mut::<Concrete>(computation_id - i, A);
                }
                i += 1;
                if computation_id == i {
                    return B::get_storage_mut::<Concrete>(computation_id - i, B);
                }
                i += 1;
                if computation_id == i {
                    return C::get_storage_mut::<Concrete>(computation_id - i, C);
                }
                i += 1;
                if computation_id == i {
                    return D::get_storage_mut::<Concrete>(computation_id - i, D);
                }
                i += 1;
                if computation_id == i {
                    return E::get_storage_mut::<Concrete>(computation_id - i, E);
                }
                i += 1;
                if computation_id == i {
                    return F::get_storage_mut::<Concrete>(computation_id - i, F);
                }
                i += 1;
                if computation_id == i {
                    return G::get_storage_mut::<Concrete>(computation_id - i, G);
                }
                i += 1;
                if computation_id == i {
                    return H::get_storage_mut::<Concrete>(computation_id - i, H);
                }
                i += 1;
                if computation_id == i {
                    return I::get_storage_mut::<Concrete>(computation_id - i, I);
                }
                i += 1;
                if computation_id == i {
                    return J::get_storage_mut::<Concrete>(computation_id - i, J);
                }
                i += 1;
                if computation_id == i {
                    return K::get_storage_mut::<Concrete>(computation_id - i, K);
                }
                i += 1;
                if computation_id == i {
                    return L::get_storage_mut::<Concrete>(computation_id - i, L);
                }
                i += 1;
                if computation_id == i {
                    return M::get_storage_mut::<Concrete>(computation_id - i, M);
                }
                i += 1;
                if computation_id == i {
                    return N::get_storage_mut::<Concrete>(computation_id - i, N);
                }
                i += 1;
                if computation_id == i {
                    return O::get_storage_mut::<Concrete>(computation_id - i, O);
                }
                i += 1;
                if computation_id == i {
                    return P::get_storage_mut::<Concrete>(computation_id - i, P);
                }
                i += 1;
                if computation_id == i {
                    return Q::get_storage_mut::<Concrete>(computation_id - i, Q);
                }
                i += 1;
                if computation_id == i {
                    return R::get_storage_mut::<Concrete>(computation_id - i, R);
                }
                i += 1;
                if computation_id == i {
                    return S::get_storage_mut::<Concrete>(computation_id - i, S);
                }
                i += 1;
                if computation_id == i {
                    return T::get_storage_mut::<Concrete>(computation_id - i, T);
                }
                i += 1;
                if computation_id == i {
                    return U::get_storage_mut::<Concrete>(computation_id - i, U);
                }
                i += 1;
                if computation_id == i {
                    return V::get_storage_mut::<Concrete>(computation_id - i, V);
                }
                i += 1;
                if computation_id == i {
                    return W::get_storage_mut::<Concrete>(computation_id - i, W);
                }
                i += 1;
                if computation_id == i {
                    return X::get_storage_mut::<Concrete>(computation_id - i, X);
                }
                i += 1;
                if computation_id == i {
                    return Y::get_storage_mut::<Concrete>(computation_id - i, Y);
                }
                i += 1;
                Z::get_storage_mut::<Concrete>(computation_id - i, Z)
            }
            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;
                if computation_id == i {
                    return A::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return S::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return T::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return U::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return V::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return W::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return X::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Y::output_is_unset(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return S::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return T::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return U::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return V::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return W::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return X::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                if computation_id == i {
                    return Y::dispatch_run(
                        cell,
                        computation_id - i,
                        original_computation_id,
                        db,
                    );
                }
                i += 1;
                Z::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }
            fn dispatch_update_output<Concrete, FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                output: Concrete::Output,
                db: &mut Db<FullComputation>,
            ) -> bool
            where
                Concrete: Computation,
                FullComputation: Computation,
                Self::Output: Eq,
            {
                let mut i = 0;
                if computation_id == i {
                    return A::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return B::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return C::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return D::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return E::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return F::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return G::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return H::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return I::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return J::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return K::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return L::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return M::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return N::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return O::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return P::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return Q::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return R::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return S::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return T::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return U::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return V::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return W::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return X::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                if computation_id == i {
                    return Y::dispatch_update_output::<
                        Concrete,
                        FullComputation,
                    >(cell, computation_id - i, original_computation_id, output, db);
                }
                i += 1;
                Z::dispatch_update_output::<
                    Concrete,
                    FullComputation,
                >(cell, computation_id - i, original_computation_id, output, db)
            }
            fn dispatch_input_to_cell<Concrete>(
                input: &Concrete,
                storage: &Self::Storage,
            ) -> Option<Cell>
            where
                Concrete: 'static + Computation + Any,
            {
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    W,
                    X,
                    Y,
                    Z,
                ) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_input_to_cell::<Concrete>(input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_input_to_cell::<Concrete>(input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_input_to_cell::<Concrete>(input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_input_to_cell::<Concrete>(input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_input_to_cell::<Concrete>(input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_input_to_cell::<Concrete>(input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_input_to_cell::<Concrete>(input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_input_to_cell::<Concrete>(input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_input_to_cell::<Concrete>(input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_input_to_cell::<Concrete>(input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_input_to_cell::<Concrete>(input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_input_to_cell::<Concrete>(input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_input_to_cell::<Concrete>(input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_input_to_cell::<Concrete>(input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_input_to_cell::<Concrete>(input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_input_to_cell::<Concrete>(input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_input_to_cell::<Concrete>(input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_input_to_cell::<Concrete>(input, R);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return S::dispatch_input_to_cell::<Concrete>(input, S);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return T::dispatch_input_to_cell::<Concrete>(input, T);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return U::dispatch_input_to_cell::<Concrete>(input, U);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<V>() {
                    return V::dispatch_input_to_cell::<Concrete>(input, V);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<W>() {
                    return W::dispatch_input_to_cell::<Concrete>(input, W);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<X>() {
                    return X::dispatch_input_to_cell::<Concrete>(input, X);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Y>() {
                    return Y::dispatch_input_to_cell::<Concrete>(input, Y);
                }
                Z::dispatch_input_to_cell::<Concrete>(input, Z)
            }
            fn dispatch_insert_new_cell<Concrete>(
                cell: Cell,
                input: Concrete,
                storage: &mut Self::Storage,
            )
            where
                Concrete: 'static + Computation + Any,
                Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let (
                    A,
                    B,
                    C,
                    D,
                    E,
                    F,
                    G,
                    H,
                    I,
                    J,
                    K,
                    L,
                    M,
                    N,
                    O,
                    P,
                    Q,
                    R,
                    S,
                    T,
                    U,
                    V,
                    W,
                    X,
                    Y,
                    Z,
                ) = storage;
                if TypeId::of::<Concrete>() == TypeId::of::<A>() {
                    return A::dispatch_insert_new_cell::<Concrete>(cell, input, A);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<B>() {
                    return B::dispatch_insert_new_cell::<Concrete>(cell, input, B);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<C>() {
                    return C::dispatch_insert_new_cell::<Concrete>(cell, input, C);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<D>() {
                    return D::dispatch_insert_new_cell::<Concrete>(cell, input, D);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<E>() {
                    return E::dispatch_insert_new_cell::<Concrete>(cell, input, E);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<F>() {
                    return F::dispatch_insert_new_cell::<Concrete>(cell, input, F);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<G>() {
                    return G::dispatch_insert_new_cell::<Concrete>(cell, input, G);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<H>() {
                    return H::dispatch_insert_new_cell::<Concrete>(cell, input, H);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<I>() {
                    return I::dispatch_insert_new_cell::<Concrete>(cell, input, I);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<J>() {
                    return J::dispatch_insert_new_cell::<Concrete>(cell, input, J);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<K>() {
                    return K::dispatch_insert_new_cell::<Concrete>(cell, input, K);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<L>() {
                    return L::dispatch_insert_new_cell::<Concrete>(cell, input, L);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<M>() {
                    return M::dispatch_insert_new_cell::<Concrete>(cell, input, M);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<N>() {
                    return N::dispatch_insert_new_cell::<Concrete>(cell, input, N);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<O>() {
                    return O::dispatch_insert_new_cell::<Concrete>(cell, input, O);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<P>() {
                    return P::dispatch_insert_new_cell::<Concrete>(cell, input, P);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Q>() {
                    return Q::dispatch_insert_new_cell::<Concrete>(cell, input, Q);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<R>() {
                    return R::dispatch_insert_new_cell::<Concrete>(cell, input, R);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<S>() {
                    return S::dispatch_insert_new_cell::<Concrete>(cell, input, S);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<T>() {
                    return T::dispatch_insert_new_cell::<Concrete>(cell, input, T);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<U>() {
                    return U::dispatch_insert_new_cell::<Concrete>(cell, input, U);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<V>() {
                    return V::dispatch_insert_new_cell::<Concrete>(cell, input, V);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<W>() {
                    return W::dispatch_insert_new_cell::<Concrete>(cell, input, W);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<X>() {
                    return X::dispatch_insert_new_cell::<Concrete>(cell, input, X);
                }
                if TypeId::of::<Concrete>() == TypeId::of::<Y>() {
                    return Y::dispatch_insert_new_cell::<Concrete>(cell, input, Y);
                }
                Z::dispatch_insert_new_cell::<Concrete>(cell, input, Z)
            }
        }
    }
    mod input {
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
        pub struct Input<T>(T);
        #[automatically_derived]
        impl<T: ::core::fmt::Debug> ::core::fmt::Debug for Input<T> {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Input", &&self.0)
            }
        }
        #[automatically_derived]
        impl<T: ::core::default::Default> ::core::default::Default for Input<T> {
            #[inline]
            fn default() -> Input<T> {
                Input(::core::default::Default::default())
            }
        }
        #[automatically_derived]
        impl<T: ::core::marker::Copy> ::core::marker::Copy for Input<T> {}
        #[automatically_derived]
        impl<T: ::core::clone::Clone> ::core::clone::Clone for Input<T> {
            #[inline]
            fn clone(&self) -> Input<T> {
                Input(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl<T> ::core::marker::StructuralPartialEq for Input<T> {}
        #[automatically_derived]
        impl<T: ::core::cmp::PartialEq> ::core::cmp::PartialEq for Input<T> {
            #[inline]
            fn eq(&self, other: &Input<T>) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::Eq> ::core::cmp::Eq for Input<T> {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<T>;
            }
        }
        #[automatically_derived]
        impl<T: ::core::hash::Hash> ::core::hash::Hash for Input<T> {
            #[inline]
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
                ::core::hash::Hash::hash(&self.0, state)
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::PartialOrd> ::core::cmp::PartialOrd for Input<T> {
            #[inline]
            fn partial_cmp(
                &self,
                other: &Input<T>,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::cmp::PartialOrd::partial_cmp(&self.0, &other.0)
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::Ord> ::core::cmp::Ord for Input<T> {
            #[inline]
            fn cmp(&self, other: &Input<T>) -> ::core::cmp::Ordering {
                ::core::cmp::Ord::cmp(&self.0, &other.0)
            }
        }
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
                {
                    ::core::panicking::panic_fmt(
                        format_args!(
                            "Input `{0}` queried before `db.update_input(..)` called",
                            std::any::type_name::<T>(),
                        ),
                    );
                }
            }
            fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
                {
                    ::core::panicking::panic_fmt(
                        format_args!(
                            "Input used without a storage type wrapper - try wrapping your type with `HashMapStorage<Input<T>>`",
                        ),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!(
                            "Input used without a storage type wrapper - try wrapping your type with `HashMapStorage<Input<T>>`",
                        ),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!(
                            "Input used without a storage type wrapper - try wrapping your type with `HashMapStorage<Input<T>>`",
                        ),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!(
                            "Input used without a storage type wrapper - try wrapping your type with `HashMapStorage<Input<T>>`",
                        ),
                    );
                }
            }
        }
    }
    mod intermediate {
        use crate::{Cell, DbHandle};
        use super::Computation;
        /// A helper type for defining intermediate Computations.
        /// This will consist of any non-input in a program. These are
        /// always functions which are cached and derive their value from
        /// other functions or inputs.
        pub struct Intermediate<T>(T);
        #[automatically_derived]
        impl<T: ::core::fmt::Debug> ::core::fmt::Debug for Intermediate<T> {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Intermediate",
                    &&self.0,
                )
            }
        }
        #[automatically_derived]
        impl<T: ::core::default::Default> ::core::default::Default for Intermediate<T> {
            #[inline]
            fn default() -> Intermediate<T> {
                Intermediate(::core::default::Default::default())
            }
        }
        #[automatically_derived]
        impl<T: ::core::marker::Copy> ::core::marker::Copy for Intermediate<T> {}
        #[automatically_derived]
        impl<T: ::core::clone::Clone> ::core::clone::Clone for Intermediate<T> {
            #[inline]
            fn clone(&self) -> Intermediate<T> {
                Intermediate(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl<T> ::core::marker::StructuralPartialEq for Intermediate<T> {}
        #[automatically_derived]
        impl<T: ::core::cmp::PartialEq> ::core::cmp::PartialEq for Intermediate<T> {
            #[inline]
            fn eq(&self, other: &Intermediate<T>) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::Eq> ::core::cmp::Eq for Intermediate<T> {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<T>;
            }
        }
        #[automatically_derived]
        impl<T: ::core::hash::Hash> ::core::hash::Hash for Intermediate<T> {
            #[inline]
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
                ::core::hash::Hash::hash(&self.0, state)
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::PartialOrd> ::core::cmp::PartialOrd for Intermediate<T> {
            #[inline]
            fn partial_cmp(
                &self,
                other: &Intermediate<T>,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::cmp::PartialOrd::partial_cmp(&self.0, &other.0)
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::Ord> ::core::cmp::Ord for Intermediate<T> {
            #[inline]
            fn cmp(&self, other: &Intermediate<T>) -> ::core::cmp::Ordering {
                ::core::cmp::Ord::cmp(&self.0, &other.0)
            }
        }
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
                {
                    ::core::panicking::panic_fmt(
                        format_args!(
                            "Intermediate used without a storage type wrapper - try wrapping your type with `HashMapStorage<Intermediate<T>>`",
                        ),
                    );
                }
            }
            fn get_function_and_output(
                _: Cell,
                _: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!(
                            "Intermediate used without a storage type wrapper - try wrapping your type with `HashMapStorage<Intermediate<T>>`",
                        ),
                    );
                }
            }
            fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!(
                            "Intermediate used without a storage type wrapper - try wrapping your type with `HashMapStorage<Intermediate<T>>`",
                        ),
                    );
                }
            }
            fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!(
                            "Intermediate used without a storage type wrapper - try wrapping your type with `HashMapStorage<Intermediate<T>>`",
                        ),
                    );
                }
            }
        }
    }
    mod singleton {
        use crate::{Cell, DbHandle};
        use super::Computation;
        /// Helper to define a Computation for a simple input type which has no fields and thus
        /// does not require a HashMap to cache each possible value.
        ///
        /// Examples include `struct SourceFile;` or `struct Time;`
        pub struct SingletonStorage<T>(T);
        #[automatically_derived]
        impl<T: ::core::fmt::Debug> ::core::fmt::Debug for SingletonStorage<T> {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "SingletonStorage",
                    &&self.0,
                )
            }
        }
        #[automatically_derived]
        impl<T: ::core::default::Default> ::core::default::Default
        for SingletonStorage<T> {
            #[inline]
            fn default() -> SingletonStorage<T> {
                SingletonStorage(::core::default::Default::default())
            }
        }
        #[automatically_derived]
        impl<T: ::core::marker::Copy> ::core::marker::Copy for SingletonStorage<T> {}
        #[automatically_derived]
        impl<T: ::core::clone::Clone> ::core::clone::Clone for SingletonStorage<T> {
            #[inline]
            fn clone(&self) -> SingletonStorage<T> {
                SingletonStorage(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl<T> ::core::marker::StructuralPartialEq for SingletonStorage<T> {}
        #[automatically_derived]
        impl<T: ::core::cmp::PartialEq> ::core::cmp::PartialEq for SingletonStorage<T> {
            #[inline]
            fn eq(&self, other: &SingletonStorage<T>) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::Eq> ::core::cmp::Eq for SingletonStorage<T> {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<T>;
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::PartialOrd> ::core::cmp::PartialOrd
        for SingletonStorage<T> {
            #[inline]
            fn partial_cmp(
                &self,
                other: &SingletonStorage<T>,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::cmp::PartialOrd::partial_cmp(&self.0, &other.0)
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::Ord> ::core::cmp::Ord for SingletonStorage<T> {
            #[inline]
            fn cmp(&self, other: &SingletonStorage<T>) -> ::core::cmp::Ordering {
                ::core::cmp::Ord::cmp(&self.0, &other.0)
            }
        }
        impl<T> SingletonStorage<T> {
            pub fn new(value: T) -> Self {
                let bytes = std::mem::size_of::<T>();
                match (&bytes, &0) {
                    (left_val, right_val) => {
                        if !(*left_val == *right_val) {
                            let kind = ::core::panicking::AssertKind::Eq;
                            ::core::panicking::assert_failed(
                                kind,
                                &*left_val,
                                &*right_val,
                                ::core::option::Option::Some(
                                    format_args!(
                                        "SingletonStorage only supports 0-sized types but `{0}` is `{1}` bytes large",
                                        std::any::type_name::<T>(),
                                        bytes,
                                    ),
                                ),
                            );
                        }
                    }
                };
                Self(value)
            }
        }
        impl<T: Computation> Computation for SingletonStorage<T> {
            type Output = <T as Computation>::Output;
            type Storage = (Option<Cell>, Self, Option<Self::Output>);
            fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
                self.0.run(handle)
            }
            fn input_to_cell(_: &Self, storage: &Self::Storage) -> Option<Cell> {
                storage.0
            }
            fn get_function_and_output(
                _: Cell,
                storage: &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                (&storage.1, storage.2.as_ref())
            }
            fn set_output(
                _: Cell,
                new_output: Self::Output,
                storage: &mut Self::Storage,
            ) {
                storage.2 = Some(new_output);
            }
            fn insert_new_cell(cell: Cell, this: Self, storage: &mut Self::Storage) {
                *storage = (Some(cell), this, None);
            }
        }
    }
    mod hashmapped {
        use std::collections::HashMap;
        use crate::{Cell, DbHandle};
        use std::hash::Hash;
        use super::Computation;
        /// A helper type for defining Computations with HashMap-backed storage
        pub struct HashMapStorage<T>(T);
        #[automatically_derived]
        impl<T: ::core::fmt::Debug> ::core::fmt::Debug for HashMapStorage<T> {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "HashMapStorage",
                    &&self.0,
                )
            }
        }
        #[automatically_derived]
        impl<T: ::core::default::Default> ::core::default::Default
        for HashMapStorage<T> {
            #[inline]
            fn default() -> HashMapStorage<T> {
                HashMapStorage(::core::default::Default::default())
            }
        }
        #[automatically_derived]
        impl<T: ::core::marker::Copy> ::core::marker::Copy for HashMapStorage<T> {}
        #[automatically_derived]
        impl<T: ::core::clone::Clone> ::core::clone::Clone for HashMapStorage<T> {
            #[inline]
            fn clone(&self) -> HashMapStorage<T> {
                HashMapStorage(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl<T> ::core::marker::StructuralPartialEq for HashMapStorage<T> {}
        #[automatically_derived]
        impl<T: ::core::cmp::PartialEq> ::core::cmp::PartialEq for HashMapStorage<T> {
            #[inline]
            fn eq(&self, other: &HashMapStorage<T>) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::Eq> ::core::cmp::Eq for HashMapStorage<T> {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<T>;
            }
        }
        #[automatically_derived]
        impl<T: ::core::hash::Hash> ::core::hash::Hash for HashMapStorage<T> {
            #[inline]
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
                ::core::hash::Hash::hash(&self.0, state)
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::PartialOrd> ::core::cmp::PartialOrd for HashMapStorage<T> {
            #[inline]
            fn partial_cmp(
                &self,
                other: &HashMapStorage<T>,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::cmp::PartialOrd::partial_cmp(&self.0, &other.0)
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::Ord> ::core::cmp::Ord for HashMapStorage<T> {
            #[inline]
            fn cmp(&self, other: &HashMapStorage<T>) -> ::core::cmp::Ordering {
                ::core::cmp::Ord::cmp(&self.0, &other.0)
            }
        }
        impl<T> HashMapStorage<T> {
            pub fn new(value: T) -> Self {
                Self(value)
            }
        }
        impl<T> Computation for HashMapStorage<T>
        where
            T: Computation + Eq + Hash,
        {
            type Output = <T as Computation>::Output;
            type Storage = (
                HashMap<Self, Cell>,
                HashMap<Cell, (Self, Option<Self::Output>)>,
            );
            fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
                self.0.run(handle)
            }
            fn input_to_cell(
                input: &Self,
                (self_to_cell, _): &Self::Storage,
            ) -> Option<Cell> {
                self_to_cell.get(input).copied()
            }
            fn get_function_and_output(
                cell: Cell,
                (_, cell_to_output): &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                let (this, output) = &cell_to_output[&cell];
                (this, output.as_ref())
            }
            fn set_output(
                cell: Cell,
                new_output: Self::Output,
                (_, cell_to_output): &mut Self::Storage,
            ) {
                cell_to_output
                    .entry(cell)
                    .and_modify(|(_, output)| {
                        *output = Some(new_output);
                    });
            }
            fn insert_new_cell(cell: Cell, function: Self, storage: &mut Self::Storage) {
                storage.0.insert(function.clone(), cell);
                storage.1.insert(cell, (function, None));
            }
        }
    }
    mod btreemapped {
        use std::collections::BTreeMap;
        use crate::{Cell, DbHandle};
        use std::hash::Hash;
        use super::Computation;
        /// A helper type for defining Computations with BTreeMap-backed storage
        pub struct BTreeMapStorage<T>(T);
        #[automatically_derived]
        impl<T: ::core::fmt::Debug> ::core::fmt::Debug for BTreeMapStorage<T> {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "BTreeMapStorage",
                    &&self.0,
                )
            }
        }
        #[automatically_derived]
        impl<T: ::core::default::Default> ::core::default::Default
        for BTreeMapStorage<T> {
            #[inline]
            fn default() -> BTreeMapStorage<T> {
                BTreeMapStorage(::core::default::Default::default())
            }
        }
        #[automatically_derived]
        impl<T: ::core::marker::Copy> ::core::marker::Copy for BTreeMapStorage<T> {}
        #[automatically_derived]
        impl<T: ::core::clone::Clone> ::core::clone::Clone for BTreeMapStorage<T> {
            #[inline]
            fn clone(&self) -> BTreeMapStorage<T> {
                BTreeMapStorage(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl<T> ::core::marker::StructuralPartialEq for BTreeMapStorage<T> {}
        #[automatically_derived]
        impl<T: ::core::cmp::PartialEq> ::core::cmp::PartialEq for BTreeMapStorage<T> {
            #[inline]
            fn eq(&self, other: &BTreeMapStorage<T>) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::Eq> ::core::cmp::Eq for BTreeMapStorage<T> {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<T>;
            }
        }
        #[automatically_derived]
        impl<T: ::core::hash::Hash> ::core::hash::Hash for BTreeMapStorage<T> {
            #[inline]
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
                ::core::hash::Hash::hash(&self.0, state)
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::PartialOrd> ::core::cmp::PartialOrd for BTreeMapStorage<T> {
            #[inline]
            fn partial_cmp(
                &self,
                other: &BTreeMapStorage<T>,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::cmp::PartialOrd::partial_cmp(&self.0, &other.0)
            }
        }
        #[automatically_derived]
        impl<T: ::core::cmp::Ord> ::core::cmp::Ord for BTreeMapStorage<T> {
            #[inline]
            fn cmp(&self, other: &BTreeMapStorage<T>) -> ::core::cmp::Ordering {
                ::core::cmp::Ord::cmp(&self.0, &other.0)
            }
        }
        impl<T> BTreeMapStorage<T> {
            pub fn new(value: T) -> Self {
                Self(value)
            }
        }
        impl<T> Computation for BTreeMapStorage<T>
        where
            T: Computation + Ord,
        {
            type Output = <T as Computation>::Output;
            type Storage = (
                BTreeMap<Self, Cell>,
                BTreeMap<Cell, (Self, Option<Self::Output>)>,
            );
            fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
                self.0.run(handle)
            }
            fn input_to_cell(
                input: &Self,
                (self_to_cell, _): &Self::Storage,
            ) -> Option<Cell> {
                self_to_cell.get(input).copied()
            }
            fn get_function_and_output(
                cell: Cell,
                (_, cell_to_output): &Self::Storage,
            ) -> (&Self, Option<&Self::Output>) {
                let (this, output) = &cell_to_output[&cell];
                (this, output.as_ref())
            }
            fn set_output(
                cell: Cell,
                new_output: Self::Output,
                (_, cell_to_output): &mut Self::Storage,
            ) {
                cell_to_output
                    .entry(cell)
                    .and_modify(|(_, output)| {
                        *output = Some(new_output);
                    });
            }
            fn insert_new_cell(cell: Cell, function: Self, storage: &mut Self::Storage) {
                storage.0.insert(function.clone(), cell);
                storage.1.insert(cell, (function, None));
            }
        }
    }
    #[macro_use]
    mod macros {}
    pub use input::{Input, OutputTypeForInput};
    pub use intermediate::{Intermediate, Run};
    pub use singleton::SingletonStorage;
    pub use hashmapped::HashMapStorage;
    pub use btreemapped::BTreeMapStorage;
    pub trait Computation: 'static + Sized + Clone {
        type Storage;
        type Output: Eq;
        fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output;
        fn input_to_cell(input: &Self, storage: &Self::Storage) -> Option<Cell>;
        fn insert_new_cell(cell: Cell, function: Self, storage: &mut Self::Storage);
        fn get_function_and_output(
            cell: Cell,
            storage: &Self::Storage,
        ) -> (&Self, Option<&Self::Output>);
        fn set_output(cell: Cell, output: Self::Output, storage: &mut Self::Storage);
        #[inline(always)]
        fn computation_id_of<T: Computation>() -> u32 {
            0
        }
        fn get_storage<Concrete: Computation + 'static>(
            computation_id: u32,
            container: &Self::Storage,
        ) -> &Concrete::Storage {
            match (&computation_id, &0) {
                (left_val, right_val) => {
                    if !(*left_val == *right_val) {
                        let kind = ::core::panicking::AssertKind::Eq;
                        ::core::panicking::assert_failed(
                            kind,
                            &*left_val,
                            &*right_val,
                            ::core::option::Option::Some(
                                format_args!("Type dispatch failed for get_storage"),
                            ),
                        );
                    }
                }
            };
            match (&TypeId::of::<Concrete::Storage>(), &TypeId::of::<Self::Storage>()) {
                (left_val, right_val) => {
                    if !(*left_val == *right_val) {
                        let kind = ::core::panicking::AssertKind::Eq;
                        ::core::panicking::assert_failed(
                            kind,
                            &*left_val,
                            &*right_val,
                            ::core::option::Option::Some(
                                format_args!(
                                    "Type dispatch failed for get_storage storage type:\n    {0}\n != {1}",
                                    std::any::type_name::<Concrete::Storage>(),
                                    std::any::type_name::<Self::Storage>(),
                                ),
                            ),
                        );
                    }
                }
            };
            unsafe { std::mem::transmute(container) }
        }
        fn get_storage_mut<Concrete: Computation + 'static>(
            computation_id: u32,
            container: &mut Self::Storage,
        ) -> &mut Concrete::Storage {
            match (&computation_id, &0) {
                (left_val, right_val) => {
                    if !(*left_val == *right_val) {
                        let kind = ::core::panicking::AssertKind::Eq;
                        ::core::panicking::assert_failed(
                            kind,
                            &*left_val,
                            &*right_val,
                            ::core::option::Option::Some(
                                format_args!(
                                    "Type dispatch failed for get_storage_mut container",
                                ),
                            ),
                        );
                    }
                }
            };
            match (&TypeId::of::<Concrete::Storage>(), &TypeId::of::<Self::Storage>()) {
                (left_val, right_val) => {
                    if !(*left_val == *right_val) {
                        let kind = ::core::panicking::AssertKind::Eq;
                        ::core::panicking::assert_failed(
                            kind,
                            &*left_val,
                            &*right_val,
                            ::core::option::Option::Some(
                                format_args!(
                                    "Type dispatch failed for get_storage_mut container type",
                                ),
                            ),
                        );
                    }
                }
            };
            unsafe { std::mem::transmute(container) }
        }
        /// True if this has any cached output
        fn output_is_unset<FullComputation: Computation>(
            cell: Cell,
            computation_id: u32,
            original_computation_id: u32,
            db: &Db<FullComputation>,
        ) -> bool {
            match (&computation_id, &0) {
                (left_val, right_val) => {
                    if !(*left_val == *right_val) {
                        let kind = ::core::panicking::AssertKind::Eq;
                        ::core::panicking::assert_failed(
                            kind,
                            &*left_val,
                            &*right_val,
                            ::core::option::Option::Some(
                                format_args!("Type dispatch failed for output_is_unset"),
                            ),
                        );
                    }
                }
            };
            let container = FullComputation::get_storage::<
                Self,
            >(original_computation_id, db.storage());
            let unset = Self::get_function_and_output(cell, container).1.is_none();
            {
                ::std::io::_print(
                    format_args!(
                        "{0} output is unset: {1}\n",
                        std::any::type_name::<Self>(),
                        unset,
                    ),
                );
            };
            unset
        }
        /// Given a Cell, TypeId pair dispatch to the correct run function
        /// and return true if the value has changed. This should also cache
        /// the new value if it has changed.
        /// Note that in dispatch functions `Self` is always the concrete, non-tuple type.
        fn dispatch_run<FullComputation: Computation>(
            cell: Cell,
            computation_id: u32,
            original_computation_id: u32,
            db: &mut Db<FullComputation>,
        ) -> bool
        where
            Self: Clone,
            Self::Output: Eq,
        {
            match (&computation_id, &0) {
                (left_val, right_val) => {
                    if !(*left_val == *right_val) {
                        let kind = ::core::panicking::AssertKind::Eq;
                        ::core::panicking::assert_failed(
                            kind,
                            &*left_val,
                            &*right_val,
                            ::core::option::Option::Some(
                                format_args!("Type dispatch failed for dispatch_run"),
                            ),
                        );
                    }
                }
            };
            let container = FullComputation::get_storage_mut::<
                Self,
            >(original_computation_id, db.storage_mut());
            let function = Self::get_function_and_output(cell, container).0.clone();
            let output = function.run(&mut db.handle(cell));
            FullComputation::dispatch_update_output::<
                Self,
                FullComputation,
            >(cell, original_computation_id, original_computation_id, output, db)
        }
        /// Dispatch to the correct update_output function to cache the new output
        /// and return true if the value has changed.
        /// Note that in dispatch functions `Self` is the current type being dispatched,
        /// `Concrete`, if present, is the non-tuple type of the target computation,
        /// and `FullComputation` is the type of the `Db` computation parameter which is
        /// usually a tuple of every possible computation.
        fn dispatch_update_output<Concrete, FullComputation>(
            cell: Cell,
            computation_id: u32,
            original_computation_id: u32,
            output: Concrete::Output,
            db: &mut Db<FullComputation>,
        ) -> bool
        where
            Concrete: Computation,
            FullComputation: Computation,
            Self::Output: Eq,
        {
            match (&computation_id, &0) {
                (left_val, right_val) => {
                    if !(*left_val == *right_val) {
                        let kind = ::core::panicking::AssertKind::Eq;
                        ::core::panicking::assert_failed(
                            kind,
                            &*left_val,
                            &*right_val,
                            ::core::option::Option::Some(
                                format_args!(
                                    "Type dispatch failed for dispatch_update_output",
                                ),
                            ),
                        );
                    }
                }
            };
            match (&TypeId::of::<Concrete::Output>(), &TypeId::of::<Self::Output>()) {
                (left_val, right_val) => {
                    if !(*left_val == *right_val) {
                        let kind = ::core::panicking::AssertKind::Eq;
                        ::core::panicking::assert_failed(
                            kind,
                            &*left_val,
                            &*right_val,
                            ::core::option::Option::Some(
                                format_args!(
                                    "Type dispatch failed for dispatch_update_output",
                                ),
                            ),
                        );
                    }
                }
            };
            let output2: Self::Output = unsafe { std::mem::transmute_copy(&output) };
            std::mem::forget(output);
            let container = FullComputation::get_storage_mut::<
                Self,
            >(original_computation_id, db.storage_mut());
            let (_, previous_value) = Self::get_function_and_output(cell, container);
            let changed = previous_value.map_or(true, |previous| output2 != *previous);
            if changed {
                Self::set_output(cell, output2, container);
            }
            changed
        }
        fn dispatch_input_to_cell<Concrete>(
            input: &Concrete,
            container: &Self::Storage,
        ) -> Option<Cell>
        where
            Concrete: 'static + Computation + Any,
        {
            match (&TypeId::of::<Concrete>(), &TypeId::of::<Self>()) {
                (left_val, right_val) => {
                    if !(*left_val == *right_val) {
                        let kind = ::core::panicking::AssertKind::Eq;
                        ::core::panicking::assert_failed(
                            kind,
                            &*left_val,
                            &*right_val,
                            ::core::option::Option::None,
                        );
                    }
                }
            };
            let input = (input as &dyn Any).downcast_ref().expect("T == Self");
            Self::input_to_cell(input, container)
        }
        fn dispatch_insert_new_cell<Concrete>(
            cell: Cell,
            input: Concrete,
            storage: &mut Self::Storage,
        )
        where
            Concrete: 'static + Computation + Any,
            Concrete::Storage: 'static,
        {
            let input = transmute_copy_checked::<Concrete, Self>(input);
            Self::insert_new_cell(cell, input, storage)
        }
    }
    fn transmute_copy_checked<A: 'static, B: 'static>(x: A) -> B {
        match (&TypeId::of::<A>(), &TypeId::of::<B>()) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    let kind = ::core::panicking::AssertKind::Eq;
                    ::core::panicking::assert_failed(
                        kind,
                        &*left_val,
                        &*right_val,
                        ::core::option::Option::None,
                    );
                }
            }
        };
        let x2: B = unsafe { std::mem::transmute_copy(&x) };
        std::mem::forget(x);
        x2
    }
}
mod db {
    use crate::cell::CellData;
    use crate::{Cell, Computation};
    use petgraph::graph::DiGraph;
    mod handle {
        use petgraph::visit::EdgeRef;
        use crate::{Cell, Computation, Db};
        /// A handle to the database during some operation.
        ///
        /// This wraps calls to the Db so that any `get` calls
        /// will be automatically registered as dependencies of
        /// the current operation.
        pub struct DbHandle<'db, C: Computation> {
            db: &'db mut Db<C>,
            current_operation: Cell,
        }
        impl<'db, C: Computation> DbHandle<'db, C> {
            pub(crate) fn new(db: &'db mut Db<C>, current_operation: Cell) -> Self {
                let edges = db
                    .cells
                    .edges(current_operation.index())
                    .map(|edge| edge.id())
                    .collect::<Vec<_>>();
                for edge in edges {
                    db.cells.remove_edge(edge);
                }
                Self { db, current_operation }
            }
            pub fn get<Concrete: Computation>(
                &mut self,
                compute: Concrete,
            ) -> &Concrete::Output
            where
                C::Output: Eq,
                C: Clone,
            {
                let dependency = self.db.get_or_insert_cell(compute);
                self.db
                    .cells
                    .update_edge(self.current_operation.index(), dependency.index(), ());
                self.db.get_with_cell::<Concrete>(dependency)
            }
        }
    }
    pub use handle::DbHandle;
    const START_VERSION: u32 = 1;
    pub struct Db<C: Computation> {
        cells: DiGraph<CellData, ()>,
        version: u32,
        storage: C::Storage,
    }
    impl<C: Computation> Db<C>
    where
        C::Storage: Default,
    {
        pub fn new() -> Self {
            Self {
                cells: DiGraph::default(),
                version: START_VERSION,
                storage: Default::default(),
            }
        }
    }
    impl<C: Computation> Db<C> {
        /// True if a given input is stale and needs to be re-computed.
        /// Inputs which have never been computed are also considered stale.
        ///
        /// This does not actually re-compute the input.
        pub fn is_stale<Concrete: Computation>(&self, input: &Concrete) -> bool {
            let Some(cell) = self.get_cell(input) else {
                {
                    ::std::io::_print(
                        format_args!("{0} is stale\n", std::any::type_name::<Concrete>()),
                    );
                };
                return true;
            };
            let r = self.is_stale_cell(cell);
            {
                ::std::io::_print(
                    format_args!(
                        "{0} is stale: {1}\n",
                        std::any::type_name::<Concrete>(),
                        r,
                    ),
                );
            };
            r
        }
        /// True if a given cell is stale and needs to be re-computed.
        /// This does not actually re-compute the input.
        pub fn is_stale_cell(&self, cell: Cell) -> bool {
            let computation_id = self.cells[cell.index()].computation_id;
            if C::output_is_unset::<C>(cell, computation_id, computation_id, self) {
                {
                    ::std::io::_print(
                        format_args!("output {0} is unset\n", computation_id),
                    );
                };
                return true;
            }
            let neighbors = self.cells.neighbors(cell.index()).collect::<Vec<_>>();
            neighbors
                .into_iter()
                .any(|dependency_id| {
                    let dependency = &self.cells[dependency_id];
                    let cell = &self.cells[cell.index()];
                    dependency.last_verified_version != self.version
                        || dependency.last_updated_version > cell.last_verified_version
                })
        }
        /// Return the corresponding Cell for a given input, if it exists.
        ///
        /// This will not update any values.
        pub fn get_cell<ConcreteC: Computation>(
            &self,
            input: &ConcreteC,
        ) -> Option<Cell> {
            C::dispatch_input_to_cell(input, &self.storage)
        }
        pub fn get_or_insert_cell<ConcreteC>(&mut self, input: ConcreteC) -> Cell
        where
            ConcreteC: Computation,
        {
            if let Some(cell) = C::dispatch_input_to_cell(&input, &self.storage) {
                cell
            } else {
                let computation_id = C::computation_id_of::<ConcreteC>();
                let new_id = self.cells.add_node(CellData::new(computation_id));
                let cell = Cell::new(new_id);
                C::dispatch_insert_new_cell(cell, input, &mut self.storage);
                cell
            }
        }
        /// Updates an input with a new value
        ///
        /// May panic in Debug mode if the input is not an input - ie. it has at least 1 dependency.
        /// Note that this step is skipped when compiling in Release mode.
        pub fn update_input<ConcreteC: Computation>(
            &mut self,
            input: ConcreteC,
            new_value: ConcreteC::Output,
        )
        where
            ConcreteC: std::fmt::Debug,
            C::Output: Eq,
        {
            let debug = ::alloc::__export::must_use({
                ::alloc::fmt::format(format_args!("{0:?}", input))
            });
            let cell_id = self.get_or_insert_cell(input);
            if true {
                if !self.is_input(cell_id) {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "`{0:?}` is not an input - inputs must have 0 dependencies",
                                debug,
                            ),
                        );
                    }
                }
            }
            let cell = &self.cells[cell_id.index()];
            let computation_id = cell.computation_id;
            let changed = C::dispatch_update_output::<
                ConcreteC,
                C,
            >(cell_id, computation_id, computation_id, new_value, self);
            let cell = &mut self.cells[cell_id.index()];
            if changed {
                self.version += 1;
                cell.last_updated_version = self.version;
                cell.last_verified_version = self.version;
            } else {
                cell.last_verified_version = self.version;
            }
        }
        fn is_input(&self, cell: Cell) -> bool {
            self.cells.neighbors(cell.index()).count() == 0
        }
        pub(crate) fn handle(&mut self, cell: Cell) -> DbHandle<C> {
            DbHandle::new(self, cell)
        }
        pub fn storage(&self) -> &C::Storage {
            &self.storage
        }
        pub fn storage_mut(&mut self) -> &mut C::Storage {
            &mut self.storage
        }
    }
    impl<C: Computation + Clone> Db<C>
    where
        C::Output: Eq,
    {
        /// Similar to `update_input` but runs the compute function
        /// instead of accepting a given value. This also will not update
        /// `self.version`
        fn run_compute_function(&mut self, cell_id: Cell)
        where
            C::Output: Eq,
        {
            let cell = &self.cells[cell_id.index()];
            let computation_id = cell.computation_id;
            let changed = C::dispatch_run::<
                C,
            >(cell_id, computation_id, computation_id, self);
            let cell = &mut self.cells[cell_id.index()];
            cell.last_verified_version = self.version;
            if changed {
                cell.last_updated_version = self.version;
            }
        }
        /// Trigger an update of the given cell, recursively checking and re-running any out of date
        /// dependencies.
        fn update_cell(&mut self, cell_id: Cell) {
            let cell = &self.cells[cell_id.index()];
            if cell.last_verified_version != self.version {
                if self.is_stale_cell(cell_id) {
                    self.run_compute_function(cell_id);
                } else {
                    let cell = &mut self.cells[cell_id.index()];
                    cell.last_verified_version = self.version;
                }
            }
        }
        /// Retrieves the up to date value for the given computation, re-running any dependencies as
        /// necessary.
        ///
        /// This function can panic if the dynamic type of the value returned by `compute.run(..)` is not `T`.
        pub fn get<Concrete: Computation>(
            &mut self,
            compute: Concrete,
        ) -> &Concrete::Output {
            let cell_id = self.get_or_insert_cell(compute);
            self.get_with_cell::<Concrete>(cell_id)
        }
        /// Retrieves the up to date value for the given cell, re-running any dependencies as
        /// necessary.
        ///
        /// This function can panic if the dynamic type of the value returned by `compute.run(..)` is not `T`.
        pub fn get_with_cell<Concrete: Computation>(
            &mut self,
            cell_id: Cell,
        ) -> &Concrete::Output {
            self.update_cell(cell_id);
            let computation_id = self.cells[cell_id.index()].computation_id;
            {
                ::std::io::_print(
                    format_args!(
                        "get_with_cell get_storage_mut::<{0}> with id {1}\n",
                        std::any::type_name::<Concrete>(),
                        computation_id,
                    ),
                );
            };
            let container = C::get_storage_mut::<
                Concrete,
            >(computation_id, self.storage_mut());
            Concrete::get_function_and_output(cell_id, container)
                .1
                .expect("cell result should have been computed already")
        }
    }
}
mod interned {}
pub use cell::Cell;
pub use computation::{
    Intermediate, Computation, Input, OutputTypeForInput, Run, HashMapStorage,
    BTreeMapStorage, SingletonStorage,
};
pub use db::{Db, DbHandle};
