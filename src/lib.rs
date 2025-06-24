//! # Inc-Complete
//!
//! inc-complete is a library for incremental compilation supporting serialization from the ground
//! up.
//!
//! In inc-complete, a central `Db` object is used to query and cache the result of _pure_
//! functions. The functions being pure is key. If there are side-effects performed then
//! they will not be re-performed when the computation's result is later cached and returned again.
//!
//! Before we create the `Db` object however, we need to define the storage type to store all
//! the computations we want to cache. In inc-complete, each computation is its own type and is
//! either an input (if it has no dependencies) or an intermediate computation. For this example
//! we're going to model the following spreadsheet:
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
//! #[derive(Clone)]
//! struct A1;
//!
//! #[derive(Clone)]
//! struct A2;
//!
//! #[derive(Clone)]
//! struct B1;
//!
//! #[derive(Clone)]
//! struct B2;
//! ```
//!
//! The derives are all necessary for some traits we'll implement later.
//!
//! Now we can define the actual storage type for all our computations. We need to
//! call `impl_storage!` to implement a few traits for us. These can also be
//! implemented manually, but there is little advantage in doing so.
//!
//! ```
//! # #[derive(Clone)]
//! # struct A1;
//! # #[derive(Clone)]
//! # struct A2;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B1;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B2;
//! # use inc_complete::storage::SingletonStorage;
//! # use inc_complete::define_input;
//! # define_input!(0, A1 -> i64, Spreadsheet);
//! # define_input!(1, A2 -> i64, Spreadsheet);
//! # use inc_complete::define_intermediate;
//! # define_intermediate!(2, B1 -> i64, Spreadsheet, |_b1, db| {
//! #     // These functions should be pure but we're going to cheat here to
//! #     // make it obvious when a function is recomputed
//! #     println!("Computing B1!");
//! #     db.get(A1) + 8
//! # });
//! # // Larger programs may wish to pass an existing function instead of a closure
//! # define_intermediate!(3, B2 -> i64, Spreadsheet, |_b2, db| {
//! #     println!("Computing B2!");
//! #     db.get(B1) + db.get(A2)
//! # });
//! use inc_complete::impl_storage;
//!
//! ##[derive(Default)]
//! struct Spreadsheet {
//!     a1: SingletonStorage<A1>,
//!     a2: SingletonStorage<A2>,
//!     b1: SingletonStorage<B1>,
//!     b2: SingletonStorage<B2>,
//! }
//!
//! // This macro may be replaced with a derive proc macro in the future
//! impl_storage!(Spreadsheet,
//!     a1: A1,
//!     a2: A2,
//!     b1: B1,
//!     b2: B2,
//! );
//! ```
//!
//! In this example, we're using `SingletonStorage` for all of our
//! computations because all of `A1`, `A2`, `B1`, and `B2` are singleton values like `()` with
//! only a single value in their type. This lets us store them with an `Option<T>` instead of a
//! `HashMap<K, V>`. If you are unsure which storage type to choose, `HashMapStorage<T>`
//! is a good default. Even if used on singletons it will give you correct
//! behavior, just with slightly worse performance than `SingletonStorage<T>`.
//!
//! Next, for `Input` types we now need to define:
//! 1. What type the input is - for this spreadsheet example all our types are `i64`.
//! 2. A unique computation id. This id uniquely identifies the particular computation type we
//!    want to cache. If there are any duplicates inc-complete may call the wrong `run` function
//!    to update a computation! These are also part of the serialized output so they are important
//!    to keep stable across changes if you want your serialization to remain backwards-compatible.
//!
//! For both of these, we can use the `define_input!` macro:
//! ```
//! # #[derive(Clone)]
//! # struct A1;
//! # #[derive(Clone)]
//! # struct A2;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B1;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B2;
//! # use inc_complete::storage::SingletonStorage;
//! # use inc_complete::{ impl_storage, DbHandle };
//! # #[derive(Default)]
//! # struct Spreadsheet {
//! #     a1: SingletonStorage<A1>,
//! #     a2: SingletonStorage<A2>,
//! #     b1: SingletonStorage<B1>,
//! #     b2: SingletonStorage<B2>,
//! # }
//! # // This macro may be replaced with a derive proc macro in the future
//! # impl_storage!(Spreadsheet,
//! #     a1: A1,
//! #     a2: A2,
//! #     b1: B1,
//! #     b2: B2,
//! # );
//! # use inc_complete::define_intermediate;
//! # define_intermediate!(2, B1 -> i64, Spreadsheet, |_b1: &B1, db: &DbHandle<Spreadsheet>| {
//! #     // These functions should be pure but we're going to cheat here to
//! #     // make it obvious when a function is recomputed
//! #     println!("Computing B1!");
//! #     db.get(A1) + 8
//! # });
//! # // Larger programs may wish to pass an existing function instead of a closure
//! # define_intermediate!(3, B2 -> i64, Spreadsheet, |_b2, db| {
//! #     println!("Computing B2!");
//! #     db.get(B1) + db.get(A2)
//! # });
//! use inc_complete::define_input;
//!
//! define_input!(0, A1 -> i64, Spreadsheet);
//! define_input!(1, A2 -> i64, Spreadsheet);
//! ```
//!
//! For intermediate computations we need to provide all of the above, along with a `run` function
//! to compute their result. This function will have access to the computation type itself
//! (which often store parameters as data) and a `DbHandle` object to query sub-computations with:
//!
//! ```
//! # #[derive(Clone)]
//! # struct A1;
//! # #[derive(Clone)]
//! # struct A2;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B1;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B2;
//! # use inc_complete::storage::SingletonStorage;
//! # #[derive(Default)]
//! # struct Spreadsheet {
//! #     a1: SingletonStorage<A1>,
//! #     a2: SingletonStorage<A2>,
//! #     b1: SingletonStorage<B1>,
//! #     b2: SingletonStorage<B2>,
//! # }
//! # // This macro may be replaced with a derive proc macro in the future
//! # impl_storage!(Spreadsheet,
//! #     a1: A1,
//! #     a2: A2,
//! #     b1: B1,
//! #     b2: B2,
//! # );
//! # use inc_complete::{ define_input, impl_storage };
//! # define_input!(0, A1 -> i64, Spreadsheet);
//! # define_input!(1, A2 -> i64, Spreadsheet);
//! use inc_complete::{ define_intermediate, DbHandle };
//!
//! define_intermediate!(2, B1 -> i64, Spreadsheet, |_b1: &B1, db: &DbHandle<Spreadsheet>| {
//!     // These functions should be pure but we're going to cheat here to
//!     // make it obvious when a function is recomputed
//!     println!("Computing B1!");
//!     db.get(A1) + 8
//! });
//!
//! // Larger programs may wish to pass an existing function instead of a closure
//! define_intermediate!(3, B2 -> i64, Spreadsheet, |_b2, db| {
//!     println!("Computing B2!");
//!     db.get(B1) + db.get(A2)
//! });
//! ```
//!
//! Ceremony aside - this code should be relatively straight-forward. We `get` the value of
//! any sub-computations we need and the `DbHandle` object automatically gives us the most
//! up to date version of those computations - we'll examine this claim a bit closer later.
//!
//! With that out of the way though, we can finally create our `Db`, set the initial values for our
//! inputs, and run our program:
//!
//! ```
//! # #[derive(Clone)]
//! # struct A1;
//! # #[derive(Clone)]
//! # struct A2;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B1;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B2;
//! # use inc_complete::storage::SingletonStorage;
//! # #[derive(Default)]
//! # struct Spreadsheet {
//! #     a1: SingletonStorage<A1>,
//! #     a2: SingletonStorage<A2>,
//! #     b1: SingletonStorage<B1>,
//! #     b2: SingletonStorage<B2>,
//! # }
//! # use inc_complete::{ define_input, impl_storage, define_intermediate };
//! # // This macro may be replaced with a derive proc macro in the future
//! # impl_storage!(Spreadsheet,
//! #     a1: A1,
//! #     a2: A2,
//! #     b1: B1,
//! #     b2: B2,
//! # );
//! # define_input!(0, A1 -> i64, Spreadsheet);
//! # define_input!(1, A2 -> i64, Spreadsheet);
//! # define_intermediate!(2, B1 -> i64, Spreadsheet, |_b1, db| {
//! #     // These functions should be pure but we're going to cheat here to
//! #     // make it obvious when a function is recomputed
//! #     println!("Computing B1!");
//! #     db.get(A1) + 8
//! # });
//! # // Larger programs may wish to pass an existing function instead of a closure
//! # define_intermediate!(3, B2 -> i64, Spreadsheet, |_b2, db| {
//! #     println!("Computing B2!");
//! #     db.get(B1) + db.get(A2)
//! # });
//! type SpreadsheetDb = inc_complete::Db<Spreadsheet>;
//!
//! fn main() {
//!     let mut db = SpreadsheetDb::new();
//!     db.update_input(A1, 12);
//!     db.update_input(A2, 4);
//!
//!     // Output:
//!     // Computing B2!
//!     // Computing B1!
//!     let b2 = db.get(B2);
//!     assert_eq!(b2, 24);
//!
//!     // No output, result of B2 is cached
//!     let b2 = db.get(B2);
//!     assert_eq!(b2, 24);
//!
//!     // Now lets update an input
//!     db.update_input(A2, 10);
//!
//!     // B2 is now stale and gets recomputed, but crucially B1
//!     // does not depend on A2 and does not get recomputed.
//!     // Output:
//!     // Computing B2!
//!     let b2 = db.get(B2);
//!     assert_eq!(b2, 30);
//! }
//! ```
//!
//! ...And that's it for basic usage! If you want to delve deeper you can manually implement
//! `Storage` for your storage type or `StorageFor` to define a new storage type for a single input
//! (like `SingletonStorage` or `HashMapStorage` which inc-complete defines).
//!
//! This example did not show it but you can also use structs with fields in your computations, e.g:
//!
//! ```
//! use inc_complete::{ storage::HashMapStorage, impl_storage, define_intermediate };
//!
//! #[derive(Default)]
//! struct MyStorageType {
//!     fibs: HashMapStorage<Fibonacci>,
//! }
//!
//! impl_storage!(MyStorageType, fibs: Fibonacci);
//!
//! // a fibonacci function with cached sub-results
//! #[derive(Clone, PartialEq, Eq, Hash)]
//! struct Fibonacci(u32);
//!
//! define_intermediate!(0, Fibonacci -> u32, MyStorageType, |fib, db| {
//!     let x = fib.0;
//!     if x <= 1 {
//!         x
//!     } else {
//!         // Not exponential time since each sub-computation will be cached!
//!         db.get(Fibonacci(x - 1)) + db.get(Fibonacci(x - 2))
//!     }
//! });
//! ```
//!
//! These fields often correspond to parameters of the function being modeled, in
//! this case the integer input to `fibonacci`.
//!
//! ## Serialization
//!
//! Serialization can be done by serializing the `Db<S>` object. All cached computations
//! are stored in the provided storage type `S` so it is up to users to decide how they
//! want to serialize this storage. As a starting point, it is recommended to tag a field
//! with `#[serde(default)]` when a new field is added to keep serialization backwards-compatible
//! when deserializing previous versions of `S`. See the source file `tests/serialize.rs`
//! as an example of this.
mod cell;
#[macro_use]
pub mod storage;
mod db;
mod interned;

pub use cell::Cell;
pub use db::{Db, DbHandle};
pub use storage::{ComputationId, OutputType, Run, Storage, StorageFor};
