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
mod cell;
#[macro_use]
mod computation;
mod db;
mod interned;

pub use ::paste;

pub use cell::Cell;
pub use computation::{
    BTreeMapStorage, Computation, HashMapStorage, Input, Intermediate, OutputTypeForInput, Run,
    SingletonStorage,
};
pub use db::{Db, DbHandle};
