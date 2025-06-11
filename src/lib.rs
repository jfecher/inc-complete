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
//! #[derive(Clone, Debug)]
//! struct A1;
//!
//! #[derive(Clone, Debug)]
//! struct A2;
//!
//! #[derive(Clone, PartialEq, Eq, Hash)]
//! struct B1;
//!
//! #[derive(Clone, PartialEq, Eq, Hash)]
//! struct B2;
//! ```
//!
//! The derives are all necessary for some traits we'll implement later.
//!
//! Now we can define a type alias for the tuple containing all our computation types:
//!
//! ```
//! # #[derive(Clone, Debug)]
//! # struct A1;
//! # #[derive(Clone, Debug)]
//! # struct A2;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B1;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B2;
//! use inc_complete::{ Input, Cached };
//!
//! type Spreadsheet = (
//!     Input<A1>,
//!     Input<A2>,
//!     Cached<B1>,
//!     Cached<B2>,
//! );
//! ```
//!
//! Note that we have to tell inc-complete whether this computation is an input or not.
//! Among other things, this affects the storage type these values are cached in. For `Input` types we
//! now need to define what type the input is. For this spreadsheet example all
//! our types are `i64`:
//!
//! ```
//! # #[derive(Clone, Debug)]
//! # struct A1;
//! # #[derive(Clone, Debug)]
//! # struct A2;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B1;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B2;
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
//! For `Cached` types we need to provide a `run` function to compute their result. This function
//! will have access to the computation type itself (which often store parameters as data) and
//! a `DbHandle` object to query sub-computations with:
//!
//! ```
//! # #[derive(Clone, Debug)]
//! # struct A1;
//! # #[derive(Clone, Debug)]
//! # struct A2;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B1;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B2;
//! # impl inc_complete::OutputTypeForInput for A1 {
//! #     type Output = i64;
//! # }
//! # impl inc_complete::OutputTypeForInput for A2 {
//! #     type Output = i64;
//! # }
//! # use inc_complete::{ Input, Cached };
//! use inc_complete::{ Run, DbHandle, Computation };
//!
//! impl Run for B1 {
//!     type Output = i64;
//!
//!     fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
//!         // These functions should be pure but we're going to cheat here to
//!         // make it obvious when a function is recomputed
//!         println!("Computing B1!");
//!         *handle.get(Input::<A1>::new()) + 8
//!     }
//! }
//!
//! impl Run for B2 {
//!     type Output = i64;
//!
//!     fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
//!         println!("Computing B2!");
//!         *handle.get(Cached::new(B1)) + *handle.get(Input::<A2>::new())
//!     }
//! }
//! ```
//!
//! Having to wrap computations in an `Input` or `Cached` wrapper each time can be
//! burdensome so in a real program we may want to define `new` functions which do this for us.
//! 
//! With that out of the way, we can finally create our `Db`, set the initial values for our
//! inputs, and run our program:
//!
//! ```
//! # #[derive(Clone, Debug)]
//! # struct A1;
//! # #[derive(Clone, Debug)]
//! # struct A2;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B1;
//! # #[derive(Clone, PartialEq, Eq, Hash)]
//! # struct B2;
//! # impl inc_complete::OutputTypeForInput for A1 {
//! #     type Output = i64;
//! # }
//! # impl inc_complete::OutputTypeForInput for A2 {
//! #     type Output = i64;
//! # }
//! # impl inc_complete::Run for B1 {
//! #     type Output = i64;
//! #     fn run(&self, handle: &mut inc_complete::DbHandle<impl inc_complete::Computation>) -> Self::Output {
//! #         // These functions should be pure but we're going to cheat here to
//! #         // make it obvious when a function is recomputed
//! #         println!("Computing B1!");
//! #         *handle.get(inc_complete::Input::<A1>::new()) + 8
//! #     }
//! # }
//! # impl inc_complete::Run for B2 {
//! #     type Output = i64;
//! #     fn run(&self, handle: &mut inc_complete::DbHandle<impl inc_complete::Computation>) -> Self::Output {
//! #         println!("Computing B2!");
//! #         *handle.get(inc_complete::Cached::new(B1)) + *handle.get(inc_complete::Input::<A2>::new())
//! #     }
//! # }
//! # type Spreadsheet = (
//! #     inc_complete::Input<A1>,
//! #     inc_complete::Input<A2>,
//! #     inc_complete::Cached<B1>,
//! #     inc_complete::Cached<B2>,
//! # );
//! # use inc_complete::{ Input, Cached };
//! use inc_complete::Db;
//! type SpreadsheetDb = Db<Spreadsheet>;
//!
//! fn main() {
//!     let mut db = SpreadsheetDb::new();
//!     db.update_input(Input::<A1>::new(), 12);
//!     db.update_input(Input::<A2>::new(), 4);
//!
//!     // Output:
//!     // Computing B2!
//!     // Computing B1!
//!     let b2 = *db.get(Cached::new(B2));
//!     assert_eq!(b2, 24);
//!
//!     // No output, result of B2 is cached
//!     let b2 = *db.get(Cached::new(B2));
//!     assert_eq!(b2, 24);
//!
//!     // Now lets update an input
//!     db.update_input(Input::<A2>::new(), 10);
//!
//!     // B2 is now stale and gets recomputed, but crucially B1
//!     // does not depend on A2 and does not get recomputed.
//!     // Output:
//!     // Computing B2!
//!     let b2 = *db.get(Cached::new(B2));
//!     assert_eq!(b2, 30);
//! }
//! ```
//!
//! ...And that's it for basic usage! If you want to delve deeper you can implement
//! your own `Input` or `Cached`-like wrapper to have more control over how your
//! type is cached by implementing the `Computation` trait.
//!
//! This example did not show it but you can also use structs with fields in your computations, e.g:
//!
//! ```
//! # use inc_complete::{ Cached, Run, DbHandle, Computation };
//! // a fibonacci function with cached sub-results 
//! #[derive(Clone, PartialEq, Eq, Hash)]
//! struct Fibonacci { x: u32 }
//!
//! impl Fibonacci {
//!     fn new(x: u32) -> Cached<Fibonacci> {
//!         Cached::new(Fibonacci { x })
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
mod computation;
mod db;
mod interned;

pub use cell::Cell;
pub use computation::{Cached, Computation, Input, OutputTypeForInput, Run};
pub use db::{Db, DbHandle};
