# 0.7.1

- Updated inc-complete-derive version. 0.7.0 incorrectly did not ship with 0.7.0 of inc-complete derive which
causes issues when deriving `Storage`.

# 0.7.0

0.7.0 Adds accumulated values

## Breaking:

- The syntax in `impl_storage!` has changed to require a comma after the last field.
- The experimental `async` feature has been removed.

## Non-breaking:

- Added `inc_complete::accumulate::{ Accumulate, Accumulator }`:
  - Query all accumulated values with `Db::get_accumulated`, collecting into any `FromIterator` type.
  Accumulated items are returned in dependency-order with the exception that all items within a single
  dependency are considered to be pushed at the end of that dependency. This means accumulated values
  from a parent computation are never interleaved between those of its child computations.
  - Use the `Accumulator<MyType>` wrapper in your storage to enable accumulating `MyType`.
    - Requires adding the following to the end of your `impl_storage!` macro:
    ```
    impl_storage!(MyStorage,
        ...
        // Add the following lines to enable accumulating for `MyType`
        @accumulators {
            my_accumulator_field_name: MyType,
            ... more accumulators ...
        }
    );
    ```
    - If you use the derive macros, add `#[inc_complete(accumulate)]` above your
    struct field:
    ```
    #[inc_complete(accumulate)]
    my_accumulator_field_name: Accumulator<MyType>,
    ```

# 0.6.2

- Added `derive` macros as alternatives to the current `macro_rules!` macros.
  - The current `macro_rules!` macros may be deprecated later.

# 0.6.1

- Switch `HashMapStorage` to use `DashMap` by default
- Optimize input dependency tracking slightly - use an FxHashSet instead of a BTreeSet.

# 0.6.0

- Optimizes graph search when an input unused by a computation is changed.
  - Computations now track inputs they indirectly use and we can cut off the graph search early
    when none of the inputs they use have changed. This greatly improves performance when you
    have a large graph with many inputs which each may affect only some subset of the graph at the
    cost of some performance of the general case.

## Breaking:

- Serialization for `CellData` includes an additional `input_dependencies` field

# 0.5.3

- Lock computations to prevent multiple threads unnecessarily running the same computation

# 0.5.2

- Allow specifying a custom `BuildHasher` for `HashMapStorage`. This defaults to `rustc_hash::FxBuildHasher` if unspecified.

# 0.5.1

- Switched to `rustc-hash` as the default hasher for `HashMapStorage`

# 0.5.0

0.5.0 Adds support for multithreading and an experimental `async` feature.

Aside from some breaking changes, the transition from 0.4.2 to 0.5.0 is expected
to be relatively smooth since computations were already required to be pure.

## Breaking:

- `Db` serialization post 0.5.0 is not compatible with older versions of this library
- Traits now require a `&DbHandle<Storage>` instead of a `&mut DbHandle<Storage>`
- `StorageFor` methods must return keys/values by value rather than reference now.
- `BTreeMapStorage` has been removed. Storage must be thread-safe now
- `HashMapStorage` is serialized as a `Vec` now for more compatibility with libraries
  such as serde-json which only support maps with string keys.
- `define_input!` and `define_intermediate!` now defines a `get` method on the computation
  type. So as an alternative to `db.get(Type)` you can now call `Type.get(db)`. This will conflict
  with any existing `get` method on the type.
- `define_input!` now defines a `set` method on the computation type. So as an alternative to
  `db.update_input(Type, value)` you can now call `Type.set(db, value)`. This will conflict with
  any existing `set` method on the type.

## Non-breaking:

- `TreeIndexStorage` has been added backed by two `scc::TreeIndex`. It is a read-optimized
  storage which may be performant if you expect your computation not to change often. `TreeIndexStorage`
  is serialized as a `Vec`.
- `DbGet` trait has been added to abstract over `Db::get` and `DbHandle::get`.

# 0.4.2

- Fixed a large bug preventing backdating from working in more cases. Due to this bug,
inc-complete did not re-run the minimal number of dependencies previously.

# 0.4.1

- Added `impl<S: Default> Default for Db<S>`

# 0.4.0

0.4.0 has many changes intended to give more control over serialization to users
and greatly simplify the trait interface for implementing custom storage types.

Now, instead of a compilation tuple which implicitly builds up storage for each type used,
users provide their own concrete struct type `S` to the `Db<S>`. This storage struct must
be able to cache every computation type used and must implement `Storage` once, and `StorageFor<C>`
for every computation type `C`.

Serialization is still done by serializing a `Db<S>`. For backwards-compatibility `S` can be defined
to match the serialization format of the old computation tuple, but users may find it easier to
extend in the future by have a breaking serialization change to their new struct type. When adding
a field to your new storage struct, it is recommended to tag it as `#[serde(default)]` to keep
backwards compatibility while deserializing an older version of your struct.

## Breaking:

- Removed `Computation`
- `Db<T>` now requires `T: Storage` and `T: StorageFor<C>` for every computation type `C` that is used.
- `define_input!` and `define_intermediate!` have a new syntax and now define `ComputationId`, `OutputType`, and `Run`
- `Run` has been split into `Run` and `OutputType`
- `Run` now takes a concrete `&mut DbHandle<S>` argument where `S` is the concrete storage type used instead of a `&mut DbHandle<impl Computation>`.
- Some `Db` methods accepting `Cell`s have now been made private

## Non-breaking:

- Added `Storage` and `StorageFor` traits
- Added `impl_storage!` derive `Storage` and forwarding impls for `StorageFor`
- Added `DbHandle::storage` and `DbHandle::storage_mut`
- Removed `paste` as a dependency
- Serde is now a required dependency for simplicity

# 0.3.4

- Removed debug output

# 0.3.3

- Macros now define items as `pub(crate)`

# 0.3.2

- Fixed `paste!` macro dependency preventing use of `define_input!` and `define_intermediate!`

# 0.3.1

- Added `define_input!` and `define_intermediate` macros for defining a `HashMapStorage<Input<T>>` type and `HashMapStorage<Intermediate<T>>` type respectively.

# 0.3.0

- Separated a `Computation`'s "derivedness" from its storage type.
  - New wrapper: `HashMapStorage`, `BTreeMapStorage`, `SingletonStorage`
    - These must be on the outside of either `Input`, `Intermediate`, or your own custom type which implements `Computation`.
  - `Input` no longer implies a singleton storage.
  - `Cached` has been renamed to `Intermediate` and no longer implies a `HashMapStorage`.

# 0.2.0

- Switch from `Value = Box<dyn Any>` to a trait based approach to preserve concrete types for deserialization
