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
