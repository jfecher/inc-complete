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
