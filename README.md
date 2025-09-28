# Inc-Complete

Inc-Complete is a library for writing **in**cremental **comp**utations that re-execute the minimum
number of steps when an input is changed. Example uses:

- Compilers: re-compile only the portion of a program which has changed
- Spreadsheets: re-compute only formulas which rely on changed cells
- Memoization: while this can be used for memoization, it is a rather heavy-handed solution since it also tracks dependencies across every memoized function.

## Status

This library is working but in a pre-1.0 state. Expect the API to change over time.
Additionally, while serialization is working, the format is not currently stable across non-bugfix releases of inc-complete.
See the CHANGELOG.md for changes for each version.

## Current Features

- [x] Load from disk
  - Manual save and load to disk to restart from where the previous program run left off
  - Just serialize and deserialize the central `Db` object
- [x] Thread-safe
- [x] Accumulator abstraction for collecting lists of items across computations (useful for compiler errors)
- [x] Cyclical dependency check - an error is issued when computations recursively depend on each other. Works even if computations are run on separate threads
- [x] Manually invoked garbage collection for old cached computation results

## Planned Features & Roadmap

- [ ] Support for interning arbitrary data (computations and results must currently be cloned)

## Quick Start

See the [docs for a quick start explanation](https://docs.rs/inc-complete/latest/inc_complete/) on how to use this library.

Also see https://github.com/jfecher/modern-compiler-architecture for a full example of an incremental and concurrent compiler using inc-complete.
