# Inc-Complete

Inc-Complete is a library for writing **in**cremental **comp**utations that re-execute the minimum
number of steps when an input is changed. Example uses:

- Compilers: re-compile only the portion of a program which has changed
- Spreadsheets: re-compute only formulas which rely on changed cells

Compared to existing solutions like [salsa](https://github.com/salsa-rs/salsa) or [adapton](https://docs.rs/adapton/latest/adapton/),
inc-complete is built from the ground-up to support one feature: (de)serialization to enable
incremental compilers across separate compiler runs. If you do not need this I recommend using
a more mature library like Salsa instead. If you do need this then continue reading!

## Status

This library is in a very early but working state. Expect the API to change over time, and
expect certain patterns to be somewhat obtuse. Additionally, while serialization is working,
the format is not currently stable across releases of inc-complete.

## Current Features

- [x] Load from disk
  - Manual save and load to disk to restart from where the previous program run left off.
  - Just serialize and deserialize the central `Db` object.
- [x] Thread-safe

## Planned Features & Roadmap

- [ ] Accumulator abstraction for collecting lists of items across computations (useful for compiler errors)
- [ ] Support for interning arbitrary data
- [ ] Cyclical dependency check for debugging
- [ ] Manually invoked garbage collection for old cached computation results

## Quick Start

See the [docs for a quick start explanation](https://docs.rs/inc-complete/latest/inc_complete/) on how to use this library.

Also see https://github.com/jfecher/modern-compiler-architecture for a full example of an incremental and concurrent compiler using inc-complete.
