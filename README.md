# Inc-Complete

Inc-Complete is a library for writing **in**cremental **comp**utations that re-execute the minimum
number of steps when an input is changed. Example uses:

- Compilers: re-compile only the portion of a program which has changed
- Spreadsheets: re-compute only formulas which rely on changed cells

## Status

This library is in initial development and should not be used at the moment.

## Planned Features

- Load from disk
  - Manual save and load to disk to restart from where the previous program run left off.
- Multi-threaded
  - Tasks requiring rebuilds of multiple sub-tasks can rebuild them simultaneously.
  - Opt-in to avoid overhead on small tasks.
