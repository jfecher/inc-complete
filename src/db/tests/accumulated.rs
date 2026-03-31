use crate::accumulate::{Accumulated, Accumulator};
use crate::{Db, define_input, define_intermediate, impl_storage};

use crate::storage::HashMapStorage;

#[derive(Default)]
struct Compiler {
    files: HashMapStorage<File>,
    parses: HashMapStorage<Parse>,
    resolves: HashMapStorage<Resolve>,
    mess_up_error_counts: HashMapStorage<MessUpErrorCount>,
    error_count: HashMapStorage<ErrorCount>,
    error_storage: HashMapStorage<Accumulated<Error>>,
    direct_accumulator: HashMapStorage<DirectAccumulator>,

    errors: Accumulator<Error>,
}

impl_storage!(Compiler,
    files: File,
    parses: Parse,
    resolves: Resolve,
    mess_up_error_counts: MessUpErrorCount,
    error_count: ErrorCount,
    error_storage: Accumulated<Error>,
    direct_accumulator: DirectAccumulator,

    @accumulators {
        errors: Error,
    }
);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct File(i32);
define_input!(0, File -> i32, Compiler);

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Error(i32);

#[derive(Debug, Clone, Hash, PartialEq, Eq, Ord, PartialOrd)]
struct Parse(i32);
define_intermediate!(1, Parse -> i32, Compiler, |ctx, db| {
    let file_number = File(ctx.0).get(db);
    db.accumulate(Error(file_number));
    file_number
});

#[derive(Debug, Clone, Hash, PartialEq, Eq, Ord, PartialOrd)]
struct Resolve(i32);
define_intermediate!(2, Resolve -> i32, Compiler, |ctx, db| {
    let file_number = Parse(ctx.0 + 1).get(db);
    db.accumulate(Error(file_number));
    let file_number = Parse(ctx.0).get(db);
    db.accumulate(Error(file_number));
    file_number
});

/// Accumulate a differing number of errors based on the input but always returns 0
#[derive(Debug, Clone, Hash, PartialEq, Eq, Ord, PartialOrd)]
struct MessUpErrorCount;
define_intermediate!(3, MessUpErrorCount -> i32, Compiler, |_, db| {
    let file_number = Parse(0).get(db);
    if file_number % 2 != 0 {
        db.accumulate(Error(100));
    }
    0
});

/// Always returns 0, but accumulates Error(200) when File(0) is odd.
/// Unlike MessUpErrorCount, this reads File(0) directly (no intermediate Parse),
/// so the only dependency of Accumulated<Error>(direct_cell) that could indicate
/// staleness is the input cell itself — which never changes its accumulated values.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Ord, PartialOrd)]
struct DirectAccumulator;
define_intermediate!(5, DirectAccumulator -> i32, Compiler, |_, db| {
    let file_number = File(0).get(db);
    if file_number % 2 != 0 {
        db.accumulate(Error(200));
    }
    0
});

/// Computations that depend on accumulated values of other computations may be incorrectly
/// not updated if their dependencies' accumulated values change but not their return value.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Ord, PartialOrd)]
struct ErrorCount;
define_intermediate!(4, ErrorCount -> usize, Compiler, |_, db| {
    let errors = db.get_accumulated::<Error, _>(MessUpErrorCount);
    println!("error_count errors: {errors:?}");
    errors.len()
});

#[test]
fn basic_accumulators() {
    let mut db = Db::<Compiler>::new();
    db.update_input(File(4), 4);
    db.update_input(File(5), 5);
    db.update_input(File(6), 6);
    db.update_input(File(7), 7);

    // accumulated items are deduplicated so each 4 & 5 only appear once
    let errors: Vec<_> = db.get_accumulated(Resolve(4)).into_iter().collect();
    assert_eq!(errors, vec![Error(4), Error(5)]);

    // Get different errors
    let errors: Vec<_> = db.get_accumulated(Resolve(5)).into_iter().collect();
    assert_eq!(errors, vec![Error(5), Error(6)]);

    // Get a subset of errors
    let errors: Vec<_> = db.get_accumulated(Parse(5)).into_iter().collect();
    assert_eq!(errors, vec![Error(5)]);
}

/// Regression test for past bug before accumulators were publically available
/// from a `DbHandle`.
#[test]
fn accumulators_broken_on_update_without_return_value_update() {
    let mut db = Db::<Compiler>::new();
    db.update_input(File(0), 0);

    // `MessUpErrorCount` emits an error when `File(0)` is even
    assert_eq!(ErrorCount.get(&db), 1);
    db.update_input(File(0), 1);

    // `File(0)` was changed, so the error count should be updated even though
    // `MessUpErrorCount` is short-circuited by `Parse(0)`'s result not chaning
    assert_eq!(ErrorCount.get(&db), 2);
}

#[test]
fn accumulators_rerun_on_input_change() {
    let mut db = Db::<Compiler>::new();
    db.update_input(File(0), 0);

    // `MessUpErrorCount` emits an error when `File(0)` is even
    let errors = db.get_accumulated::<Error, _>(ErrorCount);
    println!("Got errors {errors:?}");
    assert_eq!(errors.len(), 1);

    db.update_input(File(0), 1);

    // `File(0)` was changed, so the error count should be updated even though
    // `MessUpErrorCount` is short-circuited by `Parse(0)`'s result not chaning
    let errors = db.get_accumulated::<Error, _>(ErrorCount);
    println!("Got errors {errors:?}");
    assert_eq!(errors.len(), 2);
}

/// Regression test: accumulated values must be refreshed when the underlying cell re-ran
/// (clearing and re-accumulating), even if its return value did not change.
/// Unlike the MessUpErrorCount tests, DirectAccumulator reads File(0) directly with no
/// changing intermediate, so Accumulated<Error>(direct_cell)'s only tracked dependency
/// that could signal staleness via last_updated_version is the input cell itself — which
/// has no accumulated values and thus never changes Accumulated<Error>(input_cell)'s output.
#[test]
fn accumulated_values_update_when_direct_input_changes_but_return_value_is_stable() {
    let mut db = Db::<Compiler>::new();
    db.update_input(File(0), 1); // odd → DirectAccumulator accumulates Error(200)

    let errors = db.get_accumulated::<Error, _>(DirectAccumulator);
    assert!(errors.contains(&Error(200)));

    db.update_input(File(0), 2); // even → DirectAccumulator should NOT accumulate Error(200)

    let errors = db.get_accumulated::<Error, _>(DirectAccumulator);
    assert!(
        !errors.contains(&Error(200)),
        "Error(200) should not be present after input changed to even value; got: {errors:?}"
    );
}
