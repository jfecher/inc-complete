use crate::accumulate::Accumulator;
use crate::{define_input, define_intermediate, impl_storage, Db};

use crate::storage::HashMapStorage;

#[derive(Default)]
struct Compiler {
    files: HashMapStorage<File>,
    parses: HashMapStorage<Parse>,
    resolves: HashMapStorage<Resolve>,
    mess_up_error_counts: HashMapStorage<MessUpErrorCount>,
    error_count: HashMapStorage<ErrorCount>,

    errors: Accumulator<Error>,
}

impl_storage!(Compiler,
    files: File,
    parses: Parse,
    resolves: Resolve,
    mess_up_error_counts: MessUpErrorCount,
    error_count: ErrorCount,

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

/// Returns a differing number of errors based on the input but always returns 0
#[derive(Debug, Clone, Hash, PartialEq, Eq, Ord, PartialOrd)]
struct MessUpErrorCount;
define_intermediate!(3, MessUpErrorCount -> i32, Compiler, |_, db| {
    let file_number = File(0).get(db);
    if file_number % 2 == 0 {
        db.accumulate(Error(100));
    }
    0
});

/// Computations that depend on accumulated values of other computations may be incorrectly
/// not updated if their dependencies' accumulated values change but not their return value.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Ord, PartialOrd)]
struct ErrorCount;
define_intermediate!(4, ErrorCount -> usize, Compiler, |_, db| {
    let errors: Vec<_> = db.get_accumulated(MessUpErrorCount);
    errors.len()
});

#[test]
fn basic_accumulators() {
    let mut db = Db::<Compiler>::new();
    db.update_input(File(4), 4);
    db.update_input(File(5), 5);
    db.update_input(File(6), 6);
    db.update_input(File(7), 7);

    // Accumulated value order follows dependency order, although all accumulated values of a
    // particular dependency are pushed at the end of that dependency. That is why we see
    // [4, 5, 4, 5] rather than [5, 5, 4, 4] here.
    let errors: Vec<_> = db.get_accumulated(Resolve(4));
    assert_eq!(errors, vec![Error(5), Error(4), Error(5), Error(4)]);

    // Get different errors
    let errors: Vec<_> = db.get_accumulated(Resolve(5));
    assert_eq!(errors, vec![Error(6), Error(5), Error(6), Error(5)]);

    // Get a subset of errors
    let errors: Vec<_> = db.get_accumulated(Parse(5));
    assert_eq!(errors, vec![Error(5)]);
}

/// Documenting the bug for when it is fixed in the future
#[test]
fn accumulators_broken_on_update_without_return_value_update() {
    let mut db = Db::<Compiler>::new();
    db.update_input(File(0), 0);

    // `MessUpErrorCount` emits an error when `File(0)` is even
    assert_eq!(ErrorCount.get(&db), 1);

    // yet if we remove the condition to cause the error
    db.update_input(File(0), 1);

    // The computation is still cached because `MessUpErrorCount`'s return value
    // did not change, and thus `ErrorCount` still sees the old error count instead of 0.
    assert_eq!(ErrorCount.get(&db), 1, "If this is 1, this bug is fixed");
}
