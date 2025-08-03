// Test file to verify ASSUME_CHANGED const functionality
use inc_complete::{
    Db, DbHandle, Input, Storage, define_input, define_intermediate, intermediate,
    storage::SingletonStorage,
};
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Clone)]
struct CountedValue(i32);

static COMPARISON_COUNTER: AtomicUsize = AtomicUsize::new(0);
static COMPUTATION_COUNTER: AtomicUsize = AtomicUsize::new(0);

impl PartialEq for CountedValue {
    fn eq(&self, other: &Self) -> bool {
        COMPARISON_COUNTER.fetch_add(1, Ordering::SeqCst);
        self.0 == other.0
    }
}

impl Eq for CountedValue {}

#[derive(Clone, Input)]
#[inc_complete(
    id = 0,
    assume_changed,
    output = CountedValue,
    storage = TestStorage
)]
struct Input;

#[derive(Clone)]
struct AssumeChangedComputation;

#[derive(Default, Storage)]
struct TestStorage {
    input: SingletonStorage<Input>,
    computation: SingletonStorage<AssumeChangedComputation>,
}

// Define computation with ASSUME_CHANGED = true
// This means the storage will skip equality comparison and always assume the output changed
#[intermediate(id = 1, assume_changed)]
fn compute(_: &AssumeChangedComputation, db: &DbHandle<TestStorage>) -> CountedValue {
    COMPUTATION_COUNTER.fetch_add(1, Ordering::SeqCst);
    CountedValue(db.get(Input).0 + 2)
}

#[test]
fn test_assume_changed_skips_comparison() {
    let mut db = Db::<TestStorage>::new();

    db.update_input(Input, CountedValue(10));
    let result1 = db.get(AssumeChangedComputation);
    assert_eq!(result1.0, 12);

    assert_eq!(COMPUTATION_COUNTER.load(Ordering::SeqCst), 1);
    db.update_input(Input, CountedValue(10));
    let result2 = db.get(AssumeChangedComputation);
    assert_eq!(result2.0, 12);

    assert_eq!(COMPUTATION_COUNTER.load(Ordering::SeqCst), 2);

    assert_eq!(
        COMPARISON_COUNTER.load(Ordering::SeqCst),
        0,
        "Comparison was called, but ASSUME_CHANGED = true should skip all comparisons"
    );
}
