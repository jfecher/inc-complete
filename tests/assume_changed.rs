// Test file to verify ASSUME_CHANGED const functionality
use inc_complete::{
    Db, DbHandle, define_input, define_intermediate, impl_storage, storage::SingletonStorage,
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

#[derive(Debug, Clone)]
struct Input;

#[derive(Debug, Clone)]
struct AssumeChangedComputation;

#[derive(Default)]
struct TestStorage {
    input: SingletonStorage<Input>,
    computation: SingletonStorage<AssumeChangedComputation>,
}

impl_storage!(TestStorage,
    input: Input,
    computation: AssumeChangedComputation,
);

define_input!(0, assume_changed Input -> CountedValue, TestStorage);

// Define computation with ASSUME_CHANGED = true
// This means the storage will skip equality comparison and always assume the output changed
define_intermediate!(1, assume_changed AssumeChangedComputation -> CountedValue, TestStorage,  |_, db: &DbHandle<TestStorage>| {
    COMPUTATION_COUNTER.fetch_add(1, Ordering::SeqCst);
    CountedValue(db.get(Input).0 + 2)
});

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
