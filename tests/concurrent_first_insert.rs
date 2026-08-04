//! Regression test for a data race in `insert_new_cell`. The fast path in `Db::get_or_insert_cell`
//! lets any thread discover a cell the instant its key becomes visible in storage.
//! If a Storage impl makes the cell discoverable before the rest of the cell's data is written,
//! another thread can observe the cell the moment before the data is written.
use std::sync::atomic::{AtomicU64, Ordering};

use inc_complete::{Db, DbHandle, Storage, define_intermediate, storage::HashMapStorage};

#[derive(Default, Storage)]
struct Context {
    values: HashMapStorage<Value>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Value(u64);
define_intermediate!(0, Value -> u64, Context, compute_value);

fn compute_value(this: &Value, _db: &DbHandle<Context>) -> u64 {
    this.0 * 2
}

#[test]
fn many_threads_race_to_insert_the_same_new_key() {
    const THREADS: usize = 16;
    const ROUNDS: u64 = 2000;

    let db = Db::<Context>::new();

    for round in 0..ROUNDS {
        std::thread::scope(|scope| {
            for _ in 0..THREADS {
                scope.spawn(|| {
                    let result = Value(round).get(&db);
                    assert_eq!(result, round * 2);
                });
            }
        });
    }
}

#[derive(Default, Storage)]
struct SingletonContext {
    only: inc_complete::storage::SingletonStorage<Only>,
}

static ONLY_COMPUTED: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone)]
struct Only;
define_intermediate!(0, Only -> u64, SingletonContext, compute_only);

fn compute_only(_: &Only, _db: &DbHandle<SingletonContext>) -> u64 {
    ONLY_COMPUTED.fetch_add(1, Ordering::SeqCst);
    42
}

#[test]
fn many_threads_race_to_insert_the_same_new_singleton() {
    const THREADS: usize = 32;

    let db = Db::<SingletonContext>::new();

    std::thread::scope(|scope| {
        for _ in 0..THREADS {
            scope.spawn(|| {
                let result = Only.get(&db);
                assert_eq!(result, 42);
            });
        }
    });
}
