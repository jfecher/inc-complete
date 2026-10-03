//! A cell being re-run on one thread has its dependencies cleared until it finishes.
//! This tests is a regression against another thread seeing these cleared dependencies
//! when run at a particular time instead of waiting for the other run.
use std::{
    sync::{
        Barrier, LazyLock,
        atomic::{AtomicBool, Ordering},
    },
    time::Duration,
};

use crate::{define_input, define_intermediate, impl_storage, storage::HashMapStorage};

#[derive(Default)]
struct ConcurrentVerify {
    inputs: HashMapStorage<Input>,
    doubles: HashMapStorage<Double>,
}

impl_storage!(ConcurrentVerify,
    inputs: Input,
    doubles: Double,
);

/// Set to pause each run of `Double` before it reads its input
static PAUSE: AtomicBool = AtomicBool::new(false);

/// Reached by a paused run of `Double` and by the test once that run has started
static STARTED: LazyLock<Barrier> = LazyLock::new(|| Barrier::new(2));

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Input;
define_input!(0, Input -> u32, ConcurrentVerify);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Double;
define_intermediate!(1, Double -> u32, ConcurrentVerify, |_ctx, db| {
    if PAUSE.load(Ordering::SeqCst) {
        STARTED.wait();
        // Give the test's thread time to verify this cell while its dependencies are cleared
        std::thread::sleep(Duration::from_millis(200));
    }
    db.get(Input) * 2
});

type Db = crate::Db<ConcurrentVerify>;

#[test]
fn verifying_a_running_cell_waits_for_it() {
    let mut db = Db::new();
    db.update_input(Input, 1);
    assert_eq!(db.get(Double), 2);

    db.update_input(Input, 2);
    PAUSE.store(true, Ordering::SeqCst);
    let db = &db;
    std::thread::scope(|scope| {
        let rerun = scope.spawn(|| db.get(Double));
        STARTED.wait();
        assert_eq!(db.get(Double), 4);
        assert_eq!(rerun.join().unwrap(), 4);
    });
}
