use std::sync::atomic::{AtomicU32, Ordering};

use inc_complete::{Db, define_input, define_intermediate, impl_storage, storage::SingletonStorage};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
struct A1;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
struct A2;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
struct A3;

// Ensure we can distinguish between unit-value singletons.
// This is a regression test from a bug in the Ante compiler where `Some(())`
// was encoded the same as `None` in a tagless encoding.
#[derive(Default, Serialize, Deserialize)]
struct Spreadsheet {
    a1: SingletonStorage<A1>,
    a2: SingletonStorage<A2>,
    a3: SingletonStorage<A3>,
}

impl_storage!(Spreadsheet,
    a1: A1,
    a2: A2,
    a3: A3,
);

static COMPUTATIONS_RUN: AtomicU32 = AtomicU32::new(0);

define_input!(0, A1 -> i32, Spreadsheet);
define_intermediate!(1, A2 -> i32, Spreadsheet, |_, handle| {
    COMPUTATIONS_RUN.fetch_add(1, Ordering::SeqCst);
    handle.get(A1) + 1
});
define_intermediate!(2, A3 -> (), Spreadsheet, |_, handle| {
    COMPUTATIONS_RUN.fetch_add(1, Ordering::SeqCst);
    let _ = handle.get(A2);
});

#[test]
fn double_round_trip_preserves_cache() {
    let mut db = Db::<Spreadsheet>::new();
    db.update_input(A1, 20);
    db.get(A3);
    assert_eq!(COMPUTATIONS_RUN.load(Ordering::SeqCst), 2);

    let bytes = rmp_serde::to_vec(&db).expect("first serialize should succeed");
    let db: Db<Spreadsheet> = rmp_serde::from_slice(&bytes).expect("first deserialize should succeed");

    // A3 should already be cached from the loaded state; re-querying it must not recompute.
    db.get(A3);
    assert_eq!(COMPUTATIONS_RUN.load(Ordering::SeqCst), 2);

    let bytes = rmp_serde::to_vec(&db).expect("second serialize should succeed");
    let db: Db<Spreadsheet> = rmp_serde::from_slice(&bytes).expect("second deserialize should succeed");

    db.get(A3);
    assert_eq!(
        COMPUTATIONS_RUN.load(Ordering::SeqCst),
        2,
        "double round-trip should not have forced any recomputation"
    );
}
