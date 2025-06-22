use std::sync::atomic::{AtomicU32, Ordering};

use crate::{define_input, define_intermediate, impl_storage, storage::DashMapStorage};

#[derive(Default)]
struct EarlyCutoff {
    inputs: DashMapStorage<IntInput>,
    add1s: DashMapStorage<Five>,
    add2s: DashMapStorage<Six>,
    add3s: DashMapStorage<Seven>,
}

impl_storage!(EarlyCutoff,
    inputs: IntInput,
    add1s: Five,
    add2s: Six,
    add3s: Seven,
);

static INTERMEDIATES_RUN: AtomicU32 = AtomicU32::new(0);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct IntInput(u32);
define_input!(0, IntInput -> u32, EarlyCutoff);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Five(u32);
define_intermediate!(1, Five -> u32, EarlyCutoff, |ctx, db| {
    INTERMEDIATES_RUN.fetch_add(1, Ordering::Relaxed);
    let _ = db.get(IntInput(ctx.0));
    5
});

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Six(u32);
define_intermediate!(2, Six -> u32, EarlyCutoff, |ctx, db| {
    INTERMEDIATES_RUN.fetch_add(1, Ordering::Relaxed);
    db.get(Five(ctx.0)) + 1
});

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Seven(u32);
define_intermediate!(3, Seven -> u32, EarlyCutoff, |ctx, db| {
    INTERMEDIATES_RUN.fetch_add(1, Ordering::Relaxed);
    db.get(Six(ctx.0)) + 1
});

type Db = crate::Db<EarlyCutoff>;

#[test]
fn early_cutoff() {
    let mut db = Db::new();
    db.update_input(IntInput(5), 5);
    assert_eq!(db.get(Seven(5)), 7);
    assert_eq!(INTERMEDIATES_RUN.load(Ordering::Relaxed), 3);

    // Get it again, ensure no functions are re-run
    assert_eq!(db.get(Seven(5)), 7);
    assert_eq!(INTERMEDIATES_RUN.load(Ordering::Relaxed), 3);

    // Change an unrelated input
    db.update_input(IntInput(100), 100);
    assert_eq!(db.get(Seven(5)), 7);
    assert_eq!(INTERMEDIATES_RUN.load(Ordering::Relaxed), 3);

    // Change the original input. Ensure we can backdate
    // after re-running only the first computation
    db.update_input(IntInput(5), 15);
    assert_eq!(db.get(Seven(5)), 7);
    assert_eq!(INTERMEDIATES_RUN.load(Ordering::Relaxed), 4);
}
