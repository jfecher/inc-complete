// Ensure multiple threads do not attempt to run the same computation

use std::{sync::atomic::AtomicU32, time::Duration};

use inc_complete::{define_intermediate, storage::HashMapStorage, Db, DbHandle, Storage};

#[derive(Default, Storage)]
struct Context {
    sub1: HashMapStorage<Sub1>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Sub1(u64);
define_intermediate!(0, Sub1 -> u64, Context, sub1);

static INTERMEDIATES_RUN: AtomicU32 = AtomicU32::new(0);

fn sub1(ctx: &Sub1, db: &DbHandle<Context>) -> u64 {
    println!("sub1({})", ctx.0);
    INTERMEDIATES_RUN.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

    if ctx.0 == 1 {
        0
    } else if ctx.0 % 2 == 0 {
        std::thread::sleep(Duration::from_millis(10));
        Sub1(ctx.0 - 1).get(db)
    } else {
        Sub1(ctx.0 - 1).get(db)
    }
}

#[test]
fn computations_arent_duplicated() {
    let db = Db::<Context>::new();

    std::thread::scope(|scope| {
        let result1 = scope.spawn(|| Sub1(10).get(&db));
        let result2 = scope.spawn(|| Sub1(11).get(&db));
        let result1 = result1.join().unwrap();
        println!("Got {result1}");
        let result2 = result2.join().unwrap();
        println!("Got {result2}");

        let intermediates = INTERMEDIATES_RUN.load(std::sync::atomic::Ordering::SeqCst);
        assert_eq!(intermediates, 11);
    });
}
