use std::{panic::AssertUnwindSafe, sync::atomic::{AtomicU32, Ordering}, time::Duration};

use inc_complete::{define_intermediate, intermediate, storage::HashMapStorage, Db, DbHandle, Storage};


#[derive(Default, Storage)]
struct MyStorage {
    check: HashMapStorage<Foo>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Foo(bool);

// We expect 2 cycle errors to be issued: one for each thread.
static ASSERTS_PASSED: AtomicU32 = AtomicU32::new(0);

// Cycle between Foo(false) <-> Foo(true) but both are on separate threads
#[intermediate(id = 0)]
fn foo(ctx: &Foo, db: &DbHandle<MyStorage>) -> u32 {
    println!("Foo({}) on thread {:?}", ctx.0, std::thread::current().id());
    std::thread::sleep(Duration::from_millis(250));

    // Scenario 1: both threads operate roughly in sync and the true thread grabs the false
    // resource while the false thread grabs the true resource. Both threads should immediately
    // error with a cycle error.
    //
    // Scenario 2: both threads start quickly but one thread panics before the other tries to grab
    // the resource. The resource is now free for the other thread, and it is able to recur. The
    // other thread recurs and later errors after discovering the cycle on a single thread.
    match std::panic::catch_unwind(AssertUnwindSafe(|| Foo(!ctx.0).get(db))) {
        Ok(_) => panic!("Ran cycle on Foo({}) without error!", ctx.0),
        Err(message) => {
            if let Some(message) = message.downcast_ref::<String>() {
                // The false thread will panic with Foo(false) -> Foo(true) -> Foo(false), while the
                // true thread will panic with Foo(true) -> Foo(false) -> Foo(true). This needs
                // to return true for both.
                assert!(message.contains("Foo(false) -> Foo(true)"));
                println!("Caught cycle panic on thread {:?}", std::thread::current().id());
                ASSERTS_PASSED.fetch_add(1, Ordering::Relaxed);
            } else {
                println!("No cycle panic on thread {:?}", std::thread::current().id());
            }
            panic!()
        },
    }
}

#[test]
fn cycle_between_two_threads() {
    // Cycle: Foo(false) <-> Foo(true)
    let db = Db::<MyStorage>::new();

    std::thread::scope(|scope| {
        let f = scope.spawn(|| db.get(Foo(false)));
        let t = scope.spawn(|| db.get(Foo(true)));
        t.join().ok();
        f.join().ok();
    });

    assert_eq!(ASSERTS_PASSED.load(Ordering::SeqCst), 2);
}
