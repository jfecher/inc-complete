use std::{
    sync::atomic::AtomicU32,
    time::Duration,
};

use inc_complete::{Db, DbHandle, Storage, define_intermediate, storage::SingletonStorage};

#[derive(Default, Storage)]
struct MyStorage {
    singleton: SingletonStorage<Singleton>,
}

#[derive(Debug, Clone)]
struct Singleton;

static TIMES_RUN: AtomicU32 = AtomicU32::new(0);

#[inc_complete::intermediate(id = 0)]
fn singleton_impl(_: &Singleton, _: &DbHandle<MyStorage>) -> () {
    let times_run = TIMES_RUN.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    if times_run != 0 {
        panic!("Ran singleton more than once!");
    }
    std::thread::sleep(Duration::from_millis(500));
}

/// This test is spotty and doesn't always trigger the issue
/// so we repeat it several times
#[test]
fn ensure_singleton_not_rerun() {
    let db = Db::<MyStorage>::new();

    std::thread::scope(|scope| {
        let items = (0..100)
            .map(|_| scope.spawn(|| Singleton.get(&db)))
            .collect::<Vec<_>>();

        for item in items {
            item.join().unwrap();
        }
    });
}
