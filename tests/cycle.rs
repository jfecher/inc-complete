use inc_complete::{define_intermediate, intermediate, storage::HashMapStorage, Db, DbHandle, Storage};


#[derive(Default, Storage)]
struct MyStorage {
    check: HashMapStorage<Foo>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Foo(u32);

#[intermediate(id = 0)]
fn foo(ctx: &Foo, db: &DbHandle<MyStorage>) -> u32 {
    if ctx.0 == 0 {
        0
    // Cycle: 7 -> 6 -> 5 -> 4 -> 7
    } else if ctx.0 == 4 {
        Foo(7).get(db)
    // Cycle: 20 -> 19 -> 18 -> 17 -> 16 -> 15 -> 14 -> 13 -> 12 -> 11 -> 10 -> 20
    } else if ctx.0 == 10 {
        Foo(20).get(db)
    } else {
        Foo(ctx.0 - 1).get(db)
    }
}

#[test]
#[should_panic(expected = "Cycle:\n  Foo(5) -> Foo(4) -> Foo(7) -> Foo(6) -> Foo(5)")]
fn find_cycle_of_7() {
    // Cycle: 7 -> 6 -> 5 -> 4 -> 7
    let db = Db::<MyStorage>::new();
    let _ = Foo(5).get(&db);
}

#[test]
#[should_panic(expected = "Cycle:\n  Foo(16) -> Foo(15) -> Foo(14) -> Foo(13) -> Foo(12) -> Foo(11) -> Foo(10) -> Foo(20) -> Foo(19) -> Foo(18) -> Foo(17) -> Foo(16)")]
fn find_cycle_of_20() {
    // Cycle: 20 -> 19 -> 18 -> 17 -> 16 -> 15 -> 14 -> 13 -> 12 -> 11 -> 10 -> 20
    let db = Db::<MyStorage>::new();
    let _ = Foo(16).get(&db);
}
