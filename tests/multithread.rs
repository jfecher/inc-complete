use inc_complete::{Db, DbHandle, define_intermediate, impl_storage, storage::HashMapStorage};

#[derive(Default)]
struct Context {
    div4s: HashMapStorage<Div4>,
    div_all: HashMapStorage<DivAll>,
}

impl_storage!(Context,
    div4s: Div4,
    div_all: DivAll,
);

#[derive(Clone, Hash, PartialEq, Eq)]
struct Div4(u64);
define_intermediate!(0, Div4 -> u64, Context, div4);

fn div4<'db>(this: &Div4, db: &DbHandle<'db, Context>) -> u64 {
    if this.0 < 4 {
        0
    } else {
        1 + db.get(Div4(this.0 - 4))
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct DivAll(u64, u64, u64, u64);
define_intermediate!(1, DivAll -> u64, Context, div_all);

fn div_all<'c>(this: &DivAll, db: &DbHandle<'c, Context>) -> u64 {
    let (a, b, c, d) = std::thread::scope(|scope| {
        let a = scope.spawn(|| db.get(Div4(this.0)));
        let b = scope.spawn(|| db.get(Div4(this.1)));
        let c = scope.spawn(|| db.get(Div4(this.2)));
        let d = scope.spawn(|| db.get(Div4(this.3)));
        (a.join().unwrap(), b.join().unwrap(), c.join().unwrap(), d.join().unwrap())
    });
    assert_eq!(a, 10);
    assert_eq!(b, 10);
    assert_eq!(c, 10);
    assert_eq!(d, 10);
    a + b + c + d
}

#[test]
fn multithread_basic() {
    let db = Db::<Context>::new();

    let result = db.get(DivAll(40, 41, 42, 43));
    assert_eq!(result, 40);
}
