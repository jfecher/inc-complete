#![cfg(feature = "async")]
use inc_complete::{define_intermediate, impl_storage, storage::DashMapStorage, Db, DbHandle};

#[derive(Default)]
struct Context {
    div4s: DashMapStorage<Div4>,
    div_all: DashMapStorage<DivAll>,
}

impl_storage!(Context,
    div4s: Div4,
    div_all: DivAll,
);

#[derive(Clone, Hash, PartialEq, Eq)]
struct Div4(u64);
define_intermediate!(0, Div4 -> u64, Context, div4);

async fn div4<'db>(this: &Div4, db: &DbHandle<'db, Context>) -> u64 {
    println!("div({}) on thread {:?}", this.0, std::thread::current().id());
    if this.0 < 4 {
        0
    } else {
        tokio::task::yield_now().await;
        1 + Box::pin(db.get(Div4(this.0 - 4))).await
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct DivAll(u64, u64, u64, u64);
define_intermediate!(1, DivAll -> u64, Context, div_all);

async fn div_all<'c>(this: &DivAll, db: &DbHandle<'c, Context>) -> u64 {
    println!("div_all");

    let mut a = 0;
    let mut b = 0;
    let mut c = 0;
    let mut d = 0;
    let mut v = String::new();
    tokio_scoped::scope(|scope| {
        scope.spawn(async {
            let r = Box::pin(db.get(Div4(this.0))).await;
            a = r;
        });
        scope.spawn(async {
            let r = Box::pin(db.get(Div4(this.1))).await;
            b = r;
        });
        scope.spawn(async {
            let r = Box::pin(db.get(Div4(this.2))).await;
            c = r;
        });
        scope.spawn(async {
            let r = Box::pin(db.get(Div4(this.3))).await;
            d = r;
        });
    });
    println!("{a}, {b}, {c}, {d}");
    a + b + c + d
}

#[test]
fn test_async_basic() {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap()
        .block_on(async {
            let db = Db::<Context>::new();

            let result = db.get(DivAll(40, 41, 42, 43)).await;
            println!("{result}");
            assert_eq!(result, 40);
            panic!();
        })
}
