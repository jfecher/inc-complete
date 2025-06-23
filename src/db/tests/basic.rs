use crate::Db;
use crate::db::START_VERSION;
use crate::define_input;
use crate::define_intermediate;
use crate::impl_storage;
use crate::storage::SingletonStorage;

// Emulate this spreadsheet:
//      A
// 1 [ =20 ]
// 2 [ =A1 + 1 ]
// 3 [ =A2 + 2 ]
#[derive(Clone, Debug, Default)]
struct A1;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
struct A2;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
struct A3;

#[derive(Default)]
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

define_input!(0, A1 -> i32, Spreadsheet);
define_intermediate!(1, A2 -> i32, Spreadsheet, |_, handle| {
    handle.get(A1) + 1
});
define_intermediate!(2, A3 -> i32, Spreadsheet, |_, handle| {
    handle.get(A2) + 2
});

// Test that we can compute a basic chain of computation
//      A
// 1 [ =20 ]
// 2 [ =A1 + 1 ]
// 3 [ =A2 + 2 ]
#[test]
fn basic() {
    let mut db = Db::<Spreadsheet>::new();
    db.update_input(A1, 20);
    let result = db.get(A3);
    assert_eq!(result, 23);
}

// Test that we can re-use values from past runs
//      A
// 1 [ =20 ]
// 2 [ =A1 + 1 ]
// 3 [ =A2 + 2 ]
#[test]
fn no_recompute_basic() {
    let mut db = Db::<Spreadsheet>::new();
    db.update_input(A1, 20);
    let result1 = db.get(A3);
    let result2 = db.get(A3);
    assert_eq!(result1, 23);
    assert_eq!(result2, 23);

    // Only 1 input has been updated
    let expected_version = START_VERSION + 1;
    assert_eq!(db.version(), expected_version);

    let a1 = db.unwrap_cell_value(&A1);
    assert_eq!(a1.last_updated_version, expected_version);
    assert_eq!(a1.last_verified_version, expected_version);

    let a2 = db.unwrap_cell_value(&A2);
    assert_eq!(a2.last_updated_version, expected_version);
    assert_eq!(a2.last_verified_version, expected_version);

    let a3 = db.unwrap_cell_value(&A3);
    assert_eq!(a3.last_updated_version, expected_version);
    assert_eq!(a3.last_verified_version, expected_version);
}
