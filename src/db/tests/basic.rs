use crate::Computation;
use crate::Db;
use crate::DbHandle;
use crate::OutputType;
use crate::Intermediate;
use crate::OutputTypeForInput;
use crate::Run;
use crate::SingletonStorage;
use crate::db::START_VERSION;

// Emulate this spreadsheet:
//      A
// 1 [ =20 ]
// 2 [ =A1 + 1 ]
// 3 [ =A2 + 2 ]
#[derive(Clone, Debug, Default)]
struct A1;
impl A1 {
    fn new() -> SingletonStorage<OutputType<A1>> {
        SingletonStorage::new(OutputType::new(A1))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
struct A2;
impl A2 {
    fn new() -> SingletonStorage<Intermediate<A2>> {
        SingletonStorage::new(Intermediate::new(A2))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
struct A3;
impl A3 {
    fn new() -> SingletonStorage<Intermediate<A3>> {
        SingletonStorage::new(Intermediate::new(A3))
    }
}

type Spreadsheet = (
    SingletonStorage<OutputType<A1>>,
    SingletonStorage<Intermediate<A2>>,
    SingletonStorage<Intermediate<A3>>,
);

impl OutputTypeForInput for A1 {
    type Output = i32;
}

impl Run for A2 {
    type Output = i32;

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
        handle.get(A1::new()) + 1
    }
}

impl Run for A3 {
    type Output = i32;

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
        handle.get(A2::new()) + 2
    }
}

// Test that we can compute a basic chain of computation
//      A
// 1 [ =20 ]
// 2 [ =A1 + 1 ]
// 3 [ =A2 + 2 ]
#[test]
fn basic() {
    let mut db = Db::<Spreadsheet>::new();
    db.update_input(A1::new(), 20);
    let result = *db.get(A3::new());
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
    db.update_input(A1::new(), 20);
    let result1 = *db.get(A3::new());
    let result2 = *db.get(A3::new());
    assert_eq!(result1, 23);
    assert_eq!(result2, 23);

    // Only 1 input has been updated
    let expected_version = START_VERSION + 1;
    assert_eq!(db.version, expected_version);

    let a1 = db.unwrap_cell_value(&A1::new());
    assert_eq!(a1.last_updated_version, expected_version);
    assert_eq!(a1.last_verified_version, expected_version);

    let a2 = db.unwrap_cell_value(&A2::new());
    assert_eq!(a2.last_updated_version, expected_version);
    assert_eq!(a2.last_verified_version, expected_version);

    let a3 = db.unwrap_cell_value(&A3::new());
    assert_eq!(a3.last_updated_version, expected_version);
    assert_eq!(a3.last_verified_version, expected_version);
}
