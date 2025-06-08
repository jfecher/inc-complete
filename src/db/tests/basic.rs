use crate::db::START_VERSION;
use crate::Cached;
use crate::Computation;
use crate::DbHandle;
use crate::Db;
use crate::Input;
use crate::OutputTypeForInput;
use crate::Run;

// Emulate this spreadsheet:
//      A
// 1 [ =20 ]
// 2 [ =A1 + 1 ]
// 3 [ =A2 + 2 ]
#[derive(Clone)]
struct A1;
const A1_C: Input<A1> = Input::new();

#[derive(Clone, PartialEq, Eq, Hash)]
struct A2;
const A2_C: Cached<A2> = Cached::new(A2);

#[derive(Clone, PartialEq, Eq, Hash)]
struct A3;
const A3_C: Cached<A3> = Cached::new(A3);

type Spreadsheet = (Input<A1>, Cached<A2>, Cached<A3>);

impl OutputTypeForInput for A1 {
    type Output = i32;
}

impl Run for A2 {
    type Output = i32;

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
        handle.get(A1_C) + 1
    }
}

impl Run for A3 {
    type Output = i32;

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
        handle.get(A2_C) + 2
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
    let result = *db.get(A3_C);
    assert_eq!(result, 23);
}

// Test that we can re-use values from past runs
//      A
// 1 [ =20 ]
// 2 [ =A1 + 1 ]
// 3 [ =A2 + 2 ]
#[test]
fn no_recompute_basic() {
    let mut db = Db::new();
    let result1 = *db.get::<i32>(Basic::A3);
    let result2 = *db.get::<i32>(Basic::A3);
    assert_eq!(result1, 23);
    assert_eq!(result2, 23);

    // No input has been updated
    let expected_version = START_VERSION;
    assert_eq!(db.version, expected_version);

    let a1 = db.unwrap_cell_value(&Basic::A1);
    assert_eq!(a1.last_updated_version, expected_version);
    assert_eq!(a1.last_verified_version, expected_version);

    let a2 = db.get_cell_value(&Basic::A2);
    assert_eq!(a2.last_updated_version, expected_version);
    assert_eq!(a2.last_verified_version, expected_version);

    let a3 = db.get_cell_value(&Basic::A3);
    assert_eq!(a3.last_updated_version, expected_version);
    assert_eq!(a3.last_verified_version, expected_version);
}
