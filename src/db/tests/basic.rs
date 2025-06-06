use crate::db::START_VERSION;
use crate::DbHandle;
use crate::{Db, Run, Value};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Basic {
    A1,
    A2,
    A3,
}

//      A
// 1 [ =20 ]
// 2 [ =A1 + 1 ]
// 3 [ =A2 + 2 ]
impl Run for Basic {
    fn run(&self, db: &mut DbHandle<Self>) -> Value {
        match self {
            Basic::A1 => Value::new(20i32),
            Basic::A2 => Value::new(db.get::<i32>(Basic::A1) + 1i32),
            Basic::A3 => Value::new(db.get::<i32>(Basic::A2) + 2i32),
        }
    }
}

// Test that we can compute a basic chain of computation
//      A
// 1 [ =20 ]
// 2 [ =A1 + 1 ]
// 3 [ =A2 + 2 ]
#[test]
fn basic() {
    let mut db = Db::new();
    let result = *db.get::<i32>(Basic::A3);
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

    let a1 = db.get_cell_value(&Basic::A1);
    assert_eq!(a1.last_updated_version, expected_version);
    assert_eq!(a1.last_verified_version, expected_version);

    let a2 = db.get_cell_value(&Basic::A2);
    assert_eq!(a2.last_updated_version, expected_version);
    assert_eq!(a2.last_verified_version, expected_version);

    let a3 = db.get_cell_value(&Basic::A3);
    assert_eq!(a3.last_updated_version, expected_version);
    assert_eq!(a3.last_verified_version, expected_version);
}
