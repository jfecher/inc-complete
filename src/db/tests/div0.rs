use crate::{db::START_VERSION, Db, DbHandle, Run, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum SafeDiv {
    Numerator,
    Denominator,
    Division,
    DenominatorIs0,
    Result,
}

impl Run for SafeDiv {
    fn run(self, db: &mut DbHandle<Self>) -> Value {
        use SafeDiv::*;
        match self {
            Numerator => Value::new(6),
            Denominator => Value::new(0),
            Division => {
                eprintln!("division");
                Value::new(*db.get::<i32>(Numerator) / *db.get::<i32>(Denominator))
            }
            DenominatorIs0 => {
                eprintln!("denominator is 0?");
                Value::new(*db.get::<i32>(Denominator) == 0)
            }
            Result => {
                eprintln!("result");
                if *db.get(DenominatorIs0) {
                    Value::new(0i32)
                } else {
                    Value::new(*db.get::<i32>(Division))
                }
            }
        }
    }
}

#[test]
fn from_scratch() {
    // Run from scratch with Denominator = 0
    assert_eq!(0i32, *Db::new().get(SafeDiv::Result));
}

#[test]
fn dynamic_dependency_not_run() {
    // Given:
    //  Numerator = 4
    //  Denominator = 2
    //  Division = Numerator / Denominator
    //  DenominatorIs0 = Denominator == 0
    //  Result = if DenominatorIs0 { 0 } else { Division }
    //
    // We should expect a result of 2. When changing Denominator to 0,
    // we should avoid recalculating Division even though it was previously
    // a dependency of Result since Division is no longer required, and doing
    // so would result in a divide by zero error.
    //
    // Start with Denominator = 2, then recompute with Denominator = 0
    let mut db = Db::new();
    assert_eq!(db.version, START_VERSION);
    db.update_input(SafeDiv::Denominator, Value::new(2i32));
    assert_eq!(db.version, START_VERSION + 1);

    // 6 / 2
    assert_eq!(3i32, *db.get(SafeDiv::Result));

    db.update_input(SafeDiv::Denominator, Value::new(0i32));
    assert_eq!(db.version, START_VERSION + 2);

    // Although Division was previously a dependency of Result,
    // we shouldn't update Division due to the `DenominatorIs0` changing as well,
    // leading us into a different branch where `Division` is no longer required.
    // If we did recalculate `Division` we would get a divide by zero error.
    //
    // Shouldn't get a divide by zero here
    assert_eq!(0i32, *db.get(SafeDiv::Result));
}

/// Test that a dynamic dependency - such as Division for Result - is no longer
/// registered as a dependency after the rule is re-run and a different path is
/// taken. `dynamic_dependency_not_run` is similar but only tests the dynamic
/// dependency is not re-executed (via dividing by zero if it is). This test
/// ensures it is no longer registered as a dependency within the internal graph
/// itself, and thus that Result is not unnecessarily re-updated if Division changes
/// without Denominator also changing.
#[test]
fn dynamic_dependency_removed() {
    let mut db = Db::new();
    db.update_input(SafeDiv::Denominator, Value::new(2i32));

    // Compute with non-zero denominator so that Division is registered as a dependency
    assert_eq!(*db.get::<i32>(SafeDiv::Result), 3);
    let divide_changed_version = db.version;

    // Re-run with Denominator = 0
    db.update_input(SafeDiv::Denominator, Value::new(0i32));
    assert_eq!(*db.get::<i32>(SafeDiv::Result), 0);

    let divide0_version = db.version;
    let result_cell = db.get_cell_value(SafeDiv::Result);
    let result_last_verified = result_cell.last_verified_version;
    let result_last_updated = result_cell.last_updated_version;

    assert_eq!(result_last_verified, divide0_version);
    assert_eq!(result_last_updated, divide0_version);

    eprintln!("\nSetting numerator = 12 now");

    // Now update the Numerator and test that Result is not re-computed.
    // If Division were still a dependency, updating Numerator would trigger
    // Division to be updated, which would also update Result.
    db.update_input(SafeDiv::Numerator, Value::new(12i32));
    assert_eq!(*db.get::<bool>(SafeDiv::DenominatorIs0), true);

    // DenominatorIs0 was just verified, ensure that Result does not need to be recomputed.
    // If Division were still a dependency, we'd expect Result to be stale.
    assert!(!db.is_stale(SafeDiv::Result));

    // Division shouldn't have been updated or verified in a while
    let division_cell = db.get_cell_value(SafeDiv::Division);
    let division_last_verified = division_cell.last_verified_version;
    let division_last_updated = division_cell.last_updated_version;
    assert_eq!(division_last_verified, divide_changed_version);
    assert_eq!(division_last_updated, divide_changed_version);
}
