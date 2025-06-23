use crate::{
    Db, DbHandle, db::START_VERSION, define_input, define_intermediate, impl_storage,
    storage::SingletonStorage,
};

#[derive(Default)]
struct SafeDiv {
    numerator: SingletonStorage<Numerator>,
    denominator: SingletonStorage<Denominator>,
    division: SingletonStorage<Division>,
    denominator_is_0: SingletonStorage<DenominatorIs0>,
    result: SingletonStorage<Result>,
}

impl_storage!(SafeDiv,
    numerator: Numerator,
    denominator: Denominator,
    division: Division,
    denominator_is_0: DenominatorIs0,
    result: Result,
);

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
struct Numerator;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
struct Denominator;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
struct Division;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
struct DenominatorIs0;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
struct Result;

define_input!(0, Numerator -> i32, SafeDiv);
define_input!(1, Denominator -> i32, SafeDiv);

define_intermediate!(2, Division -> i32, SafeDiv, |_, handle: &DbHandle<SafeDiv>| {
    println!("division");
    handle.get(Numerator) / handle.get(Denominator)
});

define_intermediate!(3, DenominatorIs0 -> bool, SafeDiv, |_, handle: &DbHandle<SafeDiv>| {
    println!("denominator is 0");
    handle.get(Denominator) == 0
});

define_intermediate!(4, Result -> i32, SafeDiv, |_, handle: &DbHandle<SafeDiv>| {
    println!("result");
    if handle.get(DenominatorIs0) {
        0
    } else {
        handle.get(Division)
    }
});

type SafeDivDb = Db<SafeDiv>;

#[test]
fn from_scratch() {
    // Run from scratch
    let mut db = SafeDivDb::new();
    db.update_input(Numerator, 6);
    db.update_input(Denominator, 0);
    assert_eq!(0i32, db.get(Result));
}

#[test]
fn dynamic_dependency_not_run() {
    // Given:
    //  Numerator = 6
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
    let mut db = SafeDivDb::new();
    db.update_input(Numerator, 6);
    db.update_input(Denominator, 2);

    assert_eq!(db.version(), START_VERSION + 2);

    // 6 / 2
    assert_eq!(3i32, db.get(Result));

    println!("\n");

    db.update_input(Denominator, 0);
    assert_eq!(db.version(), START_VERSION + 3);

    // Although Division was previously a dependency of Result,
    // we shouldn't update Division due to the `DenominatorIs0` changing as well,
    // leading us into a different branch where `Division` is no longer required.
    // If we did recalculate `Division` we would get a divide by zero error.
    //
    // Shouldn't get a divide by zero here
    assert_eq!(0i32, db.get(Result));
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
    let mut db = SafeDivDb::new();
    db.update_input(Numerator, 6);
    db.update_input(Denominator, 2);

    // Compute with non-zero denominator so that Division is registered as a dependency
    assert_eq!(db.get(Result), 3);
    let divide_changed_version = db.version();

    // Re-run with Denominator = 0
    db.update_input(Denominator, 0);
    assert_eq!(db.get(Result), 0);

    let divide0_version = db.version();
    let result_cell = db.unwrap_cell_value(&Result);
    let result_last_verified = result_cell.last_verified_version;
    let result_last_updated = result_cell.last_updated_version;

    assert_eq!(result_last_verified, divide0_version);
    assert_eq!(result_last_updated, divide0_version);

    eprintln!("\nSetting numerator = 12 now");

    // Now update the Numerator and test that Result is not re-computed.
    // If Division were still a dependency, updating Numerator would trigger
    // Division to be updated, which would also update Result.
    db.update_input(Numerator, 12);
    assert!(db.get(DenominatorIs0));

    // DenominatorIs0 was just verified, ensure that Result does not need to be recomputed.
    // If Division were still a dependency, we'd expect Result to be stale.
    assert!(!db.is_stale(&Result));

    // Division shouldn't have been updated or verified in a while
    let division_cell = db.unwrap_cell_value(&Division);
    let division_last_verified = division_cell.last_verified_version;
    let division_last_updated = division_cell.last_updated_version;
    assert_eq!(division_last_verified, divide_changed_version);
    assert_eq!(division_last_updated, divide_changed_version);
}
