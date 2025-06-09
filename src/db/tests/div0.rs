use crate::{Cached, Db, DbHandle, Input, OutputTypeForInput, Run, db::START_VERSION};

type SafeDiv = (
    Input<Numerator>,
    Input<Denominator>,
    Cached<Division>,
    Cached<DenominatorIs0>,
    Cached<Result>,
);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Numerator;
const NUMERATOR: Input<Numerator> = Input::new();

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Denominator;
const DENOMINATOR: Input<Denominator> = Input::new();

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Division;
const DIVISION: Cached<Division> = Cached::new(Division);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct DenominatorIs0;
const DENOMINATOR_IS_0: Cached<DenominatorIs0> = Cached::new(DenominatorIs0);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Result;
const RESULT: Cached<Result> = Cached::new(Result);

impl OutputTypeForInput for Numerator {
    type Output = i32;
}

impl OutputTypeForInput for Denominator {
    type Output = i32;
}

impl Run for Division {
    type Output = i32;

    fn run(&self, handle: &mut DbHandle<impl crate::Computation>) -> Self::Output {
        *handle.get(NUMERATOR) / *handle.get(DENOMINATOR)
    }
}

impl Run for DenominatorIs0 {
    type Output = bool;

    fn run(&self, handle: &mut DbHandle<impl crate::Computation>) -> Self::Output {
        *handle.get(DENOMINATOR) == 0
    }
}

impl Run for Result {
    type Output = i32;

    fn run(&self, handle: &mut DbHandle<impl crate::Computation>) -> Self::Output {
        if *handle.get(DENOMINATOR_IS_0) {
            0
        } else {
            *handle.get(DIVISION)
        }
    }
}

type SafeDivDb = Db<SafeDiv>;

#[test]
fn from_scratch() {
    // Run from scratch
    let mut db = SafeDivDb::new();
    db.update_input(NUMERATOR, 6);
    db.update_input(DENOMINATOR, 0);
    assert_eq!(0i32, *db.get(RESULT));
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
    db.update_input(NUMERATOR, 6);
    db.update_input(DENOMINATOR, 2);

    assert_eq!(db.version, START_VERSION + 2);

    // 6 / 2
    assert_eq!(3i32, *db.get(RESULT));

    db.update_input(DENOMINATOR, 0);
    assert_eq!(db.version, START_VERSION + 3);

    // Although Division was previously a dependency of Result,
    // we shouldn't update Division due to the `DenominatorIs0` changing as well,
    // leading us into a different branch where `Division` is no longer required.
    // If we did recalculate `Division` we would get a divide by zero error.
    //
    // Shouldn't get a divide by zero here
    assert_eq!(0i32, *db.get(RESULT));
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
    db.update_input(NUMERATOR, 6);
    db.update_input(DENOMINATOR, 2);

    // Compute with non-zero denominator so that Division is registered as a dependency
    assert_eq!(*db.get(RESULT), 3);
    let divide_changed_version = db.version;

    // Re-run with Denominator = 0
    db.update_input(DENOMINATOR, 0);
    assert_eq!(*db.get(RESULT), 0);

    let divide0_version = db.version;
    let result_cell = db.unwrap_cell_value(&RESULT);
    let result_last_verified = result_cell.last_verified_version;
    let result_last_updated = result_cell.last_updated_version;

    assert_eq!(result_last_verified, divide0_version);
    assert_eq!(result_last_updated, divide0_version);

    eprintln!("\nSetting numerator = 12 now");

    // Now update the Numerator and test that Result is not re-computed.
    // If Division were still a dependency, updating Numerator would trigger
    // Division to be updated, which would also update Result.
    db.update_input(NUMERATOR, 12);
    assert!(*db.get(DENOMINATOR_IS_0));

    // DenominatorIs0 was just verified, ensure that Result does not need to be recomputed.
    // If Division were still a dependency, we'd expect Result to be stale.
    assert!(!db.is_stale(&RESULT));

    // Division shouldn't have been updated or verified in a while
    let division_cell = db.unwrap_cell_value(&DIVISION);
    let division_last_verified = division_cell.last_verified_version;
    let division_last_updated = division_cell.last_updated_version;
    assert_eq!(division_last_verified, divide_changed_version);
    assert_eq!(division_last_updated, divide_changed_version);
}
