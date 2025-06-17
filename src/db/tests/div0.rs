use crate::{
    db::START_VERSION, impl_storage_for_field, storage::SingletonStorage, Db, DbHandle, OutputType, Run
};

struct SafeDiv {
    numerator: SingletonStorage<Numerator>,
    denominator: SingletonStorage<Denominator>,
    division: SingletonStorage<Division>,
    denominator_is_0: SingletonStorage<DenominatorIs0>,
    result: SingletonStorage<Result>,
}

impl_storage_for_field!(SafeDiv, numerator, Numerator);
impl_storage_for_field!(SafeDiv, denominator, Denominator);
impl_storage_for_field!(SafeDiv, division, Division);
impl_storage_for_field!(SafeDiv, denominator_is_0, DenominatorIs0);
impl_storage_for_field!(SafeDiv, result, Result);

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

impl OutputType for Numerator { type Output = i32; }

impl OutputType for Denominator { type Output = i32; }

impl OutputType for Division { type Output = i32; }
impl Run<SafeDiv> for Division {
    fn run(&self, handle: &mut DbHandle<SafeDiv>) -> Self::Output {
        *handle.get(Numerator::new()) / *handle.get(Denominator::new())
    }
}

impl OutputType for DenominatorIs0 { type Output = bool; }
impl Run<SafeDiv> for DenominatorIs0 {
    fn run(&self, handle: &mut DbHandle<DenominatorIs0>) -> Self::Output {
        *handle.get(Denominator::new()) == 0
    }
}

impl OutputType for Result { type Output = i32; }
impl Run<SafeDiv> for Result {
    fn run(&self, handle: &mut DbHandle<SafeDiv>) -> Self::Output {
        if *handle.get(DenominatorIs0::new()) {
            0
        } else {
            *handle.get(Division::new())
        }
    }
}

type SafeDivDb = Db<SafeDiv>;

#[test]
fn from_scratch() {
    // Run from scratch
    let mut db = SafeDivDb::new();
    db.update_input(Numerator::new(), 6);
    db.update_input(Denominator::new(), 0);
    assert_eq!(0i32, *db.get(Result::new()));
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
    db.update_input(Numerator::new(), 6);
    db.update_input(Denominator::new(), 2);

    assert_eq!(db.version, START_VERSION + 2);

    // 6 / 2
    assert_eq!(3i32, *db.get(Result::new()));

    db.update_input(Denominator::new(), 0);
    assert_eq!(db.version, START_VERSION + 3);

    // Although Division was previously a dependency of Result,
    // we shouldn't update Division due to the `DenominatorIs0` changing as well,
    // leading us into a different branch where `Division` is no longer required.
    // If we did recalculate `Division` we would get a divide by zero error.
    //
    // Shouldn't get a divide by zero here
    assert_eq!(0i32, *db.get(Result::new()));
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
    db.update_input(Numerator::new(), 6);
    db.update_input(Denominator::new(), 2);

    // Compute with non-zero denominator so that Division is registered as a dependency
    assert_eq!(*db.get(Result::new()), 3);
    let divide_changed_version = db.version;

    // Re-run with Denominator = 0
    db.update_input(Denominator::new(), 0);
    assert_eq!(*db.get(Result::new()), 0);

    let divide0_version = db.version;
    let result_cell = db.unwrap_cell_value(&Result::new());
    let result_last_verified = result_cell.last_verified_version;
    let result_last_updated = result_cell.last_updated_version;

    assert_eq!(result_last_verified, divide0_version);
    assert_eq!(result_last_updated, divide0_version);

    eprintln!("\nSetting numerator = 12 now");

    // Now update the Numerator and test that Result is not re-computed.
    // If Division were still a dependency, updating Numerator would trigger
    // Division to be updated, which would also update Result.
    db.update_input(Numerator::new(), 12);
    assert!(*db.get(DenominatorIs0::new()));

    // DenominatorIs0 was just verified, ensure that Result does not need to be recomputed.
    // If Division were still a dependency, we'd expect Result to be stale.
    assert!(!db.is_stale(&Result::new()));

    // Division shouldn't have been updated or verified in a while
    let division_cell = db.unwrap_cell_value(&Division::new());
    let division_last_verified = division_cell.last_verified_version;
    let division_last_updated = division_cell.last_updated_version;
    assert_eq!(division_last_verified, divide_changed_version);
    assert_eq!(division_last_updated, divide_changed_version);
}
