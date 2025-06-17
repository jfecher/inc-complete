use inc_complete::{define_input, define_intermediate, impl_storage_for_field, storage::{BTreeMapStorage, HashMapStorage}, ComputationId, DbHandle, OutputType, Run, StorageFor};
use serde::{Deserialize, Serialize};

type Db = inc_complete::Db<Storage>;
type DbWithoutAsPlusBs = inc_complete::Db<StorageWithoutAsPlusBs>;

#[derive(Default, Serialize, Deserialize)]
struct Storage {
    strings: BTreeMapStorage<Strings>,
    count_as: BTreeMapStorage<CountAs>,
    count_bs: BTreeMapStorage<CountBs>,

    #[serde(default)]
    as_plus_bs: HashMapStorage<AsPlusBs>,
}

#[derive(Default, Serialize, Deserialize)]
struct StorageWithoutAsPlusBs {
    strings: BTreeMapStorage<Strings>,
    count_as: BTreeMapStorage<CountAs>,
    count_bs: BTreeMapStorage<CountBs>,
}

impl inc_complete::Storage for Storage {
    fn output_is_unset(&self, cell: inc_complete::Cell, computation_id: u32) -> bool {
        match computation_id {
            0 => self.strings.get_output(cell).is_none(),
            1 => self.count_as.get_output(cell).is_none(),
            2 => self.count_bs.get_output(cell).is_none(),
            3 => self.as_plus_bs.get_output(cell).is_none(),
            _ => panic!(),
        }
    }

    fn run_computation(db: &mut DbHandle<Self>, cell: inc_complete::Cell, computation_id: u32) -> bool {
        match computation_id {
            0 => panic!("Strings has no computation, did you forget to call `update_input`?"),
            1 => {
                let new_value = db.storage().count_as.get_input(cell).clone().run(db);
                db.storage_mut().count_as.update_output(cell, new_value)
            }
            2 => {
                let new_value = db.storage().count_bs.get_input(cell).clone().run(db);
                db.storage_mut().count_bs.update_output(cell, new_value)
            }
            3 => {
                let new_value = db.storage().as_plus_bs.get_input(cell).clone().run(db);
                db.storage_mut().as_plus_bs.update_output(cell, new_value)
            }
            _ => panic!(),
        }
    }
}

impl inc_complete::Storage for StorageWithoutAsPlusBs {
    fn output_is_unset(&self, cell: inc_complete::Cell, computation_id: u32) -> bool {
        match computation_id {
            0 => self.strings.get_output(cell).is_none(),
            1 => self.count_as.get_output(cell).is_none(),
            2 => self.count_bs.get_output(cell).is_none(),
            _ => panic!(),
        }
    }

    fn run_computation(db: &mut DbHandle<Self>, cell: inc_complete::Cell, computation_id: u32) -> bool {
        match computation_id {
            0 => panic!("Strings has no computation, did you forget to call `update_input`?"),
            1 => {
                let new_value = db.storage().count_as.get_input(cell).clone().run(db);
                db.storage_mut().count_as.update_output(cell, new_value)
            }
            2 => {
                let new_value = db.storage().count_bs.get_input(cell).clone().run(db);
                db.storage_mut().count_bs.update_output(cell, new_value)
            }
            _ => panic!(),
        }
    }
}


impl_storage_for_field!(Storage, strings, Strings);
impl_storage_for_field!(StorageWithoutAsPlusBs, strings, Strings);
define_input!(Strings, String, 0);
#[derive(Debug, Serialize, Deserialize, Clone, PartialOrd, Ord, PartialEq, Eq)]
struct Strings { name: String }

impl_storage_for_field!(Storage, count_as, CountAs);
impl_storage_for_field!(StorageWithoutAsPlusBs, count_as, CountAs);
#[derive(Serialize, Deserialize, Clone, PartialOrd, Ord, PartialEq, Eq)]
struct CountAs { name: String }

impl OutputType for CountAs {
    type Output = usize;
}

impl ComputationId for CountAs {
    fn computation_id() -> u32 {
        1
    }
}

impl Run<Storage> for CountAs {
    fn run(&self, db: &mut DbHandle<Storage>) -> Self::Output {
        count_as_impl(&self.name, db)
    }
}

impl Run<StorageWithoutAsPlusBs> for CountAs {
    fn run(&self, db: &mut DbHandle<StorageWithoutAsPlusBs>) -> Self::Output {
        count_as_impl(&self.name, db)
    }
}

impl_storage_for_field!(Storage, count_bs, CountBs);
impl_storage_for_field!(StorageWithoutAsPlusBs, count_bs, CountBs);
#[derive(Serialize, Deserialize, Clone, PartialOrd, Ord, PartialEq, Eq)]
struct CountBs { name: String }

impl OutputType for CountBs {
    type Output = usize;
}

impl ComputationId for CountBs {
    fn computation_id() -> u32 {
        2
    }
}

impl Run<Storage> for CountBs {
    fn run(&self, db: &mut DbHandle<Storage>) -> Self::Output {
        count_bs_impl(&self.name, db)
    }
}

impl Run<StorageWithoutAsPlusBs> for CountBs {
    fn run(&self, db: &mut DbHandle<StorageWithoutAsPlusBs>) -> Self::Output {
        count_bs_impl(&self.name, db)
    }
}

impl_storage_for_field!(Storage, as_plus_bs, AsPlusBs);
define_intermediate!(AsPlusBs, usize, 3, Storage, as_plus_bs_impl);
#[derive(Serialize, Deserialize, Clone, Hash, PartialEq, Eq)]
struct AsPlusBs { name: String }

fn count_as_impl<S: inc_complete::Storage + StorageFor<Strings>>(name: &String, db: &mut DbHandle<S>) -> usize {
    let input = db.get(Strings { name: name.clone() });
    input.chars().filter(|c| *c == 'a').count()
}

fn count_bs_impl<S: inc_complete::Storage + StorageFor<Strings>>(name: &String, db: &mut DbHandle<S>) -> usize {
    let input = db.get(Strings { name: name.clone() });
    input.chars().filter(|c| *c == 'b').count()
}

fn as_plus_bs_impl<S>(params: &AsPlusBs, db: &mut DbHandle<S>) -> usize where
    S: inc_complete::Storage + StorageFor<Strings> + StorageFor<CountAs> + StorageFor<CountBs>
{
    let name = &params.name;
    let a_count = *db.get(CountAs { name: name.clone() });
    let b_count = *db.get(CountBs { name: name.clone() });
    a_count + b_count
}

/// Test basic serialization and deserialization with no changes to Db structure
#[test]
fn still_cached_after_serialize() {
    let mut db = Db::new();
    let half = "50%".to_string();
    db.update_input(Strings { name: half.clone() }, "ababababab ababababab".to_string());

    assert_eq!(*db.get(CountAs { name: half.clone() }), 10);

    let serialized = serde_json::to_string(&db).unwrap();
    let mut new_db: Db = serde_json::from_str(&serialized).unwrap();

    assert!(!new_db.is_stale(&CountAs { name: half.clone() }));
    assert!(new_db.is_stale(&AsPlusBs { name: half.clone() }));

    assert_eq!(*new_db.get(AsPlusBs { name: half.clone() }), 20);

    assert!(!new_db.is_stale(&AsPlusBs { name: half.clone() }));
    assert!(db.is_stale(&AsPlusBs { name: half.clone() }));
}

/// Emulate a case where a user wants to add a new cached computation but also
/// remain backward-compatible with existing serialization.
///
/// Currently, inc-complete only supports backwards compatibility when computations
/// are added to the end of the Db's computation tuple generic argument. When
/// computations are added to the middle existing computation ids are changed.
#[test]
fn extend_preexisting_db_from_end() {
    let mut db = DbWithoutAsPlusBs::new();
    let half = "50%".to_string();
    db.update_input(Strings { name: half.clone() }, "ababababab ababababab".to_string());

    // Ensure everything in the original database is filled so we can assert
    // the new item in the new_db later on is not
    assert_eq!(*db.get(CountAs { name: half.clone() }), 10);
    assert_eq!(*db.get(CountBs { name: half.clone() }), 10);

    let serialized = serde_json::to_string(&db).unwrap();

    // Deserializing a Db here, not a DbWithoutAsPlusBs!
    let mut extended_db: Db = serde_json::from_str(&serialized).unwrap();

    assert!(!extended_db.is_stale(&CountAs { name: half.clone() }));
    assert!(!extended_db.is_stale(&CountBs { name: half.clone() }));
    assert!(extended_db.is_stale(&AsPlusBs { name: half.clone() }));

    assert_eq!(*extended_db.get(AsPlusBs { name: half.clone() }), 20);
}
