use inc_complete::{
    DbHandle, Run, StorageFor, define_input, define_intermediate, impl_storage,
    storage::{BTreeMapStorage, HashMapStorage},
};
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

impl_storage!(Storage,
    strings: Strings,
    count_as: CountAs,
    count_bs: CountBs,
    as_plus_bs: AsPlusBs,
);

#[derive(Default, Serialize, Deserialize)]
struct StorageWithoutAsPlusBs {
    strings: BTreeMapStorage<Strings>,
    count_as: BTreeMapStorage<CountAs>,
    count_bs: BTreeMapStorage<CountBs>,
}

impl_storage!(StorageWithoutAsPlusBs,
    strings: Strings,
    count_as: CountAs,
    count_bs: CountBs,
);

define_input!(0, Strings -> String, Storage | StorageWithoutAsPlusBs);
#[derive(Debug, Serialize, Deserialize, Clone, PartialOrd, Ord, PartialEq, Eq)]
struct Strings {
    name: String,
}

define_intermediate!(1, CountAs -> usize, Storage | StorageWithoutAsPlusBs, count_as_impl);
#[derive(Serialize, Deserialize, Clone, PartialOrd, Ord, PartialEq, Eq)]
struct CountAs {
    name: String,
}

define_intermediate!(2, CountBs -> usize, Storage | StorageWithoutAsPlusBs, count_bs_impl);
#[derive(Serialize, Deserialize, Clone, PartialOrd, Ord, PartialEq, Eq)]
struct CountBs {
    name: String,
}

define_intermediate!(3, AsPlusBs -> usize, Storage | StorageWithoutAsPlusBs, as_plus_bs_impl);
#[derive(Serialize, Deserialize, Clone, Hash, PartialEq, Eq)]
struct AsPlusBs {
    name: String,
}

fn count_as_impl<S: inc_complete::Storage + StorageFor<Strings>>(
    this: &CountAs,
    db: &mut DbHandle<S>,
) -> usize {
    let input = db.get(Strings {
        name: this.name.clone(),
    });
    input.chars().filter(|c| *c == 'a').count()
}

fn count_bs_impl<S: inc_complete::Storage + StorageFor<Strings>>(
    this: &CountBs,
    db: &mut DbHandle<S>,
) -> usize {
    let input = db.get(Strings {
        name: this.name.clone(),
    });
    input.chars().filter(|c| *c == 'b').count()
}

fn as_plus_bs_impl<S>(params: &AsPlusBs, db: &mut DbHandle<S>) -> usize
where
    S: inc_complete::Storage + StorageFor<Strings> + StorageFor<CountAs> + StorageFor<CountBs>,
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
    db.update_input(
        Strings { name: half.clone() },
        "ababababab ababababab".to_string(),
    );

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
    db.update_input(
        Strings { name: half.clone() },
        "ababababab ababababab".to_string(),
    );

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
