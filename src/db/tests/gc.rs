use crate::{DbHandle, define_input, define_intermediate, impl_storage};

use crate::storage::{HashMapStorage, SingletonStorage, TreeIndexStorage};

// EXAMPLE DATA
#[derive(Clone, Hash, PartialEq, Eq)]
struct HashStoredData;

#[derive(Clone)]
struct Input;

#[derive(Clone, PartialEq, Eq, Ord, PartialOrd)]
struct TreeStoredData;

#[derive(Default)]
struct MyData {
    hash: HashMapStorage<HashStoredData>,
    input: SingletonStorage<Input>,
    tree: TreeIndexStorage<TreeStoredData>,
}

impl_storage!(MyData,
    hash: HashStoredData,
    input: Input,
    tree: TreeStoredData
);

define_input!(0, Input -> i64, MyData);
define_intermediate!(1, TreeStoredData -> i64, MyData, |_, db| {
    db.get(Input) * 2
});
define_intermediate!(2, HashStoredData -> i64, MyData,
|_cache_dat: &HashStoredData, db: &DbHandle<MyData>| {
    println!("Computin some cached data!");
    db.get(TreeStoredData) + db.get(Input)
});

// Creates the following set of data:
// Input = {input}
// HashStoredData = TreeStoredData + Input
// TreeStoredData = Input * 2

use crate::Db;
use crate::db::START_VERSION;

// Tests that values from the old computations
// are removed, shamlessly stolen from basic.rs
#[test]
fn simple_garbage() {
    let mut db = Db::<MyData>::new();
    db.update_input(Input, 20);
    let result1 = db.get(TreeStoredData);
    let result2 = db.get(Input);
    let result3 = db.get(HashStoredData);
    assert_eq!(result1, 40);
    assert_eq!(result2, 20);
    assert_eq!(result3, 60);

    let mut expected_version = START_VERSION + 1;
    assert_eq!(db.version(), expected_version);

    db.with_cell_data(&Input, |inp| {
        assert_eq!(inp.last_updated_version, expected_version);
        assert_eq!(inp.last_verified_version, expected_version);
    });

    db.with_cell_data(&HashStoredData, |hsd| {
        assert_eq!(hsd.last_updated_version, expected_version);
        assert_eq!(hsd.last_verified_version, expected_version);
    });

    db.with_cell_data(&TreeStoredData, |tsd| {
        assert_eq!(tsd.last_updated_version, expected_version);
        assert_eq!(tsd.last_verified_version, expected_version);
    });

    // Since HashStoredData and TreeStoredData don't get updated/queried, they get tossed
    expected_version += 1;
    db.gc(expected_version);

    db.update_input(Input, 10);

    assert!(db.get_cell(&TreeStoredData).is_none());
    assert!(db.get_cell(&HashStoredData).is_none());
    assert!(db.get_cell(&Input).is_some());

    db.get(HashStoredData);
    db.get(TreeStoredData);

    assert!(db.get_cell(&TreeStoredData).is_some());
    assert!(db.get_cell(&HashStoredData).is_some());
    assert!(db.get_cell(&Input).is_some());
}
