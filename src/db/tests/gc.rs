use crate::{StorageFor, define_input, define_intermediate, impl_storage};

use crate::storage::{HashMapStorage, TreeIndexStorage};

#[derive(Clone, PartialEq, Eq, Ord, PartialOrd)]
struct Output(i32);

#[derive(Clone, Hash, PartialEq, Eq)]
struct Input(i32);

#[derive(Default)]
struct MyData {
    input: HashMapStorage<Input>,
    output: TreeIndexStorage<Output>,
}

impl_storage!(MyData,
    input: Input,
    output: Output,
);

define_input!(0, Input -> i64, MyData);
define_intermediate!(1, Output -> i64, MyData, |ctx, db| {
    db.get(Input(ctx.0)) * 2
});

use crate::Db;

#[test]
fn proper_collection() {
    let mut db = Db::<MyData>::new();
    let to_delete = 0;
    let to_keep = 1;

    db.update_input(Input(to_delete), 1000);
    db.get(Output(to_delete));

    db.update_input(Input(to_keep), 3);
    db.get(Output(to_keep));

    db.gc(db.version());

    // Assert these have been tossed
    assert!(
        db.storage()
            .input
            .get_cell_for_computation(&Input(to_delete))
            .is_none()
    );
    assert!(
        db.storage()
            .output
            .get_cell_for_computation(&Output(to_delete))
            .is_none()
    );

    // Assert these still exist
    assert!(
        db.storage()
            .input
            .get_cell_for_computation(&Input(to_keep))
            .is_some()
    );
    assert!(
        db.storage()
            .output
            .get_cell_for_computation(&Output(to_keep))
            .is_some()
    );
}
