use inc_complete::{define_input, define_intermediate, Computation, DbHandle};


type Db = inc_complete::Db<(Strings, CountAs, CountBs, AsPlusBs)>;

type DbWithoutAsPlusBs = inc_complete::Db<(Strings, CountAs, CountBs)>;

define_input!(
    #[derive(serde::Serialize, serde::Deserialize)]
    Strings { name: String }, get_string, String);

define_intermediate!(
    #[derive(serde::Serialize, serde::Deserialize)]
    CountAs { &name: String }, count_as, cloned usize, count_as_impl);

define_intermediate!(
    #[derive(serde::Serialize, serde::Deserialize)]
    CountBs { &name: String }, count_bs, cloned usize, count_bs_impl);

define_intermediate!(
    #[derive(serde::Serialize, serde::Deserialize)]
    AsPlusBs { &name: String }, as_plus_bs, cloned usize, as_plus_bs_impl);

fn count_as_impl(name: &String, db: &mut DbHandle<impl Computation>) -> usize {
    let input = get_string(name.clone(), db);
    input.chars().filter(|c| *c == 'a').count()
}

fn count_bs_impl(name: &String, db: &mut DbHandle<impl Computation>) -> usize {
    let input = get_string(name.clone(), db);
    input.chars().filter(|c| *c == 'b').count()
}

fn as_plus_bs_impl(name: &String, db: &mut DbHandle<impl Computation>) -> usize {
    let a_count = count_as(name.clone(), db);
    let b_count = count_bs(name.clone(), db);
    a_count + b_count
}

/// Test basic serialization and deserialization with no changes to Db structure
#[test]
fn still_cached_after_serialize() {
    let mut db = Db::new();
    let half = "50%".to_string();
    db.update_input(Strings(half.clone()), "ababababab ababababab".to_string());

    assert_eq!(count_as_db(half.clone(), &mut db), 10);

    let serialized = serde_json::to_string(&db).unwrap();
    let mut new_db: Db = serde_json::from_str(&serialized).unwrap();

    assert!(!new_db.is_stale(&CountAs(half.clone())));
    assert!(new_db.is_stale(&AsPlusBs(half.clone())));

    assert_eq!(as_plus_bs_db(half.clone(), &mut new_db), 20);

    assert!(!new_db.is_stale(&AsPlusBs(half.clone())));
    assert!(db.is_stale(&AsPlusBs(half.clone())));
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
    db.update_input(Strings(half.clone()), "ababababab ababababab".to_string());

    // Ensure everything in the original database is filled so we can assert
    // the new item in the new_db later on is not
    assert_eq!(count_as_db(half.clone(), &mut db), 10);
    assert_eq!(count_bs_db(half.clone(), &mut db), 10);

    let serialized = serde_json::to_string(&db).unwrap();

    // Deserializing a Db here, not a DbWithoutAsPlusBs!
    let mut extended_db: Db = serde_json::from_str(&serialized).unwrap();

    assert!(!extended_db.is_stale(&CountAs(half.clone())));
    assert!(!extended_db.is_stale(&CountBs(half.clone())));
    assert!(extended_db.is_stale(&AsPlusBs(half.clone())));

    assert_eq!(as_plus_bs_db(half.clone(), &mut extended_db), 20);
}
