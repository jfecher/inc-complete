use inc_complete::{
    accumulate::Accumulator, define_input, define_intermediate, intermediate, storage::SingletonStorage, Db, Input, Storage
};

// Simple calculator storage
#[derive(Default, Storage)]
struct Calculator {
    input_a: SingletonStorage<InputA>,
    input_b: SingletonStorage<InputB>,

    /// Explicitly specify the computation type
    #[inc_complete(computation = Sum)]
    sum: SingletonStorage<Sum>,

    #[inc_complete(skip)]
    #[allow(unused)]
    dummy: u32,

    #[inc_complete(accumulate)]
    logs: Accumulator<String>,
}

// Input A
#[derive(Debug, Clone, Input)]
#[inc_complete(id = 0, output = i32, storage = Calculator)]
struct InputA;

// Input B
#[derive(Debug, Clone, Input)]
#[inc_complete(id = 1, output = i32, storage = Calculator)]
struct InputB;

// Sum computation
#[derive(Debug, Clone)]
struct Sum;

#[intermediate(id = 2)]
fn compute_sum(_sum: &Sum, db: &inc_complete::DbHandle<Calculator>) -> i32 {
    db.get(InputA) + db.get(InputB)
}

#[test]
fn test_simple_calculator() {
    let mut db = Db::<Calculator>::new();

    // Set inputs
    InputA.set(&mut db, 5);
    InputB.set(&mut db, 3);

    // Get sum
    let result = db.get(Sum);
    assert_eq!(result, 8);

    // Change input A
    InputA.set(&mut db, 10);
    let result = db.get(Sum);
    assert_eq!(result, 13);

    // Change input B
    InputB.set(&mut db, 7);
    let result = db.get(Sum);
    assert_eq!(result, 17);
}
