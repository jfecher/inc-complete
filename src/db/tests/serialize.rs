use crate::{Computation, DbHandle, HashMapStorage, Intermediate, Run};

#[derive(Clone, PartialEq, Eq, Hash)]
struct Fibonacci {
    x: u32,
}

impl Fibonacci {
    fn new(x: u32) -> HashMapStorage<Intermediate<Fibonacci>> {
        HashMapStorage::new(Intermediate::new(Fibonacci { x }))
    }
}

fn fibonacci(x: u32, db: &mut DbHandle<impl Computation>) -> u32 {
    *db.get(Fibonacci::new(x))
}

fn fibonacci_impl(x: u32, db: &mut DbHandle<impl Computation>) -> u32 {
    if x <= 1 {
        x
    } else {
        // Not exponential time since each sub-computation will be cached!
        fibonacci(x - 1, db) + fibonacci(x - 2, db)
    }
}

impl Run for Fibonacci {
    type Output = u32;

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
        fibonacci_impl(self.x, handle)
    }
}
