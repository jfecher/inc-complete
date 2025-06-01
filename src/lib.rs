use std::{
    any::Any,
    collections::HashMap,
    hash::{Hash, Hasher},
};

use petgraph::graph::DiGraph;

const START_VERSION: u32 = 1;

pub struct Db<F> {
    cells: DiGraph<CellValue<F>, ()>,
    version: u32,

    input_to_cell: HashMap<F, Cell>,
}

#[derive(Copy, Clone)]
pub struct Cell(petgraph::graph::NodeIndex);

struct CellValue<F> {
    compute: F,
    result: Option<(u64, Box<dyn Any>)>,

    last_updated_version: u32,
    last_verified_version: u32,
}

impl<F> CellValue<F> {
    fn new(compute: F) -> Self {
        Self {
            compute,
            result: None,
            last_updated_version: 0,
            last_verified_version: 0,
        }
    }
}

pub trait Run: Sized {
    fn run(self, db: &mut Db<Self>) -> impl Any + Hash;
}

impl<F> Db<F> {
    pub fn new() -> Self {
        Self {
            cells: DiGraph::default(),
            input_to_cell: HashMap::default(),
            version: START_VERSION,
        }
    }
}

impl<F: Run + Copy + Eq + Hash + Clone> Db<F> {
    pub fn get<'a, T: 'static>(&'a mut self, compute: F) -> &'a T {
        let cell_id = self.input(compute);
        let cell = &self.cells[cell_id.0];

        if cell.last_verified_version != self.version {
            if cell.result.is_some() {
                let neighbors = self.cells.neighbors(cell_id.0).collect::<Vec<_>>();

                if neighbors.into_iter().any(|input_id| {
                    let input = &self.cells[input_id];
                    let cell = &self.cells[cell_id.0];
                    let need_to_update = input.last_updated_version > cell.last_verified_version;
                    if need_to_update {
                        self.update_cell(Cell(input_id));
                    }
                    need_to_update
                }) {
                    self.update_cell(cell_id);
                } else {
                    let cell = &mut self.cells[cell_id.0];
                    cell.last_verified_version = self.version;
                }
            } else
            /* cell.result is None, initialize it */
            {
                self.update_cell(cell_id);
            }
        }

        let cell = &self.cells[cell_id.0];
        let result = cell
            .result
            .as_ref()
            .expect("cell result should have been computed already")
            .1
            .as_ref();

        result
            .downcast_ref()
            .expect("Output type to `Db::get` does not match the type of the value returned by the `Run::run` function")
    }

    pub fn input(&mut self, input: F) -> Cell {
        if let Some(cell_id) = self.input_to_cell.get(&input) {
            *cell_id
        } else {
            self.version += 1;
            let new_id = self.cells.add_node(CellValue::new(input.clone()));
            let cell = Cell(new_id);
            self.input_to_cell.insert(input, cell);
            cell
        }
    }

    #[cfg(test)]
    fn get_cell(&mut self, input: F) -> &CellValue<F> {
        let cell = self.input(input);
        &self.cells[cell.0]
    }

    fn update_cell(&mut self, cell_id: Cell) {
        let cell = &self.cells[cell_id.0];
        let result = cell.compute.run(self);
        let new_hash = hash(&result);
        let cell = &mut self.cells[cell_id.0];

        if let Some((old_hash, _)) = cell.result.as_ref() {
            if new_hash == *old_hash {
                cell.last_verified_version = self.version;
                return;
            }
        }

        cell.result = Some((new_hash, Box::new(result)));
        cell.last_verified_version = self.version;
        cell.last_updated_version = self.version;
    }
}

// TODO: Use stable hash
fn hash<T: Hash>(x: T) -> u64 {
    let mut hasher = std::hash::DefaultHasher::default();
    x.hash(&mut hasher);
    hasher.finish()
}

#[cfg(test)]
mod tests {
    use crate::{Db, Run, START_VERSION};

    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    enum Basic {
        A1,
        A2,
        A3,
    }

    //      A
    // 1 [ =20 ]
    // 2 [ =A1 + 1 ]
    // 3 [ =A2 + 2 ]
    impl Run for Basic {
        fn run(self, db: &mut Db<Self>) -> impl std::any::Any + std::hash::Hash {
            match self {
                Basic::A1 => 20,
                Basic::A2 => db.get::<i32>(Basic::A1) + 1,
                Basic::A3 => db.get::<i32>(Basic::A2) + 2,
            }
        }
    }

    // Test that we can compute a basic chain of computation
    //      A
    // 1 [ =20 ]
    // 2 [ =A1 + 1 ]
    // 3 [ =A2 + 2 ]
    #[test]
    fn basic() {
        let mut db = Db::new();
        let result = *db.get::<i32>(Basic::A3);
        assert_eq!(result, 23);
    }

    // Test that we can re-use values from past runs
    //      A
    // 1 [ =20 ]
    // 2 [ =A1 + 1 ]
    // 3 [ =A2 + 2 ]
    #[test]
    fn no_recompute_basic() {
        let mut db = Db::new();
        let result1 = *db.get::<i32>(Basic::A3);
        let result2 = *db.get::<i32>(Basic::A3);
        assert_eq!(result1, 23);
        assert_eq!(result2, 23);

        // Expect 3 updates from caching A3, A2, A1
        let expected_version = START_VERSION + 3;
        assert_eq!(db.version, expected_version);

        let a1 = db.get_cell(Basic::A1);
        assert_eq!(a1.last_updated_version, expected_version);
        assert_eq!(a1.last_verified_version, expected_version);

        let a2 = db.get_cell(Basic::A2);
        assert_eq!(a2.last_updated_version, expected_version);
        assert_eq!(a2.last_verified_version, expected_version);

        let a3 = db.get_cell(Basic::A3);
        assert_eq!(a3.last_updated_version, expected_version);
        assert_eq!(a3.last_verified_version, expected_version);
    }

    #[test]
    fn early_cutoff() {
        #[derive(Clone, Copy, PartialEq, Eq, Hash)]
        enum EarlyCutoff {
            Numerator,
            Denominator,
            Division,
            DenominatorIs0,
            Result,
        }

        impl Run for EarlyCutoff {
            fn run(self, db: &mut Db<Self>) -> impl std::any::Any + std::hash::Hash {
                use EarlyCutoff::*;
                match self {
                    Numerator => 4,
                    Denominator => 2,
                    Division => db.get::<i32>(Numerator) / db.get::<i32>(Denominator),
                    DenominatorIs0 => *db.get::<i32>(Denominator) == 0,
                    Result => {
                        if *db.get(DenominatorIs0) {
                            0
                        } else {
                            *db.get(Division)
                        }
                    }
                }
            }
        }

        {
            // Run from scratch with Denominator = 0
            let mut db = Db::new();
            assert_eq!(*db.get(EarlyCutoff::Result), 0i32);
        }

        {
            // Start with Denominator = 2, then recompute with Denominator = 0
            let mut db = Db::new();
            assert_eq!(*db.get(EarlyCutoff::Result), 2i32);

            db.update_input(EarlyCutoff::Denominator, 0);
            // Shouldn't get a divide by zero here
            assert_eq!(*db.get(EarlyCutoff::Result), 0i32);
        }
    }
}
