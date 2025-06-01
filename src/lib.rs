use std::{
    collections::HashMap,
    hash::Hash,
};

use db_handle::DbHandle;
use petgraph::graph::DiGraph;

mod cell;
mod db_handle;
mod value;

use value::HashEqObj;
pub use value::Value;
pub use value::Run;

const START_VERSION: u32 = 1;

pub struct Db<F> {
    cells: DiGraph<CellValue<F>, ()>,
    version: u32,

    input_to_cell: HashMap<F, Cell>,
}

#[derive(Debug, Copy, Clone)]
pub struct Cell(petgraph::graph::NodeIndex);

struct CellValue<F> {
    compute: F,
    result: Option<(u64, Value)>,

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
    /// Retrieves the up to date value for the given computation, re-running any dependencies as
    /// necessary.
    ///
    /// This function can panic if the dynamic type of the value returned by `compute.run(..)` is not `T`.
    pub fn get<'a, T: 'static>(&'a mut self, compute: F) -> &'a T where F: std::fmt::Debug {
        let cell_id = self.cell(compute);
        self.update_cell(cell_id);

        let cell = &self.cells[cell_id.0];
        let result = &cell
            .result
            .as_ref()
            .expect("cell result should have been computed already")
            .1;

        result.downcast_obj_ref()
            .expect("Output type to `Db::get` does not match the type of the value returned by the `Run::run` function")
    }

    pub fn update_cell(&mut self, cell_id: Cell) where F: std::fmt::Debug {
        let cell = &self.cells[cell_id.0];

        eprintln!("get {:?}", cell.compute);

        if cell.last_verified_version != self.version {
            eprintln!("{:?}: last verified version {} != self.version {}",
                cell.compute,
                cell.last_verified_version, self.version);

            if cell.result.is_some() {
            eprintln!("{:?}: result is some", cell.compute);

                let neighbors = self.cells.neighbors(cell_id.0).collect::<Vec<_>>();
            eprintln!("{:?}: {} neighbors", cell.compute, neighbors.len());
            let mut i = 0;

                // recur downward to check if each dependency is up to date
                for neighbor in &neighbors {
                    self.update_cell(Cell(*neighbor));
                }

                // now if any dependency changed, update
                if neighbors.into_iter().any(|input_id| {
                    let input = &self.cells[input_id];
                    let cell = &self.cells[cell_id.0];
                    let dependency_updated = input.last_updated_version > cell.last_verified_version;

                    i += 1;
            eprintln!("{:?} neighbor {:?}: last_updated {} >? cell.last_verified {}",
                cell.compute,
                input.compute,
                input.last_updated_version, cell.last_verified_version);

                    dependency_updated
                }) {
                eprintln!("{:?}: update_cell", &self.cells[cell_id.0].compute);
                    self.run_compute_function(cell_id);
                } else {
                    let cell = &mut self.cells[cell_id.0];
                    cell.last_verified_version = self.version;
                }
            } else
            /* cell.result is None, initialize it */
            {
                eprintln!("{:?}: update_cell'", cell.compute);
                self.run_compute_function(cell_id);
            }
        } else {
            eprintln!("{:?}: last verified version {} == self.version {}, using cached value",
                cell.compute,
                cell.last_verified_version, self.version);
        }
    }

    pub fn cell(&mut self, input: F) -> Cell {
        if let Some(cell_id) = self.input_to_cell.get(&input) {
            *cell_id
        } else {
            let new_id = self.cells.add_node(CellValue::new(input.clone()));
            let cell = Cell(new_id);
            self.input_to_cell.insert(input, cell);
            cell
        }
    }

    pub fn update_input(&mut self, input: F, new_value: Value) {
        let cell_id = self.cell(input);
        let cell = &mut self.cells[cell_id.0];
        let new_hash = new_value.get_hash();

        if let Some((old_hash, _)) = cell.result.as_ref() {
            if new_hash == *old_hash {
                cell.last_verified_version = self.version;
                return;
            }
        }

        self.version += 1;
        eprintln!("Input did not match previous, bumping self.version to {}", self.version);
        cell.result = Some((new_hash, new_value));
        cell.last_updated_version = self.version;
        cell.last_verified_version = self.version;
    }

    #[cfg(test)]
    fn get_cell(&mut self, input: F) -> &CellValue<F> {
        let cell = self.cell(input);
        &self.cells[cell.0]
    }

    /// Similar to `update_input` but runs the compute function
    /// instead of accepting a given value. This also will not update
    /// `self.version`
    fn run_compute_function(&mut self, cell_id: Cell) {
        let cell = &self.cells[cell_id.0];
        let result = cell.compute.run(&mut self.handle(cell_id));
        let new_hash = result.get_hash();
        let cell = &mut self.cells[cell_id.0];

        if let Some((old_hash, _)) = cell.result.as_ref() {
            if new_hash == *old_hash {
                cell.last_verified_version = self.version;
                return;
            }
        }

        cell.result = Some((new_hash, result));
        cell.last_verified_version = self.version;
        cell.last_updated_version = self.version;
    }

    fn handle(&mut self, cell: Cell) -> DbHandle<F> {
        DbHandle::new(self, cell)
    }
}

#[cfg(test)]
mod tests {
    use crate::{db_handle::DbHandle, Db, Run, Value, START_VERSION};

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
        fn run(self, db: &mut DbHandle<Self>) -> Value {
            match self {
                Basic::A1 => {
                    eprintln!("Computing A1");
                    Value::new(20i32)
                }
                Basic::A2 => {
                    eprintln!("Computing A2");
                    Value::new(db.get::<i32>(Basic::A1) + 1i32)
                }
                Basic::A3 => {
                    eprintln!("Computing A3");
                    Value::new(db.get::<i32>(Basic::A2) + 2i32)
                }
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

        // No input has been updated
        let expected_version = START_VERSION;
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
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        enum EarlyCutoff {
            Numerator,
            Denominator,
            Division,
            DenominatorIs0,
            Result,
        }

        impl Run for EarlyCutoff {
            fn run(self, db: &mut DbHandle<Self>) -> Value {
                use EarlyCutoff::*;
                match self {
                    Numerator => {
                        eprintln!("  numerator = 4");
                        Value::new(4)
                    }
                    Denominator => {
                        eprintln!("  denominator = 4");
                        Value::new(0)
                    }
                    Division => {
                        let a = *db.get::<i32>(Numerator);
                        let b = *db.get::<i32>(Denominator);
                        let r = a / b;
                        eprintln!("  {a} / {b} = {r}");
                        Value::new(r)
                    }
                    DenominatorIs0 => {
                        let d = *db.get::<i32>(Denominator);
                        eprintln!("  ({d} == 0) = {}", d == 0);
                        Value::new(d == 0)
                    }
                    Result => {
                        let is0 = *db.get(DenominatorIs0);
                        if is0 {
                            eprintln!("  (denominator is 0 short-circuit), if result is 0");
                            Value::new(0i32)
                        } else {
                            eprintln!("  (non-zero denominator, querying division result)");
                            Value::new(*db.get::<i32>(Division))
                        }
                    }
                }
            }
        }

        {
            // Run from scratch with Denominator = 0
            assert_eq!(0i32, *Db::new().get(EarlyCutoff::Result));
        }

        eprintln!("\n\n\n");
        {
            // Start with Denominator = 2, then recompute with Denominator = 0
            let mut db = Db::new();
            assert_eq!(db.version, START_VERSION);
            db.update_input(EarlyCutoff::Denominator, Value::new(2i32));
            assert_eq!(db.version, START_VERSION + 1);

            assert_eq!(2i32, *db.get(EarlyCutoff::Result));

            eprintln!("\nRecompute:");
            db.update_input(EarlyCutoff::Denominator, Value::new(0i32));
            assert_eq!(db.version, START_VERSION + 2);

            // Shouldn't get a divide by zero here
            assert_eq!(0i32, *db.get(EarlyCutoff::Result));
        }
    }
}
