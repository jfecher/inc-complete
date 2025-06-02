use std::{collections::HashMap, hash::Hash};

use db_handle::DbHandle;
use petgraph::graph::DiGraph;

mod cell;
mod db_handle;
mod value;

use value::HashEqObj;
pub use value::Run;
pub use value::Value;

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
    pub fn get<'a, T: 'static>(&'a mut self, compute: F) -> &'a T
    where
        F: std::fmt::Debug,
    {
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

    pub fn update_cell(&mut self, cell_id: Cell)
    where
        F: std::fmt::Debug,
    {
        let cell = &self.cells[cell_id.0];

        if cell.last_verified_version != self.version {
            if cell.result.is_some() {
                let neighbors = self.cells.neighbors(cell_id.0).collect::<Vec<_>>();

                // if any dependency may have changed, update
                if neighbors.into_iter().any(|input_id| {
                    let input = &self.cells[input_id];
                    let cell = &self.cells[cell_id.0];
                    let dependency_stale = input.last_verified_version != self.version
                        || input.last_updated_version > cell.last_verified_version;

                    dependency_stale
                }) {
                    self.run_compute_function(cell_id);
                } else {
                    let cell = &mut self.cells[cell_id.0];
                    cell.last_verified_version = self.version;
                }
            } else {
                // cell.result is None, initialize it
                self.run_compute_function(cell_id);
            }
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

    /// Updates an input with a new value
    ///
    /// May panic in Debug mode if the input is not an input - ie. it has at least 1 dependency.
    /// Note that this step is skipped when compiling in Release mode.
    pub fn update_input(&mut self, input: F, new_value: Value)
    where
        F: std::fmt::Debug,
    {
        let cell_id = self.cell(input);
        debug_assert!(
            self.is_input(cell_id),
            "`{input:?}` is not an input - inputs must have 0 dependencies"
        );

        let cell = &mut self.cells[cell_id.0];
        let new_hash = new_value.get_hash();

        if let Some((old_hash, _)) = cell.result.as_ref() {
            if new_hash == *old_hash {
                cell.last_verified_version = self.version;
                return;
            }
        }

        self.version += 1;
        cell.result = Some((new_hash, new_value));
        cell.last_updated_version = self.version;
        cell.last_verified_version = self.version;
    }

    fn is_input(&self, cell: Cell) -> bool {
        self.cells.neighbors(cell.0).count() == 0
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
    use crate::{Db, Run, START_VERSION, Value, db_handle::DbHandle};

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
                Basic::A1 => Value::new(20i32),
                Basic::A2 => Value::new(db.get::<i32>(Basic::A1) + 1i32),
                Basic::A3 => Value::new(db.get::<i32>(Basic::A2) + 2i32),
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
        // Given:
        //  Numerator = 4
        //  Denominator = 2
        //  Division = Numerator / Denominator
        //  DenominatorIs0 = Denominator == 0
        //  Result = if DenominatorIs0 { 0 } else { Division }
        //
        // We should expect a result of 2. When changing Denominator to 0,
        // we should avoid recalculating Division even though it was previously
        // a dependency of Result since Division is no longer required, and doing
        // so would result in a divide by zero error.
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
                    Numerator => Value::new(6),
                    Denominator => Value::new(0),
                    Division => Value::new(*db.get::<i32>(Numerator) / *db.get::<i32>(Denominator)),
                    DenominatorIs0 => Value::new(*db.get::<i32>(Denominator) == 0),
                    Result => {
                        if *db.get(DenominatorIs0) {
                            Value::new(0i32)
                        } else {
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

        {
            // Start with Denominator = 2, then recompute with Denominator = 0
            let mut db = Db::new();
            assert_eq!(db.version, START_VERSION);
            db.update_input(EarlyCutoff::Denominator, Value::new(2i32));
            assert_eq!(db.version, START_VERSION + 1);

            // 6 / 3
            assert_eq!(3i32, *db.get(EarlyCutoff::Result));

            db.update_input(EarlyCutoff::Denominator, Value::new(0i32));
            assert_eq!(db.version, START_VERSION + 2);

            // Although Division was previously a dependency of Result,
            // we shouldn't update Division due to the `DenominatorIs0` changing as well,
            // leading us into a different branch where `Division` is no longer required.
            // If we did recalculate `Division` we would get a divide by zero error.
            //
            // Shouldn't get a divide by zero here
            assert_eq!(0i32, *db.get(EarlyCutoff::Result));
        }
    }
}
