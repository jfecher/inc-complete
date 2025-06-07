use std::{any::{Any, TypeId}, collections::HashMap, hash::{Hash, Hasher}, mem::transmute_copy};

use crate::{Cell, Db, DbHandle};



// Take a step back, what do we need:
// From input:
// - Input to Cell operation
//
// From Cell + TypeId:
// - Cell run
//   - Given only full type (A, B, C), etc
// - Cell to store result
//   - Given only full type (A, B, C), etc
// - Cell to get result
//   - May have concrete type

pub trait Computation: 'static + Sized + std::fmt::Debug {
    type InputToCell;
    type CellToOutput;
    type Output;

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output;

    fn input_to_cell(input: &Self, container: &Self::InputToCell) -> Option<Cell>;
    fn insert_new_cell(cell: Cell, function: Self, i2c: &mut Self::InputToCell, c2o: &mut Self::CellToOutput);

    fn get_function_and_output(cell: Cell, container: &Self::CellToOutput) -> (&Self, Option<&Self::Output>);
    fn set_output(cell: Cell, output: Self::Output, container: &mut Self::CellToOutput);

    // We could use TypeIds but those are not stable across rustc updates.
    // Default to 0 here and have each pair impl increment the rhs to decide IDs based
    // on their position in the computation tuple.
    #[inline(always)]
    fn computation_id_of<T: Computation>() -> u32 {
        0
    }

    fn get_c2o_container_mut<C: 'static>(computation_id: u32, container: &mut C) -> &mut Self::CellToOutput {
        assert_eq!(computation_id, Self::computation_id_of::<Self>(),
            "Type dispatch failed for get_c2o_container_mut container");

        assert_eq!(TypeId::of::<C>(), TypeId::of::<Self::CellToOutput>(),
            "Type dispatch failed for get_c2o_container_mut container type");

        // Safety: We confirmed above C == Self::InputToCell and thus `&mut C == &mut Self::InputToCell`
        unsafe {
            std::mem::transmute(container)
        }
    }

    /// Given a Cell, TypeId pair dispatch to the correct run function
    /// and return true if the value has changed. This should also cache
    /// the new value if it has changed.
    fn dispatch_run<T>(cell: Cell, computation_id: u32, db: &mut Db<impl Computation>) -> bool
        where T: Computation,
              Self::Output: Eq,
    {
        assert_eq!(computation_id, Self::computation_id_of::<Self>(),
            "Type dispatch failed for computation");

        let container = Self::get_c2o_container_mut(computation_id, db.storage_mut());
        let function = Self::get_function_and_output(cell, container).0;
        let result1 = function.run(&mut db.handle(cell));

        assert_eq!(TypeId::of::<T::Output>(), TypeId::of::<Self::Output>(),
            "Type dispatch failed for dispatch_run output type");

        // Safety: We just checked T::Output == Self::Output above, we should be copying
        // the same type.
        let result2: Self::Output = unsafe { std::mem::transmute_copy(&result1) };
        std::mem::forget(result1);

        let container = Self::get_c2o_container_mut(computation_id, db.storage_mut());

        let (_, previous_value) = Self::get_function_and_output(cell, container);
        let changed = previous_value.map_or(true, |previous| result2 != *previous);

        if changed {
            Self::set_output(cell, result2, container);
        }

        changed
    }

    fn dispatch_input_to_cell<T>(input: &T, container: &Self::InputToCell) -> Option<Cell>
        where T: 'static + Computation + Any 
    {
        assert_eq!(TypeId::of::<T>(), TypeId::of::<Self>());
        let input = (input as &dyn Any).downcast_ref().expect("T == Self");
        Self::input_to_cell(input, container)
    }

    fn dispatch_insert_new_cell<T>(cell: Cell, input: T, i2c: &mut Self::InputToCell, c2o: &mut Self::CellToOutput)
        where T: 'static + Computation + Any,
              T::InputToCell: 'static,
              T::CellToOutput: 'static,
    {
        let input = transmute_copy_checked::<T, Self>(input);
        Self::insert_new_cell(cell, input, i2c, c2o)
    }
}

fn transmute_copy_checked<A: 'static, B: 'static>(x: A) -> B {
    assert_eq!(TypeId::of::<A>(), TypeId::of::<B>());
    // Safety: We confirmed above A == B
    let x2: B = unsafe { std::mem::transmute_copy(&x) };
    // x2 is copied byte by byte, we need to ensure the destructor is not run twice.
    std::mem::forget(x);
    x2
}

fn single_hash(x: impl Hash) -> u64 {
    let mut hasher = std::hash::DefaultHasher::default();
    x.hash(&mut hasher);
    hasher.finish()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Double(i32);

impl Computation for Double {
    type Output = i32;
    type InputToCell = HashMap<Self, Cell>;
    type CellToOutput = HashMap<Cell, (Self, Option<Self::Output>)>;

    fn run(&self, _handle: &mut DbHandle<impl Computation>) -> Self::Output {
        self.0 * 2
    }

    fn input_to_cell(input: &Self, container: &Self::InputToCell) -> Option<Cell> {
        container.get(input).copied()
    }

    fn get_function_and_output(cell: Cell, container: &Self::CellToOutput) -> (&Self, Option<&Self::Output>) {
        let (this, output) = &container[&cell];
        (this, output.as_ref())
    }

    fn set_output(cell: Cell, new_output: Self::Output, container: &mut Self::CellToOutput) {
        container.entry(cell).and_modify(|(_, output)| {
            *output = Some(new_output);
        });
    }

    fn insert_new_cell(cell: Cell, function: Self, i2c: &mut Self::InputToCell, c2o: &mut Self::CellToOutput) {
        i2c.insert(function.clone(), cell);
        c2o.insert(cell, (function, None));
    }
}

/// Test a simple input which has no parameters and thus does not require storing it with a HashMap
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct SimpleInput;

impl Computation for SimpleInput {
    type Output = i32;
    type InputToCell = Option<Cell>; // only 1 input, so there can only be 1 Cell
    type CellToOutput = Option<Self::Output>; // likewise with the output

    fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
        100
    }

    fn input_to_cell(_: &Self, container: &Self::InputToCell) -> Option<Cell> {
        *container
    }

    fn get_function_and_output(_: Cell, container: &Self::CellToOutput) -> (&Self, Option<&Self::Output>) {
        (&SimpleInput, container.as_ref())
    }

    fn set_output(_: Cell, new_output: Self::Output, container: &mut Self::CellToOutput) {
        *container = Some(new_output);
    }

    fn insert_new_cell(cell: Cell, _: Self, i2c: &mut Self::InputToCell, c2o: &mut Self::CellToOutput) {
        *i2c = Some(cell);
        *c2o = None;
    }
}
