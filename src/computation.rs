use std::{any::{Any, TypeId}, collections::HashMap};

use crate::Cell;



// Take a step back, what do we need:
// From input:
// - Input to Cell operation
//
// From Cell:
// - Cell run
//   - Given only full type (A, B, C), etc
// - Cell to store result
//   - Given only full type (A, B, C), etc
// - Cell to get result
//   - May have concrete type

pub trait Computation: 'static + Sized {
    type InputToCell;
    type SelfType;
    type Output;

    fn input_to_cell(input: &Self::SelfType, container: &Self::InputToCell) -> Cell;
    fn run(&self) -> Self::Output;

    fn run_dispatch<T>(input: T) -> T::Output
        where T: 'static + Computation + Any
    {
        assert_eq!(TypeId::of::<T>(), TypeId::of::<Self>());
        input.run()
    }

    fn input_to_cell_dispatch<T>(input: T, container: &Self::InputToCell) -> Cell
        where T: 'static + Computation + Any 
    {
        assert_eq!(TypeId::of::<T>(), TypeId::of::<Self>());
        let input = (&input as &dyn Any).downcast_ref().unwrap();
        Self::input_to_cell(input, container)
    }
}

#[derive(Default, PartialEq, Eq, Hash)]
struct Double(i32);

impl Computation for Double {
    type InputToCell = HashMap<Self, Cell>;
    type Output = i32;
    type SelfType = Self;

    fn run(&self) -> Self::Output {
        self.0 * 2
    }

    fn input_to_cell(this: &Self::SelfType, container: &Self::InputToCell) -> Cell {
        container[this]
    }
}

impl<A, B> Computation for (A, B) where
    A: Computation + 'static,
    B: Computation,
{
    type InputToCell = (
        A::InputToCell,
        B::InputToCell,
    );

    type SelfType = Result<A, B>;
    type Output = Result<A::Output, B::Output>;

    fn input_to_cell_dispatch<T>(input: T, container: &Self::InputToCell) -> Cell
        where T: 'static + Computation + Any
    {
        let t_type_id = TypeId::of::<T>();

        if t_type_id == TypeId::of::<A>() {
            A::input_to_cell_dispatch(input, &container.0)
        } else {
            B::input_to_cell_dispatch(input, &container.1)
        }
    }

    fn run_dispatch<T>(input: T) -> T::Output
        where T: 'static + Computation + Any
    {
        let t_type_id = TypeId::of::<T>();

        if t_type_id == TypeId::of::<A>() {
            Ok(A::run_dispatch(input))
        } else {
            Err(B::run_dispatch(input))
        }
    }

    fn input_to_cell(input: &Self::SelfType, container: &Self::InputToCell) -> Cell {
        todo!()
    }

    fn run(&self) -> Self::Output {
        todo!()
    }
}
