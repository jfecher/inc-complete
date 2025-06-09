
use std::any::{Any, TypeId};

use crate::{Cell, Computation, Db, DbHandle};

macro_rules! enumerate {
    {$cont:ident! $data:tt $n:tt} => { $cont! $data; };
    {$cont:ident! ($($data:tt)*) ($($n:tt)*) $x:tt $($xs:tt)*} => { enumerate!{$cont! ($($data)* (($($n)*+1) $x)) ($($n)*+1) $($xs)*} };
}

macro_rules! impl_computation_for_tuple {
    ($Last:ident, $($Rest:ident),+) => {
impl<$($Rest),+, $Last> Computation for ($($Rest),+, $Last) where
    $($Rest: Computation),+,
    $Last: Computation,
{
    type Storage = (
        $($Rest::Storage),+,
        $Last::Storage,
    );
    type Output = ();

    fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
        panic!("Type dispatch failed in `run`")
    }

    fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
        panic!("Type dispatch failed in `input_to_cell`")
    }

    fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
        panic!("Type dispatch failed in `insert_new_cell`")
    }

    fn get_function_and_output(_: Cell, _: &Self::Storage) -> (&Self, Option<&Self::Output>) {
        panic!("Type dispatch failed in `get_function_and_output`")
    }

    fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
        panic!("Type dispatch failed in `set_output`")
    }

    fn computation_id_of<T: Computation>() -> u32 {
        if TypeId::of::<T>() == TypeId::of::<A>() {
            0
        } else {
            1 + B::computation_id_of::<T>()
        }
    }

    fn get_storage_mut<Concrete: Computation + 'static>(computation_id: u32, container: &mut Self::Storage) -> &mut Concrete::Storage {
        $(
            if computation_id == 0 {
                A::get_storage_mut::<Concrete>(computation_id, &mut container.0)
            }
        )+

        if computation_id == 0 {
            A::get_storage_mut::<Concrete>(computation_id, &mut container.0)
        } else {
            B::get_storage_mut::<Concrete>(computation_id - 1, &mut container.1)
        }
    }

    fn dispatch_run<FullComputation>(cell: Cell, computation_id: u32, db: &mut Db<FullComputation>) -> bool
        where FullComputation: Computation,
              Self: Clone,
              Self::Output: Eq,
    {
        if computation_id == 0 {
            // TODO: Rework generics
            A::dispatch_run(cell, computation_id, db)
        } else {
            B::dispatch_run(cell, computation_id - 1, db)
        }
    }

    fn dispatch_update_output<Concrete, FullComputation>(cell: Cell, computation_id: u32, output: Concrete::Output, db: &mut Db<FullComputation>) -> bool
        where Concrete: Computation,
              FullComputation: Computation,
              Self::Output: Eq,
    {
        if computation_id == 0 {
            // TODO: Rework generics
            A::dispatch_update_output::<Concrete, FullComputation>(cell, computation_id, output, db)
        } else {
            B::dispatch_update_output::<Concrete, FullComputation>(cell, computation_id - 1, output, db)
        }
    }

    fn dispatch_input_to_cell<Concrete>(input: &Concrete, container: &Self::Storage) -> Option<Cell>
        where Concrete: 'static + Computation + Any 
    {
        if TypeId::of::<Concrete>() == TypeId::of::<A>() {
            A::dispatch_input_to_cell::<Concrete>(input, &container.0)
        } else {
            B::dispatch_input_to_cell::<Concrete>(input, &container.1)
        }
    }

    fn dispatch_insert_new_cell<Concrete>(cell: Cell, input: Concrete, storage: &mut Self::Storage)
        where Concrete: 'static + Computation + Any,
              Concrete::Storage: 'static,
    {
        if TypeId::of::<Concrete>() == TypeId::of::<A>() {
            A::dispatch_insert_new_cell::<Concrete>(cell, input, &mut storage.0)
        } else {
            B::dispatch_insert_new_cell::<Concrete>(cell, input, &mut storage.1)
        }
    }
}
    };
}

// impl_computation_for_tuple!(A, B);

impl<A, B> Computation for (A, B) where
    A: Computation,
    B: Computation,
{
    type Storage = (
        A::Storage,
        B::Storage,
    );
    type Output = ();

    fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
        panic!("Type dispatch failed in `run`")
    }

    fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
        panic!("Type dispatch failed in `input_to_cell`")
    }

    fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
        panic!("Type dispatch failed in `insert_new_cell`")
    }

    fn get_function_and_output(_: Cell, _: &Self::Storage) -> (&Self, Option<&Self::Output>) {
        panic!("Type dispatch failed in `get_function_and_output`")
    }

    fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
        panic!("Type dispatch failed in `set_output`")
    }

    fn computation_id_of<T: Computation>() -> u32 {
        if TypeId::of::<T>() == TypeId::of::<A>() {
            0
        } else {
            1 + B::computation_id_of::<T>()
        }
    }

    fn get_storage_mut<Concrete: Computation + 'static>(computation_id: u32, container: &mut Self::Storage) -> &mut Concrete::Storage {
        if computation_id == 0 {
            A::get_storage_mut::<Concrete>(computation_id, &mut container.0)
        } else {
            B::get_storage_mut::<Concrete>(computation_id - 1, &mut container.1)
        }
    }

    fn dispatch_run<FullComputation>(cell: Cell, computation_id: u32, db: &mut Db<FullComputation>) -> bool
        where FullComputation: Computation,
              Self: Clone,
              Self::Output: Eq,
    {
        if computation_id == 0 {
            // TODO: Rework generics
            A::dispatch_run(cell, computation_id, db)
        } else {
            B::dispatch_run(cell, computation_id - 1, db)
        }
    }

    fn dispatch_update_output<Concrete, FullComputation>(cell: Cell, computation_id: u32, output: Concrete::Output, db: &mut Db<FullComputation>) -> bool
        where Concrete: Computation,
              FullComputation: Computation,
              Self::Output: Eq,
    {
        if computation_id == 0 {
            // TODO: Rework generics
            A::dispatch_update_output::<Concrete, FullComputation>(cell, computation_id, output, db)
        } else {
            B::dispatch_update_output::<Concrete, FullComputation>(cell, computation_id - 1, output, db)
        }
    }

    fn dispatch_input_to_cell<Concrete>(input: &Concrete, container: &Self::Storage) -> Option<Cell>
        where Concrete: 'static + Computation + Any 
    {
        if TypeId::of::<Concrete>() == TypeId::of::<A>() {
            A::dispatch_input_to_cell::<Concrete>(input, &container.0)
        } else {
            B::dispatch_input_to_cell::<Concrete>(input, &container.1)
        }
    }

    fn dispatch_insert_new_cell<Concrete>(cell: Cell, input: Concrete, storage: &mut Self::Storage)
        where Concrete: 'static + Computation + Any,
              Concrete::Storage: 'static,
    {
        if TypeId::of::<Concrete>() == TypeId::of::<A>() {
            A::dispatch_insert_new_cell::<Concrete>(cell, input, &mut storage.0)
        } else {
            B::dispatch_insert_new_cell::<Concrete>(cell, input, &mut storage.1)
        }
    }
}

impl<A, B, C> Computation for (A, B, C) where
    A: Computation,
    B: Computation,
    C: Computation,
{
    type Storage = (
        A::Storage,
        B::Storage,
        C::Storage,
    );
    type Output = ();

    fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
        panic!("Type dispatch failed in `run`")
    }

    fn input_to_cell(_: &Self, _: &Self::Storage) -> Option<Cell> {
        panic!("Type dispatch failed in `input_to_cell`")
    }

    fn insert_new_cell(_: Cell, _: Self, _: &mut Self::Storage) {
        panic!("Type dispatch failed in `insert_new_cell`")
    }

    fn get_function_and_output(_: Cell, _: &Self::Storage) -> (&Self, Option<&Self::Output>) {
        panic!("Type dispatch failed in `get_function_and_output`")
    }

    fn set_output(_: Cell, _: Self::Output, _: &mut Self::Storage) {
        panic!("Type dispatch failed in `set_output`")
    }

    fn computation_id_of<T: Computation>() -> u32 {
        if TypeId::of::<T>() == TypeId::of::<A>() {
            0
        } else {
            1 + B::computation_id_of::<T>()
        }
    }

    fn get_storage_mut<Concrete: Computation + 'static>(computation_id: u32, container: &mut Self::Storage) -> &mut Concrete::Storage {
        if computation_id == 0 {
            A::get_storage_mut::<Concrete>(computation_id, &mut container.0)
        } else if computation_id == 1 {
            B::get_storage_mut::<Concrete>(computation_id - 1, &mut container.1)
        } else {
            C::get_storage_mut::<Concrete>(computation_id - 2, &mut container.2)
        }
    }

    fn dispatch_run<FullComputation>(cell: Cell, computation_id: u32, db: &mut Db<FullComputation>) -> bool
        where FullComputation: Computation,
              Self: Clone,
              Self::Output: Eq,
    {
        if computation_id == 0 {
            A::dispatch_run(cell, computation_id, db)
        } else if computation_id == 1 {
            B::dispatch_run(cell, computation_id - 1, db)
        } else {
            C::dispatch_run(cell, computation_id - 2, db)
        }
    }

    fn dispatch_update_output<Concrete, FullComputation>(cell: Cell, computation_id: u32, output: Concrete::Output, db: &mut Db<FullComputation>) -> bool
        where Concrete: Computation,
              FullComputation: Computation,
              Self::Output: Eq,
    {
        if computation_id == 0 {
            A::dispatch_update_output::<Concrete, FullComputation>(cell, computation_id, output, db)
        } else if computation_id == 1 {
            B::dispatch_update_output::<Concrete, FullComputation>(cell, computation_id - 1, output, db)
        } else {
            C::dispatch_update_output::<Concrete, FullComputation>(cell, computation_id - 2, output, db)
        }
    }

    fn dispatch_input_to_cell<Concrete>(input: &Concrete, container: &Self::Storage) -> Option<Cell>
        where Concrete: 'static + Computation + Any 
    {
        let concrete_id = TypeId::of::<Concrete>();
        if concrete_id == TypeId::of::<A>() {
            A::dispatch_input_to_cell::<Concrete>(input, &container.0)
        } else if concrete_id == TypeId::of::<B>() {
            B::dispatch_input_to_cell::<Concrete>(input, &container.1)
        } else {
            C::dispatch_input_to_cell::<Concrete>(input, &container.2)
        }
    }

    fn dispatch_insert_new_cell<Concrete>(cell: Cell, input: Concrete, storage: &mut Self::Storage)
        where Concrete: 'static + Computation + Any,
              Concrete::Storage: 'static,
    {
        let concrete_id = TypeId::of::<Concrete>();
        if concrete_id == TypeId::of::<A>() {
            A::dispatch_insert_new_cell::<Concrete>(cell, input, &mut storage.0)
        } else if concrete_id == TypeId::of::<A>() {
            B::dispatch_insert_new_cell::<Concrete>(cell, input, &mut storage.1)
        } else {
            C::dispatch_insert_new_cell::<Concrete>(cell, input, &mut storage.2)
        }
    }
}
