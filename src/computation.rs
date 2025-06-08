use std::{any::{Any, TypeId}, collections::HashMap, hash::Hash};
use crate::{Cell, Db, DbHandle};

pub trait Computation: 'static + Sized + Clone {
    type Storage;
    type Output: Eq;

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output;

    fn input_to_cell(input: &Self, storage: &Self::Storage) -> Option<Cell>;
    fn insert_new_cell(cell: Cell, function: Self, storage: &mut Self::Storage);

    fn get_function_and_output(cell: Cell, storage: &Self::Storage) -> (&Self, Option<&Self::Output>);
    fn set_output(cell: Cell, output: Self::Output, storage: &mut Self::Storage);

    // We could use TypeIds but those are not stable across rustc updates.
    // Default to 0 here and have each pair impl increment the rhs to decide IDs based
    // on their position in the computation tuple.
    #[inline(always)]
    fn computation_id_of<T: Computation>() -> u32 {
        0
    }

    fn get_storage_mut<C: 'static>(computation_id: u32, container: &mut C) -> &mut Self::Storage {
        assert_eq!(computation_id, Self::computation_id_of::<Self>(),
            "Type dispatch failed for get_c2o_container_mut container");

        assert_eq!(TypeId::of::<C>(), TypeId::of::<Self::Storage>(),
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
              Self: Clone,
              Self::Output: Eq,
    {
        assert_eq!(computation_id, Self::computation_id_of::<Self>(),
            "Type dispatch failed for computation");

        let container = Self::get_storage_mut(computation_id, db.storage_mut());
        let function = Self::get_function_and_output(cell, container).0.clone();
        let output = function.run(&mut db.handle(cell));
        Self::dispatch_update_output::<Self>(cell, computation_id, output, db)
    }

    /// Dispatch to the correct update_output function to cache the new output
    /// and return true if the value has changed.
    fn dispatch_update_output<T>(cell: Cell, computation_id: u32, output: T::Output, db: &mut Db<impl Computation>) -> bool
        where T: Computation,
              Self::Output: Eq,
    {
        assert_eq!(TypeId::of::<T::Output>(), TypeId::of::<Self::Output>(),
            "Type dispatch failed for dispatch_run output type");

        // Safety: We just checked T::Output == Self::Output above, we should be copying
        // the same type.
        let output2: Self::Output = unsafe { std::mem::transmute_copy(&output) };
        std::mem::forget(output);

        let container = Self::get_storage_mut(computation_id, db.storage_mut());

        let (_, previous_value) = Self::get_function_and_output(cell, container);
        let changed = previous_value.map_or(true, |previous| output2 != *previous);

        if changed {
            Self::set_output(cell, output2, container);
        }

        changed
    }

    fn dispatch_input_to_cell<T>(input: &T, container: &Self::Storage) -> Option<Cell>
        where T: 'static + Computation + Any 
    {
        assert_eq!(TypeId::of::<T>(), TypeId::of::<Self>());
        let input = (input as &dyn Any).downcast_ref().expect("T == Self");
        Self::input_to_cell(input, container)
    }

    fn dispatch_insert_new_cell<T>(cell: Cell, input: T, storage: &mut Self::Storage)
        where T: 'static + Computation + Any,
              T::Storage: 'static,
    {
        let input = transmute_copy_checked::<T, Self>(input);
        Self::insert_new_cell(cell, input, storage)
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

/// A helper type for defining Computations with HashMap-backed storage
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cached<T>(T);

impl<T> Cached<T> {
    pub const fn new(x: T) -> Self {
        Self(x)
    }
}

pub trait Run {
    type Output: Eq;

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output;
}

impl<T> Computation for Cached<T> where
    T: 'static + Run + Eq + Hash + Clone
{
    type Output = <T as Run>::Output;
    type Storage = (
        HashMap<Self, Cell>,
        HashMap<Cell, (Self, Option<Self::Output>)>,
    );

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
        self.0.run(handle)
    }

    fn input_to_cell(input: &Self, (self_to_cell, _): &Self::Storage) -> Option<Cell> {
        self_to_cell.get(input).copied()
    }

    fn get_function_and_output(cell: Cell, (_, cell_to_output): &Self::Storage) -> (&Self, Option<&Self::Output>) {
        let (this, output) = &cell_to_output[&cell];
        (this, output.as_ref())
    }

    fn set_output(cell: Cell, new_output: Self::Output, (_, cell_to_output): &mut Self::Storage) {
        cell_to_output.entry(cell).and_modify(|(_, output)| {
            *output = Some(new_output);
        });
    }

    fn insert_new_cell(cell: Cell, function: Self, storage: &mut Self::Storage) {
        storage.0.insert(function.clone(), cell);
        storage.1.insert(cell, (function, None));
    }
}

/// Helper to define a Computation for a simple input type which has no fields and thus
/// does not require a HashMap to cache each possible value. To use in a `Computation`,
/// `T` must implement `OutputTypeForInput` to specify its output type. `T` cannot provide
/// a `run` function since it cannot have any dependencies as an input. Its value must be
/// manually set via `Db::update_input(db, input, value)`
/// 
/// Examples include `struct SourceFile;` or `struct Time;`
#[derive(Debug, Clone)]
pub struct Input<T>(std::marker::PhantomData<T>);

impl<T> Input<T> {
    pub const fn new() -> Self {
        Self(std::marker::PhantomData)
    }
}

pub trait OutputTypeForInput: Clone {
    type Output: Eq;
}

impl<T: OutputTypeForInput + 'static> Computation for Input<T> {
    type Output = <T as OutputTypeForInput>::Output;
    type Storage = (Option<Cell>, Option<Self::Output>);

    fn run(&self, _: &mut DbHandle<impl Computation>) -> Self::Output {
        panic!("Input `(name)` queried before `db.update_input(..)` called")
    }

    fn input_to_cell(_: &Self, storage: &Self::Storage) -> Option<Cell> {
        storage.0
    }

    fn get_function_and_output(_: Cell, storage: &Self::Storage) -> (&Self, Option<&Self::Output>) {
        (&Self(std::marker::PhantomData), storage.1.as_ref())
    }

    fn set_output(_: Cell, new_output: Self::Output, storage: &mut Self::Storage) {
        storage.1 = Some(new_output);
    }

    fn insert_new_cell(cell: Cell, _: Self, storage: &mut Self::Storage) {
        storage.0 = Some(cell);
        storage.1 = None;
    }
}

impl<A, B> Computation for (A, B) where
    A: Computation,
    B: Computation,
{
    type Storage = (A::Storage, B::Storage);
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

    fn get_storage_mut<C: 'static>(computation_id: u32, container: &mut C) -> &mut Self::Storage {
        if computation_id == 0 {
            A::get_storage_mut(computation_id, container)
        } else {
            B::get_storage_mut(computation_id - 1, container)
        }
    }

    fn dispatch_run<T>(cell: Cell, computation_id: u32, db: &mut Db<impl Computation>) -> bool
        where T: Computation,
              Self: Clone,
              Self::Output: Eq,
    {
        if computation_id == 0 {
            // TODO: Rework generics
            A::dispatch_run::<Self>(cell, computation_id, db)
        } else {
            B::dispatch_run::<Self>(cell, computation_id - 1, db)
        }
    }

    fn dispatch_update_output<T>(cell: Cell, computation_id: u32, output: T::Output, db: &mut Db<impl Computation>) -> bool
        where T: Computation,
              Self::Output: Eq,
    {
        if computation_id == 0 {
            // TODO: Rework generics
            A::dispatch_update_output::<Self>(cell, computation_id, output, db)
        } else {
            B::dispatch_update_output::<Self>(cell, computation_id - 1, output, db)
        }
    }

    fn dispatch_input_to_cell<T>(input: &T, container: &Self::Storage) -> Option<Cell>
        where T: 'static + Computation + Any 
    {
        if TypeId::of::<T>() == TypeId::of::<A>() {
            T::dispatch_input_to_cell::<A>(input, container)
        } else {
            T::dispatch_input_to_cell::<B>(input, container)
        }
    }

    fn dispatch_insert_new_cell<T>(cell: Cell, input: T, storage: &mut Self::Storage)
        where T: 'static + Computation + Any,
              T::Storage: 'static,
    {
        if TypeId::of::<T>() == TypeId::of::<A>() {
            T::dispatch_insert_new_cell::<A>(cell, input, storage)
        } else {
            T::dispatch_insert_new_cell::<B>(cell, input, storage)
        }
    }
}
