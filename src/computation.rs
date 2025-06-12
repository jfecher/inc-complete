use crate::{Cell, Db, DbHandle};
use std::any::{Any, TypeId};

mod tuple_impls;
mod input;
mod intermediate;
mod singleton;
mod hashmapped;
mod btreemapped;

#[macro_use]
mod macros;

pub use input::{ Input, OutputTypeForInput };
pub use intermediate::{ Intermediate, Run };
pub use singleton::SingletonStorage;
pub use hashmapped::HashMapStorage;
pub use btreemapped::BTreeMapStorage;

pub trait Computation: 'static + Sized + Clone {
    type Storage;
    type Output: Eq;

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output;

    fn input_to_cell(input: &Self, storage: &Self::Storage) -> Option<Cell>;
    fn insert_new_cell(cell: Cell, function: Self, storage: &mut Self::Storage);

    fn get_function_and_output(
        cell: Cell,
        storage: &Self::Storage,
    ) -> (&Self, Option<&Self::Output>);
    fn set_output(cell: Cell, output: Self::Output, storage: &mut Self::Storage);

    // We could use TypeIds but those are not stable across rustc updates.
    // Default to 0 here and have each pair impl increment the rhs to decide IDs based
    // on their position in the computation tuple.
    #[inline(always)]
    fn computation_id_of<T: Computation>() -> u32 {
        0
    }

    fn get_storage<Concrete: Computation + 'static>(
        computation_id: u32,
        container: &Self::Storage,
    ) -> &Concrete::Storage {
        assert_eq!(computation_id, 0, "Type dispatch failed for get_storage");

        assert_eq!(
            TypeId::of::<Concrete::Storage>(),
            TypeId::of::<Self::Storage>(),
            "Type dispatch failed for get_storage storage type:\n    {}\n != {}",
            std::any::type_name::<Concrete::Storage>(),
            std::any::type_name::<Self::Storage>(),
        );

        // Safety: We confirmed above Concrete::Storage == Self::Storage and thus `&mut Concrete::Storage == &mut Self::Storage`
        unsafe { std::mem::transmute(container) }
    }

    fn get_storage_mut<Concrete: Computation + 'static>(
        computation_id: u32,
        container: &mut Self::Storage,
    ) -> &mut Concrete::Storage {
        assert_eq!(
            computation_id, 0,
            "Type dispatch failed for get_storage_mut container"
        );

        assert_eq!(
            TypeId::of::<Concrete::Storage>(),
            TypeId::of::<Self::Storage>(),
            "Type dispatch failed for get_storage_mut container type"
        );

        // Safety: We confirmed above Concrete::Storage == Self::Storage and thus `&mut Concrete::Storage == &mut Self::Storage`
        unsafe { std::mem::transmute(container) }
    }

    /// True if this has any cached output
    fn output_is_unset<FullComputation: Computation>(
        cell: Cell,
        computation_id: u32,
        original_computation_id: u32,
        db: &Db<FullComputation>,
    ) -> bool {
        assert_eq!(
            computation_id, 0,
            "Type dispatch failed for output_is_unset"
        );

        let container = FullComputation::get_storage::<Self>(original_computation_id, db.storage());
        let unset = Self::get_function_and_output(cell, container).1.is_none();
        println!("{} output is unset: {unset}", std::any::type_name::<Self>());
        unset
    }

    /// Given a Cell, TypeId pair dispatch to the correct run function
    /// and return true if the value has changed. This should also cache
    /// the new value if it has changed.
    /// Note that in dispatch functions `Self` is always the concrete, non-tuple type.
    fn dispatch_run<FullComputation: Computation>(
        cell: Cell,
        computation_id: u32,
        original_computation_id: u32,
        db: &mut Db<FullComputation>,
    ) -> bool
    where
        Self: Clone,
        Self::Output: Eq,
    {
        assert_eq!(computation_id, 0, "Type dispatch failed for dispatch_run");

        let container =
            FullComputation::get_storage_mut::<Self>(original_computation_id, db.storage_mut());
        let function = Self::get_function_and_output(cell, container).0.clone();
        let output = function.run(&mut db.handle(cell));
        FullComputation::dispatch_update_output::<Self, FullComputation>(
            cell,
            original_computation_id,
            original_computation_id,
            output,
            db,
        )
    }

    /// Dispatch to the correct update_output function to cache the new output
    /// and return true if the value has changed.
    /// Note that in dispatch functions `Self` is the current type being dispatched,
    /// `Concrete`, if present, is the non-tuple type of the target computation,
    /// and `FullComputation` is the type of the `Db` computation parameter which is
    /// usually a tuple of every possible computation.
    fn dispatch_update_output<Concrete, FullComputation>(
        cell: Cell,
        computation_id: u32,
        original_computation_id: u32,
        output: Concrete::Output,
        db: &mut Db<FullComputation>,
    ) -> bool
    where
        Concrete: Computation,
        FullComputation: Computation,
        Self::Output: Eq,
    {
        assert_eq!(
            computation_id, 0,
            "Type dispatch failed for dispatch_update_output"
        );
        assert_eq!(
            TypeId::of::<Concrete::Output>(),
            TypeId::of::<Self::Output>(),
            "Type dispatch failed for dispatch_update_output"
        );

        // Safety: We just checked T::Output == Self::Output above, we should be copying
        // the same type.
        let output2: Self::Output = unsafe { std::mem::transmute_copy(&output) };
        std::mem::forget(output);

        let container =
            FullComputation::get_storage_mut::<Self>(original_computation_id, db.storage_mut());

        let (_, previous_value) = Self::get_function_and_output(cell, container);
        let changed = previous_value.map_or(true, |previous| output2 != *previous);

        if changed {
            Self::set_output(cell, output2, container);
        }

        changed
    }

    fn dispatch_input_to_cell<Concrete>(input: &Concrete, container: &Self::Storage) -> Option<Cell>
    where
        Concrete: 'static + Computation + Any,
    {
        assert_eq!(TypeId::of::<Concrete>(), TypeId::of::<Self>());
        let input = (input as &dyn Any).downcast_ref().expect("T == Self");
        Self::input_to_cell(input, container)
    }

    fn dispatch_insert_new_cell<Concrete>(cell: Cell, input: Concrete, storage: &mut Self::Storage)
    where
        Concrete: 'static + Computation + Any,
        Concrete::Storage: 'static,
    {
        let input = transmute_copy_checked::<Concrete, Self>(input);
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
