use std::any::{Any, TypeId};

use crate::{Cell, Computation, Db, DbHandle};

macro_rules! impl_computation_for_tuple {
    ($($Middle:ident),+ ; $Last:ident) => {
        impl<$($Middle,)+ $Last> Computation for ($($Middle,)+ $Last) where
            $($Middle: Computation,)+
            $Last: Computation
        {
            type Storage = (
                $($Middle::Storage,)+
                $Last::Storage
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

            fn computation_id_of<Concrete: Computation>() -> u32 {
                let mut i = 0;

                $(
                    if TypeId::of::<Concrete>() == TypeId::of::<$Middle>() {
                        return i;
                    }
                    i += 1;
                )+

                i + $Last::computation_id_of::<Concrete>()
            }

            fn get_storage<Concrete: Computation + 'static>(computation_id: u32, storage: &Self::Storage) -> &Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let ($($Middle,)+ $Last,) = storage;

                $(
                    if computation_id == i {
                        return $Middle::get_storage::<Concrete>(computation_id - i, $Middle)
                    }
                    i += 1;
                )+

                $Last::get_storage::<Concrete>(computation_id - i, $Last)
            }

            fn get_storage_mut<Concrete: Computation + 'static>(computation_id: u32, storage: &mut Self::Storage) -> &mut Concrete::Storage {
                let mut i = 0;
                #[allow(non_snake_case)]
                let ($($Middle,)+ $Last,) = storage;

                $(
                    if computation_id == i {
                        return $Middle::get_storage_mut::<Concrete>(computation_id - i, $Middle)
                    }
                    i += 1;
                )+

                $Last::get_storage_mut::<Concrete>(computation_id - i, $Last)
            }

            fn output_is_unset<FullComputation: Computation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &Db<FullComputation>,
            ) -> bool {
                let mut i = 0;

                $(
                    if computation_id == i {
                        return $Middle::output_is_unset(cell, computation_id - i, original_computation_id, db);
                    }
                    i += 1;
                )+

                $Last::output_is_unset(cell, computation_id - i, original_computation_id, db)
            }

            fn dispatch_run<FullComputation>(
                cell: Cell,
                computation_id: u32,
                original_computation_id: u32,
                db: &mut Db<FullComputation>,
            ) -> bool where
                FullComputation: Computation,
                Self: Clone,
                Self::Output: Eq,
            {
                let mut i = 0;

                $(
                    if computation_id == i {
                        return $Middle::dispatch_run(cell, computation_id - i, original_computation_id, db);
                    }
                    i += 1;
                )+

                $Last::dispatch_run(cell, computation_id - i, original_computation_id, db)
            }

            fn dispatch_update_output<Concrete, FullComputation>(cell: Cell, computation_id: u32, original_computation_id: u32, output: Concrete::Output, db: &mut Db<FullComputation>) -> bool
                where Concrete: Computation,
                    FullComputation: Computation,
                    Self::Output: Eq,
            {
                let mut i = 0;

                $(
                    if computation_id == i {
                        return $Middle::dispatch_update_output::<Concrete, FullComputation>(cell, computation_id - i, original_computation_id, output, db);
                    }
                    i += 1;
                )+

                $Last::dispatch_update_output::<Concrete, FullComputation>(cell, computation_id - i, original_computation_id, output, db)
            }

            fn dispatch_input_to_cell<Concrete>(input: &Concrete, storage: &Self::Storage) -> Option<Cell>
                where Concrete: 'static + Computation + Any
            {
                #[allow(non_snake_case)]
                let ($($Middle,)+ $Last) = storage;

                $(
                    if TypeId::of::<Concrete>() == TypeId::of::<$Middle>() {
                        return $Middle::dispatch_input_to_cell::<Concrete>(input, $Middle);
                    }
                )+

                $Last::dispatch_input_to_cell::<Concrete>(input, $Last)
            }

            fn dispatch_insert_new_cell<Concrete>(cell: Cell, input: Concrete, storage: &mut Self::Storage)
                where Concrete: 'static + Computation + Any,
                    Concrete::Storage: 'static,
            {
                #[allow(non_snake_case)]
                let ($($Middle,)+ $Last) = storage;

                $(
                    if TypeId::of::<Concrete>() == TypeId::of::<$Middle>() {
                        return $Middle::dispatch_insert_new_cell::<Concrete>(cell, input, $Middle);
                    }
                )+

                $Last::dispatch_insert_new_cell::<Concrete>(cell, input, $Last)
            }
        }
    };
}

// Any computation with more than 26 function types to cache will have to use nested tuples
// to specify the rest, e.g. `(fn1, fn2, .., fn25, (fn26, fn27, fn28, ..))`.
impl_computation_for_tuple!(A; Z);
impl_computation_for_tuple!(A, B; Z);
impl_computation_for_tuple!(A, B, C; Z);
impl_computation_for_tuple!(A, B, C, D; Z);
impl_computation_for_tuple!(A, B, C, D, E; Z);
impl_computation_for_tuple!(A, B, C, D, E, F; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X; Z);
impl_computation_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y; Z);
