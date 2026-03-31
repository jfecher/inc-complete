use std::{collections::BTreeSet, marker::PhantomData};

use dashmap::DashMap;
use serde::{Deserialize, Serialize};

use crate::{Cell, Computation, Run, Storage, StorageFor};

/// An accumulator is a collection which can accumulate a given cell key associated with multiple
/// values of a given type. `Accumulator<MyItem>` is an example of such a type.
///
/// This is most often a hashmap or similar map
pub trait Accumulate<Item> {
    /// Push an item to the context of the given cell
    fn accumulate(&self, cell: Cell, item: Item);

    /// Retrieve all items associated with the given cell.
    /// Note that this should only include the exact cell given, not any
    /// values accumulated from dependencies.
    fn get_accumulated<Items>(&self, cell: Cell) -> Items
    where
        Items: FromIterator<Item>;
}

pub struct Accumulator<Item> {
    map: DashMap<Cell, Vec<Item>>,
}

impl<Item> Default for Accumulator<Item> {
    fn default() -> Self {
        Self {
            map: Default::default(),
        }
    }
}

impl<Item> Accumulator<Item> {
    pub fn clear(&self, cell: Cell) {
        self.map.remove(&cell);
    }
}

impl<Item: Clone> Accumulate<Item> for Accumulator<Item> {
    fn accumulate(&self, cell: Cell, item: Item) {
        self.map.entry(cell).or_default().push(item);
    }

    fn get_accumulated<Items>(&self, cell: Cell) -> Items
    where
        Items: FromIterator<Item>,
    {
        if let Some(items) = self.map.get(&cell) {
            FromIterator::from_iter(items.iter().cloned())
        } else {
            FromIterator::from_iter(std::iter::empty())
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[serde(transparent)]
pub struct Accumulated<Item> {
    cell: Cell,
    _item: std::marker::PhantomData<Item>,
}

impl<Item> Accumulated<Item> {
    pub(crate) fn new(cell: Cell) -> Self {
        Self {
            cell,
            _item: PhantomData,
        }
    }
}

// Arbitrary semi-random value meant to not be easily accidentally used for ids in user code.
// Must be unique across all computation IDs.
pub(crate) const ACCUMULATED_COMPUTATION_ID: u32 = 0x54325243;

impl<Item: 'static> Computation for Accumulated<Item> {
    type Output = BTreeSet<Item>;
    const IS_INPUT: bool = false;
    const ASSUME_CHANGED: bool = false;

    fn computation_id() -> u32 {
        ACCUMULATED_COMPUTATION_ID
    }
}

impl<S, Item> Run<S> for Accumulated<Item>
where
    Item: 'static + Ord,
    S: Storage + StorageFor<Accumulated<Item>> + Accumulate<Item>,
{
    fn run(&self, db: &crate::DbHandle<S>) -> Self::Output {
        db.get_accumulated_with_cell(self.cell)
    }
}

impl<Item: Serialize + Clone> Serialize for Accumulator<Item> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let vec: Vec<(Cell, Vec<Item>)> = self
            .map
            .iter()
            .map(|entry| (*entry.key(), entry.value().clone()))
            .collect();

        vec.serialize(serializer)
    }
}

impl<'de, Item: Deserialize<'de>> Deserialize<'de> for Accumulator<Item> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let vec: Vec<(Cell, Vec<Item>)> = Deserialize::deserialize(deserializer)?;
        let map = vec.into_iter().collect();
        Ok(Accumulator { map })
    }
}
