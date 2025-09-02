use dashmap::DashMap;
use serde::{Deserialize, Serialize};

use crate::Cell;

/// An accumulator is a collection which can accumulate a given cell key associated with multiple
/// values of a given type. `Accumulator<MyItem>` is an example of such a type.
///
/// This is most often a hashmap or similar map
pub trait Accumulate<Item> {
    /// Push an item to the context of the given cell
    fn accumulate(&self, cell: Cell, item: Item);

    /// Retrieve all items associated with the given cells
    fn get_accumulated<Items>(&self, cells: &[Cell]) -> Items
        where Items: FromIterator<Item>;
}

pub struct Accumulator<Item> {
    map: DashMap<Cell, Vec<Item>>,
}

impl<T> Default for Accumulator<T> {
    fn default() -> Self {
        Self { map: Default::default() }
    }
}

impl<Item: Clone> Accumulate<Item> for Accumulator<Item> {
    fn accumulate(&self, cell: Cell, item: Item) {
        self.map.accumulate(cell, item);
    }

    fn get_accumulated<Items>(&self, cells: &[Cell]) -> Items where
        Items: FromIterator<Item>
    {
        self.map.get_accumulated(cells)
    }
}

impl<Item: Clone> Accumulate<Item> for DashMap<Cell, Vec<Item>> {
    fn accumulate(&self, cell: Cell, item: Item) {
        self.entry(cell).or_default().push(item);
    }

    fn get_accumulated<Items>(&self, cells: &[Cell]) -> Items
        where Items: FromIterator<Item>
    {
        let iter = cells.iter().filter_map(|cell| {
            self.get(cell).map(|items| items.clone())
        }).flatten();

        FromIterator::from_iter(iter)
    }
}

impl<Item: Serialize + Clone> Serialize for Accumulator<Item> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
        S: serde::Serializer
    {
        let vec: Vec<(Cell, Vec<Item>)> = self.map.iter().map(|entry| {
            (*entry.key(), entry.value().clone())
        }).collect();

        vec.serialize(serializer)
    }
}

impl<'de, Item: Deserialize<'de>> Deserialize<'de> for Accumulator<Item> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where
        D: serde::Deserializer<'de>
    {
        let vec: Vec<(Cell, Vec<Item>)> = Deserialize::deserialize(deserializer)?;
        let map = vec.into_iter().collect();
        Ok(Accumulator { map })
    }
}
