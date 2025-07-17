use std::{collections::BTreeSet, sync::{atomic::{AtomicU32, Ordering}, Arc}};

use dashmap::{mapref::one::Ref, DashMap};
use serde::{ser::SerializeStruct, Deserialize, Serialize};

use crate::Cell;

pub(crate) struct InputSets {
    ids_to_sets: DashMap<InputSetId, Arc<BTreeSet<Cell>>>,
    sets_to_ids: DashMap<Arc<BTreeSet<Cell>>, InputSetId>,
    next_id: AtomicU32,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub(crate) struct InputSetId(u32);

impl InputSetId {
    /// All computations start with the empty set for input dependencies and are
    /// reset to the empty set when being recomputed so it is helpful to keep
    /// track of this idea separately.
    pub(crate) const EMPTY_INPUT_SET: InputSetId = InputSetId(0);

    pub(crate) fn is_empty(self) -> bool {
        self == Self::EMPTY_INPUT_SET
    }
}

impl InputSets {
    pub(crate) fn new() -> Self {
        let this = Self {
            ids_to_sets: DashMap::new(),
            sets_to_ids: DashMap::new(),
            next_id: AtomicU32::new(0),
        };
        let id = this.get_or_insert_set(BTreeSet::new());
        assert_eq!(id, InputSetId::EMPTY_INPUT_SET);
        this
    }

    fn get_or_insert_set(&self, set: BTreeSet<Cell>) -> InputSetId {
        if let Some(id) = self.sets_to_ids.get(&set) {
            return *id;
        }

        let set = Arc::new(set);
        let id = InputSetId(self.next_id.fetch_add(1, Ordering::Relaxed));
        self.ids_to_sets.insert(id, set.clone());
        self.sets_to_ids.insert(set, id);
        id
    }

    pub(crate) fn get_set(&self, id: InputSetId) -> Ref<InputSetId, Arc<BTreeSet<Cell>>> {
        self.ids_to_sets.get(&id).unwrap()
    }

    /// Union the two sets, returning the resulting set which may be the
    /// same as one of the inputs. Most of the time, this is expected to
    /// be one of the inputs.
    pub(crate) fn union(&self, original_id: InputSetId, new_id: InputSetId) -> InputSetId {
        if original_id == new_id {
            return original_id;
        }
        if original_id == InputSetId::EMPTY_INPUT_SET {
            return new_id;
        }
        if new_id == InputSetId::EMPTY_INPUT_SET {
            return original_id;
        }

        let mut merged_set = self.get_set(original_id).as_ref().clone();
        for cell in self.get_set(new_id).iter() {
            merged_set.insert(*cell);
        }

        self.get_or_insert_set(merged_set)
    }

    /// Inserts a new input dependency into the given existing set
    pub(crate) fn insert(&self, input_dependencies: InputSetId, dependency: Cell) -> InputSetId {
        let set_ref = self.get_set(input_dependencies);
        if set_ref.contains(&dependency) {
            return input_dependencies;
        }

        // TODO: Can we query whether `self U dependency` already exists before 
        // constructing and inserting into a new BTreeSet?
        let mut set = set_ref.as_ref().clone();
        drop(set_ref);
        set.insert(dependency);

        self.get_or_insert_set(set)
    }
}

impl Serialize for InputSets {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer
    {
        let ids_to_sets: Vec<(InputSetId, Arc<BTreeSet<Cell>>)> = self.ids_to_sets.iter().map(|kv| (*kv.key(), kv.value().clone())).collect();
        let next_id = self.next_id.load(Ordering::Relaxed);

        let mut s = serializer.serialize_struct("InputSets", 2)?;
        s.serialize_field("ids_to_sets", &ids_to_sets)?;
        s.serialize_field("next_id", &next_id)?;
        s.end()
    }
}

#[derive(Deserialize)]
#[serde(rename = "InputSets")]
struct InputSetsDeserialize {
    ids_to_sets: Vec<(InputSetId, Arc<BTreeSet<Cell>>)>,
    next_id: u32,
}

impl<'de> Deserialize<'de> for InputSets {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de> 
    {
        let sets = InputSetsDeserialize::deserialize(deserializer)?;

        let ids_to_sets = DashMap::new();
        let sets_to_ids = DashMap::new();

        for (id, set) in sets.ids_to_sets {
            ids_to_sets.insert(id, set.clone());
            sets_to_ids.insert(set, id);
        }

        let next_id = AtomicU32::new(sets.next_id);
        Ok(InputSets {
            ids_to_sets,
            sets_to_ids,
            next_id,
        })
    }
}
