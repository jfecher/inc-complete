use std::any::Any;
use std::hash::{Hash, Hasher};

use crate::db_handle::DbHandle;

pub trait Run: Sized {
    fn run(self, db: &mut DbHandle<Self>) -> Value;
}

pub struct Value(Box<dyn ValueTrait>);

impl Value {
    pub fn new(obj: impl ValueTrait) -> Value {
        Value(Box::new(obj))
    }

    pub fn downcast_obj_ref<T: 'static>(&self) -> Option<&T> {
        let obj = self.0.as_ref() as &dyn Any;
        obj.downcast_ref()
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(Value)")
    }
}

impl HashEqObj for Value {
    fn hash_value(&self, hasher: &mut std::hash::DefaultHasher) {
        self.0.hash_value(hasher);
    }

    fn eq_value(&self, other: &dyn ValueTrait) -> bool {
        self.0.eq_value(other)
    }
}

impl Eq for Value {}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ref().eq_value(other.0.as_ref())
    }
}

impl<T: ValueTrait> From<T> for Value {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

pub trait ValueTrait: Any + HashEqObj {}

pub trait HashEqObj {
    /// Object-safe hash
    fn hash_value(&self, hasher: &mut std::hash::DefaultHasher);

    /// Object-safe Eq
    fn eq_value(&self, other: &dyn ValueTrait) -> bool;

    fn get_hash(&self) -> u64 {
        let mut hasher = std::hash::DefaultHasher::default();
        self.hash_value(&mut hasher);
        hasher.finish()
    }
}

impl<T> ValueTrait for T where T: Any + Hash + Eq {}

impl<T> HashEqObj for T where T: Any + Hash + Eq {
    fn hash_value(&self, hasher: &mut std::hash::DefaultHasher) {
        self.hash(hasher);
    }

    fn eq_value(&self, other: &dyn ValueTrait) -> bool {
        let Some(other) = (other as &dyn Any).downcast_ref() else {
            return false;
        };
        self == other
    }
}

#[cfg(test)]
mod tests {
    use super::Value;

    #[test]
    fn dyn_eq() {
        let i32_0 = Value::new(0i32);
        let i32_1 = Value::new(1i32);
        let u32_0 = Value::new(0u32);

        assert_eq!(i32_0, i32_0, "i32_0 == i32_0");
        assert_ne!(i32_0, i32_1, "i32_0 == i32_1");
        assert_ne!(i32_0, u32_0, "i32_0 == u32_0");
        assert_eq!(i32_0, Value::new(0i32), "i32_0 == fresh i32_0");
    }
}
