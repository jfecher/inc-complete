use crate::Db;

/// Format `Self` into a debug string similar to the `Debug` trait but
/// with a `Db` to provide additional context. This is helpful to format
/// id-based types into a more human readable format by grabbing additional
/// data stored elsewhere.
pub trait DebugWithDb<Storage>: Sized {
    fn debug_with_db<'a>(&'a self, db: &'a Db<Storage>) -> DebugWithDbWrapper<'a, Self, Storage> {
        DebugWithDbWrapper { obj: self, db }
    }

    fn fmt_with_db(&self, _db: &Db<Storage>, f: &mut std::fmt::Formatter) -> std::fmt::Result;
}

pub fn debug_with_db<'a, T, Storage>(obj: &'a T, db: &'a Db<Storage>) -> DebugWithDbWrapper<'a, T, Storage> {
    DebugWithDbWrapper { obj, db }
}

/// Wrapper type returned by [DebugWithDb::debug_with_db] used for formatting
pub struct DebugWithDbWrapper<'a, T, S> {
    obj: &'a T,
    db: &'a Db<S>,
}

impl<T, S> std::fmt::Debug for DebugWithDbWrapper<'_, T, S>
    where
        T: DebugWithDb<S>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.obj.fmt_with_db(self.db, f)
    }
}

impl<T: std::fmt::Debug, S> DebugWithDb<S> for T {
    fn fmt_with_db(&self, _db: &Db<S>, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
