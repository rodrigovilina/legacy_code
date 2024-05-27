use crate::id::Id;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct BulletId {
  pub id: u64,
}

pub trait HasBulletId {
  fn bullet_id(&self) -> BulletId;
}

impl BulletId {
  pub fn new(id: u64) -> Self {
    Self { id }
  }
}

impl From<u64> for BulletId {
  fn from(id: u64) -> Self {
    Self::new(id)
  }
}

impl From<BulletId> for u64 {
  fn from(val: BulletId) -> Self {
    val.id
  }
}

impl Id for BulletId {}
