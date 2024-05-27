use crate::id::Id;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct MonsterId {
  pub id: u64,
}

impl MonsterId {
  pub fn new(id: u64) -> Self {
    Self { id }
  }
}

pub trait HasMonsterId {
  fn monster_id(&self) -> MonsterId;
}

impl From<u64> for MonsterId {
  fn from(id: u64) -> Self {
    Self::new(id)
  }
}

impl From<MonsterId> for u64 {
  fn from(val: MonsterId) -> Self {
    val.id
  }
}

impl Id for MonsterId {}
