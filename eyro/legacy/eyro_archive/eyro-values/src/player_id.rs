use crate::id::Id;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct PlayerId {
  pub id: u64,
}

impl PlayerId {
  pub fn new(id: u64) -> Self {
    Self { id }
  }
}

pub trait HasPlayerId {
  fn player_id(&self) -> PlayerId;
}

impl From<u64> for PlayerId {
  fn from(id: u64) -> Self {
    Self::new(id)
  }
}

impl From<PlayerId> for u64 {
  fn from(val: PlayerId) -> Self {
    val.id
  }
}

impl Id for PlayerId {}
