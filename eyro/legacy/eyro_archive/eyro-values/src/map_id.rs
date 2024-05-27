use crate::id::Id;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct MapId {
  pub id: u64,
}

pub trait HasMapId {
  fn map_id(&self) -> MapId;
}

impl MapId {
  pub fn new(id: u64) -> Self {
    Self { id }
  }
}

impl From<u64> for MapId {
  fn from(id: u64) -> Self {
    Self::new(id)
  }
}

impl From<MapId> for u64 {
  fn from(val: MapId) -> Self {
    val.id
  }
}

impl Id for MapId {}
