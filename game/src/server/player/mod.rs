pub mod id;
pub mod repository;

use {self::id::PlayerId, super::map::id::MapId};

#[derive(Clone, Debug)]
pub struct Player {
  #[allow(dead_code)]
  id: PlayerId,
  map_id: MapId,
}

impl Player {
  pub fn new(id: PlayerId, map_id: MapId) -> Self {
    Self { id, map_id }
  }

  pub fn map_id(&self) -> MapId {
    self.map_id
  }

  #[allow(dead_code)]
  pub fn id(&self) -> PlayerId {
    self.id
  }
}
