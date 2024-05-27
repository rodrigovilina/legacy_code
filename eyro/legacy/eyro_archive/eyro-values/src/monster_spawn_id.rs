use crate::id::Id;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct MonsterSpawnId {
  pub id: u64,
}

impl MonsterSpawnId {
  pub fn new(id: u64) -> Self {
    Self { id }
  }
}

pub trait HasMonsterSpawnId {
  fn monster_spawn_id(&self) -> MonsterSpawnId;
}

impl From<u64> for MonsterSpawnId {
  fn from(id: u64) -> Self {
    Self::new(id)
  }
}

impl From<MonsterSpawnId> for u64 {
  fn from(monster_spawn_id: MonsterSpawnId) -> Self {
    monster_spawn_id.id
  }
}

impl Id for MonsterSpawnId {}
