use {
  crate::Repository,
  eyro_entities::monster_spawn::MonsterSpawn,
  eyro_values::{monster_id::MonsterId, monster_spawn_id::MonsterSpawnId},
  std::slice::{Iter, IterMut},
};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct MonsterSpawnRepository {
  monster_spawns: Vec<MonsterSpawn>,
  next_id: u64,
}

impl Repository<MonsterSpawn, MonsterSpawnId> for MonsterSpawnRepository {
  fn iter(&self) -> Iter<MonsterSpawn> {
    self.monster_spawns.iter()
  }

  fn iter_mut(&mut self) -> IterMut<MonsterSpawn> {
    self.monster_spawns.iter_mut()
  }

  fn find(&self, id: MonsterSpawnId) -> Option<&MonsterSpawn> {
    self
      .iter()
      .find(|monster_spawn: &&MonsterSpawn| monster_spawn.id() == id)
  }

  fn destroy(&mut self, id: MonsterSpawnId) {
    self
      .monster_spawns
      .retain(|monster_spawn: &MonsterSpawn| monster_spawn.id() != id);
  }
}

impl MonsterSpawnRepository {
  pub fn new(monster_spawns: Vec<MonsterSpawn>) -> Self {
    Self {
      monster_spawns,
      next_id: 0,
    }
  }

  pub fn update_monster_for_spawn(&mut self, id: MonsterSpawnId, monster_id: MonsterId) {
    if let Some(spawn) = self
      .iter_mut()
      .find(|monster_spawn| monster_spawn.id() == id)
    {
      spawn.set_monster_id(monster_id)
    };
  }

  pub fn kill(&mut self, monster_id: MonsterId) {
    if let Some(spawn) = self
      .iter_mut()
      .find(|monster_spawn| monster_spawn.monster_id() == Some(monster_id))
    {
      spawn.remove_monster_id()
    };
  }
}
