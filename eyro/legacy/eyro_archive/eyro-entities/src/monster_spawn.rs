use {
  crate::monster_race::MonsterRace,
  eyro_values::{
    map_id::MapId,
    monster_id::MonsterId,
    monster_spawn_id::MonsterSpawnId,
    position::{Position, Positionable},
    size::{Size, Sizeable},
  },
};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct MonsterSpawn {
  id: MonsterSpawnId,
  race: MonsterRace,
  position: Position,
  map_id: MapId,
  size: Size,
  monster_id: Option<MonsterId>,
}

impl MonsterSpawn {
  pub fn new(
    id: MonsterSpawnId,
    race: MonsterRace,
    position: Position,
    map_id: MapId,
    size: Size,
  ) -> Self {
    Self {
      id,
      race,
      position,
      map_id,
      monster_id: None,
      size,
    }
  }

  pub fn id(&self) -> MonsterSpawnId {
    self.id
  }

  pub fn race(&self) -> MonsterRace {
    self.race
  }

  pub fn map_id(&self) -> MapId {
    self.map_id
  }

  pub fn monster_id(&self) -> Option<MonsterId> {
    self.monster_id
  }

  pub fn remove_monster_id(&mut self) {
    self.monster_id = None;
  }

  pub fn set_monster_id(&mut self, monster_id: MonsterId) {
    self.monster_id = Some(monster_id);
  }
}

impl Positionable for MonsterSpawn {
  fn position(&self) -> Position {
    self.position
  }
}

impl Sizeable for MonsterSpawn {
  fn size(&self) -> Size {
    self.size
  }
}
