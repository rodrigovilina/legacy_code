use {
  crate::{map::MapRepository, Repository},
  eyro_entities::{monster::Monster, monster_race::MonsterRace},
  eyro_values::{
    hp_bar::HpBar,
    map_id::MapId,
    monster_id::MonsterId,
    monster_movement::MonsterMovement,
    position::Position,
    size::{Size, Sizeable},
  },
};

#[derive(Debug, Default)]
pub struct MonsterRepository {
  monsters: Vec<Monster>,
  next_id: u64,
}

impl Repository<Monster, MonsterId> for MonsterRepository {
  fn iter(&self) -> std::slice::Iter<Monster> {
    self.monsters.iter()
  }

  fn iter_mut(&mut self) -> std::slice::IterMut<Monster> {
    self.monsters.iter_mut()
  }

  fn find(&self, id: MonsterId) -> Option<&Monster> {
    self.iter().find(|monster: &&Monster| monster.id() == id)
  }

  fn destroy(&mut self, id: MonsterId) {
    self.monsters.retain(|monster: &Monster| monster.id() != id);
  }
}

impl MonsterRepository {
  pub fn new() -> Self {
    Self {
      monsters: vec![],
      next_id: 0,
    }
  }

  pub fn push(
    &mut self,
    map_id: MapId,
    race: MonsterRace,
    position: Position,
    hp_bar: HpBar,
  ) -> MonsterId {
    let id = MonsterId::new(self.next_id);
    self.monsters.push(Monster::new(
      id,
      map_id,
      race,
      position,
      hp_bar,
      MonsterMovement::Idle { frames: 10 },
    ));
    self.next_id += 1;
    id
  }

  pub fn iter_with_map_size(&self, maps: &MapRepository) -> Vec<(MonsterId, Size)> {
    self
      .iter()
      .map(|monster: &Monster| {
        (monster.id(), {
          let map = maps.find(monster.map_id()).unwrap();
          map.size()
        })
      })
      .collect()
  }

  pub fn find_mut(&mut self, id: MonsterId) -> Option<&mut Monster> {
    self.monsters.iter_mut().find(|monster| monster.id() == id)
  }

  pub fn kill(&mut self, monster_id: MonsterId) {
    self.monsters.retain(|monster| monster.id() != monster_id);
  }

  pub fn where_map(&self, map_id: MapId) -> Vec<&Monster> {
    self
      .monsters
      .iter()
      .filter(|monster: &&Monster| monster.map_id() == map_id)
      .collect()
  }
}
