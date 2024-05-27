use {
  crate::server::Server,
  eyro_entities::monster_spawn::MonsterSpawn,
  eyro_values::{
    hp_bar::HpBar,
    monster_id::MonsterId,
    position::{Position, Positionable},
    size::Sizeable,
  },
  rand::{prelude::ThreadRng, Rng},
};

impl Server {
  pub fn respawn_enemies(&mut self, monster_spawns: Vec<MonsterSpawn>) {
    for monster_spawn in monster_spawns {
      match monster_spawn.monster_id() {
        Some(_) => continue,
        None => {
          let mut rng: ThreadRng = rand::thread_rng();

          let position: Position = Position::new(
            rng.gen_range(0..monster_spawn.size().width()) as i64 + monster_spawn.position().x(),
            rng.gen_range(0..monster_spawn.size().height()) as i64 + monster_spawn.position().y(),
          );
          let monster_id: MonsterId = self.monsters_mut().push(
            monster_spawn.map_id(),
            monster_spawn.race(),
            position,
            HpBar::from_max(monster_spawn.race().max_hp()),
          );
          self
            .monster_spawns_mut()
            .update_monster_for_spawn(monster_spawn.id(), monster_id);
        }
      }
    }
  }
}
