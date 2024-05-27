use {
  crate::server::Server,
  eyro_repositories::Repository,
  eyro_values::{monster_id::MonsterId, position::PositionableMut, size::Size},
};

impl Server {
  pub fn move_monsters(&mut self, monsters_with_size: Vec<(MonsterId, Size)>) {
    for monster in self.monsters_mut().iter_mut() {
      let map_size = monsters_with_size
        .iter()
        .find(|(monster_id, _)| *monster_id == monster.id())
        .unwrap()
        .1;
      monster.update_posistion();
      monster.update_movement_state(map_size);
    }
  }
}
