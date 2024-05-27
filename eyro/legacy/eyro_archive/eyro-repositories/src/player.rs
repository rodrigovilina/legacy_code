use {
  crate::Repository,
  eyro_entities::player::Player,
  eyro_values::{map_id::MapId, player_id::PlayerId},
};

#[derive(Debug, Default)]
pub struct PlayerRepository {
  pub players: Vec<Player>,
}

impl Repository<Player, PlayerId> for PlayerRepository {
  fn iter(&self) -> std::slice::Iter<Player> {
    self.players.iter()
  }

  fn iter_mut(&mut self) -> std::slice::IterMut<Player> {
    self.players.iter_mut()
  }

  fn find(&self, id: PlayerId) -> Option<&Player> {
    self
      .players
      .iter()
      .find(|player: &&Player| player.id() == id)
  }

  fn destroy(&mut self, id: PlayerId) {
    self.players.retain(|player: &Player| player.id() != id)
  }
}

impl PlayerRepository {
  pub fn new(players: Vec<Player>) -> Self {
    Self { players }
  }

  pub fn where_map(&self, map_id: impl Into<MapId>) -> Vec<&Player> {
    let map_id: MapId = map_id.into();

    self
      .players
      .iter()
      .filter(|player| player.map_id() == map_id)
      .collect()
  }
}
