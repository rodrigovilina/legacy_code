use {
  super::{id::PlayerId, Player},
  crate::server::map::{id::MapId, repository::MapRepository},
};

pub struct PlayerRepository {
  pub players: Vec<Player>,
}

impl PlayerRepository {
  // pub fn where_map(&self, map_id: &MapId) -> Vec<&Player> {
  //   self
  //     .players
  //     .iter()
  //     .filter(|player: &&Player| player.map_id == *map_id)
  //     .collect()
  // }

  pub fn new(_maps: &MapRepository) -> PlayerRepository {
    PlayerRepository {
      players: vec![Player::new(PlayerId::new(0), MapId(0))],
    }
  }

  pub fn get(&self, player_id: impl Into<PlayerId>) -> Player {
    self.players[player_id.into().get()].clone()
  }
}
