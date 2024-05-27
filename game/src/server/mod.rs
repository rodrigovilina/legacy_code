use self::{
  map::{id::MapId, repository::MapRepository, Map},
  player::{id::PlayerId, repository::PlayerRepository, Player},
};

pub mod map;
pub mod player;

pub struct Server {
  maps: MapRepository,
  players: PlayerRepository,
}

impl Server {
  pub fn new() -> Self {
    let maps: MapRepository = MapRepository::new();
    let players: PlayerRepository = PlayerRepository::new(&maps);

    Self { maps, players }
  }

  pub fn get_player(&self, player_id: impl Into<PlayerId>) -> Player {
    self.players.get(player_id)
  }

  pub fn get_map(&self, map_id: impl Into<MapId>) -> Map {
    self.maps.get(map_id)
  }
}
