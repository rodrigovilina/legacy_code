use {
  eyro_client::Client,
  eyro_entities::{
    map::Map, monster_race::MonsterRace, monster_spawn::MonsterSpawn, player::Player,
  },
  eyro_packets::{
    clients_to_server_packets::ClientsToServerPackets,
    server_to_clients_packets::ServerToClientsPackets,
  },
  eyro_repositories::{
    bullet::BulletRepository, map::MapRepository, monster::MonsterRepository,
    monster_spawn::MonsterSpawnRepository, player::PlayerRepository,
  },
  eyro_values::{
    attack::Attack, hp_bar::HpBar, map_id::MapId, monster_spawn_id::MonsterSpawnId,
    player_id::PlayerId, position::Position, size::Size,
  },
};

use crate::server::Server;

pub trait Init {
  fn init() -> Self;
}

impl Init for Server {
  fn init() -> Self {
    Self {
      frame: 0,
      maps: MapRepository::new(vec![
        Map::new(MapId::new(0), Size::new(800, 1200)),
        Map::new(MapId::new(1), Size::new(800, 800)),
      ]),
      bullets: BulletRepository::new(),
      monsters: MonsterRepository::new(),
      players: PlayerRepository::new(vec![
        Player::new(
          PlayerId::new(0),
          MapId::new(0),
          Size::new(50, 50),
          Position::new(0, 0),
          Attack::new(1),
          HpBar::maxed_at(5),
        ),
        Player::new(
          PlayerId::new(1),
          MapId::new(0),
          Size::new(90, 90),
          Position::new(100, 100),
          Attack::new(1),
          HpBar::maxed_at(5),
        ),
        Player::new(
          PlayerId::new(2),
          MapId::new(1),
          Size::new(23, 73),
          Position::new(200, 200),
          Attack::new(1),
          HpBar::maxed_at(5),
        ),
      ]),
      monster_spawns: MonsterSpawnRepository::new(vec![
        MonsterSpawn::new(
          MonsterSpawnId::new(0),
          MonsterRace::PORING,
          Position::new(100, 100),
          MapId::new(0),
          Size::new(500, 500),
        ),
        MonsterSpawn::new(
          MonsterSpawnId::new(1),
          MonsterRace::PORING,
          Position::new(100, 100),
          MapId::new(0),
          Size::new(500, 500),
        ),
      ]),
      s2c_packets: ServerToClientsPackets::default(),
      c2s_packets: ClientsToServerPackets::default(),
      sync_client: Client::map(MapId::new(0)).unwrap(),
    }
  }
}
