use {
  crate::{
    config::SERVER_DEBUG,
    use_cases::{
      create_bullets::CreateBullets, increase_frame::IncreaseFrame, move_players::MovePlayers,
      respond_to_map_requests::RespondToMapRequest, send_packets::SendPackets,
    },
  },
  eyro_client::{draw::Draw, packets::Packets, tick::Tick, Client},
  eyro_entities::{
    map::Map, monster_race::MonsterRace, monster_spawn::MonsterSpawn, player::Player,
  },
  eyro_packets::{
    clients_to_server_packets::ClientsToServerPackets,
    server_to_clients_packets::ServerToClientsPackets,
  },
  eyro_repositories::{
    bullet::BulletRepository, map::MapRepository, monster::MonsterRepository,
    monster_spawn::MonsterSpawnRepository, player::PlayerRepository, Repository,
  },
  eyro_values::{
    attack::Attack, hp_bar::HpBar, map_id::MapId, monster_id::MonsterId,
    monster_spawn_id::MonsterSpawnId, player_id::PlayerId, position::Position, size::Size,
  },
  std::mem,
};

#[derive(Default)]
pub struct Server {
  pub frame: u64,
  pub maps: MapRepository,
  pub bullets: BulletRepository,
  pub monsters: MonsterRepository,
  pub players: PlayerRepository,
  pub monster_spawns: MonsterSpawnRepository,
  pub s2c_packets: ServerToClientsPackets,
  pub c2s_packets: ClientsToServerPackets,
  pub sync_client: Client,
}

impl Server {
  pub fn tick(&mut self) {
    let mut packets: ClientsToServerPackets = self.fetch_client_packets();
    println!("{:?}", packets);
    self.debug();
    self.increase_frame();
    self.respond_to_map_requests(&mut packets);
    self.move_players(&packets);
    self.move_monsters(self.monsters_with_map_size());
    self.create_bullets(packets);
    self.move_bullets();
    self.hit_enemies();
    let monster_spawns: Vec<MonsterSpawn> = self.monster_spawns().iter().cloned().collect();
    self.respawn_enemies(monster_spawns);
    self.send_packets();
    self.draw();
    self.sleep();
  }

  fn fetch_client_packets(&mut self) -> ClientsToServerPackets {
    self.sync_client.tick().unwrap();
    self.c2s_packets.include(self.sync_client.packets());
    mem::take(&mut self.c2s_packets)
  }

  fn draw(&mut self) {
    self.sync_client.draw();
  }

  fn debug(&self) {
    if SERVER_DEBUG {
      // println!("{:#?}", self);
      println!();
    }
  }

  pub fn bullets(&self) -> &BulletRepository {
    &self.bullets
  }

  pub fn bullets_mut(&mut self) -> &mut BulletRepository {
    &mut self.bullets
  }

  pub fn maps(&self) -> &MapRepository {
    &self.maps
  }

  pub fn maps_mut(&mut self) -> &mut MapRepository {
    &mut self.maps
  }

  pub fn monsters(&self) -> &MonsterRepository {
    &self.monsters
  }

  pub fn monsters_mut(&mut self) -> &mut MonsterRepository {
    &mut self.monsters
  }

  pub fn monster_spawns(&self) -> &MonsterSpawnRepository {
    &self.monster_spawns
  }

  pub fn monster_spawns_mut(&mut self) -> &mut MonsterSpawnRepository {
    &mut self.monster_spawns
  }

  pub fn players(&self) -> &PlayerRepository {
    &self.players
  }

  pub fn players_mut(&mut self) -> &mut PlayerRepository {
    &mut self.players
  }

  pub fn monsters_with_map_size(&self) -> Vec<(MonsterId, Size)> {
    self.monsters().iter_with_map_size(self.maps())
  }
}
