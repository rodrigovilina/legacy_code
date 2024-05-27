use {
  crate::{
    client_to_server_packets::ClientToServerPackets, player_bullet_packet::PlayerBulletPacket,
    player_movement_packet::PlayerMovementPacket, request_map_size_packet::RequestMapSizePacket,
  },
  eyro_values::{client_id::ClientId, player_id::PlayerId},
  std::collections::HashMap,
};

#[derive(Clone, Debug, Default)]
pub struct ClientsToServerPackets {
  player_movements: HashMap<PlayerId, PlayerMovementPacket>,
  player_bullets: HashMap<PlayerId, PlayerBulletPacket>,
  map_size_requests: HashMap<ClientId, RequestMapSizePacket>,
}

impl ClientsToServerPackets {
  pub fn player_bullets(&self) -> &HashMap<PlayerId, PlayerBulletPacket> {
    &self.player_bullets
  }

  pub fn player_movements(&self) -> &HashMap<PlayerId, PlayerMovementPacket> {
    &self.player_movements
  }

  pub fn map_size_requests(&self) -> &HashMap<ClientId, RequestMapSizePacket> {
    &self.map_size_requests
  }

  pub fn get_player_movement(&self, player_id: PlayerId) -> Option<&PlayerMovementPacket> {
    self.player_movements.get(&player_id)
  }

  pub fn include(&mut self, packets: ClientToServerPackets) {
    if let Some(player_movement) = packets.player_movement {
      self
        .player_movements
        .insert(packets.player_id, player_movement);
    }

    if let Some(player_bullet) = packets.player_bullet {
      self.player_bullets.insert(packets.player_id, player_bullet);
    }

    if let Some(request_map_size) = packets.request_map_size {
      self
        .map_size_requests
        .insert(packets.client_id, request_map_size);
    }
  }
}
