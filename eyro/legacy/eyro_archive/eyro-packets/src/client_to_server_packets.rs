use {
  crate::{
    player_bullet_packet::PlayerBulletPacket, player_movement_packet::PlayerMovementPacket,
    request_map_size_packet::RequestMapSizePacket,
  },
  eyro_values::{client_id::ClientId, player_id::PlayerId},
};

#[derive(Clone, Debug, Default)]
pub struct ClientToServerPackets {
  pub client_id: ClientId,
  pub player_id: PlayerId,
  pub player_movement: Option<PlayerMovementPacket>,
  pub player_bullet: Option<PlayerBulletPacket>,
  pub request_map_size: Option<RequestMapSizePacket>,
}
