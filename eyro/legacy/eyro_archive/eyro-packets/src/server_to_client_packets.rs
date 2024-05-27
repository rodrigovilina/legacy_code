use {crate::map_size_packet::MapSizePacket, eyro_values::client_id::ClientId};

#[derive(Clone, Debug, Default)]
pub struct ServerToClientPackets {
  pub client_id: ClientId,
  pub map_size_packet: Option<MapSizePacket>,
}
