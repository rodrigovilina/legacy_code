use {
  crate::{map_size_packet::MapSizePacket, server_to_client_packets::ServerToClientPackets},
  eyro_values::client_id::ClientId,
  std::collections::HashMap,
};

#[derive(Clone, Debug, Default)]
pub struct ServerToClientsPackets {
  map_sizes: HashMap<ClientId, MapSizePacket>,
}

impl ServerToClientsPackets {
  pub fn include(&mut self, packets: ServerToClientPackets) {
    if let Some(map_size) = packets.map_size_packet {
      self.map_sizes.insert(map_size.client_id, map_size);
    }
  }

  pub fn for_client(&mut self, client_id: ClientId) -> ServerToClientPackets {
    ServerToClientPackets {
      map_size_packet: self.map_sizes.get(&client_id).cloned(),
      ..ServerToClientPackets::default()
    }
  }
}
