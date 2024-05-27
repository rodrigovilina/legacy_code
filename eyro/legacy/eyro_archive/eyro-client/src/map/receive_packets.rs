use {
  super::MapClient, crate::receive_packets::ReceivePackets,
  eyro_packets::server_to_client_packets::ServerToClientPackets,
  eyro_repositories::player::PlayerRepository,
};

impl ReceivePackets for MapClient {
  fn receive_packets(&mut self, packets: ServerToClientPackets) {
    println!("{:?}", packets);
    match self {
      MapClient::Disconnected { .. } => {
        if let ServerToClientPackets {
          map_size_packet: Some(map_size),
          ..
        } = packets
        {
          let players: PlayerRepository = PlayerRepository::default();
          *self = MapClient::connected(map_size.id, map_size.size, players).unwrap()
        }
      }
      MapClient::Connected { .. } => {}
    }
  }
}
