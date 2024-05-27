use {
  super::MapClient,
  crate::packets::Packets,
  eyro_packets::{
    client_to_server_packets::ClientToServerPackets, request_map_size_packet::RequestMapSizePacket,
  },
};

impl Packets for MapClient {
  fn packets(&self) -> ClientToServerPackets {
    let packets = match self {
      MapClient::Disconnected { map_id } => ClientToServerPackets {
        request_map_size: Some(RequestMapSizePacket { map_id: *map_id }),
        ..ClientToServerPackets::default()
      },
      MapClient::Connected { .. } => ClientToServerPackets::default(),
    };

    println!("{:?}", packets);
    packets
  }
}
