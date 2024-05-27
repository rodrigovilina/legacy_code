use {
  crate::server::Server,
  eyro_entities::map::Map,
  eyro_packets::{
    clients_to_server_packets::ClientsToServerPackets, map_size_packet::MapSizePacket,
    server_to_client_packets::ServerToClientPackets,
  },
  eyro_repositories::Repository,
  eyro_values::{map_id::MapId, size::Sizeable},
};

pub trait RespondToMapRequest {
  fn respond_to_map_requests(&mut self, packets: &mut ClientsToServerPackets);
}

impl RespondToMapRequest for Server {
  fn respond_to_map_requests(&mut self, packets: &mut ClientsToServerPackets) {
    // for every map size request, include into the s2c_packets a map size packet with the size
    // of the map requested
    for (client_id, packet) in packets.map_size_requests() {
      let map_id: MapId = packet.map_id;
      let map: &Map = self.maps_mut().find(map_id).unwrap();
      let map_size_packet: MapSizePacket = MapSizePacket {
        id: map_id,
        client_id: *client_id,
        size: map.size(),
      };
      let client_packets = ServerToClientPackets {
        map_size_packet: Some(map_size_packet),
        client_id: *client_id,
      };
      self.s2c_packets.include(client_packets);
    }
  }
}
