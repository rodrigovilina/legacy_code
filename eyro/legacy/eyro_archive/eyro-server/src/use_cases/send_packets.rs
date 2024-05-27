use {
  crate::server::Server,
  eyro_client::receive_packets::ReceivePackets,
  eyro_packets::{
    server_to_client_packets::ServerToClientPackets,
    server_to_clients_packets::ServerToClientsPackets,
  },
  eyro_values::client_id::ClientId,
  std::mem,
};

pub trait SendPackets {
  fn send_packets(&mut self);
}

impl SendPackets for Server {
  fn send_packets(&mut self) {
    let mut clients_packets: ServerToClientsPackets = mem::take(&mut self.s2c_packets);
    let packets: ServerToClientPackets = clients_packets.for_client(ClientId::new(0));
    self.sync_client.receive_packets(packets);
  }
}
