use {crate::Client, eyro_packets::server_to_client_packets::ServerToClientPackets};

pub trait ReceivePackets {
  fn receive_packets(&mut self, packets: ServerToClientPackets);
}

impl ReceivePackets for Client {
  fn receive_packets(&mut self, packets: ServerToClientPackets) {
    match self {
      Client::Map(client) => client.receive_packets(packets),
      Client::Null(client) => client.receive_packets(packets),
      Client::None => {}
    }
  }
}
