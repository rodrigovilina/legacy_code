use eyro_packets::client_to_server_packets::ClientToServerPackets;

use crate::Client;

pub trait Packets {
  fn packets(&self) -> ClientToServerPackets;
}

impl Packets for Client {
  fn packets(&self) -> ClientToServerPackets {
    match self {
      Client::Map(client) => client.packets(),
      Client::Null(client) => client.packets(),
      Client::None => ClientToServerPackets::default(),
    }
  }
}
