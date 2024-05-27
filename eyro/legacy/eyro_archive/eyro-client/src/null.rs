use {
  crate::{packets::Packets, receive_packets::ReceivePackets, tick::Tick},
  eyro_packets::{
    client_to_server_packets::ClientToServerPackets,
    server_to_client_packets::ServerToClientPackets,
  },
};

#[derive(Debug, Default)]
pub struct NullClient();

impl NullClient {
  pub fn new() -> Self {
    Self()
  }

  pub fn draw(&mut self) {}
}

impl Packets for NullClient {
  fn packets(&self) -> ClientToServerPackets {
    ClientToServerPackets::default()
  }
}

impl Tick for NullClient {
  fn tick(&mut self) -> Option<()> {
    Some(())
  }
}

impl ReceivePackets for NullClient {
  fn receive_packets(&mut self, _packets: ServerToClientPackets) {}
}
