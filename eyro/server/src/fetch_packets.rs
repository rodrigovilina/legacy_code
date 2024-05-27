use {crate::server::Server, core::packet::Packet};

pub trait FetchPackets {
  fn fetch_packets(&mut self) -> Vec<Packet>;
}

impl FetchPackets for Server {
  fn fetch_packets(&mut self) -> Vec<Packet> {
    self
      .clients
      .clients
      .iter_mut()
      .flat_map(|c| Packet::read_multiple_from_stream(&mut c.stream))
      .collect()
  }
}
