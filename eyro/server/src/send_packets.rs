use {crate::Server, core::packet::Packet, std::io::Write};

pub trait SendPackets {
  fn send_packets(&mut self);
}

impl SendPackets for Server {
  fn send_packets(&mut self) {
    self.clients.clients.iter_mut().for_each(|c| {
      let packet: Packet = Packet::HealthCheck;
      let _ = c.stream.write(&packet.as_bytes());
    });
  }
}
