use {
  crate::{
    connect_clients::ConnectClients, fetch_packets::FetchPackets, send_packets::SendPackets, server::Server,
  },
  std::{thread::sleep, time::Duration},
};

pub trait Tick {
  fn tick(&mut self) -> Result<(), String>;
}

impl Tick for Server {
  fn tick(&mut self) -> Result<(), String> {
    self.connect_clients()?;
    let _packets = self.fetch_packets();
    self.send_packets();
    sleep(Duration::from_millis(1000));
    Ok(())
  }
}
