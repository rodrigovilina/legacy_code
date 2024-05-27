use {crate::{client_pool::ClientPool, IncomingPackets, IP}, std::net::TcpListener};

#[derive(Debug)]
pub struct Server {
  pub listener: TcpListener,
  pub clients: ClientPool,
  pub _incoming_packets: IncomingPackets,
  pub next_client_id: u64,
}

impl Server {
  pub fn new() -> Result<Self, String> {
    let listener: TcpListener = TcpListener::bind(IP).map_err(|e| e.to_string())?;
    let clients: ClientPool = ClientPool::default();
    let incoming_packets: IncomingPackets = IncomingPackets::default();

    Ok(Self {
      listener,
      clients,
      _incoming_packets: incoming_packets,
      next_client_id: 0,
    })
  }
}
