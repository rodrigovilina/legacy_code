use {
  crate::{client_connection::Client, server::Server},
  core::packet::Packet,
  std::io::{ErrorKind, Write},
};

pub trait ConnectClient {
  fn connect_client(&mut self) -> Result<bool, String>;
}

impl ConnectClient for Server {
  fn connect_client(&mut self) -> Result<bool, String> {
    self
      .listener
      .set_nonblocking(true)
      .map_err(|_| "Cannot set non-blocking".to_string())?;

    match self.listener.accept() {
      Ok((mut stream, ip)) => {
        let client_id: u64 = self.next_client_id;
        let ack_packet: Packet = Packet::AcknowledgeConnection { client_id };
        stream
          .write(&ack_packet.as_bytes())
          .map_err(|e| e.to_string())?;
        let client = Client { stream, ip };
        self.clients.push(client);
        self.next_client_id += 1;
        Ok(true)
      }
      Err(ref e) if e.kind() == ErrorKind::WouldBlock => Ok(false),
      Err(e) => Err(e.to_string()),
    }
  }
}
