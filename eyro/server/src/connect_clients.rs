use crate::{connect_client::ConnectClient, server::Server};

pub trait ConnectClients {
  fn connect_clients(&mut self) -> Result<(), String>;
}

impl ConnectClients for Server {
  fn connect_clients(&mut self) -> Result<(), String> {
    let mut cont: bool = true;
    while cont {
      cont = self.connect_client()?;
    }
    Ok(())
  }
}
