use crate::client_connection::Client;

#[derive(Debug, Default)]
pub struct ClientPool {
  pub clients: Vec<Client>,
}

impl ClientPool {
  pub fn push(&mut self, client: Client) {
    self.clients.push(client);
  }
}
