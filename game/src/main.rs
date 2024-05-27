mod client;
mod server;

use {crate::client::Client, once_cell::sync::Lazy, server::Server, std::sync::Mutex};

static SERVER: Lazy<Mutex<Server>> = Lazy::new(|| Mutex::new(Server::new()));

fn main() -> Result<(), String> {
  let mut client: Client = Client::new();
  client.main_loop();

  Ok(())
}
