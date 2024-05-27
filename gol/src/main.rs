use {client::Client, server::Server};

mod client;
mod game_loop;
mod server;

use game_loop::game_loop;

fn main() -> Result<(), String> {
  let client: Client = Client::new()?;
  let server: Server = Server::new();
  game_loop(client, server)?;
  Ok(())
}
