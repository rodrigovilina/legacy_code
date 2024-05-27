#![warn(clippy::complexity)]
#![warn(clippy::expect_used)]
#![warn(clippy::nursery)]
#![warn(clippy::panic)]
#![warn(clippy::pedantic)]
#![warn(clippy::perf)]
#![warn(clippy::unwrap_used)]

mod client_connection;
mod client_pool;
mod connect_client;
mod connect_clients;
mod fetch_packets;
mod send_packets;
mod tick;
mod server;

use {crate::tick::Tick, server::Server};

const IP: &str = "127.0.0.1:8765";

fn main() -> Result<(), String> {
  let mut server: Server = Server::new()?;

  loop {
    server.tick()?;
    println!("{server:#?}");
  }
}

#[derive(Debug, Default)]
struct IncomingPackets();

