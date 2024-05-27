use eyro_server::{init::Init, server::Server};

fn main() {
  let mut server: Server = Server::init();

  loop {
    server.tick();
  }
}
