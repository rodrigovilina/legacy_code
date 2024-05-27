use crate::Client;

pub trait Draw {
  fn draw(&mut self);
}

impl Draw for Client {
  fn draw(&mut self) {
    match self {
      Client::Map(client) => client.draw(),
      Client::Null(client) => client.draw(),
      Client::None => {}
    }
  }
}
