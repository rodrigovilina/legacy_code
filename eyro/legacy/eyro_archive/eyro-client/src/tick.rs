use crate::Client;

pub trait Tick {
  fn tick(&mut self) -> Option<()>;
}

impl Tick for Client {
  fn tick(&mut self) -> Option<()> {
    match self {
      Client::Map(client) => client.tick(),
      Client::Null(client) => client.tick(),
      Client::None => Some(()),
    }
  }
}
