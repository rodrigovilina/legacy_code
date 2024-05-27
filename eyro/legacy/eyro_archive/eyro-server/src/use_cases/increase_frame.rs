use crate::server::Server;

pub trait IncreaseFrame {
  fn increase_frame(&mut self);
}

impl IncreaseFrame for Server {
  fn increase_frame(&mut self) {
    self.frame += 1;
  }
}
