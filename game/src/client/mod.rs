mod backend;
mod frontend;
mod main_loop;

use self::{backend::Backend, frontend::Frontend};

pub struct Client {
  back: Backend,
  front: Frontend,
}

impl Client {
  pub fn new() -> Self {
    Self {
      back: Backend::new(0),
      front: Frontend::new().unwrap(),
    }
  }
}
