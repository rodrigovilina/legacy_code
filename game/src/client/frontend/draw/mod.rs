mod screen;

use super::Frontend;

pub trait Draw {
  fn draw(&self, frontend: &mut Frontend);
}
