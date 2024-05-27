#[derive(Clone, Debug, Copy)]
pub struct Size(pub usize, pub usize);

impl Size {
  pub fn x(&self) -> usize {
    self.0
  }

  pub fn y(&self) -> usize {
    self.1
  }
}
