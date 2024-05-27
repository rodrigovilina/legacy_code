#[derive(Clone, Copy, Debug)]
pub struct PlayerId(usize);

impl PlayerId {
  pub fn new(item: usize) -> Self {
    Self(item)
  }

  pub fn get(&self) -> usize {
    self.0
  }
}

impl From<usize> for PlayerId {
  fn from(item: usize) -> Self {
    PlayerId::new(item)
  }
}
