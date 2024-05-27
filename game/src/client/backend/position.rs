#[derive(Clone, Debug, Copy)]
pub struct Position(isize, isize);

impl Position {
  pub fn new(x: isize, y: isize) -> Self {
    Self(x, y)
  }

  pub fn x(&self) -> isize {
    self.0
  }

  pub fn y(&self) -> isize {
    self.1
  }
}
