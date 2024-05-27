use {super::acceleration::Acceleration, std::ops::Add};

#[derive(Debug, Clone, Copy)]
pub struct Velocity(isize, isize);

impl Default for Velocity {
  fn default() -> Self {
    Self(0, 0)
  }
}

impl Velocity {
  pub fn new(x: isize, y: isize) -> Self {
    Self(x, y)
  }

  #[allow(dead_code)]
  pub fn left() -> Self {
    Self(-1, 0)
  }

  #[allow(dead_code)]
  pub fn right() -> Self {
    Self(1, 0)
  }

  pub fn x(&self) -> isize {
    self.0
  }

  pub fn y(&self) -> isize {
    self.1
  }
}

impl Add<Acceleration> for Velocity {
  type Output = Velocity;

  fn add(self, rhs: Acceleration) -> Self::Output {
    Velocity(self.x() + rhs.x(), self.y() + rhs.y())
  }
}
