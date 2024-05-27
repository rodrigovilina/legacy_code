use {
  super::velocity::Velocity,
  std::ops::{Add, Sub},
};

#[derive(Debug, Clone, Copy)]
pub struct Acceleration(isize, isize);

impl Default for Acceleration {
  fn default() -> Self {
    Self(0, 0)
  }
}

impl Acceleration {
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

impl Sub<Velocity> for Acceleration {
  type Output = Acceleration;

  fn sub(self, rhs: Velocity) -> Self::Output {
    Acceleration(self.x() - rhs.x(), self.y() - rhs.y())
  }
}

impl Sub<Acceleration> for Acceleration {
  type Output = Acceleration;

  fn sub(self, rhs: Acceleration) -> Self::Output {
    Acceleration(self.x() - rhs.x(), self.y() - rhs.y())
  }
}

impl Add<Acceleration> for Acceleration {
  type Output = Acceleration;

  fn add(self, rhs: Acceleration) -> Self::Output {
    Acceleration(self.x() + rhs.x(), self.y() + rhs.y())
  }
}
