use std::{default::Default, ops::Add};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Acceleration {
  Pixel { x: i64, y: i64 },
  Cents { x: i64, y: i64 },
}

impl Acceleration {
  pub fn new(x: i64, y: i64) -> Self {
    Self::Pixel { x, y }
  }

  pub fn cents(x: i64, y: i64) -> Self {
    Self::Cents { x, y }
  }

  pub fn in_pixels(&self) -> Self {
    match self {
      Self::Pixel { x: _, y: _ } => *self,
      Self::Cents { x, y } => Self::Pixel {
        x: x / 100,
        y: y / 100,
      },
    }
  }

  pub fn in_cents(&self) -> Self {
    match self {
      Self::Pixel { x, y } => Self::Cents {
        x: x * 100,
        y: y * 100,
      },
      Self::Cents { x: _, y: _ } => *self,
    }
  }
}
pub trait Acceleratable {
  fn acceleration(&self) -> Acceleration;

  fn acceleration_x_in_pixels(&self) -> i64 {
    match self.acceleration().in_pixels() {
      Acceleration::Pixel { x, y: _ } => x,
      Acceleration::Cents { x: _, y: _ } => unreachable!(),
    }
  }

  fn acceleration_y_in_pixels(&self) -> i64 {
    match self.acceleration().in_pixels() {
      Acceleration::Pixel { x: _, y } => y,
      Acceleration::Cents { x: _, y: _ } => unreachable!(),
    }
  }

  fn acceleration_x_in_cents(&self) -> i64 {
    match self.acceleration().in_cents() {
      Acceleration::Pixel { x: _, y: _ } => unreachable!(),
      Acceleration::Cents { x, y: _ } => x,
    }
  }

  fn acceleration_y_in_cents(&self) -> i64 {
    match self.acceleration().in_cents() {
      Acceleration::Pixel { x: _, y: _ } => unreachable!(),
      Acceleration::Cents { x: _, y } => y,
    }
  }
}

pub trait AcceleratableMut {
  fn update_acceleration(&mut self);
}

impl Default for Acceleration {
  fn default() -> Self {
    Self::Pixel {
      x: Default::default(),
      y: Default::default(),
    }
  }
}

impl Add<Acceleration> for Acceleration {
  type Output = Acceleration;

  fn add(self, rhs: Acceleration) -> Self::Output {
    match (self, rhs) {
      (Acceleration::Pixel { x: lx, y: ly }, Acceleration::Pixel { x: rx, y: ry }) => {
        Acceleration::new(lx + rx, ly + ry)
      }
      (Acceleration::Pixel { x, y }, Acceleration::Cents { x: rx, y: ry }) => {
        Acceleration::cents(x * 100 + rx, y * 100 + ry)
      }

      (Acceleration::Cents { x, y }, Acceleration::Pixel { x: rx, y: ry }) => {
        Acceleration::cents(x + rx * 100, y + ry * 100)
      }

      (Acceleration::Cents { x: lx, y: ly }, Acceleration::Cents { x: rx, y: ry }) => {
        Acceleration::cents(lx + rx, ly + ry)
      }
    }
  }
}
