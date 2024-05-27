use std::ops::Add;

use crate::velocity::Velocity;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Position {
  Pixel { x: i64, y: i64 },
  Cents { x: i64, y: i64 },
}
pub trait Positionable {
  fn position(&self) -> Position;

  fn left(&self) -> i64 {
    self.position().x()
  }

  fn top(&self) -> i64 {
    self.position().y()
  }
}

pub trait PositionableMut {
  fn update_posistion(&mut self);
}

impl Position {
  pub fn new(x: i64, y: i64) -> Self {
    Self::Pixel { x, y }
  }

  pub fn x(&self) -> i64 {
    match self {
      Self::Pixel { x, .. } => *x,
      Self::Cents { x, .. } => *x / 100,
    }
  }

  pub fn y(&self) -> i64 {
    match self {
      Self::Pixel { y, .. } => *y,
      Self::Cents { y, .. } => *y / 100,
    }
  }
}

impl Default for Position {
  fn default() -> Self {
    Self::Pixel {
      x: Default::default(),
      y: Default::default(),
    }
  }
}

impl Add<Velocity> for Position {
  type Output = Self;

  fn add(self, rhs: Velocity) -> Self::Output {
    match (self, rhs) {
      (Self::Pixel { x, y }, Velocity::Pixel { x: vel_x, y: vel_y }) => Self::Pixel {
        x: x + vel_x,
        y: y + vel_y,
      },
      (Self::Pixel { x, y }, Velocity::Cents { x: rx, y: ry }) => Self::Cents {
        x: x * 100 + rx,
        y: y * 100 + ry,
      },
      (Self::Cents { x, y }, Velocity::Pixel { x: rx, y: ry }) => Self::Cents {
        x: x + rx * 100,
        y: y + ry * 100,
      },
      (Self::Cents { x, y }, Velocity::Cents { x: rx, y: ry }) => Self::Cents {
        x: x + rx,
        y: y + ry,
      },
    }
  }
}
