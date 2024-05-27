use {
  crate::acceleration::Acceleration,
  std::{default::Default, ops::Add},
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Velocity {
  Pixel { x: i64, y: i64 },
  Cents { x: i64, y: i64 },
}

pub trait HasVelocity {
  fn velocity(&self) -> Velocity;
}

pub trait HasVelocityMut {
  fn update_velocity(&mut self);
}

impl Velocity {
  pub fn new(x: i64, y: i64) -> Self {
    Self::Pixel { x, y }
  }
}

impl Default for Velocity {
  fn default() -> Self {
    Self::Pixel {
      x: Default::default(),
      y: Default::default(),
    }
  }
}

impl Velocity {
  pub fn clamp(self, min: i64, max: i64) -> Self {
    match self {
      Self::Pixel { x, y } => {
        let x: i64 = x.max(min).min(max);
        let y: i64 = y.max(min).min(max);

        Self::Pixel { x, y }
      }
      Self::Cents { x, y } => {
        let x: i64 = x.max(min).min(max);
        let y: i64 = y.max(min).min(max);

        Self::Cents { x, y }
      }
    }
  }

  pub fn normalize(self) -> Self {
    match self {
      Self::Pixel { x, y } => {
        let x = x * 100;
        let y = y * 100;
        let magnitude: i64 = ((x * x + y * y) as f64).sqrt() as i64;
        let x: i64 = x * 100 / magnitude;
        let y: i64 = y * 100 / magnitude;

        Self::Cents { x, y }
      }
      Self::Cents { x, y } => {
        let magnitude: i64 = ((x * x + y * y) as f64).sqrt() as i64;
        let x: i64 = x / magnitude;
        let y: i64 = y / magnitude;

        Self::Cents { x, y }
      }
    }
  }
}

impl Add<Acceleration> for Velocity {
  type Output = Velocity;

  fn add(self, rhs: Acceleration) -> Self::Output {
    match (self, rhs) {
      (Velocity::Pixel { x, y }, Acceleration::Pixel { x: ax, y: ay }) => {
        Velocity::new(x + ax, y + ay)
      }
      (Velocity::Cents { x, y }, Acceleration::Pixel { x: ax, y: ay }) => Velocity::Cents {
        x: x + ax * 100,
        y: y + ay * 100,
      },
      (Velocity::Pixel { x, y }, Acceleration::Cents { x: ax, y: ay }) => Velocity::Cents {
        x: x * 100 + ax,
        y: y * 100 + ay,
      },
      (Velocity::Cents { x, y }, Acceleration::Cents { x: ax, y: ay }) => Velocity::Cents {
        x: x + ax,
        y: y + ay,
      },
    }
  }
}
