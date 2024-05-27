use crate::client::backend::velocity::Velocity;

use super::Player;

pub trait UpdateVelocity {
  fn update_velocity(self) -> Self;
}

impl UpdateVelocity for Player {
  fn update_velocity(self) -> Self {
    let vel: Velocity = self.vel + self.acc;
    let vel: Velocity = Velocity::new(vel.x().clamp(-30, 30), vel.y().clamp(-30, 30));
    Self { vel, ..self }
  }
}
