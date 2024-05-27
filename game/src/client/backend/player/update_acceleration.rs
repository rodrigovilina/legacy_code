use {
  super::{Player, GRAVITY},
  crate::client::backend::{acceleration::Acceleration, game_input::GameInput},
};

pub trait UpdateAcceleration {
  fn update_acceleration(self, game_input: &GameInput) -> Self;
}

impl UpdateAcceleration for Player {
  fn update_acceleration(self, game_input: &GameInput) -> Self {
    let gravity: Acceleration = *GRAVITY;
    let acc: Acceleration = Acceleration::default() + gravity;
    let acc: Acceleration = match game_input {
      GameInput {
        left: true,
        right: false,
        ..
      } => Acceleration::new(acc.x() - 1, acc.y()),
      GameInput {
        left: false,
        right: true,
        ..
      } => Acceleration::new(acc.x() + 1, acc.y()),
      _ => acc,
    };
    let acc: Acceleration = if !self.is_airbound() {
      match game_input {
        GameInput { up: true, .. } => Acceleration::new(acc.x(), acc.y() - 10),
        _ => acc,
      }
    } else {
      acc
    };
    let acc: Acceleration = Acceleration::new(acc.x().clamp(-3, 3), acc.y().clamp(-10, 3));
    Self { acc, ..self }
  }
}
