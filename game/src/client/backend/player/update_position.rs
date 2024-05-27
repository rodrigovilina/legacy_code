use crate::client::backend::position::Position;

use super::Player;

pub trait UpdatePosition {
  fn update_position(self) -> Self;
}

impl UpdatePosition for Player {
  fn update_position(self) -> Self {
    let x: isize = self.pos.x() + self.vel.x();
    let y: isize = self.pos.y() + self.vel.y();
    Self {
      pos: Position::new(x, y),
      ..self
    }
  }
}

impl Player {}
