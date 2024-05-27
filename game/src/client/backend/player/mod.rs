mod update_acceleration;
mod update_position;
mod update_velocity;

use {
  self::{
    update_acceleration::UpdateAcceleration, update_position::UpdatePosition,
    update_velocity::UpdateVelocity,
  },
  super::{
    acceleration::Acceleration, game_input::GameInput, position::Position, size::Size,
    velocity::Velocity,
  },
  crate::server,
  once_cell::sync::Lazy,
};

const GRAVITY: Lazy<Acceleration> = Lazy::new(|| Acceleration::new(0, 1));

#[derive(Clone, Debug)]
pub struct Player {
  size: Size,
  pos: Position,
  vel: Velocity,
  acc: Acceleration,
}

impl Player {
  pub fn new() -> Self {
    Self {
      size: Size(20, 40),
      pos: Position::new(100, 100),
      vel: Velocity::default(),
      acc: Acceleration::default(),
    }
  }

  pub fn pos(&self) -> Position {
    self.pos
  }

  pub fn size(&self) -> Size {
    self.size
  }

  fn debug(self) -> Self {
    println!("{:#?}", self);
    self
  }

  pub fn update(self, game_input: &GameInput) -> Self {
    self
      .update_acceleration(game_input)
      .update_velocity()
      .update_position()
      .collide_with_floor(600)
      .collide_with_left_wall(0)
      .collide_with_right_wall(800)
      .debug()
  }

  pub fn collide_with_floor(self, floor_y: isize) -> Self {
    if self.bottom() > floor_y {
      let y: isize = floor_y - self.size.1 as isize;
      Self {
        pos: Position::new(self.pos.x(), y),
        vel: Velocity::new(self.vel.x(), 0),
        ..self
      }
    } else {
      self
    }
  }

  fn collide_with_left_wall(self, left_wall_x: isize) -> Self {
    if self.left() < left_wall_x {
      let x: isize = left_wall_x;
      Self {
        pos: Position::new(x, self.pos.y()),
        vel: Velocity::new(0, self.vel.y()),
        ..self
      }
    } else {
      self
    }
  }

  fn collide_with_right_wall(self, right_wall_x: isize) -> Self {
    if self.right() > right_wall_x {
      let x: isize = right_wall_x - self.size.0 as isize;
      Self {
        pos: Position::new(x, self.pos.y()),
        vel: Velocity::new(0, self.vel.y()),
        ..self
      }
    } else {
      self
    }
  }

  fn left(&self) -> isize {
    self.pos.x()
  }

  fn right(&self) -> isize {
    self.pos.x() + self.size.0 as isize
  }

  fn bottom(&self) -> isize {
    self.pos.y() + self.size.1 as isize
  }

  fn is_airbound(&self) -> bool {
    self.bottom() < 600
  }
}

impl From<server::player::Player> for Player {
  fn from(_item: server::player::Player) -> Self {
    Player::new()
  }
}
