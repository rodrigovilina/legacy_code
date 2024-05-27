use crate::{
  position::{Position, Positionable},
  size::{Size, Sizeable},
};

pub struct Static {
  pub position: Position,
  pub size: Size,
}

pub trait Spatial: Positionable + Sizeable {
  fn collides_with(&self, other: &dyn Spatial) -> bool {
    let self_x: i64 = self.left();
    let self_y: i64 = self.top();
    let self_width: i64 = self.width() as i64;
    let self_height: i64 = self.height() as i64;
    let other_x: i64 = other.left();
    let other_y: i64 = other.top();
    let other_width: i64 = other.width() as i64;
    let other_height: i64 = other.height() as i64;

    self_x < other_x + other_width
      && self_x + self_width > other_x
      && self_y < other_y + other_height
      && self_y + self_height > other_y
  }

  fn right(&self) -> i64 {
    self.left() + self.width() as i64
  }

  fn bottom(&self) -> i64 {
    self.top() + self.height() as i64
  }

  fn center(&self) -> (i64, i64) {
    (
      (self.left() + self.width() as i64) / 2,
      (self.top() + self.height() as i64) / 2,
    )
  }
}
