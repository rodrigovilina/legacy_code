use crate::{
  acceleration::{Acceleratable, Acceleration},
  position::{Position, Positionable},
  size::{Size, Sizeable},
  velocity::{HasVelocity, Velocity},
};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct MovableBody {
  size: Size,
  position: Position,
  velocity: Velocity,
  acceleration: Acceleration,
}

pub trait Physical: Sizeable + Positionable + HasVelocity + Acceleratable {}
