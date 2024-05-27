use crate::position::Position;

#[derive(Clone, Debug, PartialEq)]
pub enum MonsterMovement {
  Idle { frames: u64 },
  MovingTowards { target: Position },
}

impl Default for MonsterMovement {
  fn default() -> Self {
    MonsterMovement::Idle { frames: 0 }
  }
}

pub trait HasMonsterMovement {
  fn movement(&self) -> &MonsterMovement;
  fn movement_mut(&mut self) -> &mut MonsterMovement;
}
