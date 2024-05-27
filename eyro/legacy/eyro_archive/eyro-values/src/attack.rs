#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Attack {
  pub attack: u64,
}

pub trait HasAttack {
  fn attack(&self) -> Attack;
}

impl Attack {
  pub fn new(attack: u64) -> Self {
    Self { attack }
  }

  pub fn u64(&self) -> u64 {
    self.attack
  }
}

impl From<u64> for Attack {
  fn from(value: u64) -> Self {
    Self { attack: value }
  }
}

impl From<Attack> for u64 {
  fn from(value: Attack) -> Self {
    value.attack
  }
}
