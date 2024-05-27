use {
  crate::{attack::Attack, max_hp::MaxHp},
  std::ops::{Div, Sub},
};

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct CurrentHp {
  pub current_hp: u64,
}

pub trait HasCurrentHp {
  fn current_hp(&self) -> CurrentHp;
  fn current_hp_u64(&self) -> u64 {
    self.current_hp().current_hp
  }
}

impl CurrentHp {
  pub fn new(current_hp: u64) -> Self {
    Self { current_hp }
  }
}

impl Sub<Attack> for CurrentHp {
  type Output = CurrentHp;

  fn sub(self, rhs: Attack) -> Self::Output {
    Self::new(self.current_hp - rhs.attack)
  }
}

impl Div<MaxHp> for CurrentHp {
  type Output = f64;

  fn div(self, rhs: MaxHp) -> Self::Output {
    self.current_hp as f64 / rhs.max_hp as f64
  }
}
