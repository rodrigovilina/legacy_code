use super::{attack::Attack, current_hp::CurrentHp, max_hp::MaxHp};

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct HpBar {
  pub current_hp: CurrentHp,
  pub max_hp: MaxHp,
}

impl HpBar {
  pub fn maxed_at(hp: u64) -> Self {
    Self {
      current_hp: CurrentHp::new(hp),
      max_hp: MaxHp::new(hp),
    }
  }

  pub fn from_max(max_hp: MaxHp) -> Self {
    Self {
      current_hp: CurrentHp::new(max_hp.max_hp),
      max_hp,
    }
  }

  pub fn damage(&mut self, attack: Attack) {
    self.current_hp = self.current_hp - attack
  }

  pub fn current_hp(&self) -> CurrentHp {
    self.current_hp
  }

  pub fn max_hp(&self) -> MaxHp {
    self.max_hp
  }
}

pub trait HasHpBar {
  fn hp_bar(&self) -> HpBar;
  fn current_hp(&self) -> CurrentHp {
    self.hp_bar().current_hp()
  }
  fn max_hp(&self) -> MaxHp {
    self.hp_bar().max_hp()
  }

  fn current_hp_ratio(&self) -> f64 {
    self.current_hp() / self.max_hp()
  }
}
