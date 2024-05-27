#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct MaxHp {
  pub max_hp: u64,
}

pub trait HasMaxHp {
  fn max_hp(&self) -> MaxHp;
  fn max_hp_u64(&self) -> u64 {
    self.max_hp().max_hp
  }
}

impl MaxHp {
  pub const fn new(max_hp: u64) -> Self {
    Self { max_hp }
  }
}

impl From<u64> for MaxHp {
  fn from(max_hp: u64) -> Self {
    Self::new(max_hp)
  }
}

impl From<MaxHp> for u64 {
  fn from(val: MaxHp) -> Self {
    val.max_hp
  }
}
