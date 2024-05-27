use eyro_values::{
  max_hp::MaxHp,
  size::{Size, Sizeable},
};

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct MonsterRace {
  name: &'static str,
  color: (u8, u8, u8),
  size: Size,
  max_hp: MaxHp,
}

impl MonsterRace {
  pub const PORING: MonsterRace = MonsterRace {
    name: "Poring",
    color: (255, 0, 0),
    size: Size::new(32, 32),
    max_hp: MaxHp::new(5),
  };

  pub fn max_hp(&self) -> MaxHp {
    self.max_hp
  }
}

impl Sizeable for MonsterRace {
  fn size(&self) -> Size {
    self.size
  }
}
