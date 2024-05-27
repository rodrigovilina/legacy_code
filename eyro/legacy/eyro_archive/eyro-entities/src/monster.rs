use {
  crate::monster_race::MonsterRace,
  eyro_values::{
    acceleration::Acceleration,
    attack::Attack,
    hp_bar::{HasHpBar, HpBar},
    map_id::MapId,
    monster_id::MonsterId,
    monster_movement::MonsterMovement,
    position::{Position, Positionable, PositionableMut},
    r#static::Spatial,
    size::{Size, Sizeable},
    velocity::{HasVelocity, Velocity},
  },
  rand::{prelude::ThreadRng, Rng},
  std::cmp::Ordering,
};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Monster {
  id: MonsterId,
  map_id: MapId,
  race: MonsterRace,
  pos: Position,
  vel: Velocity,
  acc: Acceleration,
  hp_bar: HpBar,
  movement: MonsterMovement,
}

impl Monster {
  pub fn new(
    id: MonsterId,
    map_id: MapId,
    race: MonsterRace,
    pos: Position,
    hp_bar: HpBar,
    movement: MonsterMovement,
  ) -> Self {
    Self {
      id,
      map_id,
      race,
      pos,
      vel: Velocity::default(),
      acc: Acceleration::default(),
      hp_bar,
      movement,
    }
  }

  pub fn id(&self) -> MonsterId {
    self.id
  }

  pub fn map_id(&self) -> MapId {
    self.map_id
  }

  pub fn update_movement_state(&mut self, map_size: Size) {
    match self.movement {
      MonsterMovement::Idle { frames: 0 } => {
        let mut rng: ThreadRng = rand::thread_rng();
        let x = rng.gen_range(0..map_size.width() as i64);
        let y = rng.gen_range(0..map_size.height() as i64);

        self.movement = MonsterMovement::MovingTowards {
          target: Position::new(x, y),
        };
      }
      MonsterMovement::Idle { frames } => {
        self.movement = MonsterMovement::Idle { frames: frames - 1 };
      }
      MonsterMovement::MovingTowards { target } => {
        if target == self.pos {
          self.movement = MonsterMovement::Idle { frames: 10 };
        }
      }
    }
  }

  pub fn damage(&mut self, attack: Attack) {
    self.hp_bar.damage(attack)
  }
}

impl Sizeable for Monster {
  fn size(&self) -> Size {
    self.race.size()
  }
}

impl Positionable for Monster {
  fn position(&self) -> Position {
    self.pos
  }
}

impl PositionableMut for Monster {
  fn update_posistion(&mut self) {
    match self.movement {
      MonsterMovement::Idle { frames: _ } => {}
      MonsterMovement::MovingTowards { target } => {
        let new_pos_x: i64 = adjust_value_towards(self.pos.x(), target.x(), 1);
        let new_pos_y: i64 = adjust_value_towards(self.pos.y(), target.y(), 1);
        let new_pos: Position = Position::new(new_pos_x, new_pos_y);
        self.pos = new_pos;
      }
    }
  }
}

impl HasVelocity for Monster {
  fn velocity(&self) -> Velocity {
    self.vel
  }
}

impl Spatial for Monster {}

impl HasHpBar for Monster {
  fn hp_bar(&self) -> HpBar {
    self.hp_bar
  }
}

fn adjust_value_towards(val: i64, goal: i64, max_step: i64) -> i64 {
  match val.cmp(&goal) {
    Ordering::Less => std::cmp::min(val + max_step, goal),
    Ordering::Greater => std::cmp::max(val - max_step, goal),
    _ => val,
  }
}
