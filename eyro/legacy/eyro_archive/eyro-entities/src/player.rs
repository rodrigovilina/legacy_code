use eyro_values::{
  acceleration::Acceleration,
  attack::Attack,
  hp_bar::HpBar,
  map_id::MapId,
  player_id::PlayerId,
  position::{Position, Positionable, PositionableMut},
  size::{Size, Sizeable, SizeableMut},
  velocity::{HasVelocity, HasVelocityMut, Velocity},
};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Player {
  id: PlayerId,
  map_id: MapId,
  size: Size,
  pos: Position,
  vel: Velocity,
  acc: Acceleration,
  attack: Attack,
  hp_bar: HpBar,
}

impl Player {
  pub fn new(
    id: PlayerId,
    map_id: MapId,
    size: Size,
    pos: Position,
    attack: Attack,
    hp_bar: HpBar,
  ) -> Self {
    Self {
      id,
      map_id,
      pos,
      size,
      vel: Velocity::default(),
      acc: Acceleration::default(),
      attack,
      hp_bar,
    }
  }

  pub fn acc(&self) -> Acceleration {
    self.acc
  }

  pub fn attack(&self) -> Attack {
    self.attack
  }

  pub fn map_id(&self) -> MapId {
    self.map_id
  }

  pub fn id(&self) -> PlayerId {
    self.id
  }

  pub fn update(mut self, acceleration: Acceleration) -> Self {
    self.update_acc(acceleration);
    self.update_velocity();
    self.update_posistion();
    self
  }
}

impl Sizeable for Player {
  fn size(&self) -> Size {
    self.size
  }
}

impl SizeableMut for Player {
  fn set_size(&mut self, size: Size) {
    self.size = size;
  }
}

impl Positionable for Player {
  fn position(&self) -> Position {
    self.pos
  }
}

impl PositionableMut for Player {
  fn update_posistion(&mut self) {
    let pos: Position = self.position() + self.velocity();
    self.pos = pos;
  }
}

impl HasVelocity for Player {
  fn velocity(&self) -> Velocity {
    self.vel
  }
}

impl HasVelocityMut for Player {
  fn update_velocity(&mut self) {
    let vel: Velocity = self.velocity() + self.acc();
    let vel: Velocity = vel.clamp(-3000, 3000);

    self.vel = vel;
  }
}

impl Player {
  fn update_acc(&mut self, acceleration: Acceleration) {
    let acc: Acceleration = acceleration;

    self.acc = acc;
  }
}
