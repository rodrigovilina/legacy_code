use eyro_values::{
  attack::{Attack, HasAttack},
  bullet_id::{BulletId, HasBulletId},
  map_id::{HasMapId, MapId},
  player_id::{HasPlayerId, PlayerId},
  position::{Position, Positionable, PositionableMut},
  r#static::Spatial,
  size::{Size, Sizeable},
  velocity::{HasVelocity, Velocity},
};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Bullet {
  id: BulletId,
  player_id: PlayerId,
  map_id: MapId,
  position: Position,
  velocity: Velocity,
  attack: Attack,
}

impl Bullet {
  pub fn new(
    id: BulletId,
    player_id: PlayerId,
    map_id: MapId,
    position: Position,
    velocity: Velocity,
    attack: Attack,
  ) -> Self {
    Self {
      id,
      player_id,
      map_id,
      position,
      velocity,
      attack,
    }
  }

  pub fn id(&self) -> BulletId {
    self.id
  }

  pub fn map_id(&self) -> MapId {
    self.map_id
  }
}

impl Positionable for Bullet {
  fn position(&self) -> Position {
    self.position
  }
}

impl PositionableMut for Bullet {
  fn update_posistion(&mut self) {
    let pos: Position = self.position() + self.velocity();
    self.position = pos;
  }
}

impl Sizeable for Bullet {
  fn size(&self) -> Size {
    Size::new(10, 10)
  }
}

impl Spatial for Bullet {}

impl HasVelocity for Bullet {
  fn velocity(&self) -> Velocity {
    self.velocity
  }
}

impl HasAttack for Bullet {
  fn attack(&self) -> Attack {
    self.attack
  }
}

impl HasBulletId for Bullet {
  fn bullet_id(&self) -> BulletId {
    self.id
  }
}

impl HasPlayerId for Bullet {
  fn player_id(&self) -> PlayerId {
    self.player_id
  }
}

impl HasMapId for Bullet {
  fn map_id(&self) -> MapId {
    self.map_id
  }
}
