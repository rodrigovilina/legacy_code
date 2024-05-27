use {
  crate::Repository,
  eyro_entities::bullet::Bullet,
  eyro_values::{bullet_id::BulletId, map_id::MapId},
  std::slice::{Iter, IterMut},
};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct BulletRepository {
  bullets: Vec<Bullet>,
  next_id: u64,
}

impl Repository<Bullet, BulletId> for BulletRepository {
  fn iter(&self) -> Iter<Bullet> {
    self.bullets.iter()
  }

  fn iter_mut(&mut self) -> IterMut<Bullet> {
    self.bullets.iter_mut()
  }

  fn find(&self, id: BulletId) -> Option<&Bullet> {
    self.iter().find(|bullet: &&Bullet| bullet.id() == id)
  }

  fn destroy(&mut self, id: BulletId) {
    self.bullets.retain(|bullet: &Bullet| bullet.id() != id);
  }
}

impl BulletRepository {
  pub fn new() -> Self {
    Self {
      bullets: Vec::new(),
      next_id: 0,
    }
  }

  pub fn push(&mut self, bullet: Bullet) {
    self.bullets.push(bullet);
    self.next_id += 1;
  }

  pub fn next_id(&self) -> BulletId {
    BulletId::new(self.next_id)
  }

  pub fn where_map(&self, map_id: MapId) -> Vec<&Bullet> {
    self
      .bullets
      .iter()
      .filter(|bullet| bullet.map_id() == map_id)
      .collect()
  }
}
