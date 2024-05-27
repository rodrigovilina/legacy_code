pub mod bullet;
pub mod map;
pub mod monster;
pub mod monster_spawn;
pub mod player;

use std::slice::{Iter, IterMut};

pub trait Repository<T, U> {
  fn iter(&self) -> Iter<T>;
  fn iter_mut(&mut self) -> IterMut<T>;
  fn find(&self, id: U) -> Option<&T>;
  fn destroy(&mut self, id: U);
}
