pub mod id;
pub mod repository;

use self::id::MapId;

#[derive(Clone, Copy, Debug)]
pub struct Map {
  pub id: MapId,
  pub size: (usize, usize),
}
