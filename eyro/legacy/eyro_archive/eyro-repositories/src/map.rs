use {
  crate::Repository,
  eyro_entities::map::Map,
  eyro_values::map_id::MapId,
  std::slice::{Iter, IterMut},
};

#[derive(Debug, Default)]
pub struct MapRepository {
  maps: Vec<Map>,
}

impl MapRepository {
  pub fn new(maps: Vec<Map>) -> Self {
    Self { maps }
  }
}

impl Repository<Map, MapId> for MapRepository {
  fn iter(&self) -> Iter<Map> {
    self.maps.iter()
  }

  fn iter_mut(&mut self) -> IterMut<Map> {
    self.maps.iter_mut()
  }

  fn find(&self, id: MapId) -> Option<&Map> {
    self.iter().find(|map: &&Map| map.id() == id)
  }

  fn destroy(&mut self, id: MapId) {
    self.maps.retain(|map| map.id() != id);
  }
}
