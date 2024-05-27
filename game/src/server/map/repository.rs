use super::{Map, MapId};

pub struct MapRepository {
  pub maps: Vec<Map>,
}

impl MapRepository {
  pub fn new() -> Self {
    Self {
      maps: vec![Map {
        id: MapId(0),
        size: (11, 11),
      }],
    }
  }

  pub fn get(&self, id: impl Into<MapId>) -> Map {
    let id = id.into();
    self
      .maps
      .iter()
      .find(|map| map.id == id)
      .expect("Map not found")
      .clone()
  }
}
