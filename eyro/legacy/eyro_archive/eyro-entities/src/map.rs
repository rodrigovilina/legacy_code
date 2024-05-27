use eyro_values::{
  map_id::{HasMapId, MapId},
  size::{Size, Sizeable},
};

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Map {
  id: MapId,
  size: Size,
}

impl Map {
  pub fn new(id: MapId, size: Size) -> Self {
    Self { id, size }
  }

  pub fn id(&self) -> MapId {
    self.id
  }
}

impl Sizeable for Map {
  fn size(&self) -> Size {
    self.size
  }
}

impl HasMapId for Map {
  fn map_id(&self) -> MapId {
    self.id()
  }
}
