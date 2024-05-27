use crate::server;

#[derive(Clone)]
pub struct Map();

impl From<server::map::Map> for Map {
  fn from(_item: server::map::Map) -> Self {
    Map()
  }
}
