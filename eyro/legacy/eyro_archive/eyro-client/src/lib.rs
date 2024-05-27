pub mod draw;
pub mod map;
pub mod null;
pub mod packets;
pub mod receive_packets;
pub mod tick;

use {eyro_values::map_id::MapId, map::MapClient, null::NullClient};

pub enum Client {
  Map(MapClient),
  Null(NullClient),
  None,
}

impl Client {
  pub fn none() -> Self {
    Self::None
  }

  pub fn null() -> Self {
    Self::Null(NullClient::new())
  }

  pub fn map(map_id: MapId) -> Result<Self, String> {
    Ok(Self::Map(MapClient::disconnected(map_id)))
  }
}

impl Default for Client {
  fn default() -> Self {
    Self::none()
  }
}
