use eyro_values::{client_id::ClientId, map_id::MapId, size::Size};

#[derive(Clone, Debug, Default)]
pub struct MapSizePacket {
  pub id: MapId,
  pub client_id: ClientId,
  pub size: Size,
}
