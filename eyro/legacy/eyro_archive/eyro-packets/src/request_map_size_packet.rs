use eyro_values::map_id::MapId;

#[derive(Clone, Debug, Default)]
pub struct RequestMapSizePacket {
  pub map_id: MapId,
}
