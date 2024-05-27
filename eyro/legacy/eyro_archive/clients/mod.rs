pub mod client;
pub mod map;
pub mod maps;
pub mod player;

use {crate::Server, eyro_packets::Packets};

pub trait SyncViewer {
  fn tick(&mut self) -> Option<()>;
  fn draw(&mut self, server: &Server);
  fn events(&self) -> Packets {
    Packets::default()
  }
}

#[macro_export]
macro_rules! create_client {
  (Client, $param:expr) => {
    ClientSyncViewer::new($param.into()).unwrap()
  };
  (Map, $param:expr) => {
    MapSyncViewer::new($param).unwrap()
  };
  (Maps, $params:expr) => {
    MapsSyncViewer::new($params)
  };
  (Player, $param:expr) => {
    PlayerSyncViewer::new($param).unwrap()
  };
}
