use crate::id::Id;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct ClientId {
  pub id: u64,
}

pub trait HasClientId {
  fn client_id(&self) -> ClientId;
}

impl ClientId {
  pub fn new(id: u64) -> Self {
    Self { id }
  }
}

impl From<u64> for ClientId {
  fn from(id: u64) -> Self {
    Self::new(id)
  }
}

impl From<ClientId> for u64 {
  fn from(val: ClientId) -> Self {
    val.id
  }
}

impl Id for ClientId {}
