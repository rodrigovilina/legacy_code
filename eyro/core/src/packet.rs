use std::{io::Read, net::TcpStream};

#[derive(Debug, PartialEq, Eq)]
pub struct AccountCharacter {
  name: String,
}

impl AccountCharacter {
  #[must_use]
  pub fn as_bytes(&self) -> Vec<u8> {
    [
      self.name.len().to_be_bytes().to_vec(),
      self.name.as_bytes().to_vec(),
    ]
    .concat()
  }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Packet {
  AcknowledgeConnection { client_id: u64 },
  HealthCheck,
  Login { username: String, password: String },
  AccountCharacters { characters: Vec<AccountCharacter> },
}

impl Packet {
  #[must_use]
  pub fn size_in_bytes(&self) -> u64 {
    self.as_bytes().len() as u64
  }

  #[must_use]
  pub fn as_bytes(&self) -> Vec<u8> {
    match self {
      Self::AcknowledgeConnection { client_id } => {
        [vec![0], client_id.to_be_bytes().to_vec()].concat()
      }
      Self::HealthCheck => vec![1],
      Self::Login { username, password } => [
        vec![2],
        username.len().to_be_bytes().to_vec(),
        username.as_bytes().to_vec(),
        password.len().to_be_bytes().to_vec(),
        password.as_bytes().to_vec(),
      ]
      .concat(),
      Self::AccountCharacters { characters } => {
        let bytes: Vec<u8> = characters
          .iter()
          .flat_map(AccountCharacter::as_bytes)
          .collect();
        [vec![3], bytes].concat()
      }
    }
  }

  #[must_use]
  pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
    match bytes {
      [0, client_id @ ..] => {
        let client_id = u64::from_be_bytes(client_id.try_into().ok()?);
        Some(Self::AcknowledgeConnection { client_id })
      }
      [1] => Some(Self::HealthCheck),
      [2, rest @ ..] => {
        let mut username_len = [0u8; 8];
        username_len.copy_from_slice(&rest[0..8]);
        let username_len = u64::from_be_bytes(username_len);
        let mut username = vec![0u8; usize::try_from(username_len).ok()?];
        let var_name = 8 + usize::try_from(username_len).ok()?;
        username.copy_from_slice(&rest[8..var_name]);
        let username = String::from_utf8(username).ok()?;
        let mut password_len = [0u8; 8];
        password_len.copy_from_slice(&rest[var_name..8 + var_name]);
        let password_len = u64::from_be_bytes(password_len);
        let mut password = vec![0u8; usize::try_from(password_len).ok()?];
        password.copy_from_slice(
          &rest[(var_name + 8)..(8 + var_name) + usize::try_from(password_len).ok()?],
        );
        let password = String::from_utf8(password).ok()?;
        Some(Self::Login { username, password })
      }
      _ => None,
    }
  }

  #[allow(clippy::missing_panics_doc)]
  pub fn read_multiple_from_stream(stream: &mut TcpStream) -> Vec<Self> {
    #[allow(clippy::unwrap_used)]
    stream
      .set_nonblocking(true)
      .map_err(|_| "Cannot set non-blocking".to_string())
      .unwrap();
    std::iter::from_fn(|| Self::read_from_stream(stream)).collect()
  }

  pub fn read_from_stream(stream: &mut TcpStream) -> Option<Self> {
    let mut packet_type: [u8; 1] = [0; 1];

    match stream.peek(&mut packet_type) {
      Ok(1) => {
        let _: u8 = packet_type[0];
        // println!("Successfully read packet type: {pt:?}");
      }
      Ok(0) | Err(_) => return None,
      Ok(_) => unreachable!(),
    }

    match packet_type[0] {
      0 => {
        let mut buf: [u8; 3] = [0; 3];
        stream.read_exact(&mut buf).ok()?;
        Self::from_bytes(&buf)
      }
      1 => {
        stream.read_exact(&mut [0; 1]).ok()?;
        Some(Self::HealthCheck)
      }
      2 => {
        stream.read_exact(&mut [0; 1]).ok()?;
        let username: String = get_sized_string(stream).ok()?;
        let password: String = get_sized_string(stream).ok()?;
        Some(Self::Login { username, password })
      }
      _ => None,
    }
  }
}

fn get_sized_string(stream: &mut TcpStream) -> Result<String, String> {
  let mut len_buffer: [u8; 8] = [0; 8];
  stream
    .read_exact(&mut len_buffer)
    .map_err(|e| e.to_string())?;
  let len_buffer: u64 = u64::from_be_bytes(len_buffer);
  let mut buffer = vec![0; usize::try_from(len_buffer).map_err(|e| e.to_string())?];
  stream.read_exact(&mut buffer).map_err(|e| e.to_string())?;
  String::from_utf8(buffer).map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  #[allow(clippy::unwrap_used)]
  fn as_bytes() {
    let packet = Packet::AcknowledgeConnection { client_id: 1337 };
    let reserialized = Packet::from_bytes(&packet.as_bytes());
    assert_eq!(packet, reserialized.unwrap());

    let packet = Packet::HealthCheck;
    let reserialized = Packet::from_bytes(&packet.as_bytes());
    assert_eq!(packet, reserialized.unwrap());

    let packet = Packet::Login {
      username: "vaporyhumo".to_string(),
      password: "eyro".to_string(),
    };
    let reserialized = Packet::from_bytes(&packet.as_bytes());
    assert_eq!(packet, reserialized.unwrap());
  }
}
