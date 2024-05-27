use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct End {}

impl End {
  const fn new() -> Self {
    Self {}
  }

  pub const fn token() -> Token {
    Token::End
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^end")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(_lexeme, rest)| (Self::token(), rest))
  }
}

impl Default for End {
  fn default() -> Self {
    Self::new()
  }
}

impl From<End> for Token {
  fn from(_: End) -> Self {
    End::token()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(End::new(), End {});
  }

  #[test]
  fn test_token() {
    assert_eq!(End::token(), Token::End);
  }

  #[test]
  fn test_split() {
    assert_eq!(End::split("end"), Some(("end".to_string(), String::new())));
  }

  #[test]
  fn test_default() {
    assert_eq!(End::default(), End {});
  }

  #[test]
  fn test_node_from() {
    assert_eq!(Token::from(End::new()), Token::End);
  }

  #[test]
  fn test_lex() {
    assert_eq!(End::lex("end"), Some((Token::End, String::new())));
  }
}
