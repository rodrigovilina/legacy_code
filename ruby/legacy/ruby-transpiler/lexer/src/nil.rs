use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct Nil {}

impl Nil {
  const fn new() -> Self {
    Self {}
  }

  pub const fn token() -> Token {
    Token::Nil
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^nil")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(_lexeme, rest)| (Self::token(), rest))
  }
}

impl Default for Nil {
  fn default() -> Self {
    Self::new()
  }
}

impl From<Nil> for Token {
  fn from(_: Nil) -> Self {
    Nil::token()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(Nil::new(), Nil {});
  }

  #[test]
  fn test_token() {
    assert_eq!(Nil::token(), Token::Nil);
  }

  #[test]
  fn test_split() {
    assert_eq!(Nil::split("nil"), Some(("nil".to_string(), String::new())));
  }

  #[test]
  fn test_default() {
    assert_eq!(Nil::default(), Nil {});
  }

  #[test]
  fn test_node_from() {
    assert_eq!(Token::from(Nil::new()), Token::Nil);
  }

  #[test]
  fn test_lex() {
    assert_eq!(Nil::lex("nil"), Some((Token::Nil, String::new())));
  }
}
