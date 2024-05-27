use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct False {}

impl False {
  const fn new() -> Self {
    Self {}
  }

  pub const fn token() -> Token {
    Token::False
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^false")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(_lexeme, rest)| (Self::token(), rest))
  }
}

impl Default for False {
  fn default() -> Self {
    Self::new()
  }
}

impl From<False> for Token {
  fn from(_: False) -> Self {
    False::token()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(False::new(), False {});
  }

  #[test]
  fn test_token() {
    assert_eq!(False::token(), Token::False);
  }

  #[test]
  fn test_split() {
    assert_eq!(
      False::split("false"),
      Some(("false".to_string(), String::new()))
    );
  }

  #[test]
  fn test_default() {
    assert_eq!(False::default(), False {});
  }

  #[test]
  fn test_node_from() {
    assert_eq!(Token::from(False::new()), Token::False);
  }

  #[test]
  fn test_lex() {
    assert_eq!(False::lex("false"), Some((Token::False, String::new())));
  }
}
