use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct True {}

impl True {
  const fn new() -> Self {
    Self {}
  }

  pub const fn token() -> Token {
    Token::True
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^true")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(_lexeme, rest)| (Self::token(), rest))
  }
}

impl Default for True {
  fn default() -> Self {
    Self::new()
  }
}

impl From<True> for Token {
  fn from(_: True) -> Self {
    True::token()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(True::new(), True {});
  }

  #[test]
  fn test_token() {
    assert_eq!(True::token(), Token::True);
  }

  #[test]
  fn test_split() {
    assert_eq!(True::split("true"), Some(("true".to_string(), String::new())));
  }

  #[test]
  fn test_default() {
    assert_eq!(True::default(), True {});
  }

  #[test]
  fn test_node_from() {
    assert_eq!(Token::from(True::new()), Token::True);
  }

  #[test]
  fn test_lex() {
    assert_eq!(True::lex("true"), Some((Token::True, String::new())));
  }
}
