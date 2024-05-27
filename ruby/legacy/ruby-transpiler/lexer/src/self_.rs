use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct Self_ {}

impl Self_ {
  const fn new() -> Self {
    Self {}
  }

  pub const fn token() -> Token {
    Token::Self_
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^self")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(_lexeme, rest)| (Self::token(), rest))
  }
}

impl Default for Self_ {
  fn default() -> Self {
    Self::new()
  }
}

impl From<Self_> for Token {
  fn from(_: Self_) -> Self {
    Self_::token()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(Self_::new(), Self_ {});
  }

  #[test]
  fn test_token() {
    assert_eq!(Self_::token(), Token::Self_);
  }

  #[test]
  fn test_split() {
    assert_eq!(Self_::split("self"), Some(("self".to_string(), String::new())));
  }

  #[test]
  fn test_default() {
    assert_eq!(Self_::default(), Self_ {});
  }

  #[test]
  fn test_node_from() {
    assert_eq!(Token::from(Self_::new()), Token::Self_);
  }

  #[test]
  fn test_lex() {
    assert_eq!(Self_::lex("self"), Some((Token::Self_, String::new())));
  }
}
