use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct Class {}

impl Class {
  const fn new() -> Self {
    Self {}
  }

  pub const fn token() -> Token {
    Token::Class
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^class")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(_lexeme, rest)| (Self::token(), rest))
  }
}

impl Default for Class {
  fn default() -> Self {
    Self::new()
  }
}

impl From<Class> for Token {
  fn from(_: Class) -> Self {
    Class::token()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(Class::new(), Class {});
  }

  #[test]
  fn test_token() {
    assert_eq!(Class::token(), Token::Class);
  }

  #[test]
  fn test_split() {
    assert_eq!(
      Class::split("class"),
      Some(("class".to_string(), String::new()))
    );
  }

  #[test]
  fn test_default() {
    assert_eq!(Class::default(), Class {});
  }

  #[test]
  fn test_node_from() {
    assert_eq!(Token::from(Class::new()), Token::Class);
  }

  #[test]
  fn test_lex() {
    assert_eq!(Class::lex("class"), Some((Token::Class, String::new())));
  }
}
