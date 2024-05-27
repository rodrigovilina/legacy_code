use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct Id {
  lexeme: String,
}

impl Id {
  fn new(lexeme: &str) -> Self {
    Self { lexeme: lexeme.to_string() }
  }

  pub fn token(lexeme: &str) -> Token {
    Token::Id(lexeme.to_string())
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^[a-z_][a-zA-Z0-9_]*")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(lexeme, rest)| (Self::token(&lexeme), rest))
  }
}

impl Default for Id {
  fn default() -> Self {
    Self::new("foo")
  }
}

impl From<Id> for Token {
  fn from(id: Id) -> Self {
    Id::token(&id.lexeme)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(Id::new("foo"), Id { lexeme: "foo".to_string() });
  }

  #[test]
  fn test_token() {
    assert_eq!(Id::token("foo"), Token::Id("foo".to_string()));
  }

  #[test]
  fn test_split() {
    assert_eq!(Id::split("foo"), Some(("foo".to_string(), String::new())));
  }

  #[test]
  fn test_default() {
    assert_eq!(Id::default(), Id::new("foo"));
  }

  #[test]
  fn test_node_from() {
    assert_eq!(Token::from(Id::new("foo")), Token::Id("foo".to_string()));
  }

  #[test]
  fn test_lex() {
    assert_eq!(Id::lex("foo"), Some((Id::token("foo"), String::new())));
  }
}
