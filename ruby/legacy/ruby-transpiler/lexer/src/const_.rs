use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct Const {
  lexeme: String,
}

impl Const {
  fn new(lexeme: &str) -> Self {
    Self { lexeme: lexeme.to_string() }
  }

  pub fn token(lexeme: &str) -> Token {
    Token::Const(lexeme.to_string())
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^(::)?([A-Z][a-zA-Z0-9_]*)(::[A-Z][A-Za-z0-9_]*)*")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(lexeme, rest)| (Self::token(&lexeme), rest))
  }
}

impl Default for Const {
  fn default() -> Self {
    Self::new("Foo")
  }
}

impl From<Const> for Token {
  fn from(const_: Const) -> Self {
    Const::token(&const_.lexeme)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(Const::new("Foo"), Const { lexeme: "Foo".to_string() });
  }

  #[test]
  fn test_token() {
    assert_eq!(Const::token("Foo"), Token::Const("Foo".to_string()));
  }

  #[test]
  fn test_split() {
    assert_eq!(Const::split("Foo"), Some(("Foo".to_string(), String::new())));
  }

  #[test]
  fn test_default() {
    assert_eq!(Const::default(), Const::new("Foo"));
  }

  #[test]
  fn test_node_from() {
    assert_eq!(Token::from(Const::new("Foo")), Token::Const("Foo".to_string()));
  }

  #[test]
  fn test_lex() {
    assert_eq!(Const::lex("Foo"), Some((Const::token("Foo"), String::new())));
  }
}
