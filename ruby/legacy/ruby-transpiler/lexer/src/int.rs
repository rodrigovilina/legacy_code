use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct Int {
  lexeme: String,
}

impl Int {
  fn new(lexeme: &str) -> Self {
    Self { lexeme: lexeme.to_string() }
  }

  pub fn token(lexeme: &str) -> Token {
    Token::Int(lexeme.to_string())
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^-?[1-9][0-9_]*")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(lexeme, rest)| (Self::token(&lexeme), rest))
  }
}

impl Default for Int {
  fn default() -> Self {
    Self::new("0")
  }
}

impl From<Int> for Token {
  fn from(int: Int) -> Self {
    Int::token(&int.lexeme)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(
      Int::new("1234567890"),
      Int { lexeme: "1234567890".to_string() }
    );
  }

  #[test]
  fn test_token() {
    assert_eq!(Int::token("1234567890"), Token::Int("1234567890".to_string()));
  }

  #[test]
  fn test_split() {
    assert_eq!(
      Int::split("1234567890"),
      Some(("1234567890".to_string(), String::new()))
    );
  }

  #[test]
  fn test_default() {
    assert_eq!(Int::default(), Int::new("0"));
  }

  #[test]
  fn test_node_from() {
    assert_eq!(
      Token::from(Int::new("1234567890")),
      Token::Int("1234567890".to_string())
    );
  }

  #[test]
  fn test_lex() {
    assert_eq!(
      Int::lex("1234567890"),
      Some((Int::token("1234567890"), String::new()))
    );
  }
}
