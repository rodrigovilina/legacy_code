use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct DString {
  lexeme: String,
}

impl DString {
  fn new(lexeme: &str) -> Self {
    Self { lexeme: lexeme.to_string() }
  }

  pub fn token(lexeme: &str) -> Token {
    Token::DString(lexeme.to_string())
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^\"[a-zA-Z0-9_ ]*\"")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(lexeme, rest)| (Self::token(&lexeme), rest))
  }
}

impl Default for DString {
  fn default() -> Self {
    Self::new("\"foo\"")
  }
}

impl From<DString> for Token {
  fn from(id: DString) -> Self {
    DString::token(&id.lexeme)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(
      DString::new("\"foo\""),
      DString { lexeme: "\"foo\"".to_string() }
    );
  }

  #[test]
  fn test_token() {
    assert_eq!(
      DString::token("\"foo\""),
      Token::DString("\"foo\"".to_string())
    );
  }

  #[test]
  fn test_split() {
    assert_eq!(
      DString::split("\"foo\""),
      Some(("\"foo\"".to_string(), String::new()))
    );
  }

  #[test]
  fn test_default() {
    assert_eq!(DString::default(), DString::new("\"foo\""));
  }

  #[test]
  fn test_node_from() {
    assert_eq!(
      Token::from(DString::new("\"foo\"")),
      Token::DString("\"foo\"".to_string())
    );
  }

  #[test]
  fn test_lex() {
    assert_eq!(
      DString::lex("\"foo\""),
      Some((DString::token("\"foo\""), String::new()))
    );
  }
}
