use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct LVar {
  lexeme: String,
}

impl LVar {
  fn new(lexeme: &str) -> Self {
    Self { lexeme: lexeme.to_string() }
  }

  pub fn token(lexeme: &str) -> Token {
    Token::LVar(lexeme.to_string())
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^@[a-z_][a-zA-Z0-9_]*")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(lexeme, rest)| (Self::token(&lexeme), rest))
  }
}

impl Default for LVar {
  fn default() -> Self {
    Self::new("@foo")
  }
}

impl From<LVar> for Token {
  fn from(global: LVar) -> Self {
    LVar::token(&global.lexeme)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(LVar::new("@foo"), LVar { lexeme: "@foo".to_string() });
  }

  #[test]
  fn test_token() {
    assert_eq!(LVar::token("@foo"), Token::LVar("@foo".to_string()));
  }

  #[test]
  fn test_split() {
    assert_eq!(LVar::split("@foo"), Some(("@foo".to_string(), String::new())));
  }

  #[test]
  fn test_default() {
    assert_eq!(LVar::default(), LVar::new("@foo"));
  }

  #[test]
  fn test_node_from() {
    assert_eq!(Token::from(LVar::new("@foo")), Token::LVar("@foo".to_string()));
  }

  #[test]
  fn test_lex() {
    assert_eq!(LVar::lex("@foo"), Some((LVar::token("@foo"), String::new())));
  }
}
