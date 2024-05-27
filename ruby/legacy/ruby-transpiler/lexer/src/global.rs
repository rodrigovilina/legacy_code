use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct Global {
  lexeme: String,
}

impl Global {
  fn new(lexeme: &str) -> Self {
    Self { lexeme: lexeme.to_string() }
  }

  pub fn token(lexeme: &str) -> Token {
    Token::Global(lexeme.to_string())
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^\\$[a-z_][a-zA-Z0-9_]*")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(lexeme, rest)| (Self::token(&lexeme), rest))
  }
}

impl Default for Global {
  fn default() -> Self {
    Self::new("$foo")
  }
}

impl From<Global> for Token {
  fn from(global: Global) -> Self {
    Global::token(&global.lexeme)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(Global::new("$foo"), Global { lexeme: "$foo".to_string() });
  }

  #[test]
  fn test_token() {
    assert_eq!(Global::token("$foo"), Token::Global("$foo".to_string()));
  }

  #[test]
  fn test_split() {
    assert_eq!(
      Global::split("$foo"),
      Some(("$foo".to_string(), String::new()))
    );
  }

  #[test]
  fn test_default() {
    assert_eq!(Global::default(), Global::new("$foo"));
  }

  #[test]
  fn test_node_from() {
    assert_eq!(
      Token::from(Global::new("$foo")),
      Token::Global("$foo".to_string())
    );
  }

  #[test]
  fn test_lex() {
    assert_eq!(
      Global::lex("$foo"),
      Some((Global::token("$foo"), String::new()))
    );
  }
}
