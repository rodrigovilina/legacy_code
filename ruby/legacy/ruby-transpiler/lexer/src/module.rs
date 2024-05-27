use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct Module {}

impl Module {
  const fn new() -> Self {
    Self {}
  }

  pub const fn token() -> Token {
    Token::Module
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^module")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(_lexeme, rest)| (Self::token(), rest))
  }
}

impl Default for Module {
  fn default() -> Self {
    Self::new()
  }
}

impl From<Module> for Token {
  fn from(_: Module) -> Self {
    Module::token()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(Module::new(), Module {});
  }

  #[test]
  fn test_token() {
    assert_eq!(Module::token(), Token::Module);
  }

  #[test]
  fn test_split() {
    assert_eq!(
      Module::split("module"),
      Some(("module".to_string(), String::new()))
    );
  }

  #[test]
  fn test_default() {
    assert_eq!(Module::default(), Module {});
  }

  #[test]
  fn test_node_from() {
    assert_eq!(Token::from(Module::new()), Token::Module);
  }

  #[test]
  fn test_lex() {
    assert_eq!(Module::lex("module"), Some((Token::Module, String::new())));
  }
}
