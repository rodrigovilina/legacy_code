use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct WSpace {
  lexeme: String,
}

impl WSpace {
  fn new(lexeme: &str) -> Self {
    Self { lexeme: lexeme.to_string() }
  }

  pub fn token(lexeme: &str) -> Token {
    Token::WSpace(lexeme.to_string())
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^[ \t\n]+")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(lexeme, rest)| (Self::token(&lexeme), rest))
  }
}

impl Default for WSpace {
  fn default() -> Self {
    Self::new(" ")
  }
}

impl From<WSpace> for Token {
  fn from(_: WSpace) -> Self {
    WSpace::token(" ")
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(WSpace::new(" "), WSpace { lexeme: " ".to_string() });
  }

  #[test]
  fn test_token() {
    assert_eq!(WSpace::token(" "), Token::WSpace(" ".to_string()));
  }

  #[test]
  fn test_split() {
    assert_eq!(WSpace::split(" "), Some((" ".to_string(), String::new())));
  }

  #[test]
  fn test_default() {
    assert_eq!(WSpace::default(), WSpace::new(" "));
  }

  #[test]
  fn test_node_from() {
    assert_eq!(Token::from(WSpace::new(" ")), Token::WSpace(" ".to_string()));
  }

  #[test]
  fn test_lex() {
    assert_eq!(
      WSpace::lex(" "),
      Some((Token::WSpace(" ".to_string()), String::new()))
    );
  }
}
