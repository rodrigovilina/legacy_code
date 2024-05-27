use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct Symbol {
  lexeme: String,
}

impl Symbol {
  fn new(lexeme: &str) -> Self {
    Self { lexeme: lexeme.to_string() }
  }

  pub fn token(lexeme: &str) -> Token {
    Token::Symbol(lexeme.to_string())
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^:[a-z_][a-zA-Z0-9_]*")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(lexeme, rest)| (Self::token(&lexeme), rest))
  }
}

impl Default for Symbol {
  fn default() -> Self {
    Self::new(":foo")
  }
}

impl From<Symbol> for Token {
  fn from(symbol: Symbol) -> Self {
    Symbol::token(&symbol.lexeme)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(Symbol::new(":foo"), Symbol { lexeme: ":foo".to_string() });
  }

  #[test]
  fn test_token() {
    assert_eq!(Symbol::token(":foo"), Token::Symbol(":foo".to_string()));
  }

  #[test]
  fn test_split() {
    assert_eq!(
      Symbol::split(":foo"),
      Some((":foo".to_string(), String::new()))
    );
  }

  #[test]
  fn test_default() {
    assert_eq!(Symbol::default(), Symbol::new(":foo"));
  }

  #[test]
  fn test_node_from() {
    assert_eq!(
      Token::from(Symbol::new(":foo")),
      Token::Symbol(":foo".to_string())
    );
  }

  #[test]
  fn test_lex() {
    assert_eq!(
      Symbol::lex(":foo"),
      Some((Symbol::token(":foo"), String::new()))
    );
  }
}
