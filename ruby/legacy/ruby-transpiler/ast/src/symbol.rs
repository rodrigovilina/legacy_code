use crate::Node;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Symbol {
  pub lexeme: String,
}

impl Symbol {
  #[must_use]
  pub fn new(lexeme: &str) -> Self {
    Self { lexeme: lexeme.to_string() }
  }

  #[must_use]
  pub fn node(lexeme: &str) -> Node {
    Node::Symbol(Self::new(lexeme))
  }
}

impl Default for Symbol {
  fn default() -> Self {
    Self::new(":foo")
  }
}

impl From<Symbol> for Node {
  fn from(symbol: Symbol) -> Self {
    Symbol::node(&symbol.lexeme)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_false_new() {
    assert_eq!(Symbol::new(":foo"), Symbol { lexeme: ":foo".to_string() });
  }

  #[test]
  fn test_false_node() {
    assert_eq!(Symbol::node(":foo"), Node::Symbol(Symbol::new(":foo")));
  }

  #[test]
  fn test_false_default() {
    assert_eq!(Symbol::default(), Symbol::new(":foo"));
  }

  #[test]
  fn test_false_from() {
    assert_eq!(Node::from(Symbol::new(":foo")), Symbol::node(":foo"));
  }
}
