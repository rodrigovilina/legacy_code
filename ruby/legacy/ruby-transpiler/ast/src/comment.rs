use crate::Node;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Comment {
  pub lexeme: String,
}

impl Comment {
  #[must_use]
  pub fn new(lexeme: &str) -> Self {
    Self { lexeme: lexeme.to_string() }
  }

  #[must_use]
  pub fn node(lexeme: &str) -> Node {
    Node::Comment(Self::new(lexeme))
  }
}

impl Default for Comment {
  fn default() -> Self {
    Self::new("# foo")
  }
}

impl From<Comment> for Node {
  fn from(global: Comment) -> Self {
    Comment::node(&global.lexeme)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_false_new() {
    assert_eq!(Comment::new("# foo"), Comment { lexeme: "# foo".to_string() });
  }

  #[test]
  fn test_false_node() {
    assert_eq!(Comment::node("# foo"), Node::Comment(Comment::new("# foo")));
  }

  #[test]
  fn test_false_default() {
    assert_eq!(Comment::default(), Comment::new("# foo"));
  }

  #[test]
  fn test_false_from() {
    assert_eq!(Node::from(Comment::new("# foo")), Comment::node("# foo"));
  }
}
