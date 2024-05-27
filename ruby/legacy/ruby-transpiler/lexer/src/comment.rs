use crate::{split, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct Comment {
  lexeme: String,
}

impl Comment {
  fn new(lexeme: &str) -> Self {
    Self { lexeme: lexeme.to_string() }
  }

  pub fn token(lexeme: &str) -> Token {
    Token::Comment(lexeme.to_string())
  }

  fn split(string: &str) -> Option<(String, String)> {
    split::split(string, "^#.*")
  }

  pub fn lex(string: &str) -> Option<(Token, String)> {
    Self::split(string).map(|(lexeme, rest)| (Self::token(&lexeme), rest))
  }
}

impl Default for Comment {
  fn default() -> Self {
    Self::new("# foo")
  }
}

impl From<Comment> for Token {
  fn from(global: Comment) -> Self {
    Comment::token(&global.lexeme)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_new() {
    assert_eq!(Comment::new("# foo"), Comment { lexeme: "# foo".to_string() });
  }

  #[test]
  fn test_token() {
    assert_eq!(Comment::token("# foo"), Token::Comment("# foo".to_string()));
  }

  #[test]
  fn test_split() {
    assert_eq!(
      Comment::split("# foo"),
      Some(("# foo".to_string(), String::new()))
    );
  }

  #[test]
  fn test_default() {
    assert_eq!(Comment::default(), Comment::new("# foo"));
  }

  #[test]
  fn test_node_from() {
    assert_eq!(
      Token::from(Comment::new("# foo")),
      Token::Comment("# foo".to_string())
    );
  }

  #[test]
  fn test_lex() {
    assert_eq!(
      Comment::lex("# foo"),
      Some((Comment::token("# foo"), String::new()))
    );
  }
}
