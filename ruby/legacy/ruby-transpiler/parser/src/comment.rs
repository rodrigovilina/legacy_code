use {
  crate::parse::Parse,
  ast::{Comment, Node},
  lexer::Token,
};

impl Parse for Comment {
  fn parse(tokens: &[Token]) -> Option<Node> {
    tokens.first().and_then(|token| match token {
      Token::Comment(lexeme) => Some(Self::node(lexeme)),
      _ => None,
    })
  }
}

pub trait ParseFrom {
  fn parse_from(tokens: &[Token]) -> Option<(Node, Vec<Token>)>;
}

impl ParseFrom for Comment {
  fn parse_from(tokens: &[Token]) -> Option<(Node, Vec<Token>)> {
    tokens.first().and_then(|token| match token {
      Token::Comment(lexeme) => {
        Some((Self::node(lexeme), tokens[1..].to_vec()))
      }
      _ => None,
    })
  }
}
