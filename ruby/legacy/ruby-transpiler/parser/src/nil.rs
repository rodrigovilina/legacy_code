use {
  crate::parse::Parse,
  ast::{Nil, Node},
  lexer::Token,
};

impl Parse for Nil {
  fn parse(tokens: &[Token]) -> Option<Node> {
    tokens.first().and_then(|token| match token {
      Token::Nil => Some(Self::node()),
      _ => None,
    })
  }
}
