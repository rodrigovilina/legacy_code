use {
  crate::parse::Parse,
  ast::{Node, True},
  lexer::Token,
};

impl Parse for True {
  fn parse(tokens: &[Token]) -> Option<Node> {
    tokens.first().and_then(|token| match token {
      Token::True => Some(Self::node()),
      _ => None,
    })
  }
}
