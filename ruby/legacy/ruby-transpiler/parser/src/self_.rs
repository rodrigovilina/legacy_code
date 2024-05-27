use {
  crate::parse::Parse,
  ast::{Node, Self_},
  lexer::Token,
};

impl Parse for Self_ {
  fn parse(tokens: &[Token]) -> Option<Node> {
    tokens.first().and_then(|token| match token {
      Token::Self_ => Some(Self::node()),
      _ => None,
    })
  }
}
