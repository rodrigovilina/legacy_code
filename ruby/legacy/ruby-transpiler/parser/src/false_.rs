use {
  crate::parse::Parse,
  ast::{False, Node},
  lexer::Token,
};

impl Parse for False {
  fn parse(tokens: &[Token]) -> Option<Node> {
    tokens.first().and_then(|token| match token {
      Token::False => Some(Self::node()),
      _ => None,
    })
  }
}
