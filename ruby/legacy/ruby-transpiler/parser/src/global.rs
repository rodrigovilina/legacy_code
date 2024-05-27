use {
  crate::parse::Parse,
  ast::{Global, Node},
  lexer::Token,
};

impl Parse for Global {
  fn parse(tokens: &[Token]) -> Option<Node> {
    tokens.first().and_then(|token| match token {
      Token::Global(lexeme) => Some(Self::node(lexeme)),
      _ => None,
    })
  }
}
