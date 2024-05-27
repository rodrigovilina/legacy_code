use {
  crate::parse::Parse,
  ast::{Int, Node},
  lexer::Token,
};

impl Parse for Int {
  fn parse(tokens: &[Token]) -> Option<Node> {
    tokens.first().and_then(|token| match token {
      Token::Int(lexeme) => Some(Self::node(lexeme)),
      _ => None,
    })
  }
}
