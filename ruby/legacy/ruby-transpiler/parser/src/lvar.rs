use {
  crate::parse::Parse,
  ast::{LVar, Node},
  lexer::Token,
};

impl Parse for LVar {
  fn parse(tokens: &[Token]) -> Option<Node> {
    tokens.first().and_then(|token| match token {
      Token::LVar(lexeme) => Some(Self::node(lexeme)),
      _ => None,
    })
  }
}
