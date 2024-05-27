use {
  crate::parse::Parse,
  ast::{Node, Symbol},
  lexer::Token,
};

impl Parse for Symbol {
  fn parse(tokens: &[Token]) -> Option<Node> {
    tokens.first().and_then(|token| match token {
      Token::Symbol(lexeme) => Some(Self::node(lexeme)),
      _ => None,
    })
  }
}
