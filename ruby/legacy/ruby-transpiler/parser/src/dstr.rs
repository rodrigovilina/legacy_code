use {
  crate::parse::Parse,
  ast::{Dstr, Node},
  lexer::Token,
};

impl Parse for Dstr {
  fn parse(tokens: &[Token]) -> Option<Node> {
    match tokens {
      [Token::DString(string)] => Some(Self::new(string).into()),
      _ => None,
    }
  }
}
