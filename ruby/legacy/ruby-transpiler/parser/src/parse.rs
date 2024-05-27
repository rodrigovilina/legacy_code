use {ast::Node, lexer::Token};

pub trait Parse {
  fn parse(tokens: &[Token]) -> Option<Node>;
}
