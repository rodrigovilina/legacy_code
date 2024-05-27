use ast::Comment;

use crate::comment::ParseFrom;

use {
  crate::parse::Parse,
  ast::{Begin, Node},
  lexer::Token,
};

impl Parse for Begin {
  fn parse(tokens: &[Token]) -> Option<Node> {
    if tokens.is_empty() {
      return Some(Begin::new(vec![]).node());
    }
    Comment::parse_from(tokens).and_then(|(node, tokens)| {
      Comment::parse_from(&tokens)
        .map(|(second_node, _)| Begin::new(vec![node, second_node]).node())
    })
  }
}
