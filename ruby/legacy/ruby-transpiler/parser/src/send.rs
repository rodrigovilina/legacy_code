use ast::{Dstr, Id};

use {
  crate::parse::Parse,
  ast::{Node, Send},
  lexer::Token,
};

impl Parse for Send {
  fn parse(tokens: &[Token]) -> Option<Node> {
    match tokens {
      [Token::Id(name)] => Some(Self::new(name, None::<Id>).into()),
      [Token::Id(name), Token::DString(arg)] => {
        Some(Self::new(name, Some(Dstr::new(arg))).into())
      }
      _ => None,
    }
  }
}

// impl Parse for SendArg {
//   fn parse(tokens: &[Token]) -> Option<Node> {
//     match tokens {
//       [Token::Id(name)] => Some(Id::new(name).into()),
//       [Token::DString(arg)] => Some(Dstr::new(arg).into()),
//       _ => None,
//     }
//   }
// }

#[cfg(test)]
mod tests {
  use ast::s;

  use super::*;

  #[test]
  fn test_parse() {
    assert_eq!(
      Send::parse(&[Token::Id("puts".to_string())]),
      Some(s!(send, nil, "puts").into())
    );
  }
}
