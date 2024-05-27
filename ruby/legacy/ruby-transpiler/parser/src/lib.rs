use {
  ast::{
    Begin, Class, Comment, Const, Dstr, False, Global, Int, LVar, Module, Nil,
    Node, Self_, Send, Symbol, True,
  },
  lexer::Token,
  parse::Parse,
};

mod begin;
mod class;
mod comment;
mod const_;
mod dstr;
mod false_;
mod global;
mod int;
mod lvar;
mod module;
mod nil;
mod parse;
mod self_;
mod send;
mod symbol;
mod true_;

const fn is_whitespace(token: &Token) -> bool {
  !matches!(token, Token::WSpace(_))
}

/// # Panics
///
/// Will panic if there is an unexpected token.
#[must_use]
pub fn parse(string: &str) -> Node {
  let tokens: Vec<Token> = Token::lex(string);
  let tokens: Vec<Token> = tokens.into_iter().filter(is_whitespace).collect();
  Begin::parse(&tokens)
    .or_else(|| False::parse(&tokens))
    .or_else(|| Comment::parse(&tokens))
    .or_else(|| Const::parse(&tokens))
    .or_else(|| Global::parse(&tokens))
    .or_else(|| Int::parse(&tokens))
    .or_else(|| True::parse(&tokens))
    .or_else(|| Nil::parse(&tokens))
    .or_else(|| Self_::parse(&tokens))
    .or_else(|| Symbol::parse(&tokens))
    .or_else(|| LVar::parse(&tokens))
    .or_else(|| Dstr::parse(&tokens))
    .or_else(|| Module::parse(&tokens))
    .or_else(|| Class::parse(&tokens))
    .or_else(|| Begin::parse(&tokens))
    .or_else(|| Send::parse(&tokens))
    .unwrap_or_else(|| panic!("Unexpected token: {tokens:?}"))
}

#[cfg(test)]
mod tests {
  use {
    super::*,
    ast::{s, Id},
  };

  #[test]
  fn test_parse() {
    assert_eq!(parse("1234567890"), s!(int, "1234567890").into());
    assert_eq!(parse("-1234567890"), s!(int, "-1234567890").into());
    assert_eq!(parse("false"), s!(false).into());
    assert_eq!(parse("nil"), s!(nil).into());
    assert_eq!(parse("self"), s!(self).into());
    assert_eq!(parse(":sym"), s!(sym, ":sym").into());
    assert_eq!(parse("true"), s!(true).into());
    assert_eq!(parse(" "), s!(begin).into());
    assert_eq!(parse("puts"), s!(send, nil, "puts").into());
    assert_eq!(
      parse("puts \"hello world\""),
      s!(send, nil, "puts", s!(dstr, "\"hello world\"")).into()
    );
    assert_eq!(
      parse("module Hola end"),
      s!(module s!(const "Hola"), nil).into(),
    );
  }
}
