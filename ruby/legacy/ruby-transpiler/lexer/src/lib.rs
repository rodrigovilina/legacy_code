use {
  class::Class, comment::Comment, const_::Const, dstring::DString, end::End,
  false_::False, global::Global, id::Id, int::Int, lvar::LVar, module::Module,
  nil::Nil, self_::Self_, symbol::Symbol, true_::True, wspace::WSpace,
};

mod class;
mod comment;
mod const_;
mod dstring;
mod end;
mod false_;
mod global;
mod id;
mod int;
mod lvar;
mod module;
mod nil;
mod self_;
mod split;
mod symbol;
mod true_;
mod wspace;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
  Class,
  Comment(String),
  Const(String),
  DString(String),
  End,
  False,
  Global(String),
  Id(String),
  Int(String),
  LVar(String),
  Nil,
  Module,
  Self_,
  Symbol(String),
  True,
  WSpace(String),
}

impl Token {
  fn lex_token(string: &str) -> Option<(Self, String)> {
    WSpace::lex(string)
      .or_else(|| Comment::lex(string))
      .or_else(|| Const::lex(string))
      .or_else(|| Int::lex(string))
      .or_else(|| False::lex(string))
      .or_else(|| True::lex(string))
      .or_else(|| Nil::lex(string))
      .or_else(|| Class::lex(string))
      .or_else(|| LVar::lex(string))
      .or_else(|| Module::lex(string))
      .or_else(|| End::lex(string))
      .or_else(|| Self_::lex(string))
      .or_else(|| Symbol::lex(string))
      .or_else(|| Id::lex(string))
      .or_else(|| Global::lex(string))
      .or_else(|| DString::lex(string))
  }

  /// # Panics
  ///
  /// Will panic if there is an unexpected token.
  #[must_use]
  pub fn lex(string: &str) -> Vec<Self> {
    let mut tokens: Vec<Self> = Vec::new();
    let mut string: String = string.to_string();

    loop {
      if string.is_empty() {
        return tokens;
      }
      match Self::lex_token(&string) {
        Some((token, rest)) => {
          tokens.push(token);
          string = rest;
        }
        None => {
          if string == "\n" {
            panic!("Unexpected token: \\n")
          } else {
            panic!("Unexpected token: {string}")
          }
        }
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_lex() {
    let lex = Token::lex;
    assert_eq!(lex("1234567890"), vec![Int::token("1234567890")]);
    assert_eq!(lex("-1234567890"), vec![Int::token("-1234567890")]);
    assert_eq!(lex("false"), vec![False::token()]);
    assert_eq!(lex("nil"), vec![Nil::token()]);
    assert_eq!(lex("true"), vec![True::token()]);
    assert_eq!(lex("self"), vec![Self_::token()]);
    assert_eq!(lex(":sym"), vec![Symbol::token(":sym")]);
    assert_eq!(lex("hello"), vec![Id::token("hello")]);
    assert_eq!(lex("module"), vec![Module::token()]);
    assert_eq!(lex("class"), vec![Class::token()]);
    assert_eq!(lex("\"hello\""), vec![DString::token("\"hello\"")]);
    assert_eq!(
      lex("puts \"hello\""),
      vec![Id::token("puts"), WSpace::token(" "), DString::token("\"hello\"")]
    );
    assert_eq!(lex(" "), vec![WSpace::token(" ")]);
    assert_eq!(lex("# foo"), vec![Comment::token("# foo")]);
    assert_eq!(
      lex("# foo\n"),
      vec![Comment::token("# foo"), WSpace::token("\n")]
    );
  }
}
