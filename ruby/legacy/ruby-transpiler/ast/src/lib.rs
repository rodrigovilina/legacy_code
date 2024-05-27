#![allow(dead_code)]

pub use {
  begin::Begin, class::Class, comment::Comment, const_::Const, dstr::Dstr,
  false_::False, global::Global, id::Id, int::Int, lvar::LVar, module::Module,
  nil::Nil, self_::Self_, send::Send, symbol::Symbol, true_::True,
};

pub mod begin;
pub mod class;
pub mod comment;
pub mod const_;
pub mod dstr;
pub mod false_;
pub mod global;
pub mod id;
pub mod int;
pub mod lvar;
pub mod module;
pub mod nil;
pub mod s;
pub mod self_;
pub mod send;
pub mod symbol;
pub mod true_;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Node {
  Begin(Begin),
  Class(Class),
  Comment(Comment),
  Const(Const),
  Dstr(Dstr),
  False(False),
  Global(Global),
  Id(Id),
  Int(Int),
  LVar(LVar),
  Module(Module),
  Nil(Nil),
  Self_(Self_),
  Send(Send),
  Symbol(Symbol),
  True(True),
}
