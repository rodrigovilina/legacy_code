use {crate::unparse::Unparse, ast::Node};

impl Unparse for Node {
  fn unparse(&self) -> String {
    match self {
      Self::Begin(begin) => begin.unparse(),
      Self::Class(class) => class.unparse(),
      Self::Comment(comment) => comment.unparse(),
      Self::Const(const_) => const_.unparse(),
      Self::Dstr(dstr) => dstr.unparse(),
      Self::False(false_) => false_.unparse(),
      Self::Global(global) => global.unparse(),
      Self::Id(id) => id.unparse(),
      Self::Int(int) => int.unparse(),
      Self::LVar(lvar) => lvar.unparse(),
      Self::Module(module) => module.unparse(),
      Self::Nil(nil) => nil.unparse(),
      Self::Self_(self_) => self_.unparse(),
      Self::Send(send) => send.unparse(),
      Self::Symbol(symbol) => symbol.unparse(),
      Self::True(true_) => true_.unparse(),
    }
  }
}
