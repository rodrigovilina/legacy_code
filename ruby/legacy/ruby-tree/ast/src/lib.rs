#[derive(Debug, PartialEq)]
pub struct Alias {
  pub from: AliasArg,
  pub to: AliasArg,
}

#[derive(Debug, PartialEq)]
pub enum AliasArg {
  Global(Global),
  Id(Id),
  Symbol(Symbol),
}

#[derive(Debug, PartialEq)]
pub struct Begin {
  pub statements: Vec<BeginStatement>,
}

#[derive(Debug, PartialEq)]
pub enum BeginStatement {
  Alias(Alias),
  True(True),
}

pub struct Class {
  pub name: Constant,
  pub superclass: Option<Constant>,
  pub body: Begin,
}

#[derive(Debug, PartialEq)]
pub struct Def {
  pub name: String,
  pub tipe: Option<Constant>,
  pub body: Begin,
}
