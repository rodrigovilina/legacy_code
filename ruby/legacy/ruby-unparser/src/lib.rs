use nonempty::NonEmpty;

mod _false;
mod _nil;
mod _self;
mod _true;
mod alias;
mod begin;
mod class;
mod constant;
mod def;
mod global_variable;
mod id;
mod indent;
mod range;
mod symbol;

pub trait Unparseable {
  fn unparse(&self) -> String;
}

impl Unparseable for Node {
  fn unparse(&self) -> String {
    match self {
      Node::Alias { .. } => alias::unparse_alias(&self),
      Node::Begin { .. } => begin::unparse_begin(&self),
      Node::Class { .. } => class::unparse_class(&self),
      Node::Constant { .. } => constant::unparse_constant(&self),
      Node::Def { .. } => def::unparse_def(&self),
      Node::False => _false::unparse_false(&self),
      Node::GlobalVariable { .. } => global_variable::unparse_global_variable(&self),
      Node::Id { .. } => id::unparse_id(&self),
      Node::Nil => _nil::unparse_nil(&self),
      Node::SubClass { .. } => class::unparse_subclass(&self),
      Node::Symbol { .. } => symbol::unparse_symbol(&self),
      Node::True => _true::unparse_true(&self),
      Node::Zelf => _self::unparse_self(&self),
      _ => todo!(),
    }
  }
}

type NodeBox = Box<Node>;

#[derive(Debug, PartialEq)]
pub enum Node {
  Alias(NodeBox, NodeBox),
  Begin { body: Vec<NodeBox> },
  Class { name: NodeBox },
  Constant { top_level: bool, name: NonEmpty<String> },
  Def { name: NodeBox, body: NodeBox },
  False,
  GlobalVariable { name: String },
  Id { name: String },
  Nil,
  Range(NodeBox, NodeBox),
  SubClass { name: NodeBox, superclass: NodeBox },
  Symbol { name: String },
  True,
  Zelf,
}
