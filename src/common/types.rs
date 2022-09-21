use smallstr::SmallString;
use smallvec::SmallVec;

type TypeSmallVec<T> = SmallVec<[T; 4]>;
type TypeSmallStr = SmallString<[u8; 16]>;

#[derive(Debug)]
pub struct NodeType {}

#[derive(Debug)]
pub enum WireType {
    Dimensionable(TypeSmallVec<WireTypeInternal>),
    /// Used e.g. as a type for an immediate:
    /// an integer `n` is typed as `AtLeastEq(Constant(log_2(n)))`
    AtLeastEq(WireTypeInternal),
}

#[derive(Debug)]
pub enum WireTypeInternal {
    Constant(u64),
    Variable(String),
    MonOp(WireTypeMonOp, Box<WireTypeInternal>),
    BinOp(WireTypeBinOp,)
}

#[derive(Debug)]
pub enum WireTypeMonOp {

}

#[derive(Debug)]
pub enum WireTypeBinOp {
    Plus,
    Minus,
    Div,
    Times,
    Modulo,
}

pub struct WireTypeToken {

}

pub struct WireTypeContext {
    node: TypeSmallVec<WireTypeInternal>,
}

