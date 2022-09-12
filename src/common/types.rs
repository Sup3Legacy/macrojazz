use smallstr::SmallString;
use smallvec::SmallVec;

type TypeSmallVec<T> = SmallVec<[T; 4]>;
type TypeSmallStr = SmallString<[u8; 16]>;

#[derive(Debug)]
pub struct NodeType {}

#[derive(Debug)]
pub struct WireType {
    dims: TypeSmallVec<WireTypeInternal>,
}

#[derive(Debug)]
pub enum WireTypeInternal {
    Constant(u64),
    Variable(String),
    
    /// Used e.g. as a type for an immediate:
    /// an integer `n` is typed as `AtLeastEq()`
    /// FIXME this does not belong here
    AtLeastEq(Box<WireTypeInternal>),
}
