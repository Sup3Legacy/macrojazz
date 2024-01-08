mod smt;
mod warnings;
mod static_expr;
pub mod expr;
mod type_tree;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum StaticErrorType {
    Error,
    Int,
    Bool
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum StaticType {
    Int,
    Bool
}
