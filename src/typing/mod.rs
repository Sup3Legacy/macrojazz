use crate::parser::*;
mod smt;
mod warnings;
pub mod static_expr;
pub mod expr;
pub mod type_tree;

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

