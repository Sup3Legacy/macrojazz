mod smt;
mod warnings;
mod static_expr;

pub enum StaticErrorType {
    Error,
    Int,
    Bool
}

pub enum StaticType {
    Int,
    Bool
}
