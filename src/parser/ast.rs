use crate::common::location::Located;

/// The type for all identifiers
pub type EarlyIdentifier = String;

/// The type for function parameters
pub type EarlyParams = Vec<Located<EarlyExpression>>;

pub type EarlyProgram = Vec<Located<EarlyNode>>;

/// A 2-adic operator
#[derive(Debug)]
pub enum EarlyBinOp {
    Plus,
    Minus,
    Div,
    Mult,
    Modulo,
    Or,
    And,
    Smaller,
    SmallerEq,
    Greater,
    GreaterEq,
    Equals,
    NEquals,
    Concat,
    BitOr,
    BitAnd,
    BitXOr,
}

/// A 1-adic operator
#[derive(Debug)]
pub enum EarlyMonOp {
    Minus,
    Not,
}

/// A immediate
#[derive(Debug)]
pub enum EarlyLiteral {
    Int(usize),
    Bool(bool),
}

#[derive(Debug)]
pub enum EarlyIndex {
    Range {
        lhs: Option<Located<EarlyExpression>>,
        rhs: Option<Located<EarlyExpression>>,
        rhs_iinclusive: bool,
    },
    Simple(Located<EarlyExpression>),
}

#[derive(Debug)]
pub enum EarlyExpression {
    Ident(Located<EarlyIdentifier>),
    Literal(Located<EarlyLiteral>),
    MonOp {
        operand: Box<Located<EarlyExpression>>,
        operator: Located<EarlyMonOp>,
    },
    BinOp {
        lhs: Box<Located<EarlyExpression>>,
        rhs: Box<Located<EarlyExpression>>,
        operator: Located<EarlyBinOp>,
    },
    FuncCall {
        func_name: Located<EarlyIdentifier>,
        static_params: EarlyParams,
        runtime_params: EarlyParams,
    },
    Index {
        lhs: Box<Located<EarlyExpression>>,
        index: Box<Located<EarlyIndex>>,
    },
}

#[derive(Debug)]
pub enum EarlyStamementLhs {
    Ident(Located<EarlyIdentifier>),
    // TODO Add slice as a LHS?
}

#[derive(Debug)]
pub enum EarlyStatement {
    Affect {
        lhs: EarlyStamementLhs,
        rhs: Box<Located<EarlyExpression>>,
    },
    IfThenElse {
        condition: Box<Located<EarlyExpression>>,
        if_block: Located<Block>,
        else_block: Option<Located<Block>>,
    }
}

pub type Block = Vec<Located<EarlyStatement>>;

#[derive(Debug)]
pub struct EarlyRuntimeArg {
    name: Located<EarlyIdentifier>,
    size: Option<usize>,
}

#[derive(Debug)]
pub enum EarlyStaticType {
    Int,
    Bool,
}

#[derive(Debug)]
pub struct EarlyStaticArg {
    name: Located<EarlyIdentifier>,
    typ: Option<Located<EarlyStaticType>>,
}

#[derive(Debug)]
pub struct EarlyNode {
    name: Located<EarlyIdentifier>,
    static_args: Located<Vec<Located<EarlyStaticArg>>>,
    runtime_args: Located<Vec<Located<EarlyRuntimeArg>>>,
    runtime_outs: Located<Vec<Located<EarlyRuntimeArg>>>,
    block: Located<Block>,
}
