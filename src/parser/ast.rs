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

#[derive(Debug)]
pub enum EarlyOperator {
    Reg,
}

/// A 1-adic operator
#[derive(Debug)]
pub enum EarlyMonOp {
    Minus,
    Not,
    BitNot,
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
        rhs_inclusive: bool,
    },
    Simple(Located<EarlyExpression>),
}

#[derive(Debug)]
pub enum EarlyExpression {
    Ident(EarlyIdentifier),
    Literal(EarlyLiteral),
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
    Tuple(Located<Vec<Located<EarlyExpression>>>),
}

#[derive(Debug)]
pub enum EarlyStatementLhs {
    Ident(Located<EarlyIdentifier>),
    Tuple(Located<Vec<Located<EarlyIdentifier>>>),
    // TODO Add slice as a LHS?
}

#[derive(Debug)]
pub enum EarlyStatement {
    Affect {
        lhs: EarlyStatementLhs,
        rhs: Box<Located<EarlyExpression>>,
    },
    IfThenElse {
        condition: Box<Located<EarlyExpression>>,
        if_block: Located<EarlyBlock>,
        else_block: Option<Located<EarlyBlock>>,
    },
}

pub type EarlyBlock = Vec<Located<EarlyStatement>>;

#[derive(Debug)]
pub struct EarlyRuntimeArg {
    pub name: Located<EarlyIdentifier>,
    pub typ: Option<Located<EarlyExpression>>,
}

#[derive(Debug)]
pub enum EarlyStaticType {
    Int,
    Bool,
}

#[derive(Debug)]
pub struct EarlyStaticArg {
    pub name: Located<EarlyIdentifier>,
    pub typ: Option<Located<EarlyStaticType>>,
}

#[derive(Debug)]
pub struct EarlyNode {
    pub name: Located<EarlyIdentifier>,
    pub static_args: Option<Located<Vec<Located<EarlyStaticArg>>>>,
    pub runtime_args: Located<Vec<Located<EarlyRuntimeArg>>>,
    pub runtime_outs: Located<Vec<Located<EarlyRuntimeArg>>>,
    pub block: Located<EarlyBlock>,
}
