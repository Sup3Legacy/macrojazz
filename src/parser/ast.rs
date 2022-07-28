use crate::common::location::Located;

/// The type for all identifiers
pub type EarlyIdentifier = String;

/// The type for function parameters
pub type EarlyParams = Vec<Located<EarlyExpression>>;

pub type EarlyStaticParams = Vec<Located<EarlyStaticExpression>>;

pub type EarlyProgram = Vec<Located<EarlyNode>>;

/// A 2-adic operator
#[derive(Debug)]
pub enum EarlyBinOp {
    Concat,
    BitOr,
    BitAnd,
    BitXOr,
}

#[derive(Debug)]
pub enum EarlyStaticBinOp {
    Plus,
    Minus,
    Div,
    Mult,
    Modulo,
    Or,
    And,
    Equals,
    NEquals,
}

#[derive(Debug)]
pub enum EarlyOperator {
    Reg,
}

/// A 1-adic operator
#[derive(Debug)]
pub enum EarlyMonOp {
    BitNot,
}

#[derive(Debug)]
pub enum EarlyStaticMonOp {
    Minus,
    Not,
}

/// A immediate
#[derive(Debug)]
pub enum EarlyLiteral {
    Int(u64),
}

#[derive(Debug)]
pub enum EarlyStaticLiteral {
    Int(u64),
    Bool(bool),
}

#[derive(Debug)]
pub enum EarlyIndex {
    Range {
        lhs: Option<Located<EarlyStaticExpression>>,
        rhs: Option<Located<EarlyStaticExpression>>,
        rhs_inclusive: bool,
    },
    Simple(EarlyStaticExpression),
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
        static_params: Option<EarlyStaticParams>,
        runtime_params: EarlyParams,
        builtin: Located<bool>,
    },
    Index {
        lhs: Box<Located<EarlyExpression>>,
        index: Box<Located<EarlyIndex>>,
    },
    Tuple(Vec<Located<EarlyExpression>>),
    IfThenElse {
        condition: Box<Located<EarlyStaticExpression>>,
        if_block: Box<Located<EarlyExpression>>,
        else_block: Box<Located<EarlyExpression>>,
    },
}

#[derive(Debug)]
pub enum EarlyStaticExpression {
    Ident(EarlyIdentifier),
    Literal(EarlyStaticLiteral),
    MonOp {
        operand: Box<Located<EarlyStaticExpression>>,
        operator: Located<EarlyStaticMonOp>,
    },
    BinOp {
        lhs: Box<Located<EarlyStaticExpression>>,
        rhs: Box<Located<EarlyStaticExpression>>,
        operator: Located<EarlyStaticBinOp>,
    },
    IfThenElse {
        condition: Box<Located<EarlyStaticExpression>>,
        if_block: Box<Located<EarlyStaticExpression>>,
        else_block: Box<Located<EarlyStaticExpression>>,
    },
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
        condition: Box<Located<EarlyStaticExpression>>,
        if_block: Located<EarlyBlock>,
        else_block: Located<EarlyBlock>,
    },
}

pub type EarlyBlock = Vec<Located<EarlyStatement>>;

#[derive(Debug)]
pub struct EarlyRuntimeArg {
    pub name: Located<EarlyIdentifier>,
    pub typ: Option<Located<EarlyStaticExpression>>,
}

#[derive(Debug)]
pub enum EarlyStaticBaseType {
    Int,
    Bool,
}

#[derive(Debug)]
pub struct EarlyStaticType {
    pub base: Option<Located<EarlyStaticBaseType>>,
    pub refinement: Option<Located<EarlyStaticExpression>>,
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
