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
    Let {
        lhs: Located<EarlyLhs>,
        rhs: Box<Located<EarlyExpression>>,
        scope: Box<Located<EarlyExpression>>,
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
pub enum EarlyLhs {
    Ident(EarlyIdentifier),
    Tuple(Vec<Located<EarlyIdentifier>>),
    // TODO Add slice as a LHS?
}

#[derive(Debug)]
pub struct EarlyArg {
    pub name: Located<EarlyIdentifier>,
    pub typ: Option<Located<EarlyType>>,
}

#[derive(Debug)]
pub struct EarlyNodeInputType(pub Vec<Located<EarlyArg>>);

#[derive(Debug)]
pub enum EarlyWireSize {
    Expression(EarlyStaticExpression),
    Inferred,
}

#[derive(Debug)]
pub enum EarlyType {
    // A tuple type
    Tuple(Vec<Located<EarlyType>>),
    // The expression is the wire size. TODO generalize to multiple dimensions
    Base(EarlyWireSize),
    Unit,
    // `_` type
    // Infered,
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
    pub runtime_args: Located<EarlyNodeInputType>,
    pub runtime_outs: Located<EarlyType>,
    pub block: Located<EarlyExpression>,
}
