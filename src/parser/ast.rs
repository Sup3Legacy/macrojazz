use crate::common::{location::Located, source::SourceId};

pub type EarlyLocated<T> = Located<(), T>;

impl<T> EarlyLocated<T> {
    pub fn new(inner: T, file: SourceId, start: usize, end: usize) -> Self {
        Self::__new(inner, (), file, start, end)
    }

    pub fn from_range(inner: T, file: SourceId, range: std::ops::Range<usize>) -> Self {
        Self::__from_range(inner, (), file, range)
    }
}

/// The type for all identifiers
pub type EarlyIdentifier = String;

/// The type for function parameters
pub type EarlyParams = Vec<EarlyLocated<EarlyExpression>>;

pub type EarlyStaticParams = Vec<EarlyLocated<EarlyStaticExpression>>;

pub type EarlyProgram = Vec<EarlyLocated<EarlyNode>>;

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
        lhs: Option<EarlyLocated<EarlyStaticExpression>>,
        rhs: Option<EarlyLocated<EarlyStaticExpression>>,
        rhs_inclusive: bool,
    },
    Simple(EarlyStaticExpression),
}

#[derive(Debug)]
pub enum EarlyExpression {
    Ident(EarlyIdentifier),
    Literal(EarlyLiteral),
    MonOp {
        operand: Box<EarlyLocated<EarlyExpression>>,
        operator: EarlyLocated<EarlyMonOp>,
    },
    BinOp {
        lhs: Box<EarlyLocated<EarlyExpression>>,
        rhs: Box<EarlyLocated<EarlyExpression>>,
        operator: EarlyLocated<EarlyBinOp>,
    },
    FuncCall {
        func_name: EarlyLocated<EarlyIdentifier>,
        static_params: Option<EarlyStaticParams>,
        runtime_params: EarlyParams,
        builtin: EarlyLocated<bool>,
    },
    Index {
        lhs: Box<EarlyLocated<EarlyExpression>>,
        index: Box<EarlyLocated<EarlyIndex>>,
    },
    Tuple(Vec<EarlyLocated<EarlyExpression>>),
    IfThenElse {
        condition: Box<EarlyLocated<EarlyStaticExpression>>,
        if_block: Box<EarlyLocated<EarlyExpression>>,
        else_block: Box<EarlyLocated<EarlyExpression>>,
    },
    Let {
        lhs: EarlyLocated<EarlyLhs>,
        rhs: Box<EarlyLocated<EarlyExpression>>,
        scope: Box<EarlyLocated<EarlyExpression>>,
    },
}

#[derive(Debug)]
pub enum EarlyStaticExpression {
    Ident(EarlyIdentifier),
    Literal(EarlyStaticLiteral),
    MonOp {
        operand: Box<EarlyLocated<EarlyStaticExpression>>,
        operator: EarlyLocated<EarlyStaticMonOp>,
    },
    BinOp {
        lhs: Box<EarlyLocated<EarlyStaticExpression>>,
        rhs: Box<EarlyLocated<EarlyStaticExpression>>,
        operator: EarlyLocated<EarlyStaticBinOp>,
    },
    IfThenElse {
        condition: Box<EarlyLocated<EarlyStaticExpression>>,
        if_block: Box<EarlyLocated<EarlyStaticExpression>>,
        else_block: Box<EarlyLocated<EarlyStaticExpression>>,
    },
}

#[derive(Debug)]
pub enum EarlyLhs {
    Ident(EarlyIdentifier),
    Tuple(Vec<EarlyLocated<EarlyIdentifier>>),
    // TODO Add slice as a LHS?
}

#[derive(Debug)]
pub struct EarlyArg {
    pub name: EarlyLocated<EarlyIdentifier>,
    pub typ: Option<EarlyLocated<EarlyType>>,
}

#[derive(Debug)]
pub struct EarlyNodeInputType(pub Vec<EarlyLocated<EarlyArg>>);

#[derive(Debug)]
pub enum EarlyWireSize {
    Expression(EarlyStaticExpression),
    Inferred,
}

#[derive(Debug)]
pub enum EarlyType {
    // A tuple type
    Tuple(Vec<EarlyLocated<EarlyType>>),
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
    pub base: Option<EarlyLocated<EarlyStaticBaseType>>,
    pub refinement: Option<EarlyLocated<EarlyStaticExpression>>,
}

#[derive(Debug)]
pub struct EarlyStaticArg {
    pub name: EarlyLocated<EarlyIdentifier>,
    pub typ: Option<EarlyLocated<EarlyStaticType>>,
}

#[derive(Debug)]
pub struct EarlyNode {
    pub name: EarlyLocated<EarlyIdentifier>,
    pub static_args: Option<EarlyLocated<Vec<EarlyLocated<EarlyStaticArg>>>>,
    pub runtime_args: EarlyLocated<EarlyNodeInputType>,
    pub runtime_outs: EarlyLocated<EarlyType>,
    pub block: EarlyLocated<EarlyExpression>,
}
