use crate::common::{location::Located, source::SourceId};

pub type EarlyLocated<T> = Located<(), T>;

impl<T: Clone> EarlyLocated<T> {
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

pub struct EarlyRuntimeTypeDef {}

pub struct EarlyStaticTypeDef {
    ident: EarlyLocated<EarlyIdentifier>,
}

/// A 2-adic operator
#[derive(Clone, Debug)]
pub enum EarlyBinOp {
    Concat,
    BitOr,
    BitAnd,
    BitXOr,
}

#[derive(Clone, Debug)]
pub enum EarlyStaticBinOp {
    Plus,
    Minus,
    Div,
    Mult,
    Modulo,
    Exp,
    Or,
    And,
    Equals,
    NEquals,

    Ge,
    Gt,
    Le,
    Lt,
}

#[derive(Clone, Debug)]
pub enum EarlyOperator {
    Reg,
}

/// A 1-adic operator
#[derive(Clone, Debug)]
pub enum EarlyMonOp {
    BitNot,
}

#[derive(Clone, Debug)]
pub enum EarlyStaticMonOp {
    Minus,
    Not,
}

/// A immediate
#[derive(Clone, Debug)]
pub enum EarlyLiteral {
    Int(u64),
}

#[derive(Clone, Debug)]
pub enum EarlyStaticLiteral {
    Int(u64),
    Bool(bool),
}

#[derive(Clone, Debug)]
pub enum EarlyIndex {
    Range {
        lhs: Option<EarlyLocated<EarlyStaticExpression>>,
        rhs: Option<EarlyLocated<EarlyStaticExpression>>,
        rhs_inclusive: bool,
    },
    Simple(EarlyStaticExpression),
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum EarlyLhs {
    Ident(EarlyIdentifier),
    Tuple(Vec<EarlyLocated<EarlyIdentifier>>),
    // TODO Add slice as a LHS?
}

#[derive(Clone, Debug)]
pub struct EarlyArg {
    pub name: EarlyLocated<EarlyIdentifier>,
    pub typ: Option<EarlyLocated<EarlyType>>,
}

pub type EarlyNodeInputType = Vec<EarlyLocated<EarlyArg>>;

#[derive(Clone, Debug)]
pub enum EarlyType {
    // A tuple type
    Tuple(Vec<EarlyLocated<EarlyType>>),
    // The expression is the wire size. TODO generalize to multiple dimensions
    Base(EarlyLocated<EarlyStaticExpression>),
    Unit,
    // `_` type
    // Infered,
}

#[derive(Clone, Debug)]
pub enum EarlyStaticBaseType {
    Int,
    Bool,
}

#[derive(Clone, Debug)]
pub struct EarlyStaticType {
    pub base: Option<EarlyLocated<EarlyStaticBaseType>>,
    pub refinement: Option<EarlyLocated<EarlyStaticExpression>>,
}

#[derive(Clone, Debug)]
pub struct EarlyStaticArg {
    pub name: EarlyLocated<EarlyIdentifier>,
    pub typ: Option<EarlyLocated<EarlyStaticType>>,
}

#[derive(Clone, Debug)]
pub struct EarlyNode {
    pub name: EarlyLocated<EarlyIdentifier>,
    pub static_args: Option<EarlyLocated<Vec<EarlyLocated<EarlyStaticArg>>>>,
    pub runtime_args: EarlyLocated<EarlyNodeInputType>,
    pub runtime_outs: EarlyLocated<EarlyType>,
    pub block: EarlyLocated<EarlyExpression>,
}
