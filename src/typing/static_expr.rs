use crate::Located;
use crate::parser::ast::*;
use crate::typing::*;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum StaticTypingError {
    IdentNotFound,
}

pub type StaticIdentifier = String;

pub type StaticLocated<T> = Located<StaticType, T>;

impl<T> StaticLocated<T> {
    pub fn from_early(early: EarlyLocated<EarlyStaticExpression>, exp: T, typ: StaticType) -> Self {
        Self::__from_loc(exp, typ, early.loc)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum StaticMonop {
    Not,
    Neg
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum StaticBinop {
    Plus,
    Mult,
    Minus,
    Div,
    Exp,

    Or,
    And,
    XOr,
}

#[derive(Debug)]
pub enum StaticExpression {
    Ident(StaticIdentifier),
    IntLiteral(u64),
    BoolLiteral(bool),
    MonOp {
        operand: Box<StaticLocated<StaticExpression>>,
        operator: StaticLocated<StaticMonop>,
    },
    BinOp {
        lhs: Box<StaticLocated<StaticExpression>>,
        rhs: Box<StaticLocated<StaticExpression>>,
        operator: StaticLocated<StaticBinop>,
    },
    IfThenElse {
        condition: Box<StaticLocated<StaticExpression>>,
        if_block: Box<StaticLocated<StaticExpression>>,
        else_block: Box<StaticLocated<StaticExpression>>,
    },
}

// The error should be an ariadne report (or maybe multiple ones)
pub fn type_static(
    exp: EarlyLocated<EarlyStaticExpression>,
    env: HashMap<String, StaticType>,
) -> Result<StaticExpression, StaticTypingError> {
    match exp.inner {
        EarlyStaticExpression::Ident(id) => env
            .get(&id)
            .ok_or(StaticTypingError::IdentNotFound)
            .map(|v| ),
        EarlyStaticExpression::Literal(lit) => 
            match lit {
                EarlyStaticLiteral::Int(_) => Ok(StaticType::Int),
                EarlyStaticLiteral::Bool(_) => Ok(StaticType::Bool),
            },
        EarlyStaticExpression::MonOp { operand, operator } => todo!(),
        EarlyStaticExpression::BinOp { lhs, rhs, operator } => todo!(),
        EarlyStaticExpression::IfThenElse {
            condition,
            if_block,
            else_block,
        } => todo!(),
    }
}
