use super::static_expr::*;
use crate::parser::ast::*;
use crate::typing::*;
use crate::Located;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TTBinOp {
    Plus,
    Minus,
    Mult,
    Exp,
    Div,

    Or,
    And,
    XOr,
}

impl TTBinOp {
    pub fn from_static(so: StaticBinop) -> Self {
        match so {
            StaticBinop::Plus => Self::Plus,
            StaticBinop::Mult => Self::Mult,
            StaticBinop::Minus => Self::Minus,
            StaticBinop::Div => Self::Div,
            StaticBinop::Exp => Self::Exp,
            StaticBinop::Or => Self::Or,
            StaticBinop::And => Self::And,
            StaticBinop::XOr => Self::XOr,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TTMonOp {
    Neg,
    Not,
}

impl TTMonOp {
    pub fn from_static(so: StaticMonop) -> Self {
        match so {
            StaticMonop::Neg => Self::Neg,
            StaticMonop::Not => Self::Not,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TT {
    Ident(String),
    IntLiteral(u64),
    BoolLiteral(bool),
    MonOp {
        operand: Box<TT>,
        operator: TTMonOp,
    },
    BinOp {
        lhs: Box<TT>,
        rhs: Box<TT>,
        operator: TTBinOp,
    },
    Ife {
        condition: Box<TT>,
        if_block: Box<TT>,
        else_block: Box<TT>,
    },
}

impl StaticTypedLocated<StaticExpression> {
    pub fn to_tt(self) -> TT {
        match self.inner {
            StaticExpression::Ident(id) => TT::Ident(id),
            StaticExpression::IntLiteral(i) => TT::IntLiteral(i),
            StaticExpression::BoolLiteral(b) => TT::BoolLiteral(b),
            StaticExpression::MonOp { operand, operator } => TT::MonOp {
                operand: Box::new(operand.to_tt()),
                operator: TTMonOp::from_static(operator.inner),
            },
            StaticExpression::BinOp { lhs, rhs, operator } => TT::BinOp {
                lhs: Box::new(lhs.to_tt()),
                rhs: Box::new(rhs.to_tt()),
                operator: TTBinOp::from_static(operator.inner),
            },
            StaticExpression::IfThenElse {
                condition,
                if_block,
                else_block,
            } => TT::Ife {
                condition: Box::new(condition.to_tt()),
                if_block: Box::new(if_block.to_tt()),
                else_block: Box::new(else_block.to_tt()),
            },
        }
    }
}
