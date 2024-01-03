use super::static_expr::*;
use crate::parser::ast::*;
use crate::typing::*;
use crate::Located;
use std::collections::HashMap;
use z3::ast::Ast;
use z3::ast::{Bool, Int};
use z3::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TTBinOp {
    Plus,
    Minus,
    Mult,
    Exp,
    Div,
    Modulo,

    Or,
    And,
    XOr,

    Ge,
    Gt,
    Le,
    Lt,

    Eq,
    NEq,
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
            StaticBinop::Modulo => Self::Modulo,
            StaticBinop::Ge => Self::Ge,
            StaticBinop::Gt => Self::Gt,
            StaticBinop::Le => Self::Le,
            StaticBinop::Lt => Self::Lt,
            StaticBinop::Eq => Self::Eq,
            StaticBinop::NEq => Self::NEq,
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

pub enum TTz3<'a> {
    Int(Int<'a>),
    Bool(Bool<'a>),
}

impl<'a> TTz3<'a> {
    pub fn bit(ctx: &'a z3::Context) -> Self {
        TTz3::Int(Int::from_u64(ctx, 1))
    }

    pub fn u64(ctx: &'a z3::Context) -> Self {
        TTz3::Int(Int::from_u64(ctx, 64))
    }
}

impl TT {
    // TODO: z3_env will hold the mapping from identifier to z3 context variable references
    /// Turn TT to z3 term
    pub fn to_z3<'a>(self, ctx: &'a z3::Context, z3_env: &HashMap<String, TTz3<'a>>) -> TTz3<'a> {
        match self {
            TT::Ident(_) => todo!(),
            TT::IntLiteral(i) => TTz3::Int(z3::ast::Int::from_u64(ctx, i)),
            TT::BoolLiteral(b) => TTz3::Bool(z3::ast::Bool::from_bool(ctx, b)),
            TT::MonOp { operand, operator } => {
                let operand_z3 = operand.to_z3(ctx, z3_env);
                match (operand_z3, operator) {
                    (TTz3::Int(i), TTMonOp::Neg) => TTz3::Int(i.unary_minus()),
                    (TTz3::Bool(b), TTMonOp::Not) => TTz3::Bool(b.not()),
                    _ => unreachable!(),
                }
            }
            TT::BinOp { lhs, rhs, operator } => {
                let lhs_z3 = lhs.to_z3(ctx, z3_env);
                let rhs_z3 = rhs.to_z3(ctx, z3_env);
                match (lhs_z3, operator, rhs_z3) {
                    (TTz3::Int(i), TTBinOp::Plus, TTz3::Int(j)) => {
                        TTz3::Int(Int::add(ctx, &[&i, &j]))
                    }
                    (TTz3::Int(i), TTBinOp::Minus, TTz3::Int(j)) => {
                        TTz3::Int(Int::sub(ctx, &[&i, &j]))
                    }
                    (TTz3::Int(i), TTBinOp::Mult, TTz3::Int(j)) => {
                        TTz3::Int(Int::mul(ctx, &[&i, &j]))
                    }
                    // i.power(j) returns a Real...
                    (TTz3::Int(i), TTBinOp::Exp, TTz3::Int(j)) => todo!(),
                    (TTz3::Int(i), TTBinOp::Div, TTz3::Int(j)) => TTz3::Int(i.div(&j)),
                    (TTz3::Int(i), TTBinOp::Modulo, TTz3::Int(j)) => TTz3::Int(i.modulo(&j)),

                    (TTz3::Int(i), TTBinOp::Ge, TTz3::Int(j)) => TTz3::Bool(i.ge(&j)),
                    (TTz3::Int(i), TTBinOp::Gt, TTz3::Int(j)) => TTz3::Bool(i.gt(&j)),
                    (TTz3::Int(i), TTBinOp::Le, TTz3::Int(j)) => TTz3::Bool(i.le(&j)),
                    (TTz3::Int(i), TTBinOp::Lt, TTz3::Int(j)) => TTz3::Bool(i.lt(&j)),

                    (TTz3::Int(i), TTBinOp::Eq, TTz3::Int(j)) => TTz3::Bool(i._eq(&j)),
                    (TTz3::Int(i), TTBinOp::NEq, TTz3::Int(j)) => TTz3::Bool(i._eq(&j).not()),

                    (TTz3::Bool(b), TTBinOp::Or, TTz3::Bool(v)) => {
                        TTz3::Bool(Bool::or(ctx, &[&b, &v]))
                    }
                    (TTz3::Bool(b), TTBinOp::And, TTz3::Bool(v)) => {
                        TTz3::Bool(Bool::and(ctx, &[&b, &v]))
                    }
                    (TTz3::Bool(b), TTBinOp::XOr, TTz3::Bool(v)) => TTz3::Bool(b.xor(&v)),

                    (TTz3::Bool(b), TTBinOp::Eq, TTz3::Bool(v)) => TTz3::Bool(b._eq(&v)),
                    (TTz3::Bool(b), TTBinOp::NEq, TTz3::Bool(v)) => TTz3::Bool(b._eq(&v).not()),
                    _ => unreachable!(),
                }
            }
            TT::Ife {
                condition,
                if_block,
                else_block,
            } => {
                let condition_z3 = condition.to_z3(ctx, z3_env);
                let if_z3 = if_block.to_z3(ctx, z3_env);
                let else_z3 = else_block.to_z3(ctx, z3_env);

                // TODO: ife in static expressions not supported for now
                todo!()
            }
        }
    }
}
