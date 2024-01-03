use crate::parser::ast::*;
use crate::typing::*;
use crate::Located;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum StaticTypingError {
    IdentNotFound,
    Monop,
    ConditionNotBool,
    ConditionTypeMismatch,
}

pub type StaticIdentifier = String;

pub type StaticLocated<T> = Located<(), T>;
pub type StaticTypedLocated<T> = Located<StaticType, T>;

impl<T: Clone> StaticLocated<T> {
    pub fn from_early(early: EarlyLocated<EarlyStaticExpression>, exp: T) -> Self {
        Self::__from_loc(exp, (), early.loc)
    }
}

impl<T: Clone> StaticTypedLocated<T> {
    pub fn from_early(early: EarlyLocated<EarlyStaticExpression>, exp: T, typ: StaticType) -> Self {
        Self::__from_loc(exp, typ, early.loc)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum StaticMonop {
    Not,
    Neg,
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum StaticBinop {
    Plus,
    Mult,
    Minus,
    Div,
    Exp,
    Modulo,

    Ge,
    Gt,
    Le,
    Lt,

    Or,
    And,
    XOr,

    Eq,
    NEq,
}

impl StaticBinop {
    pub fn from_early(eb: EarlyStaticBinOp) -> Self {
        match eb {
            EarlyStaticBinOp::Plus => Self::Plus,
            EarlyStaticBinOp::Minus => Self::Minus,
            EarlyStaticBinOp::Div => Self::Div,
            EarlyStaticBinOp::Mult => Self::Mult,
            EarlyStaticBinOp::Modulo => Self::Modulo,
            EarlyStaticBinOp::Exp => Self::Exp,
            EarlyStaticBinOp::Or => Self::Or,
            EarlyStaticBinOp::And => Self::And,
            EarlyStaticBinOp::Equals => Self::Eq,
            EarlyStaticBinOp::NEquals => Self::NEq,
            EarlyStaticBinOp::Ge => Self::Ge,
            EarlyStaticBinOp::Gt => Self::Gt,
            EarlyStaticBinOp::Le => Self::Le,
            EarlyStaticBinOp::Lt => Self::Lt,
        }
    }
}

#[derive(Clone, Debug)]
pub enum StaticExpression {
    Ident(StaticIdentifier),
    IntLiteral(u64),
    BoolLiteral(bool),
    MonOp {
        operand: Box<StaticTypedLocated<StaticExpression>>,
        operator: StaticLocated<StaticMonop>,
    },
    BinOp {
        lhs: Box<StaticTypedLocated<StaticExpression>>,
        rhs: Box<StaticTypedLocated<StaticExpression>>,
        operator: StaticLocated<StaticBinop>,
    },
    IfThenElse {
        condition: Box<StaticTypedLocated<StaticExpression>>,
        if_block: Box<StaticTypedLocated<StaticExpression>>,
        else_block: Box<StaticTypedLocated<StaticExpression>>,
    },
}

// The error should be an ariadne report (or maybe multiple ones)
pub fn type_static(
    exp: EarlyLocated<EarlyStaticExpression>,
    env: &mut HashMap<String, StaticType>,
) -> Result<StaticTypedLocated<StaticExpression>, StaticLocated<StaticTypingError>> {
    let loc = exp.loc;
    match exp.inner {
        EarlyStaticExpression::Ident(id) => env
            .get(&id)
            .ok_or(StaticLocated::__from_loc(
                StaticTypingError::IdentNotFound,
                (),
                loc.clone(),
            ))
            .map(|v| StaticTypedLocated::__from_loc(StaticExpression::Ident(id), *v, loc)),
        EarlyStaticExpression::Literal(lit) => match lit {
            EarlyStaticLiteral::Int(i) => Ok(StaticTypedLocated::__from_loc(
                StaticExpression::IntLiteral(i),
                StaticType::Int,
                loc,
            )),
            EarlyStaticLiteral::Bool(b) => Ok(StaticTypedLocated::__from_loc(
                StaticExpression::BoolLiteral(b),
                StaticType::Bool,
                loc,
            )),
        },
        EarlyStaticExpression::MonOp { operand, operator } => {
            let operand_exp = type_static(*operand, env)?;
            match (operand_exp.custom, operator.inner) {
                (StaticType::Int, EarlyStaticMonOp::Minus) => Ok(StaticTypedLocated::__from_loc(
                    StaticExpression::MonOp {
                        operand: Box::new(operand_exp),
                        operator: StaticLocated::__from_loc(
                            StaticMonop::Neg,
                            (),
                            operator.loc.clone(),
                        ),
                    },
                    StaticType::Int,
                    loc.clone(),
                )),
                (StaticType::Bool, EarlyStaticMonOp::Not) => Ok(StaticTypedLocated::__from_loc(
                    StaticExpression::MonOp {
                        operand: Box::new(operand_exp),
                        operator: StaticLocated::__from_loc(
                            StaticMonop::Not,
                            (),
                            operator.loc.clone(),
                        ),
                    },
                    StaticType::Bool,
                    loc.clone(),
                )),
                _ => Err(StaticLocated::__from_loc(
                    StaticTypingError::Monop,
                    (),
                    loc.clone(),
                )),
            }
        }
        EarlyStaticExpression::BinOp { lhs, rhs, operator } => {
            let typed_lhs = type_static(*lhs, env)?;
            let typed_rhs = type_static(*rhs, env)?;
            let op = StaticBinop::from_early(operator.inner);
            let return_type = match (typed_lhs.custom, typed_rhs.custom) {
                (StaticType::Int, StaticType::Int) => match op {
                    StaticBinop::Plus
                    | StaticBinop::Mult
                    | StaticBinop::Minus
                    | StaticBinop::Div
                    | StaticBinop::Exp
                    | StaticBinop::Modulo
                    | StaticBinop::Ge
                    | StaticBinop::Gt
                    | StaticBinop::Le
                    | StaticBinop::Lt => StaticType::Int,
                    StaticBinop::Eq | StaticBinop::NEq => StaticType::Bool,
                    // TODO: handle wrong types
                    _ => unreachable!(),
                },
                (StaticType::Bool, StaticType::Bool) => match op {
                    StaticBinop::Or
                    | StaticBinop::And
                    | StaticBinop::XOr
                    | StaticBinop::Eq
                    | StaticBinop::NEq => StaticType::Bool,
                    // TODO  Same
                    _ => unreachable!(),
                },
                // TODO  Same
                _ => unreachable!(),
            };
            Ok(StaticTypedLocated::__from_loc(
                    StaticExpression::BinOp {
                        lhs: Box::new(typed_lhs),
                        rhs: Box::new(typed_rhs),
                        operator: StaticLocated::__from_loc(
                            op,
                            (),
                            operator.loc.clone(),
                        ),
                    },
                    return_type,
                    loc.clone(),
                ))
        }
        EarlyStaticExpression::IfThenElse {
            condition,
            if_block,
            else_block,
        } => {
            let condition_exp = type_static(*condition, env)?;
            if condition_exp.custom != StaticType::Bool {
                return Err(StaticLocated::__from_loc(
                    StaticTypingError::ConditionNotBool,
                    (),
                    condition_exp.loc.clone(),
                ));
            }
            let if_exp = type_static(*if_block, env)?;
            let else_exp = type_static(*else_block, env)?;
            if if_exp.custom == else_exp.custom {
                let return_type = if_exp.custom.clone();
                Ok(StaticTypedLocated::__from_loc(
                    StaticExpression::IfThenElse {
                        condition: Box::new(condition_exp),
                        if_block: Box::new(if_exp),
                        else_block: Box::new(else_exp),
                    },
                    return_type,
                    loc.clone(),
                ))
            } else {
                Err(StaticLocated::__from_loc(
                    StaticTypingError::ConditionTypeMismatch,
                    (),
                    loc.clone(),
                ))
            }
        }
    }
}
