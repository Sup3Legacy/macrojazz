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

    Or,
    And,
    XOr,
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
        EarlyStaticExpression::BinOp { lhs, rhs, operator } => todo!(),
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
